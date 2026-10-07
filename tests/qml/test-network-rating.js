const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

// Execute the actual rating functions and event handlers. Qt event delivery is
// checked separately in the offscreen component probe; this suite needs Node only.
const source = fs.readFileSync(path.resolve(__dirname, '../../inst/qml/common/NetworkRating.qml'), 'utf8');
function extract(pattern) {
    const match = source.match(pattern);
    assert.ok(match, 'rating component contract changed: ' + pattern);
    return match[1];
}
const functions = ['synchronize', 'recordDisplayed', 'finishNumericEdit'].map(name =>
    extract(new RegExp('^    (function ' + name + '\\([^]*?^    })', 'm'))).join('\n');
const editedHandler = extract(/^        onTextEdited: {([^]*?)^        }/m);
const finishedHandler = extract(/^        onEditingFinished: (.*)$/m);
const acceptedHandler = extract(/^        function onAccepted\(\) { (.*) }$/m);
const buttonHandler = extract(/^        onClicked: (.*)$/m);

function environment(value = 0) {
    const display = {
        initialized: true, userEdited: false, editedText: '',
        displayValue: String(value), control: {acceptableInput: true}
    };
    let numeric = value;
    Object.defineProperty(display, 'value', {
        get: () => numeric,
        set: value => { numeric = Number(value); display.displayValue = String(numeric); }
    });
    const context = vm.createContext({
        display, slider: {}, direction: {currentIndex: 0}, boundField: {value},
        maximum: 1, signedRating: false, magnitudeMode: false, synchronizing: false,
        recordings: 0, rated: false,
        recorded() { context.recordings++; context.rated = true; }
    });
    context.rating = context;
    vm.runInContext(functions, context);
    // QML resolves these unqualified names against the DoubleField instance.
    vm.runInContext('function editEvent() { with (display) {' + editedHandler + '} }', context);
    context.synchronize();
    return {
        context, display,
        type(text, acceptable = true) {
            display.displayValue = text;
            display.control.acceptableInput = acceptable;
            context.editEvent();
        },
        blur() { vm.runInContext(finishedHandler, context); },
        accept() { vm.runInContext(acceptedHandler, context); }
    };
}

let passed = 0;
function test(name, check) {
    try { check(); passed++; }
    catch (error) { console.error(name); throw error; }
}

test('untouched numeric fields stay unrated through repeated focus loss', () => {
    const e = environment();
    e.blur(); e.blur();
    assert.equal(e.context.boundField.value, 0);
    assert.equal(e.context.recordings, 0);
    assert.equal(e.context.rated, false);
});

test('a valid numeric edit records once when focus leaves', () => {
    const e = environment();
    e.type('0.7'); e.blur(); e.blur();
    assert.equal(e.context.boundField.value, 0.7);
    assert.equal(e.context.recordings, 1);
    assert.equal(e.context.rated, true);
});

test('explicitly typing zero confirms it even when the number is unchanged', () => {
    const e = environment();
    e.type('0'); e.blur();
    assert.equal(e.context.boundField.value, 0);
    assert.equal(e.context.recordings, 1);
    assert.equal(e.context.rated, true);
});

test('explicit numeric acceptance can confirm unchanged zero without a duplicate blur write', () => {
    const e = environment();
    e.accept(); e.blur();
    assert.equal(e.context.boundField.value, 0);
    assert.equal(e.context.recordings, 1);
    assert.equal(e.context.rated, true);
});

test('acceptance after an edit records once before editingFinished', () => {
    const e = environment();
    e.type('0.4'); e.accept(); e.blur();
    assert.equal(e.context.boundField.value, 0.4);
    assert.equal(e.context.recordings, 1);
});

test('invalid edits do not confirm a rating and a subsequent valid edit can be recorded', () => {
    const e = environment();
    e.type('2', false); e.blur();
    assert.equal(e.context.recordings, 0);
    assert.equal(e.context.boundField.value, 0);
    e.type('0.2'); e.blur();
    assert.equal(e.context.boundField.value, 0.2);
    assert.equal(e.context.recordings, 1);
});

for (const event of ['blur', 'accept']) {
    test('restoring invalid text before ' + event + ' does not confirm the previous zero', () => {
        const e = environment();
        e.type('2', false);
        e.display.value = 0;
        e.display.control.acceptableInput = true;
        e[event]();
        assert.equal(e.context.recordings, 0);
        assert.equal(e.context.rated, false);
    });
}

test('a cleared field restored to its default does not confirm zero', () => {
    const e = environment();
    e.type('', false);
    e.display.value = 0;
    e.display.control.acceptableInput = true;
    e.blur();
    assert.equal(e.context.recordings, 0);
});

test('programmatic value and scale synchronization do not record a rating', () => {
    const e = environment();
    e.context.boundField.value = 0.3;
    e.context.synchronize(); e.blur();
    e.context.maximum = 100;
    e.context.synchronize(); e.blur();
    assert.equal(e.display.value, 30);
    assert.equal(e.context.boundField.value, 0.3);
    assert.equal(e.context.recordings, 0);
});

test('synchronization cancels pending text rather than recording its replacement', () => {
    const e = environment();
    e.type('0.7');
    e.context.maximum = 10;
    e.context.synchronize(); e.blur();
    assert.equal(e.context.boundField.value, 0);
    assert.equal(e.context.recordings, 0);
    e.type('7'); e.blur();
    assert.equal(e.context.boundField.value, 0.7);
    assert.equal(e.context.recordings, 1);
});

test('uninitialized or synchronizing fields do not acquire user edit intent', () => {
    const e = environment();
    e.display.initialized = false;
    e.type('0.7'); e.blur(); e.accept();
    assert.equal(e.context.recordings, 0);
    assert.equal(e.display.userEdited, false);
    e.display.initialized = true;
    e.context.synchronizing = true;
    e.type('0.7'); e.blur(); e.accept();
    assert.equal(e.context.recordings, 0);
    assert.equal(e.display.userEdited, false);
});

test('magnitude edits retain the chosen decreasing direction and scale', () => {
    const e = environment(-0.2);
    e.context.maximum = 100;
    e.context.signedRating = true;
    e.context.magnitudeMode = true;
    e.context.synchronize(); e.blur();
    assert.equal(e.context.recordings, 0);
    e.type('70'); e.blur();
    assert.equal(e.context.boundField.value, -0.7);
    assert.equal(e.context.recordings, 1);
});

test('the midpoint and set-to-zero buttons still record deliberate ratings', () => {
    const e = environment();
    e.context.maximum = 100;
    vm.runInContext(buttonHandler, e.context);
    assert.equal(e.context.boundField.value, 0.5);
    assert.equal(e.context.recordings, 1);
    e.context.signedRating = true;
    vm.runInContext(buttonHandler, e.context);
    assert.equal(e.context.boundField.value, 0);
    assert.equal(e.context.recordings, 2);
});

console.log(`${passed} Network rating checks passed`);
