const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

// Exercise the actual Form functions with the documented JASP list/field API.
// This is a logic test, not a native JASP/QML signal or rendering test.
const qml = fs.readFileSync(path.resolve(__dirname, '../../inst/qml/Network.qml'), 'utf8');
const presetSource = fs.readFileSync(path.resolve(__dirname, '../../inst/qml/common/NetworkPresetData.js'), 'utf8')
    .replace(/^\.pragma library\r?\n/, '');
const Presets = vm.createContext({ qsTranslate: (_, text) => text });
vm.runInContext(presetSource, Presets);

function extractFunction(source, name) {
    const match = new RegExp('\\bfunction\\s+' + name + '\\s*\\(').exec(source);
    assert.ok(match, 'missing actual QML function ' + name);
    const firstBrace = source.indexOf('{', match.index);
    let depth = 0, quote = '', comment = '';
    for (let i = firstBrace; i < source.length; i++) {
        const c = source[i], next = source[i + 1];
        if (comment === '//') { if (c === '\n') comment = ''; continue; }
        if (comment === '/*') { if (c === '*' && next === '/') { comment = ''; i++; } continue; }
        if (quote) {
            if (c === '\\') i++;
            else if (c === quote) quote = '';
            continue;
        }
        if (c === '"' || c === "'" || c === '`') { quote = c; continue; }
        if (c === '/' && (next === '/' || next === '*')) { comment = c + next; i++; continue; }
        if (c === '{') depth++;
        if (c === '}' && --depth === 0) return source.slice(match.index, i + 1);
    }
    throw new Error('unclosed QML function ' + name);
}

const functions = [...qml.matchAll(/^ {4}function\s+([\w$]+)\s*\(/gm)]
    .map(match => extractFunction(qml, match[1])).join('\n');
const plain = value => JSON.parse(JSON.stringify(value));
let cases = 0, failures = 0;
function test(name, callback) {
    try { callback(); cases++; }
    catch (error) { failures++; console.error(name + ': ' + error.stack); }
}

function environment(problemCount = 3) {
    const writes = [];
    function field(value, label, property = 'value') {
        const result = { initialized: true };
        Object.defineProperty(result, property, {
            enumerable: true, get: () => value,
            set: next => { writes.push(label); value = next; }
        });
        return result;
    }
    let nextKey = 0;
    const rows = [];
    function addRow() {
        const key = 'row-' + nextKey++;
        rows.push({ key, controls: {
            problemName: field('Old ' + key, key + '.name'),
            problemDescription: field('Definition ' + key, key + '.description'),
            problemSeverity: field(0.7, key + '.severity'),
            problemSeverityRated: field(true, key + '.rated', 'checked')
        } });
    }
    for (let i = 0; i < problemCount; i++) addRow();
    const problems = {
        initialized: true, rows, missing: new Set(), addMissing: '', addUninitialized: '', blockAdd: false,
        get count() { return rows.length; },
        get columnsNames() { return rows.map(row => row.key); },
        getRowControl(key, name) {
            if (this.missing.has(key + '.' + name)) return null;
            const row = rows.find(row => row.key === key);
            return row ? row.controls[name] || null : null;
        },
        addItem() {
            if (this.blockAdd || rows.length >= 10) return;
            writes.push('problems.add'); addRow();
            if (this.addMissing) this.missing.add(rows[rows.length - 1].key + '.' + this.addMissing);
            if (this.addUninitialized) rows[rows.length - 1].controls[this.addUninitialized].initialized = false;
        },
        removeItem(index) {
            if (index < 0 || index >= rows.length) return;
            writes.push('problems.remove'); rows.splice(index, 1);
        }
    };
    function assessment(label, checked) {
        const items = [{ from: 'A', to: 'B', strength: 0.6 }, { from: 'B', to: 'A', strength: -0.3 }];
        const manual = {
            initialized: true, items, removed: [], get count() { return items.length; },
            removeItem(index) { writes.push(label + '.manual'); this.removed.push(index); items.splice(index, 1); }
        };
        const matrix = {};
        for (const from of ['A', 'B']) {
            matrix[from] = { initialized: true, columnsNames: ['A', 'B'], fields: {},
                getRowControl(to, name) { return name === 'connectionStrength' ? this.fields[to] || null : null; } };
            for (const to of ['A', 'B']) matrix[from].fields[to] = field(from === to ? 0.2 : -0.8, label + '.' + from + to);
        }
        const generated = { initialized: true, columnsNames: ['A', 'B'], matrix,
            getRowControl(from, name) { return name === 'targets' ? matrix[from] || null : null; } };
        return { manual, generated, mode: field(checked, label + '.mode', 'checked') };
    }
    const assessments = [assessment('first', true), assessment('second', false)];
    const connectionList = {
        initialized: true, columnsNames: ['first', 'second'], missing: new Set(),
        getRowControl(key, name) {
            if (this.missing.has(key + '.' + name)) return null;
            const item = assessments[key === 'first' ? 0 : 1];
            return ({connections: item.manual, allConnectionStrengths: item.generated, allConnections: item.mode})[name] || null;
        }
    };
    const descriptions = { Alpha: {currentValue: 'First definition', initialized: true}, Beta: {currentValue: '', initialized: true} };
    const presetPreview = {
        columnsNames: ['Alpha', 'Beta'],
        get count() { return this.columnsNames.length; },
        getRowControl(name, key) { return key === 'presetDescription' ? descriptions[name] || null : null; }
    };
    const context = vm.createContext({
        Presets, qsTr: text => text, problems, connectionList, actionError: '',
        builtInPresets: Presets.builtInPresets(),
        networkSeverityMaximum: field(1, 'severityMaximum'),
        networkConnectionMaximum: field(1, 'connectionMaximum'),
        networkConnectionInput: field('magnitude', 'connectionInput', 'currentValue'),
        networkPresetSaveName: field('Old preset', 'presetName'),
        networkExportRequest: field(true, 'CSV.request', 'checked'),
        networkExportSession: field('session-1', 'CSV.session'),
        networkSavePath: field('/somewhere/results.csv', 'CSV.path'),
        networkPresetLoadPath: {value: '/presets/example.json'},
        networkPresetLoadRequest: {checked: false},
        loadedToken: {currentValue: 'session-1\n/presets/example.json\nfalse', initialized: true},
        loadedPresetName: {currentValue: 'Loaded set', initialized: true},
        loadedSeverityMaximum: {currentValue: '10', initialized: true},
        loadedConnectionMaximum: {currentValue: '100', initialized: true},
        loadedConnectionInput: {currentValue: 'signed', initialized: true},
        presetPreview
    });
    context.networkForm = context;
    vm.runInContext(functions, context, {filename: 'Network.qml extracted Form functions'});
    function assessmentState(item) {
        return { manual: plain(item.manual.items), mode: item.mode.checked,
            matrix: Object.fromEntries(Object.entries(item.generated.matrix).map(([key, targets]) =>
                [key, targets && Object.fromEntries(Object.entries(targets.fields).map(([key, field]) => [key, field && field.value]))])) };
    }
    function state() {
        return { problems: rows.map(row => ({key: row.key, controls: plain(row.controls)})),
            assessments: assessments.map(assessmentState),
            severityMaximum: context.networkSeverityMaximum.value,
            connectionMaximum: context.networkConnectionMaximum.value,
            input: context.networkConnectionInput.currentValue,
            presetName: context.networkPresetSaveName.value,
            csv: [context.networkExportRequest.checked, context.networkExportSession.value, context.networkSavePath.value] };
    }
    return {context, assessments, writes, state, assessmentState, descriptions};
}

function definition(count = 3, mode = 'signed') {
    const scales = {severityMax: 10, connectionMax: 100};
    if (mode !== undefined) scales.connectionInput = mode;
    return {schemaVersion: 1, name: 'Chosen set',
        problems: Array.from({length: count}, (_, i) => i % 2 ?
            {problemName: 'New ' + i} : {problemName: 'New ' + i, problemDescription: 'Meaning ' + i}), scales};
}

test('clear touches both stores of only the selected assessment', () => {
    const env = environment(), before = env.state(), first = env.assessments[0];
    assert.equal(env.context.clearAssessment(first.manual, first.generated, first.mode), true);
    assert.deepEqual(first.manual.removed, [1, 0]);
    assert.equal(first.manual.count, 0);
    assert.equal(first.mode.checked, false);
    assert.deepEqual(env.assessmentState(first).matrix, {A: {A: 0, B: 0}, B: {A: 0, B: 0}});
    assert.deepEqual(env.state().assessments[1], before.assessments[1]);
    assert.deepEqual(env.state().problems, before.problems);
    assert.deepEqual(env.state().csv, before.csv);
    assert.equal(env.writes.some(key => key.startsWith('CSV.')), false);
});

test('connection control discovery includes hidden/self fields and does not mutate', () => {
    const env = environment(), first = env.assessments[0], before = env.state();
    const controls = env.context.connectionControls(first.manual, first.generated, first.mode);
    assert.equal(controls.fields.length, 4);
    assert.equal(controls.fields[0], first.generated.matrix.A.fields.A);
    assert.deepEqual(env.state(), before);
    assert.equal(env.writes.length, 0);
    env.context.clearConnectionControls(controls);
    assert.equal(first.manual.count, 0);
    assert.equal(first.mode.checked, false);
});

for (const missing of ['manual', 'generated', 'mode', 'targets', 'field']) {
    test('missing assessment ' + missing + ' fails before mutation', () => {
        const env = environment(), first = env.assessments[0];
        if (missing === 'targets') first.generated.matrix.A = null;
        if (missing === 'field') first.generated.matrix.B.fields.A = null;
        const before = env.state();
        assert.equal(env.context.clearAssessment(missing === 'manual' ? null : first.manual,
            missing === 'generated' ? null : first.generated, missing === 'mode' ? null : first.mode), false);
        assert.deepEqual(env.state(), before);
        assert.equal(env.writes.length, 0);
        assert.notEqual(env.context.actionError, '');
    });
}

for (const pending of ['manual', 'generated', 'mode', 'targets', 'field', 'empty matrix', 'empty targets']) {
    test('assessment ' + pending + ' readiness fails before any clearing', () => {
        const env = environment(), first = env.assessments[0];
        if (pending === 'targets') first.generated.matrix.A.initialized = false;
        else if (pending === 'field') first.generated.matrix.B.fields.A.initialized = false;
        else if (pending === 'empty matrix') first.generated.columnsNames = [];
        else if (pending === 'empty targets') first.generated.matrix.A.columnsNames = [];
        else first[pending].initialized = false;
        const before = env.state();
        assert.equal(env.context.clearAssessment(first.manual, first.generated, first.mode), false);
        assert.deepEqual(env.state(), before);
        assert.equal(env.writes.length, 0);
        assert.notEqual(env.context.actionError, '');
    });
}

for (const count of [2, 3, 6]) {
    test('apply validated preset with ' + count + ' rows clears ratings and preserves export state', () => {
        const env = environment(), before = env.state(), input = definition(count);
        const retained = env.context.problems.rows.slice(0, Math.min(3, count)).map(row => row.controls);
        assert.equal(env.context.applyPreset(input), true);
        assert.equal(env.context.problems.count, count);
        for (let i = 0; i < count; i++) {
            const row = env.context.problems.rows[i].controls;
            assert.equal(row.problemName.value, input.problems[i].problemName);
            assert.equal(row.problemDescription.value, input.problems[i].problemDescription || '');
            assert.equal(row.problemSeverity.value, 0);
            assert.equal(row.problemSeverityRated.checked, false);
            if (i < retained.length) assert.equal(row, retained[i]);
        }
        for (const item of env.assessments) {
            assert.equal(item.manual.count, 0);
            assert.equal(item.mode.checked, false);
            assert.deepEqual(env.assessmentState(item).matrix, {A: {A: 0, B: 0}, B: {A: 0, B: 0}});
        }
        assert.equal(env.context.networkSeverityMaximum.value, 10);
        assert.equal(env.context.networkConnectionMaximum.value, 100);
        assert.equal(env.context.networkConnectionInput.currentValue, 'signed');
        assert.equal(env.context.networkPresetSaveName.value, 'Chosen set');
        assert.deepEqual(env.state().csv, before.csv);
        assert.equal(env.writes.some(key => key.startsWith('CSV.')), false);
    });
}

test('omitted input mode preserves the current presentation', () => {
    const env = environment(), input = definition();
    delete input.scales.connectionInput;
    assert.equal(env.context.applyPreset(input), true);
    assert.equal(env.context.networkConnectionInput.currentValue, 'magnitude');
});

for (const invalid of [null, {}, {...definition(), schemaVersion: 2}, {...definition(), name: ' '}, {...definition(), patientRatings: []}]) {
    test('invalid preset does not mutate any settings', () => {
        const env = environment(), before = env.state();
        assert.equal(env.context.applyPreset(invalid), false);
        assert.deepEqual(env.state(), before);
        assert.equal(env.writes.length, 0);
        assert.notEqual(env.context.actionError, '');
    });
}

for (const field of ['problemName', 'problemDescription', 'problemSeverity', 'problemSeverityRated']) {
    test('missing existing ' + field + ' is detected before mutation', () => {
        const env = environment(), before = env.state();
        env.context.problems.missing.add('row-1.' + field);
        assert.equal(env.context.applyPreset(definition(6)), false);
        assert.deepEqual(env.state(), before);
        assert.equal(env.writes.length, 0);
    });
    test('uninitialized existing ' + field + ' is detected before mutation', () => {
        const env = environment();
        env.context.problems.rows[1].controls[field].initialized = false;
        const before = env.state();
        assert.equal(env.context.applyPreset(definition(6)), false);
        assert.deepEqual(env.state(), before);
        assert.equal(env.writes.length, 0);
    });
}

for (const pending of ['problems', 'connectionList', 'empty assessments']) {
    test('unready ' + pending + ' prevents preset writes', () => {
        const env = environment();
        if (pending === 'empty assessments') env.context.connectionList.columnsNames = [];
        else env.context[pending].initialized = false;
        const before = env.state();
        assert.equal(env.context.applyPreset(definition(6)), false);
        assert.deepEqual(env.state(), before);
        assert.ok(env.writes.every(key => key === 'problems.add' || key === 'problems.remove'));
    });
}

test('missing added controls roll back additions without clearing ratings', () => {
    const env = environment(), before = env.state();
    env.context.problems.addMissing = 'problemSeverity';
    assert.equal(env.context.applyPreset(definition(6)), false);
    assert.deepEqual(env.state(), before);
    assert.ok(env.writes.every(key => key === 'problems.add' || key === 'problems.remove'));
});

test('uninitialized added controls roll back additions without clearing ratings', () => {
    const env = environment(), before = env.state();
    env.context.problems.addUninitialized = 'problemSeverity';
    assert.equal(env.context.applyPreset(definition(6)), false);
    assert.deepEqual(env.state(), before);
    assert.ok(env.writes.every(key => key === 'problems.add' || key === 'problems.remove'));
});

test('refused row addition terminates without clearing ratings', () => {
    const env = environment(), before = env.state();
    env.context.problems.blockAdd = true;
    assert.equal(env.context.applyPreset(definition(6)), false);
    assert.deepEqual(env.state(), before);
    assert.equal(env.writes.length, 0);
});

test('missing later assessment rolls back preparation and preserves all ratings', () => {
    const env = environment(), before = env.state();
    env.context.connectionList.missing.add('second.allConnectionStrengths');
    assert.equal(env.context.applyPreset(definition(6)), false);
    assert.deepEqual(env.state(), before);
    assert.ok(env.writes.every(key => key === 'problems.add' || key === 'problems.remove'));
});

test('uninitialized later assessment rolls back preparation and preserves all ratings', () => {
    const env = environment(), before = env.state();
    env.assessments[1].generated.initialized = false;
    assert.equal(env.context.applyPreset(definition(6)), false);
    assert.deepEqual(env.state(), before);
    assert.ok(env.writes.every(key => key === 'problems.add' || key === 'problems.remove'));
});

test('imported preset uses definitions and scales without patient data', () => {
    const env = environment(), before = env.state();
    assert.deepEqual(plain(env.context.importedPreset()), {
        schemaVersion: 1, name: 'Loaded set',
        problems: [{problemName: 'Alpha', problemDescription: 'First definition'}, {problemName: 'Beta', problemDescription: ''}],
        scales: {severityMax: 10, connectionMax: 100, connectionInput: 'signed'}
    });
    assert.equal(env.context.importedPresetReady(), true);
    assert.deepEqual(env.state(), before);
    assert.equal(env.writes.length, 0);
});

for (const condition of ['token', 'session', 'path', 'request', 'description', 'description-initialization', 'scale', 'name']) {
    test('loaded preset readiness rejects ' + condition, () => {
        const env = environment(), context = env.context;
        if (condition === 'token') context.loadedToken.currentValue = 'stale';
        if (condition === 'session') context.networkExportSession.value = '';
        if (condition === 'path') context.networkPresetLoadPath.value = '/different.json';
        if (condition === 'request') context.networkPresetLoadRequest.checked = true;
        if (condition === 'description') delete env.descriptions.Alpha;
        if (condition === 'description-initialization') env.descriptions.Alpha.initialized = false;
        if (condition === 'scale') context.loadedSeverityMaximum.currentValue = '1001';
        if (condition === 'name') context.loadedPresetName.currentValue = '';
        const before = env.state(), writes = env.writes.length;
        assert.equal(context.importedPresetReady(), false);
        assert.deepEqual(env.state(), before);
        assert.equal(env.writes.length, writes);
    });
}

for (const metadata of ['loadedPresetName', 'loadedSeverityMaximum', 'loadedConnectionMaximum', 'loadedConnectionInput', 'loadedToken']) {
    test('loaded preset waits for ' + metadata + ' initialization', () => {
        const env = environment(), before = env.state();
        env.context[metadata].initialized = false;
        assert.equal(env.context.importedPresetReady(), false);
        assert.deepEqual(env.state(), before);
        assert.equal(env.writes.length, 0);
    });
}

test('empty imported problem source is not ready', () => {
    const env = environment();
    env.context.presetPreview.columnsNames = [];
    assert.equal(env.context.importedPresetReady(), false);
    assert.equal(env.writes.length, 0);
});

for (const [metadata, value] of [
    ['loadedPresetName', ''], ['loadedPresetName', '   '],
    ['loadedSeverityMaximum', ''], ['loadedSeverityMaximum', 'not a number'],
    ['loadedSeverityMaximum', '0'], ['loadedSeverityMaximum', '1.5'],
    ['loadedConnectionMaximum', ''], ['loadedConnectionMaximum', 'Infinity'],
    ['loadedConnectionMaximum', '1001'], ['loadedConnectionInput', ''],
    ['loadedConnectionInput', 'decreases'], ['loadedToken', '']
]) {
    test('invalid scalar metadata ' + metadata + '=' + JSON.stringify(value) + ' is not ready', () => {
        const env = environment(), before = env.state();
        env.context[metadata].currentValue = value;
        assert.equal(env.context.importedPresetReady(), false);
        assert.deepEqual(env.state(), before);
        assert.equal(env.writes.length, 0);
    });
}

const upgrades = fs.readFileSync(path.resolve(__dirname, '../../inst/Upgrades.qml'), 'utf8');
assert.match(upgrades, /functionName:\s*"Network"/);
assert.match(upgrades, /fromVersion:\s*"0\.1"/);
assert.match(upgrades, /toVersion:\s*"0\.1\.1"/);
const migrationSource = upgrades.replace('jsFunction: function(options)', 'function migrateProblemRows(options)');
const migrate = vm.runInNewContext('(' + extractFunction(migrationSource, 'migrateProblemRows') + ')');

test('migration marks all legacy severities including zero as recorded', () => {
    const problems = [0, 0.37, 1].map((severity, index) => ({problemName: 'Legacy ' + index, problemSeverity: severity}));
    const result = migrate({problems});
    assert.deepEqual(plain(result), problems.map(problem => ({...problem, problemSeverityRated: true})));
    assert.deepEqual(result.map(problem => problem.problemSeverity), [0, 0.37, 1]);
});

test('migration preserves existing rated flags and all other row values', () => {
    const problems = [
        {problemName: 'Unrated', problemDescription: 'Meaning', problemSeverity: 0, problemSeverityRated: false},
        {problemName: 'Rated', problemSeverity: 0.5, problemSeverityRated: true, extra: 'preserved'}
    ];
    const before = plain(problems);
    assert.deepEqual(plain(migrate({problems})), before);
});

test('migration is idempotent and does not touch other options', () => {
    const options = {problems: [{problemName: 'Legacy', problemSeverity: 0.8}],
        networkSavePath: '/existing.csv', networkExportRequest: true, connectionList: [{name: 'Assessment', connections: []}]};
    const other = plain({...options, problems: undefined});
    const first = plain(migrate(options));
    assert.deepEqual(plain(migrate({problems: first})), first);
    assert.deepEqual(plain({...options, problems: undefined}), other);
});

test('migration handles an empty or missing problem list', () => {
    assert.deepEqual(plain(migrate({problems: []})), []);
    assert.deepEqual(plain(migrate({})), []);
});

console.log(cases + ' Network action cases passed; ' + failures + ' failed.');
if (failures) process.exitCode = 1;
