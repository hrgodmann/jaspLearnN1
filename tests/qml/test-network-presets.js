const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');

const file = path.resolve(__dirname, '../../inst/qml/common/NetworkPresetData.js');
const source = fs.readFileSync(file, 'utf8').replace(/^\.pragma library\r?\n/, '');
const context = vm.createContext({});
vm.runInContext(source, context, { filename: file });

const listsFile = path.resolve(__dirname, "../../inst/qml/common/NetworkPresetLists.qml");
const listsSource = fs.readFileSync(listsFile, "utf8");
context.qsTr = text => text;
vm.runInContext(listsSource.slice(listsSource.indexOf("{", listsSource.indexOf("QtObject")) + 1, listsSource.lastIndexOf("}")), context, {filename: listsFile});
let checks = 0;
function check(value, message) { assert.ok(value, message); checks++; }
function preset() {
    return { schemaVersion: 1, name: 'My template', problems: [
        { problemName: 'Worry' }, { problemName: 'Sleep', problemDescription: 'At night' }
    ], scales: { severityMax: 10, connectionMax: 100 } };
}
function rejects(value, code) {
    const result = context.validatePreset(value);
    check(result.valid === false && result.errorCode === code, code);
}

const builtIns = context.builtInPresets();
check(builtIns.length === 4, 'four built-in sets');
check(/not validated/.test(context.builtInNotice()), 'provenance notice');
for (const item of builtIns) {
    check(context.validatePreset(item.preset).valid, 'valid built-in ' + item.id);
    check(item.preset.problems.length >= 4 && item.preset.problems.length <= 7, 'short built-in set');
    check(item.preset.scales.severityMax === 1 && item.preset.scales.connectionMax === 1, 'legacy scale defaults');
}
builtIns[0].preset.problems[0].problemName = 'Changed';
check(context.builtInPresets()[0].preset.problems[0].problemName === 'Low mood', 'built-ins are fresh objects');

let input = preset();
input.name = '  My template  ';
input.problems[0].problemName = ' Worry ';
input.problems[1].problemDescription = '  At night  ';
const serialized = context.serializePreset(input);
check(serialized.valid && typeof serialized.json === 'string', 'serialize valid template');
check(serialized.preset.name === 'My template' && serialized.preset.problems[0].problemName === 'Worry', 'trim names');
check(serialized.preset.problems[1].problemDescription === 'At night', 'trim description');
check(context.serializePreset(context.parsePreset(serialized.json).preset).json === serialized.json, 'JSON round trip');
check(input.name === '  My template  ' && input.problems[0].problemName === ' Worry ', 'input not mutated');
serialized.preset.problems[0].problemName = 'Changed';
check(input.problems[0].problemName === ' Worry ', 'result does not alias input');
check(!Object.hasOwn(context.validatePreset(preset()).preset.scales, 'connectionInput'), 'omitted mode stays omitted');

for (const inputMode of ['signed', 'magnitude']) {
    input = preset(); input.scales.connectionInput = inputMode;
    check(context.parsePreset(context.serializePreset(input).json).preset.scales.connectionInput === inputMode, 'input mode round trip');
}
for (const max of [1, 7, 1000]) {
    input = preset(); input.scales.severityMax = max; input.scales.connectionMax = max;
    check(context.validatePreset(input).valid, 'scale boundary ' + max);
}
for (const count of [2, 10]) {
    input = preset(); input.problems = Array.from({length: count}, (_, i) => ({problemName: 'Problem ' + i}));
    check(context.validatePreset(input).valid, 'problem count boundary ' + count);
}
for (const value of [null, [], 'template', 3]) rejects(value, 'invalid-preset');
for (const version of [undefined, 2, '1', null]) {
    input = preset(); input.schemaVersion = version; rejects(input, 'unsupported-version');
}
for (const name of ['', '   ', 2, 'bad\nname', 'bad\u0000name']) {
    input = preset(); input.name = name; rejects(input, 'invalid-name');
}
for (const problems of [null, [], [{problemName: 'Only one'}], Array.from({length: 11}, (_, i) => ({problemName: 'P' + i}))]) {
    input = preset(); input.problems = problems; rejects(input, 'invalid-problems');
}
input = preset(); input.problems[0] = null; rejects(input, 'invalid-problem');
input = preset(); input.problems[0].problemName = ' '; rejects(input, 'invalid-problem-name');
input = preset(); input.problems[1].problemName = ' Worry '; rejects(input, 'duplicate-problem-name');
check(context.validatePreset(input).errorIndex === 1, 'problem index reported');
input = preset(); input.problems[0].problemDescription = 7; rejects(input, 'invalid-problem-description');
for (const max of [0, -1, 1.5, 1001, NaN, Infinity, '10']) {
    input = preset(); input.scales.severityMax = max; rejects(input, 'invalid-scales');
    input = preset(); input.scales.connectionMax = max; rejects(input, 'invalid-scales');
}
input = preset(); input.scales = null; rejects(input, 'invalid-scales');
input = preset(); delete input.scales.connectionMax; rejects(input, 'invalid-scales');
input = preset(); input.scales.connectionInput = 'execute'; rejects(input, 'invalid-connection-input');
for (const key of ['patientName', 'ratings', 'connectionList', 'path', 'execute']) {
    input = preset(); input[key] = 'not allowed'; rejects(input, 'unexpected-fields');
}
input = preset(); input.problems[0].problemSeverity = 0.5; rejects(input, 'unexpected-fields');
input = preset(); input.scales.path = '/tmp/patient.json'; rejects(input, 'unexpected-fields');
input = preset(); Object.defineProperty(input, '__proto__', { enumerable: true, value: {} });
rejects(input, 'unexpected-fields');
for (const text of ['', 'function () {}', '{', '{"schemaVersion":1,}', undefined, 1]) {
    check(context.parsePreset(text).errorCode === 'invalid-json', 'reject malformed JSON');
}
check(context.serializePreset(null).errorCode === 'invalid-preset', 'serialization validates first');
const fixtures = JSON.parse(fs.readFileSync(path.resolve(__dirname, '../fixtures/network-preset-normalization.json'), 'utf8'));
for (const fixture of fixtures) {
    const result = context.validatePreset(fixture.input);
    check(result.valid === fixture.valid, fixture.description);
    if (result.valid) {
        assert.deepEqual(JSON.parse(JSON.stringify(result.preset)), fixture.expected, fixture.description);
        checks++;
    }
}
console.log(checks + ' preset helper checks passed.');
