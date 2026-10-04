// Source-level checks of the actual form handlers; these do not replace JASP
// Desktop save/reopen/duplicate/undo acceptance tests.
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const assert = require("node:assert/strict");
const source = fs.readFileSync(path.join(__dirname, "../../inst/qml/Forecasting.qml"), "utf8");
let checks = 0;
function equal(actual, expected) { assert.equal(actual, expected); checks++; }
function initialization(id) {
    const start = source.indexOf("id: " + id + "\n");
    assert.ok(start >= 0);
    const match = source.slice(start).match(/onInitializedChanged:\s*([^\n]+)/);
    assert.ok(match);
    return match[1];
}
const requestInitialization = initialization("forecastExportRequest");
const sessionInitialization = initialization("forecastExportSession");
const click = source.match(/onClicked:\s*([^\n]+)/)[1];
const enabled = source.match(/enabled:\s*(forecastSave\.value[^\n]+)/)[1];
for (const saved of [false, true]) {
    const request = { initialized: false, checked: saved };
    vm.runInNewContext(requestInitialization, request);
    equal(request.checked, saved);
    request.initialized = true;
    vm.runInNewContext(requestInitialization, request);
    equal(request.checked, false);
    const context = { forecastExportRequest: request };
    vm.runInNewContext(click, context);
    equal(request.checked, true);
    vm.runInNewContext(click, context);
    equal(request.checked, false);
}
const session = { initialized: false, value: "saved-session", Date: { now: () => 100 }, Math: { random: () => .2 } };
vm.runInNewContext(sessionInitialization, session);
equal(session.value, "saved-session");
session.initialized = true;
vm.runInNewContext(sessionInitialization, session);
equal(session.value, "session-100-0.2");
session.Date.now = () => 101;
vm.runInNewContext(sessionInitialization, session);
equal(session.value, "session-101-0.2");
for (const destination of ["", "forecast.csv"])
    for (const currentSession of ["", "session-current"])
        for (const horizon of [0, 1]) {
            const context = { forecastSave: { value: destination }, forecastExportSession: { value: currentSession }, forecastLength: { value: horizon } };
            equal(vm.runInNewContext(enabled, context), destination !== "" && currentSession !== "" && horizon > 0);
        }
console.log(`Forecasting action source checks passed: ${checks}.`);
