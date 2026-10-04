.pragma library

// Copyright (C) 2026 University of Amsterdam and Netherlands eScience Center
// SPDX-License-Identifier: AGPL-3.0-or-later

// Presets contain reusable definitions only: never ratings or assessment tabs.
// Translatable built-in prompts live in NetworkPresetLists.qml.

function _isObject(value)
{
    return value !== null && typeof value === "object" && !Array.isArray(value);
}

function _owns(value, key)
{
    return Object.prototype.hasOwnProperty.call(value, key);
}

function _hasOnlyKeys(value, allowed)
{
    var keys = Object.keys(value);
    for (var i = 0; i < keys.length; i++)
        if (allowed.indexOf(keys[i]) === -1)
            return false;
    return true;
}

function _failure(code, index)
{
    var result = { valid: false, errorCode: code };
    if (typeof index === "number")
        result.errorIndex = index;
    return result;
}

// Match .ln1NetPresetTrim exactly; built-in trim differs across R/JavaScript.
function _trimPresetText(value)
{
    return value.replace(/^[\u0009-\u000d\u0020\u0085\u00a0\u1680\u180e\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]+|[\u0009-\u000d\u0020\u0085\u00a0\u1680\u180e\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]+$/g, "");
}

function _isName(value)
{
    return typeof value === "string" && _trimPresetText(value).length > 0 &&
        !/[\u0000-\u001f\u007f]/.test(value);
}

function _isScaleMaximum(value)
{
    return typeof value === "number" && isFinite(value) &&
        value >= 1 && value <= 1000 && Math.floor(value) === value;
}

// Return a fresh normalized definition, or a stable error code for the QML UI
// to translate. errorIndex is zero-based when a particular problem is invalid.
// An omitted connectionInput stays omitted so applying a preset can preserve
// the current display mode. It does not change the canonical signed ratings.
function validatePreset(value)
{
    if (!_isObject(value))
        return _failure("invalid-preset");
    if (!_hasOnlyKeys(value, ["schemaVersion", "name", "problems", "scales"]))
        return _failure("unexpected-fields");
    if (!_owns(value, "schemaVersion") || value.schemaVersion !== 1)
        return _failure("unsupported-version");
    if (!_owns(value, "name") || !_isName(value.name))
        return _failure("invalid-name");
    if (!_owns(value, "problems") || !Array.isArray(value.problems) ||
            value.problems.length < 2 || value.problems.length > 10)
        return _failure("invalid-problems");

    var problems = [];
    var names = [];
    for (var i = 0; i < value.problems.length; i++)
    {
        var source = value.problems[i];
        if (!_isObject(source))
            return _failure("invalid-problem", i);
        if (!_hasOnlyKeys(source, ["problemName", "problemDescription"]))
            return _failure("unexpected-fields", i);
        if (!_owns(source, "problemName") || !_isName(source.problemName))
            return _failure("invalid-problem-name", i);
        var name = _trimPresetText(source.problemName);
        if (names.indexOf(name) !== -1)
            return _failure("duplicate-problem-name", i);
        names.push(name);

        var problem = { problemName: name };
        if (_owns(source, "problemDescription"))
        {
            if (typeof source.problemDescription !== "string")
                return _failure("invalid-problem-description", i);
            problem.problemDescription = _trimPresetText(source.problemDescription);
        }
        problems.push(problem);
    }

    if (!_owns(value, "scales") || !_isObject(value.scales))
        return _failure("invalid-scales");
    if (!_hasOnlyKeys(value.scales, ["severityMax", "connectionMax", "connectionInput"]))
        return _failure("unexpected-fields");
    if (!_owns(value.scales, "severityMax") || !_owns(value.scales, "connectionMax") ||
            !_isScaleMaximum(value.scales.severityMax) || !_isScaleMaximum(value.scales.connectionMax))
        return _failure("invalid-scales");

    var scales = {
        severityMax: value.scales.severityMax,
        connectionMax: value.scales.connectionMax
    };
    if (_owns(value.scales, "connectionInput"))
    {
        if (value.scales.connectionInput !== "signed" && value.scales.connectionInput !== "magnitude")
            return _failure("invalid-connection-input");
        scales.connectionInput = value.scales.connectionInput;
    }

    return {
        valid: true,
        preset: { schemaVersion: 1, name: _trimPresetText(value.name), problems: problems, scales: scales }
    };
}

function parsePreset(jsonText)
{
    if (typeof jsonText !== "string")
        return _failure("invalid-json");
    var value;
    try
    {
        value = JSON.parse(jsonText);
    }
    catch (error)
    {
        return _failure("invalid-json");
    }
    return validatePreset(value);
}

function serializePreset(value)
{
    var result = validatePreset(value);
    if (result.valid)
        result.json = JSON.stringify(result.preset, null, 2);
    return result;
}
