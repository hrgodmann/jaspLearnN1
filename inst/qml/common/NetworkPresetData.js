.pragma library

// Copyright (C) 2026 University of Amsterdam and Netherlands eScience Center
// SPDX-License-Identifier: AGPL-3.0-or-later

// These are original LearnN1 prompts, not reproduced diagnostic criteria or
// validated questionnaires. Clinicians and patients can edit them together.
// Presets contain reusable definitions only: never ratings or assessment tabs.

function builtInNotice()
{
    return qsTranslate("NetworkPresets", "These original LearnN1 prompts are editable starting points, not validated questionnaires or diagnostic criteria. Choose and adapt the problems together with the person concerned.");
}

function builtInPresets()
{
    return [
        _builtIn("lowMood", qsTranslate("NetworkPresets", "Low mood"), [
            qsTranslate("NetworkPresets", "Low mood"),
            qsTranslate("NetworkPresets", "Less enjoyment"),
            qsTranslate("NetworkPresets", "Low energy"),
            qsTranslate("NetworkPresets", "Self-criticism"),
            qsTranslate("NetworkPresets", "Social withdrawal"),
            qsTranslate("NetworkPresets", "Difficulty starting tasks")
        ]),
        _builtIn("worryAnxiety", qsTranslate("NetworkPresets", "Worry and anxiety"), [
            qsTranslate("NetworkPresets", "Worrying"),
            qsTranslate("NetworkPresets", "Feeling tense"),
            qsTranslate("NetworkPresets", "Seeking reassurance"),
            qsTranslate("NetworkPresets", "Avoiding uncertainty"),
            qsTranslate("NetworkPresets", "Difficulty relaxing"),
            qsTranslate("NetworkPresets", "Concentration difficulty")
        ]),
        _builtIn("sleepDifficulties", qsTranslate("NetworkPresets", "Sleep difficulties"), [
            qsTranslate("NetworkPresets", "Trouble falling asleep"),
            qsTranslate("NetworkPresets", "Waking at night"),
            qsTranslate("NetworkPresets", "Waking too early"),
            qsTranslate("NetworkPresets", "Daytime tiredness"),
            qsTranslate("NetworkPresets", "Worry about sleep")
        ]),
        _builtIn("stressAvoidance", qsTranslate("NetworkPresets", "Stress and avoidance"), [
            qsTranslate("NetworkPresets", "Feeling overwhelmed"),
            qsTranslate("NetworkPresets", "Putting things off"),
            qsTranslate("NetworkPresets", "Avoiding difficult tasks"),
            qsTranslate("NetworkPresets", "Irritability"),
            qsTranslate("NetworkPresets", "Difficulty switching off")
        ])
    ];
}

function _builtIn(id, label, names)
{
    var problems = [];
    for (var i = 0; i < names.length; i++)
        problems.push({ problemName: names[i] });
    return {
        id: id,
        label: label,
        preset: {
            schemaVersion: 1,
            name: label,
            problems: problems,
            scales: { severityMax: 1, connectionMax: 1 }
        }
    };
}

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

function _isName(value)
{
    return typeof value === "string" && value.trim().length > 0 &&
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
        var name = source.problemName.trim();
        if (names.indexOf(name) !== -1)
            return _failure("duplicate-problem-name", i);
        names.push(name);

        var problem = { problemName: name };
        if (_owns(source, "problemDescription"))
        {
            if (typeof source.problemDescription !== "string")
                return _failure("invalid-problem-description", i);
            problem.problemDescription = source.problemDescription.trim();
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
        preset: { schemaVersion: 1, name: value.name.trim(), problems: problems, scales: scales }
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
