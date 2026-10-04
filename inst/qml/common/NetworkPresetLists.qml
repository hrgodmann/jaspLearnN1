import QtQuick

// Copyright (C) 2026 University of Amsterdam and Netherlands eScience Center
// SPDX-License-Identifier: AGPL-3.0-or-later

// Original editable prompts, kept in QML for JASP translation extraction.
QtObject
{
    function builtInNotice()
    {
        return qsTr("These original LearnN1 prompts are editable starting points, not validated questionnaires or diagnostic criteria. Choose and adapt the problems together with the person concerned.");
    }

    function builtInPresets()
    {
        return [
            _builtIn("lowMood", qsTr("Low mood"), [
                qsTr("Low mood"),
                qsTr("Less enjoyment"),
                qsTr("Low energy"),
                qsTr("Self-criticism"),
                qsTr("Social withdrawal"),
                qsTr("Difficulty starting tasks")
            ]),
            _builtIn("worryAnxiety", qsTr("Worry and anxiety"), [
                qsTr("Worrying"),
                qsTr("Feeling tense"),
                qsTr("Seeking reassurance"),
                qsTr("Avoiding uncertainty"),
                qsTr("Difficulty relaxing"),
                qsTr("Concentration difficulty")
            ]),
            _builtIn("sleepDifficulties", qsTr("Sleep difficulties"), [
                qsTr("Trouble falling asleep"),
                qsTr("Waking at night"),
                qsTr("Waking too early"),
                qsTr("Daytime tiredness"),
                qsTr("Worry about sleep")
            ]),
            _builtIn("stressAvoidance", qsTr("Stress and avoidance"), [
                qsTr("Feeling overwhelmed"),
                qsTr("Putting things off"),
                qsTr("Avoiding difficult tasks"),
                qsTr("Irritability"),
                qsTr("Difficulty switching off")
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

}
