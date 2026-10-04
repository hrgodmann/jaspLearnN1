//
// Copyright (C) 2025 University of Amsterdam and Netherlands eScience Center
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Controls as QtControls
import QtQuick.Layouts
import JASP
import JASP.Controls

import "./common" as Common
import "./common/NetworkPresetData.js" as Presets

Form
{
	id: networkForm
    property string actionError: ""
    property var builtInPresets: presetLists.builtInPresets()
    Common.NetworkPresetLists { id: presetLists }

    function presetChoices() {
        return builtInPresets.map(function(item) { return {label: item.label, value: item.id} })
    }

    function applyPreset(definition) {
        var checked = Presets.validatePreset(definition)
        if (!checked.valid) {
            actionError = qsTr("This preset is invalid. Check its names, scale settings and format version.")
            return false
        }
        var preset = checked.preset
        if (!connectionList.initialized || connectionList.columnsNames.length === 0) {
            actionError = qsTr("The assessment controls are still loading. Try applying the preset again when they are ready.")
            return false
        }
        // Validate existing rows before touching ratings. Adding rows is the
        // only preparatory mutation; undo those additions if a control is not ready.
        var originalCount = problems.count
        var fields = problemControls()
        if (!fields) {
            actionError = qsTr("The problem controls are still loading. Try applying the preset again when they are ready.")
            return false
        }
        while (problems.count < preset.problems.length) {
            var before = problems.count
            problems.addItem()
            if (problems.count === before) break
        }
        fields = problemControls()
        var assessmentKeys = connectionList.columnsNames
        var assessments = []
        var ready = fields !== null && problems.count >= preset.problems.length
        for (var i = 0; i < assessmentKeys.length && ready; ++i) {
            var key = assessmentKeys[i]
            var controls = connectionControls(
                connectionList.getRowControl(key, "connections"),
                connectionList.getRowControl(key, "allConnectionStrengths"),
                connectionList.getRowControl(key, "allConnections"))
            if (!controls) ready = false
            else assessments.push(controls)
        }
        if (!ready) {
            while (problems.count > originalCount) problems.removeItem(problems.count - 1)
            actionError = qsTr("The assessment controls are still loading. No ratings were cleared. Try applying the preset again when the controls are ready.")
            return false
        }
        assessments.forEach(clearConnectionControls)
        while (problems.count > preset.problems.length) problems.removeItem(problems.count - 1)
        for (var j = 0; j < preset.problems.length; ++j) {
            var problem = preset.problems[j]
            fields[j].name.value = problem.problemName
            fields[j].description.value = problem.problemDescription || ""
            fields[j].severity.value = 0
            fields[j].rated.checked = false
        }
        networkSeverityMaximum.value = preset.scales.severityMax
        networkConnectionMaximum.value = preset.scales.connectionMax
        if (preset.scales.connectionInput !== undefined)
            networkConnectionInput.currentValue = preset.scales.connectionInput
        networkPresetSaveName.value = preset.name
        actionError = ""
        return true
    }

    function problemControls() {
        if (!problems.initialized) return null
        var keys = problems.columnsNames
        var fields = []
        for (var i = 0; i < keys.length; ++i) {
            var row = {
                name: problems.getRowControl(keys[i], "problemName"),
                description: problems.getRowControl(keys[i], "problemDescription"),
                severity: problems.getRowControl(keys[i], "problemSeverity"),
                rated: problems.getRowControl(keys[i], "problemSeverityRated")
            }
            if (!row.name || !row.description || !row.severity || !row.rated) return null
            if (!row.name.initialized || !row.description.initialized ||
                    !row.severity.initialized || !row.rated.initialized) return null
            fields.push(row)
        }
        return fields
    }

    function importedPresetReady() {
        var metadata = [loadedPresetName, loadedSeverityMaximum, loadedConnectionMaximum, loadedConnectionInput, loadedToken]
        if (metadata.some(function(control) { return !control.initialized })) return false
        if (!networkExportSession.value || !networkPresetLoadPath.value || presetPreview.count < 2) return false
        var expected = networkExportSession.value + "\n" + networkPresetLoadPath.value + "\n" + String(networkPresetLoadRequest.checked)
        if (loadedToken.currentValue !== expected) return false
        var names = presetPreview.columnsNames
        for (var i = 0; i < names.length; ++i) {
            var description = presetPreview.getRowControl(names[i], "presetDescription")
            if (!description || !description.initialized) return false
        }
        return Presets.validatePreset(importedPreset()).valid
    }

    function importedPreset() {
        var names = presetPreview.columnsNames
        return {
            schemaVersion: 1,
            name: loadedPresetName.currentValue,
            problems: names.map(function(name) {
                var description = presetPreview.getRowControl(name, "presetDescription")
                return {problemName: name, problemDescription: description ? description.currentValue : ""}
            }),
            scales: {
                severityMax: Number(loadedSeverityMaximum.currentValue),
                connectionMax: Number(loadedConnectionMaximum.currentValue),
                connectionInput: loadedConnectionInput.currentValue
            }
        }
    }


    function connectionControls(manual, generated, mode) {
        if (!manual || !generated || !mode) return null
        if (!manual.initialized || !generated.initialized || !mode.initialized) return null
        var fields = []
        var keys = generated.columnsNames
        if (keys.length === 0) return null
        for (var i = 0; i < keys.length; ++i) {
            var targets = generated.getRowControl(keys[i], "targets")
            if (!targets || !targets.initialized) return null
            var targetKeys = targets.columnsNames
            if (targetKeys.length === 0) return null
            for (var j = 0; j < targetKeys.length; ++j) {
                var field = targets.getRowControl(targetKeys[j], "connectionStrength")
                if (!field || !field.initialized) return null
                fields.push(field)
            }
        }
        return {manual: manual, fields: fields, mode: mode}
    }

    function clearConnectionControls(controls) {
        controls.fields.forEach(function(field) { field.value = 0 })
        for (var i = controls.manual.count - 1; i >= 0; --i)
            controls.manual.removeItem(i)
        controls.mode.checked = false
    }

    function clearAssessment(manual, generated, mode) {
        var controls = connectionControls(manual, generated, mode)
        if (!controls) {
            actionError = qsTr("The assessment is still loading. Try again when its controls are ready.")
            return false
        }
        clearConnectionControls(controls)
        actionError = ""
        return true
    }

	Group
	{
		columns: 2

		Common.IntroText{}
	}

    Section {
        title: qsTr("Rating scales")
        info: qsTr("These settings change the displayed units, preserving existing ratings and their signs. A maximum of 100 uses rating points, not probabilities or percentages of causation.")
        IntegerField {
            id: networkSeverityMaximum
            name: "networkSeverityMaximum"
            label: qsTr("Severity maximum")
            defaultValue: 1
            min: 1
            max: 1000
            info: qsTr("Severity runs from 0 (none) to this maximum. Common choices are 1, 10 and 100.")
        }
        IntegerField {
            id: networkConnectionMaximum
            name: "networkConnectionMaximum"
            label: qsTr("Connection magnitude maximum")
            defaultValue: 1
            min: 1
            max: 1000
            info: qsTr("Sets the magnitude of the strongest perceived relationship. Common choices are 1, 10 and 100. Zero always means no perceived relationship.")
        }
        DropDown {
            id: networkConnectionInput
            name: "networkConnectionInput"
            label: qsTr("Connection entry")
            values: [{label: qsTr("Signed rating"), value: "signed"},
                     {label: qsTr("Magnitude and direction"), value: "magnitude"}]
            info: qsTr("Signed ratings run from minus the maximum to plus the maximum. Magnitude uses 0 to the maximum with a separate increases/decreases choice. Both entry styles describe the same signed relationships.")
        }

    }

    Section {
        title: qsTr("Connection summaries")
        info: qsTr("These options apply to assessments with Connection summaries enabled.")
        CheckBox {
            name: "networkConnectionCounts"
            label: qsTr("Include connection counts in summaries")
            checked: false
            info: qsTr("Counts distinct nonzero incoming and outgoing connections. The existing strengths describe their magnitude; zero-rated connections do not count.")
        }
    }

    Section {
        title: qsTr("Symptom presets")
        info: qsTr("Presets save reusable problem definitions and rating scales. They exclude severity ratings, connections, assessment results and CSV destinations.")
        Label {
            text: presetLists.builtInNotice()
            textFormat: Text.PlainText
            wrapMode: Text.WordWrap
            Layout.preferredWidth: 400 * preferencesModel.uiScale
        }
        DropDown {
            id: builtInChoice
            isBound: false
            label: qsTr("Starting list")
            values: networkForm.presetChoices()
        }
        Label {
            text: networkForm.builtInPresets[builtInChoice.currentIndex] ?
                networkForm.builtInPresets[builtInChoice.currentIndex].preset.problems.map(function(problem) { return problem.problemName }).join(", ") : ""
            textFormat: Text.PlainText
            wrapMode: Text.WordWrap
            Layout.preferredWidth: 400 * preferencesModel.uiScale
        }
        CheckBox {
            id: confirmBuiltIn
            isBound: false
            checked: false
            label: qsTr("Replace problems and clear ratings in every assessment")
        }
        Button {
            label: qsTr("Apply starting list")
            enabled: confirmBuiltIn.checked && builtInChoice.currentIndex >= 0
            onClicked: if (networkForm.applyPreset(networkForm.builtInPresets[builtInChoice.currentIndex].preset))
                confirmBuiltIn.checked = false
        }

        FileSelector {
            id: networkPresetLoadPath
            name: "networkPresetLoadPath"
            label: qsTr("Preset to load")
            filter: "*.json"
            save: false
            info: qsTr("Select a symptom preset JSON file to preview. The current assessment is unchanged until Apply loaded preset is pressed.")
        }
        Button {
            label: qsTr("Read preset again")
            enabled: networkPresetLoadPath.value !== ""
            onClicked: networkPresetLoadRequest.checked = !networkPresetLoadRequest.checked
            CheckBox {
                id: networkPresetLoadRequest
                name: "networkPresetLoadRequest"
                checked: false
                visible: false
            }
        }
        ComponentsList {
            id: presetPreview
            isBound: false
            title: qsTr("Loaded problems")
            rSource: "networkPresetPreview.names"
            addItemManually: false
            rowComponent: RowLayout {
                Label { text: rowValue; textFormat: Text.PlainText }
                DropDown {
                    name: "presetDescription"
                    isBound: false
                    visible: false
                    rSource: "networkPresetPreview.descriptions." + rowIndex
                }
            }
        }
        DropDown { id: loadedPresetName; isBound: false; visible: false; rSource: "networkPresetPreview.name" }
        DropDown { id: loadedSeverityMaximum; isBound: false; visible: false; rSource: "networkPresetPreview.severityMaximum" }
        DropDown { id: loadedConnectionMaximum; isBound: false; visible: false; rSource: "networkPresetPreview.connectionMaximum" }
        DropDown { id: loadedConnectionInput; isBound: false; visible: false; rSource: "networkPresetPreview.connectionInput" }
        DropDown { id: loadedToken; isBound: false; visible: false; rSource: "networkPresetPreview.loadedToken" }
        Label {
            visible: loadedPresetName.currentValue !== ""
            text: qsTr("%1 — severity maximum: %2; connection magnitude maximum: %3.")
                .arg(loadedPresetName.currentValue).arg(loadedSeverityMaximum.currentValue).arg(loadedConnectionMaximum.currentValue)
            textFormat: Text.PlainText
            wrapMode: Text.WordWrap
            Layout.preferredWidth: 400 * preferencesModel.uiScale
        }

        CheckBox {
            id: confirmLoaded
            isBound: false
            checked: false
            label: qsTr("Replace problems and clear ratings in every assessment")
        }
        Button {
            label: qsTr("Apply loaded preset")
            enabled: confirmLoaded.checked && networkForm.importedPresetReady()
            onClicked: if (networkForm.applyPreset(networkForm.importedPreset())) confirmLoaded.checked = false
        }
        TextField {
            id: networkPresetSaveName
            name: "networkPresetSaveName"
            label: qsTr("Preset name")
            value: qsTr("My symptom set")
        }
        FileSelector {
            id: networkPresetSavePath
            name: "networkPresetSavePath"
            label: qsTr("Preset destination")
            filter: "*.json"
            save: true
        }
        Button {
            label: qsTr("Save symptom preset")
            enabled: networkPresetSavePath.value !== "" && networkExportSession.value !== ""
            onClicked: networkPresetSaveRequest.checked = !networkPresetSaveRequest.checked
            CheckBox {
                id: networkPresetSaveRequest
                name: "networkPresetSaveRequest"
                checked: false
                visible: false
                onInitializedChanged: if (initialized) checked = false
            }
        }
        Label {
            text: qsTr("Only Save symptom preset writes the preset file. It replaces an existing file at the destination. Patient ratings and connections are excluded; review names and definitions before sharing.")
            textFormat: Text.PlainText
            wrapMode: Text.WordWrap
            Layout.preferredWidth: 400 * preferencesModel.uiScale
        }
    }

	ComponentsList
	{
		id: problems
		name: "problems"
		title: qsTr("Problems")
		minimumItems: 2
		maximumItems: 10
		headerLabels: [qsTr("Name"), qsTr("Shared severity")]
		defaultValues: [
			{"problemName": qsTr("Problem 1"), "problemSeverity": 0, "problemSeverityRated": false},
			{"problemName": qsTr("Problem 2"), "problemSeverity": 0, "problemSeverityRated": false},
			{"problemName": qsTr("Problem 3"), "problemSeverity": 0, "problemSeverityRated": false}
		]
		info: qsTr("Define problems and confirm each severity rating, including zero. Severity is shared across all assessment tabs. Unconnected problems remain visible.")

		rowComponent: RowLayout
		{
            ColumnLayout {
                TextField {
                    name: "problemName"
                    value: qsTr("Problem %1").arg(rowIndex + 1)
                    fieldWidth: 150 * preferencesModel.uiScale
                    info: qsTr("A unique name for this problem.")
                }
                TextField {
                    name: "problemDescription"
                    value: ""
                    placeholderText: qsTr("Optional definition")
                    fieldWidth: 150 * preferencesModel.uiScale
                    info: qsTr("Define what this problem means. Definitions enter reusable presets; avoid personal details before sharing.")
                }
            }

			ColumnLayout
			{
			    DoubleField {
			        id: problemSeverity
			        name: "problemSeverity"
			        visible: false
			        defaultValue: 0
			        min: 0
			        max: 1
			        negativeValues: false
			        decimals: 10
			    }
			    Common.NetworkRating {
			        boundField: problemSeverity
			        maximum: networkSeverityMaximum.value
			        signedRating: false
			        magnitudeMode: false
			        ratingLabel: qsTr("Problem severity; zero means none.")
			        onRecorded: problemSeverityRated.checked = true
			    }
			    CheckBox {
			        id: problemSeverityRated
			        name: "problemSeverityRated"
			        label: checked ? qsTr("Severity recorded") : qsTr("Not yet rated — confirm severity")
			        checked: false
			        info: qsTr("Confirm the severity, including a genuine zero. Unchecked values are shown as not rated and are not treated as zero severity.")
			    }
			}
		}
	}

	TabView
	{
		id: connectionList
		name: "connectionList"
		// title: qsTr("Problem Connections")
		maximumItems: 10
		newItemName: qsTr("Time %1").arg(connectionList.count + 1)
		optionKey: "name"
		info: qsTr("Name each assessment for its occasion or reference period. Rate its connections for the same period and context.")
		content: Group
		{
			childControlsArea.anchors.leftMargin: jaspTheme.contentMargin

			CheckBox
			{
				id: allConnections
				name: "allConnections"
				label: qsTr("All possible connections")
				checked: false
				info: qsTr("When checked, automatically creates all possible directed connections between problems. Adjust the strength of each connection using the sliders.")
			}

			ComponentsList
			{
				id: connections
				name: "connections"
				title: qsTr("Problem Connections")
				visible: !allConnections.checked
				preferredWidth: connectionList.width - 2 * jaspTheme.contentMargin
				minimumItems: 0
				maximumItems: 20
				headerLabels: [qsTr("From"), qsTr("To"), qsTr("Strength")]
				info: qsTr("Define perceived directed connections for the period represented by this tab. A positive rating means that an increase in the source increases the target; a negative rating means that it decreases the target. Rate the perceived strength, not your certainty. To show a network without connections, remove all rows. An unfinished row must be completed or removed.")
				defaultValues: []
				rowComponent: RowLayout
				{
					Layout.columnSpan: 4

					spacing: 24 * preferencesModel.uiScale

					DropDown
					{
						id: from
						name: "connectionFrom"
						source: "problems.problemName"
						addEmptyValue: true
						info: qsTr("The source problem of this connection.")
					}

					DropDown
					{
						id: to
						name: "connectionTo"
						source: "problems.problemName"
						addEmptyValue: true
						info: qsTr("The target problem of this connection.")
					}

					ColumnLayout
					{
					    DoubleField {
					        id: connectionStrength
					        name: "connectionStrength"
					        visible: false
					        defaultValue: 0
					        min: -1
					        max: 1
					        negativeValues: true
					        decimals: 10
					    }
					    Common.NetworkRating {
					        boundField: connectionStrength
					        maximum: networkConnectionMaximum.value
					        signedRating: true
					        magnitudeMode: networkConnectionInput.currentValue === "magnitude"
					        ratingLabel: qsTr("Perceived connection strength; zero means no perceived relationship.")

					    }

					}
				}
			}

			ComponentsList
			{
				id: allConnectionStrengths
				name: "allConnectionStrengths"
				visible: allConnections.checked
				source: "problems.problemName"
				title: qsTr("Connection Strengths")
				preferredWidth: connectionList.width - 2 * jaspTheme.contentMargin
				info: qsTr("Adjust the strength of each directed connection between problems.")
				rowComponent: Group
				{
					id: fromGroup
					preferredWidth: connectionList.width - 4 * jaspTheme.contentMargin
					property string fromName: rowValue

					Label
					{
						text: qsTr("From: %1").arg(rowValue)
						font.bold: true
					}

					ComponentsList
					{
						name: "targets"
						source: "problems.problemName"
						preferredWidth: connectionList.width - 6 * jaspTheme.contentMargin
						rowComponent: RowLayout
						{
							property bool isSelf: rowValue === fromGroup.fromName
							visible: !isSelf
							height: isSelf ? 0 : implicitHeight
							spacing: 10 * preferencesModel.uiScale

							Label
							{
								text: rowValue
								Layout.preferredWidth: 120 * preferencesModel.uiScale
							}

							ColumnLayout
							{
							    DoubleField {
							        id: allConnectionStrength
							        name: "connectionStrength"
							        visible: false
							        defaultValue: 0
							        min: -1
							        max: 1
							        negativeValues: true
							        decimals: 10
							    }
							    Common.NetworkRating {
							        boundField: allConnectionStrength
							        maximum: networkConnectionMaximum.value
							        signedRating: true
							        magnitudeMode: networkConnectionInput.currentValue === "magnitude"
							        ratingLabel: qsTr("Perceived connection strength; zero means no perceived relationship.")

							    }

							}
						}
					}
				}
			}

            Group {
                title: qsTr("Clear this assessment")
                CheckBox {
                    id: confirmClearConnections
                    isBound: false
                    checked: false
                    label: qsTr("Confirm removal of all connections in this assessment")
                    info: qsTr("Both manual and all-pairs ratings will be cleared. Problems, severity and other assessments are retained. Existing exported files are not changed.")
                }
                Button {
                    label: qsTr("Remove all connections in this assessment")
                    enabled: confirmClearConnections.checked
                    onClicked: if (networkForm.clearAssessment(connections, allConnectionStrengths, allConnections))
                        confirmClearConnections.checked = false
                }
            }

			Group
			{
				title: qsTr("Options")
				info: qsTr("Output options for this time point.")

				columns: 2

				CheckBox
				{
					name: "plotNetwork"
					label: qsTr("Network plot")
					checked: true
					info: qsTr("Displays a network plot for this time point showing all selected problems as nodes. Zero-rated connections produce no arrow; their ratings remain in the edge weight table and CSV export.")
				}

				CheckBox
				{
					name: "centrality"
					label: qsTr("Connection summaries")
					checked: false
					info: qsTr("Show shared severity, absolute strength and signed sums. Opposite signs can cancel. These descriptive summaries do not determine treatment priorities.")
				}

				CheckBox
				{
					name: "edgeWeightTable"
					label: qsTr("Edge weight table")
					checked: false
					info: qsTr("Displays a table listing all connections with their source, target, and weight for this time point.")
				}
			}
		}
	}

    Label {
        visible: networkForm.actionError !== ""
        text: networkForm.actionError
        textFormat: Text.PlainText
        wrapMode: Text.WordWrap
        Layout.preferredWidth: 400 * preferencesModel.uiScale
    }

	FileSelector
	{
		id:		networkSavePath
		name:	"networkSavePath"
		label:	qsTr("CSV destination")
		filter:	"*.csv"
		save:	true
		info:	qsTr("Choose a destination, then press Export CSV / Save again. Only that button writes the file. The export status identifies completed and omitted assessments.")
	}

	Button
	{
		label: qsTr("Export CSV / Save again")
		info: qsTr("Writes the current completed assessments to the selected CSV destination. An existing file at that destination will be replaced. Press again to retry after a save failure.")
		enabled: networkSavePath.value != "" && networkExportSession.value != ""
		onClicked: networkExportRequest.checked = !networkExportRequest.checked
		CheckBox
		{
			id: networkExportRequest
			name: "networkExportRequest"
			checked: false
			visible: false
			onInitializedChanged: if (initialized) checked = false
		}
	}

	TextField
	{
		id: networkExportSession
		name: "networkExportSession"
		value: ""
		visible: false
		// Initialization binds saved values before this signal and blocks analysis
		// updates until every control is ready. Never reuse a saved export session.
		onInitializedChanged: if (initialized) value = "session-" + Date.now() + "-" + Math.random()
	}

	Label
	{
		text: qsTr("Only Export CSV / Save again writes the file. An existing file at the selected destination will be replaced.")
		wrapMode: Text.WordWrap
		Layout.preferredWidth: 400 * preferencesModel.uiScale
	}

	Section
	{
		title: qsTr("Plots")
		info: qsTr("Options for customizing the appearance of network plots.")

		columns: 1

		Group
		{
			columns: 1

			DropDown
			{
				id: layout
				name: "plotLayout"
				label: qsTr("Layout")
				info: qsTr("The layout algorithm used to arrange the nodes in the network plot.")
				values: [

					{ label: qsTr("Circular"), value: "linear" },
					{ label: qsTr("Sugiyama"), value: "sugiyama" }
				]
			}

			DropDown
			{
				name: "colorPalette"
				label: qsTr("Color palette")
				info: qsTr("The color palette used for the severity fill in the network plot.")
				values: [
					{ label: qsTr("Gray"),		value: "gray"	 },
					{ label: qsTr("Viridis"),	value: "viridis" },
					{ label: qsTr("Blue"),		value: "blue"	 }
				]
			}
		}

		Group
		{
			columns: 2

			Group {
				title: qsTr("Problem Severity")
				info: qsTr("Control how problem severity is visually represented on the nodes.")

				CheckBox
				{
					name: "plotSeverityFill"
					label: qsTr("Color")
					info: qsTr("Map problem severity to the fill color of the node labels.")
				}

				CheckBox
				{
					name: "plotSeveritySize"
					label: qsTr("Size")
					checked: true
					info: qsTr("Map problem severity to the size of the nodes.")
				}

				CheckBox
				{
					name: "plotSeverityAlpha"
					label: qsTr("Opacity")
					info: qsTr("Map problem severity to the opacity of the nodes.")
				}
			}

			Group {
				title: qsTr("Connection Strength")
				info: qsTr("Control how connection strength is visually represented on the edges.")

				CheckBox
				{
					name: "plotStrengthColor"
					label: qsTr("Color")
					checked: true
					info: qsTr("Map connection strength to the color of the edges.")
				}

				CheckBox
				{
					name: "plotStrengthWidth"
					label: qsTr("Width")
					checked: true
					info: qsTr("Map connection strength to the width of the edges.")
				}

				CheckBox
				{
					name: "plotStrengthAlpha"
					label: qsTr("Opacity")
					info: qsTr("Map connection strength to the opacity of the edges.")
				}
			}
		}
	}
}
