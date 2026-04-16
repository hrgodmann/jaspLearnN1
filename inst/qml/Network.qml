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
import QtQuick.Layouts
import JASP
import JASP.Controls

import "./common" as Common

Form
{
	Group
	{
		columns: 2

		Common.IntroText{}
	}

	ComponentsList
	{
		id: problems
		name: "problems"
		title: qsTr("Problems")
		minimumItems: 2
		maximumItems: 10
		headerLabels: [qsTr("Name"), qsTr("Severity")]
		defaultValues: [
			{"problemName": qsTr("Problem 1"), "problemSeverity": 0.5},
			{"problemName": qsTr("Problem 2"), "problemSeverity": 0.5},
			{"problemName": qsTr("Problem 3"), "problemSeverity": 0.5}
		]
		info: qsTr("Define the problems (symptoms) and their severity. Use the slider to set the severity of each problem between 0 and 1.")

		rowComponent: RowLayout
		{
			TextField
			{
				name: "problemName"
				value: qsTr("New Problem")
				fieldWidth: 150 * preferencesModel.uiScale
				info: qsTr("The name of this problem.")
			}

			Slider
			{
				name: "problemSeverity"
				value: 0.5
				min: 0
				max: 1
				vertical: false
				info: qsTr("The severity of this problem, ranging from 0 (none) to 1 (maximum).")
			}
		}
	}

	TabView
	{
		id: connectionList
		name: "connectionList"
		// title: qsTr("Problem Connections")
		maximumItems: 10
		newItemName: qsTr("Time ") + (connectionList.count + 1)
		optionKey: "name"
		info: qsTr("Each tab represents a time point. Define the connections between problems and their strengths for each time point.")
		content: Group
		{
			childControlsArea.anchors.leftMargin: jaspTheme.contentMargin

			ComponentsList
			{
				id: connections
				name: "connections"
				title: qsTr("Problem Connections")
				preferredWidth: connectionList.width - 2 * jaspTheme.contentMargin
				minimumItems: 0
				maximumItems: 20
				headerLabels: [qsTr("From"), qsTr("To"), qsTr("Strength")]
				info: qsTr("Define the directed connections between problems. Select the source and target problems, and set the strength of each connection.")
				defaultValues: connectionList.count === 1 ? [
					{"connectionFrom": qsTr("Problem 1"), "connectionTo": qsTr("Problem 2"), "connectionStrength": 0.5},
					{"connectionFrom": qsTr("Problem 2"), "connectionTo": qsTr("Problem 3"), "connectionStrength": -0.5},
					{"connectionFrom": qsTr("Problem 1"), "connectionTo": qsTr("Problem 3"), "connectionStrength": -0.5}
				] : [{"connectionFrom": "", "connectionTo": "", "connectionStrength": 0}]
				rowComponent: RowLayout
				{
					Layout.columnSpan: 4

					spacing: 72 * preferencesModel.uiScale

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

					Slider
					{
						name: "connectionStrength"
						value: 0
						min: -1
						max: 1
						vertical: false
						info: qsTr("The strength of this connection, ranging from -1 (strong negative) to 1 (strong positive).")
					}
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
					info: qsTr("Displays a network plot for this time point showing problems as nodes and connections as edges.")
				}

				CheckBox
				{
					name: "centrality"
					label: qsTr("Centrality statistics")
					checked: false
					info: qsTr("Displays a table with in-degree and out-degree centrality for each problem at this time point.")
				}
			}
		}
	}

	FileSelector
	{
		name:	"problemSavePath"
		label:	qsTr("Save problems")
		filter:	"*.csv"
		save:	true
		info:	qsTr("Saves the problem names and severity values to a .csv file.")
	}

	FileSelector
	{
		name:	"connectionSavePath"
		label:	qsTr("Save connections")
		filter:	"*.csv"
		save:	true
		info:	qsTr("Saves the connection definitions and strengths to a .csv file.")
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
					{ label: qsTr("Viridis"),	value: "viridis" },
					{ label: qsTr("Gray"),		value: "gray"	 },
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
