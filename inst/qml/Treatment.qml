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
	id: treatmentForm

	// Phase names for the comparison selectors in simulation mode. Reading count,
	// columnsNames and each name field's value makes QML re-evaluate the binding when
	// rows are added, removed, initialized or renamed. JASP's native
	// "simPhaseEffects.simPhaseName" source did not show the default phases in JASP 0.98.1.
	function simulationPhaseNames()
	{
		var rowCount = simPhaseEffects.count
		var keys = simPhaseEffects.columnsNames
		var names = []
		for (var i = 0; i < keys.length; ++i)
		{
			var field = simPhaseEffects.getRowControl(keys[i], "simPhaseName")
			var name = field && field.value !== undefined && field.value !== null ? String(field.value) : ""
			if (name.trim() !== "" && names.indexOf(name) < 0)
				names.push(name)
		}
		return names
	}

	// JASP notifies levelsChanged for assignment and label edits, but a pure
	// label reorder only reaches the assigned list's model in JASP 0.98.1.
	property int phaseLevelsRevision: 0
	readonly property QtObject phaseLevelConnections: Connections
	{
		target: phaseVariable.model
		function onLabelsReordered() { treatmentForm.phaseLevelsRevision += 1 }
	}

	function loadedPhaseNames()
	{
		var revision = phaseLevelsRevision
		return phaseVariable.levels
	}

	Group
	{
		columns: 2

		Common.IntroText{}
	}

	Section
	{
		title: qsTr("Data")
		id: sectionData
		expanded: true
		columns:1
		info: qsTr("Specify the data source: simulate data with configurable parameters or load variables from a dataset.")

		Common.InputType
		{
			id: inputType
		}

		VariablesForm
		{
			visible: inputType.value == "loadData"

			AvailableVariablesList { name: "allVariablesList" }
			AssignedVariablesList
			{
				name:				"dependent"
				title:				qsTr("Dependent Variable")
				allowedColumns:		["scale"]
				singleVariable:		true
				info:				qsTr("The outcome measured over time (e.g., symptom severity). For a missing measurement, keep the row and fill in its time and phase, leaving only the outcome empty. Missing outcomes are not imputed.")
			}
			AssignedVariablesList
			{
				name:			"time"
				title:			qsTr("Time")
				allowedColumns:	["scale"]
				singleVariable:	true
				info:			qsTr("Use one equally spaced numeric clock across all phases. Keep time and phase filled in for missing outcomes. Rows are sorted by time.")
			}
			AssignedVariablesList
			{
				id:				phaseVariable
				name:			"phase"
				title:			qsTr("Phase Variable")
				allowedColumns:	["nominal"]
				singleVariable:	true
				info:			qsTr("Give each continuous phase a distinct label, such as Baseline 1, Treatment, and Baseline 2. Fill in phase labels even for missing outcomes.")
			}
		}

		Group
		{
			title: qsTr("Simulation Options")
			visible: inputType.value == "simulateData"
			info: qsTr("Configure the parameters for generating simulated treatment data.")

			Group
			{
				columns: 2

				Group
				{
					title: qsTr("Dependent Variable")
					info: qsTr("Parameters for the simulated dependent variable.")

					DoubleField
					{
						name: "simDependentMean"
						label: qsTr("Mean")
						defaultValue: 0.0
						info: qsTr("The mean of the dependent variable before adding phase and time effects.")
					}

					DoubleField
					{
						name: "simDependentSd"
						label: qsTr("Standard deviation")
						defaultValue: 1.0
						min: 0
						info: qsTr("Noise standard deviation. Zero shows a deterministic series in the data plot; model inference requires residual variation.")
					}
				}

				Group
				{
					title: qsTr("Time")
					info: qsTr("Parameters for the simulated time effects.")

					DoubleField
					{
						name: "simTimeEffect"
						label: qsTr("Phase-local effect")
						defaultValue: 0.0
						negativeValues: true
						info: qsTr("The trend restarts at time 1 in each phase. A nonzero trend can therefore create a jump even when phase effects are zero. Autocorrelation remains continuous.")
					}

					DoubleField
					{
						name: "simTimeEffectAutocorrelation"
						label: qsTr("Autocorrelation")
						defaultValue: 0
						min: -1
						max: 1
						inclusive: JASP.None
						negativeValues: true
						info: qsTr("The first-order correlation of the noise. Values must be strictly between -1 and 1.")
					}
				}
			}

			IntegerField
			{
				name: "seed"
				label: qsTr("Seed")
				defaultValue: 1
				min: 0
				info: qsTr("Sets the random number generator seed for reproducible simulations.")
			}

			Group
			{
				title: qsTr("Phase Effect")
				info: qsTr("Define the treatment phases and their effects on the dependent variable.")

				ComponentsList
				{
					id: simPhaseEffects
					name: "simPhaseEffects"
					preferredWidth: sectionData.width - 8 * jaspTheme.contentMargin
					minimumItems: 2
					headerLabels: [qsTr("Name"), qsTr("Phase"), qsTr("Phase × Time"), qsTr("Time points")]
					info: qsTr("Each row defines a treatment phase with its name, effect on the dependent variable, interaction with time, and number of time points.")
					defaultValues: [
						{"simPhaseName": "Pre-treatment", "simPhaseEffectSimple": 0.0, "simPhaseEffectInteraction": 0.0, "simPhaseEffectN": 20},
						{"simPhaseName": "Treatment", "simPhaseEffectSimple": 5.0, "simPhaseEffectInteraction": 0.0, "simPhaseEffectN": 20},
						{"simPhaseName": "Post-treatment", "simPhaseEffectSimple": 5.0, "simPhaseEffectInteraction": -0.1, "simPhaseEffectN": 20}
					]
					rowComponent: RowLayout
					{
						Layout.columnSpan: 4

						spacing: 72 * preferencesModel.uiScale

						TextField
						{
							name: "simPhaseName"
							defaultValue: qsTr("Phase %1").arg(rowIndex + 1)
							info: qsTr("The name of this treatment phase.")
						}

						DoubleField
						{
							name: "simPhaseEffectSimple"
							defaultValue: 0.0
							negativeValues: true
							info: qsTr("The constant effect of this phase on the dependent variable.")
						}

						DoubleField
						{
							name: "simPhaseEffectInteraction"
							defaultValue: 0.0
							negativeValues: true
							info: qsTr("The interaction effect between this phase and time.")
						}

						IntegerField
						{
							name: "simPhaseEffectN"
							defaultValue: 20
							min: 2
							max: 1000
							info: qsTr("At least two time points per phase are needed to estimate trends; uncertainty needs more than two per phase on average. These are estimation limits, not reliable-inference guarantees. Simulations allow at most 1,000 time points across all phases to keep fitting responsive.")
						}
					}
				}
				Label { text: qsTr("Maximum 1,000 time points across all phases.") }
			}
		}

		CIField { name: "coefficientCiLevel"; label: qsTr("Confidence interval"); info: qsTr("The confidence level for model coefficients, phase comparisons, phase estimates, and autocorrelation.") }
	}

	Section
	{
		title: qsTr("Phase Comparisons")
		expanded: true
		columns: 1

		CheckBox
		{
			name: "phaseComparisons"
			label: qsTr("Compare phase endpoints and slopes")
			checked: true
			info: qsTr("Compare each phase's own endpoint and its forward trend. Endpoint differences concern two different times, not treatment-onset change. Approximate tests do not identify what caused a difference.")

			DropDown
			{
				name: "comparisonPhase"
				label: qsTr("Compared phase")
				// Initialize after the input type, the simulation rows and the phase variable.
				depends: [inputType, simPhaseEffects, "phase"]
				// One values binding avoids both intermediate resets and JASP 0.98.1's
				// sticky useSourceLevels flag when a source switches between modes.
				values: inputType.value == "simulateData"
					? treatmentForm.simulationPhaseNames()
					: treatmentForm.loadedPhaseNames()
				addEmptyValue: true
				indexDefaultValue: 0
				placeholderText: qsTr("Second phase in time (automatic)")
				info: qsTr("Choose the phase from which the reference phase is subtracted. The automatic choice is the second phase in chronological order of first occurrence, not alphabetical order.")
			}

			DropDown
			{
				name: "referencePhase"
				label: qsTr("Reference phase")
				depends: [inputType, simPhaseEffects, "phase"]
				// Keep both selectors on the same labels-only path.
				values: inputType.value == "simulateData"
					? treatmentForm.simulationPhaseNames()
					: treatmentForm.loadedPhaseNames()
				addEmptyValue: true
				indexDefaultValue: 0
				placeholderText: qsTr("First phase in time (automatic)")
				info: qsTr("Choose the phase subtracted from the comparison phase. The automatic choice is the first phase in chronological order of first occurrence, not alphabetical order.")
			}
		}
	}

	Section
	{
		title: qsTr("Output Options")
		info: qsTr("Configure which plots and tables are shown in the output.")

		CheckBox
		{
			name: "plotData"
			label: qsTr("Plot data")
			checked: true
			info: qsTr("Displays a plot of the data with time on the x-axis and the dependent variable on the y-axis, colored by phase.")
		}

		CheckBox
		{
			name: "plotAnalysis"
			label: qsTr("Plot analysis")
			checked: true
			info: qsTr("Displays the data with fitted regression lines per treatment phase, showing estimated level shifts and trend changes.")
		}

		CheckBox
		{
			name: "phaseSummary"
			label: qsTr("Phase estimates")
			checked: false
			info: qsTr("Reports each phase's fitted level at its own last scheduled time and its slope per unit of forward time, with confidence intervals. A missing outcome at the last scheduled time does not move the endpoint to an earlier measurement.")
		}

		CheckBox
		{
			name: "coefficientsTable"
			label: qsTr("Coefficients table")
			checked: false
			info: qsTr("Displays the model coefficients with standard errors, t-values, p-values, and confidence intervals. Intercept and phase main-effect coefficients describe levels and differences at regression time 0. Use Phase Comparisons for differences between named phases at their own endpoints and between their slopes.")
		}

		CheckBox
		{
			name: "autocorrelationTable"
			label: qsTr("Autocorrelation table")
			checked: false
			info: qsTr("Displays the estimated residual correlation over one measurement interval, with confidence interval. Missing measurements preserve the elapsed number of intervals.")
		}
	}
}
