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
				info:			qsTr("A continuous numeric time variable across all phases. Values must be complete, unique and equally spaced; rows are sorted by time. Do not restart time at each phase. Keep a row for every missing measurement occasion. The selected values also define the time scale of the regression trends.")
			}
			AssignedVariablesList
			{
				name:			"phase"
				title:			qsTr("Phase Variable")
				allowedColumns:	["nominal"]
				singleVariable:	true
				info:			qsTr("A categorical variable indicating the treatment phase (e.g., pre-treatment, treatment, post-treatment). Every row needs a phase, including missing outcomes. Autocorrelation continues across phase boundaries.")
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
						label: "Mean"
						defaultValue: 0.0
						info: qsTr("The mean of the dependent variable before adding phase and time effects.")
					}

					DoubleField
					{
						name: "simDependentSd"
						label: "Standard deviation"
						defaultValue: 1.0
						info: qsTr("The standard deviation of the noise added to the simulated data.")
					}
				}

				Group
				{
					title: qsTr("Time")
					info: qsTr("Parameters for the simulated time effects.")

					DoubleField
					{
						name: "simTimeEffect"
						label: qsTr("Effect")
						defaultValue: 0.0
						negativeValues: true
						info: qsTr("The linear effect of time on the dependent variable. In simulations, regression time starts at 1 within each phase; autocorrelation uses a continuous sequence across all phases.")
					}

					DoubleField
					{
						name: "simTimeEffectAutocorrelation"
						label: qsTr("Autocorrelation")
						defaultValue: 0
						min: -1
						max: 1
						negativeValues: true
						info: qsTr("The first-order autocorrelation of the noise process. Values range from -1 to 1.")
					}
				}
			}

			IntegerField
			{
				name: "seed"
				label: qsTr("Seed")
				defaultValue: 1
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
					minimumItems: 1
					headerLabels: [qsTr("Name"), qsTr("Phase"), qsTr("Phase × Time"), qsTr("Time points")]
					info: qsTr("Each row defines a treatment phase with its name, effect on the dependent variable, interaction with time, and number of time points.")
					defaultValues: [
						{"simPhaseName": "Pre-treament", "simPhaseEffectSimple": 0.0, "simPhaseEffectInteraction": 0.0, "simPhaseEffectN": 20},
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
							defaultValue: qsTr("Phase ") + (rowIndex + 1)
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
							info: qsTr("The number of time points in this phase.")
						}
					}
				}
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
			info: qsTr("Reports the comparison phase minus the reference phase for fitted endpoint levels and slopes. Each endpoint is evaluated at that phase's own last scheduled time, including a scheduled time with a missing outcome; this is not a change at treatment onset. Slopes describe change per unit of forward time. Confidence intervals and p-values use approximate inference from the AR(1) model estimated by REML and may be unreliable in short series or with strong autocorrelation.")

			DropDown
			{
				name: "comparisonPhase"
				label: qsTr("Compared phase")
				source: inputType.value == "simulateData" ? ["simPhaseEffects.simPhaseName"] : [{name: "phase", use: "levels"}]
				addEmptyValue: true
				indexDefaultValue: 0
				placeholderText: qsTr("Second phase in time (automatic)")
				info: qsTr("Choose the phase from which the reference phase is subtracted. The automatic choice is the second phase in chronological order of first occurrence, not alphabetical order.")
			}

			DropDown
			{
				name: "referencePhase"
				label: qsTr("Reference phase")
				source: inputType.value == "simulateData" ? ["simPhaseEffects.simPhaseName"] : [{name: "phase", use: "levels"}]
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
