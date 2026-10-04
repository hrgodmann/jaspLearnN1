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
		info: qsTr("Specify the data source: simulate data with configurable ARIMA parameters or load variables from a dataset.")

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
				info:				qsTr("The time series variable to model and forecast.")
			}
			AssignedVariablesList
			{
				name:			"time"
				title:			qsTr("Time")
				singleVariable: true
				allowedColumns: ["scale"]
				info:			qsTr("Numeric, equally spaced measurement times. Rows are ordered by this variable. Include a row for each measurement occasion, including missing outcomes. Without a time variable, row order defines consecutive measurement steps.")
			}
			AssignedVariablesList
			{
				name:			"covariates"
				title:			qsTr("Covariates")
				allowedColumns:	["scale"]
				info:			qsTr("Numeric predictors fitted jointly with ARIMA errors. Historical predictor values must be complete. To forecast, append rows with an empty outcome and a value for every predictor at every requested future time. These can be known values or an explicit scenario.")
			}
		}

		Group
		{
			title: qsTr("Simulation Options")
			visible: inputType.value == "simulateData"
			info: qsTr("Configure the ARIMA model parameters for generating simulated data.")

			columns: 1

			Group
			{
				columns: 2

				DoubleField
				{
					name: "noiseSd"
					label: qsTr("Noise standard deviation")
					defaultValue: 1.0
					min: 0
					info: qsTr("The standard deviation of the noise component in the simulated ARIMA process.")
				}

				IntegerField
				{
					name: "numSamples"
					label: qsTr("N")
					defaultValue: 100
					min: 2
					info: qsTr("The number of time points to simulate.")
				}
			}

			Group
			{
				columns: 1

				Group
				{
					title: qsTr("Autoregressive (AR) order p")
					info: qsTr("Specify the autoregressive coefficients. Each row represents a lag with its corresponding effect size.")

					ComponentsList
					{
						name: "simArEffects"
						preferredWidth: (sectionData.width - 8 * jaspTheme.contentMargin) / 2
						headerLabels: [qsTr("Lag"), qsTr("Effect")]
						info: qsTr("Add rows to increase the AR order. Each row specifies the effect at the given lag.")
						defaultValues: [
							{"simArLag": 1, "simArEffect": 0.2}
						]
						rowComponent: RowLayout
						{
							IntegerField
							{
								name: "simArLag"
								enabled: false
								defaultValue: rowIndex + 1
								info: qsTr("The lag number for this autoregressive term.")
							}
						
							DoubleField
							{
								name: "simArEffect"
								defaultValue: 0.2
								negativeValues: true
								info: qsTr("The autoregressive coefficient at this lag.")
							}
						}
					}
				}

				IntegerField
				{
					name:   "simIEffect"
					id:     d
					label:  qsTr("Difference (I) degree d")
					defaultValue: 1
					min: 0
					info: qsTr("The degree of differencing applied to the time series to achieve stationarity.")
				}

				Group
				{
					title: qsTr("Moving average (MA) order q")
					info: qsTr("Specify the moving average coefficients. Each row represents a lag with its corresponding effect size.")

					ComponentsList
					{
						name: "simMaEffects"
						preferredWidth: (sectionData.width - 8 * jaspTheme.contentMargin) / 2
						headerLabels: [qsTr("Lag"), qsTr("Effect")]
						info: qsTr("Add rows to increase the MA order. Each row specifies the effect at the given lag.")
						defaultValues: [
							{"simMaLag": 1, "simMaEffect": 0.8}
						]
						rowComponent: RowLayout
						{
							IntegerField
							{
								name: "simMaLag"
								enabled: false
								defaultValue: rowIndex + 1
								info: qsTr("The lag number for this moving average term.")
							}
						
							DoubleField
							{
								name: "simMaEffect"
								defaultValue: 0.8
								negativeValues: true
								info: qsTr("The moving average coefficient at this lag.")
							}
						}
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
		}

		CIField { name: "coefficientCiLevel"; label: qsTr("Confidence interval"); info: qsTr("The confidence level for the coefficient confidence intervals.") }
	}

	Section
	{
		title: qsTr("Model")
		info: qsTr("Specify how the ARIMA model is selected.")

		RadioButtonGroup
		{
			name: "modelSpecification"
			title: qsTr("Model specification")
			info: qsTr("Choose whether the ARIMA order is selected automatically or specified manually.")

			RadioButton
			{
				value: "auto"
				id: autoModel
				label: qsTr("Automatic")
				checked: true
				info: qsTr("Automatically selects the best-fitting ARIMA model using the selected information criterion.")

				RadioButtonGroup
				{
					name: "modelSpecificationAutoIc"
					title: qsTr("Information criterion")
					radioButtonsOnSameRow: true
					info: qsTr("The information criterion used for automatic ARIMA model selection.")

					RadioButton { value: "aicc";	label: qsTr("AICc");	checked: true;	info: qsTr("Corrected Akaike information criterion.") }
					RadioButton { value: "aic";	label: qsTr("AIC");						info: qsTr("Akaike information criterion.") }
					RadioButton { value: "bic";	label: qsTr("BIC");						info: qsTr("Bayesian information criterion.") }
				}
			}

			RadioButton
			{
				value: "custom"
				id: manualModel
				label: qsTr("Manual")
				info: qsTr("Fits the chosen orders, including a mean at d = 0 or drift at d = 1. No constant is included for d > 1.")

				Group
				{
					columns: 1

					IntegerField
					{
						name: "p"
						label: qsTr("Autoregressive (AR) order p")
						defaultValue: 1
						min: 0
						info: qsTr("The autoregressive order of the fitted ARIMA model.")
					}

					IntegerField
					{
						name: "d"
						label: qsTr("Difference (I) degree d")
						defaultValue: 0
						min: 0
						info: qsTr("The differencing order of the fitted ARIMA model.")
					}

					IntegerField
					{
						name: "q"
						label: qsTr("Moving average (MA) order q")
						defaultValue: 0
						min: 0
						info: qsTr("The moving average order of the fitted ARIMA model.")
					}
				}
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
			info: qsTr("Displays a plot of the observed time series data.")

			CheckBox
			{
				name: "plotPoints"
				label: qsTr("Points")
				checked: true
				info: qsTr("Show individual data points in the data plot.")
			}

			CheckBox
			{
				name: "plotLine"
				label: qsTr("Line")
				checked: true
				info: qsTr("Show a connecting line between data points in the data plot.")
			}
		}
	}

	Section
	{
		title: qsTr("Forecasting")
		info: qsTr("Forecasts start after the last observed outcome. With covariates, supply future predictor values and leave future outcomes empty. Intervals condition on the fitted model and supplied predictor values; parameter, model-selection and predictor-scenario uncertainty are not included.")
		IntegerField
		{
			name: "forecastLength"
			id: forecastLength
			label: qsTr("Number of forecasts")
			min: 0
			max: 10000
			defaultValue: 0
			info: qsTr("Measurement steps to forecast. Supply this many future predictor rows when using covariates. The 10000-step limit controls resource use, not statistical reliability.")
		}
		FileSelector
		{
			id: forecastSave
			name:				"forecastSave"
			label:				qsTr("Save forecasts as")
			placeholderText:	qsTr("e.g. forecasts.csv")
			filter:				"*.csv"
			save:				true
			enabled:			forecastLength.value > 0
			fieldWidth:			180 * preferencesModel.uiScale
			info:				qsTr("Choose the CSV destination. Selecting a file or editing the analysis does not save; press Export CSV / Save again.")
		}
		Button
		{
			label: qsTr("Export CSV / Save again")
			enabled: forecastSave.value !== "" && forecastExportSession.value !== "" && forecastLength.value > 0
			onClicked: forecastExportRequest.checked = !forecastExportRequest.checked
			CheckBox
			{
				id: forecastExportRequest
				name: "forecastExportRequest"
				checked: false
				visible: false
				onInitializedChanged: if (initialized) checked = false
			}
		}
		TextField
		{
			id: forecastExportSession
			name: "forecastExportSession"
			value: ""
			visible: false
			// Saved controls bind before this signal; JASP blocks analysis updates
			// until initialization completes. Never reuse a saved write session.
			onInitializedChanged: if (initialized) value = "session-" + Date.now() + "-" + Math.random()
		}
		Label
		{
			text: qsTr("Only Export CSV / Save again writes the file. An existing file at the selected destination will be replaced.")
			wrapMode: Text.WordWrap
			Layout.preferredWidth: 400 * preferencesModel.uiScale
		}
		CheckBox
		{
			name:		"forecastTimeSeries"
			id:			forecastTimeSeries
			label:		qsTr("Time series plot")
			info:		qsTr("Plots the forecasts (and observed values) (y-axis) over time (x-axis)")
			RadioButtonGroup
			{
				name:	"forecastTimeSeriesType"
				radioButtonsOnSameRow: true
				info: qsTr("Choose how the forecasts are displayed in the time series plot.")
				RadioButton { value: "points";	label: qsTr("Points"); info: qsTr("Display forecasts as individual points.") }
				RadioButton { value: "line";	label: qsTr("Line"); info: qsTr("Display forecasts as a connected line.") }
				RadioButton { value: "both";	label: qsTr("Both");	checked: true; info: qsTr("Display forecasts as both points and a line.") }
			}
			CheckBox
			{
				name:		"forecastTimeSeriesObserved"
				id:			forecastTimeSeriesObserved
				label:		qsTr("Observed data")
				checked:	true
				info:		qsTr("Include the observed data alongside the forecasts in the time series plot.")
			}
		}
		CheckBox
		{
			name:	"forecastTable"
			id:		forecastTable
			label:	qsTr("Forecasts table")
			info:	qsTr("Displays a table with the forecasted values.")
		}
	}
}
