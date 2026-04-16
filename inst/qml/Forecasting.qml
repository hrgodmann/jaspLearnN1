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

		CheckBox
		{
			name: "plotData"
			label: qsTr("Plot data")
			checked: true
			info: qsTr("Displays a plot of the observed time series data.")
		}
	}

	Section
	{
		title: qsTr("Data")
		id: sectionData
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
				info:			qsTr("The time variable indicating when each observation was recorded.")
			}
			AssignedVariablesList
			{
				name:			"covariates"
				title:			qsTr("Covariates")
				allowedColumns:	["scale"]
				info:			qsTr("Additional predictor variables to include in the model.")
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
					label: "Noise std. deviation"
					defaultValue: 1.0
					info: qsTr("The standard deviation of the noise component in the simulated ARIMA process.")
				}

				IntegerField
				{
					name: "numSamples"
					label: qsTr("N")
					defaultValue: 100
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
	}

	Section
	{
		title: qsTr("Estimation Options")
		info: qsTr("Additional analysis options.")

		CIField { name: "coefficientCiLevel"; label: qsTr("Confidence interval"); info: qsTr("The confidence level for the coefficient confidence intervals.") }
	}

	Section
	{
		title: qsTr("Plots")
		info: qsTr("Options for customizing data plots.")

		Group
		{
			title: qsTr("Data plot")
			info: qsTr("Options for the observed data plot.")

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
		info: qsTr("Configure and display forecasts based on the fitted ARIMA model.")
		IntegerField
		{
			name: "forecastLength"
			id: forecastLength
			label: qsTr("Number of forecasts")
			min: 0
			max: 1e6
			defaultValue: 0
			info: qsTr("Determines the number forecasts to make.")
		}
		FileSelector
		{
			name:				"forecastSave"
			label:				qsTr("Save forecasts as")
			placeholderText:	qsTr("e.g. forecasts.csv")
			filter:				"*.csv"
			save:				true
			enabled:			forecastLength.value > 0
			fieldWidth:			180 * preferencesModel.uiScale
			info:				qsTr("Saves the forecasts in a seperate .csv file.")
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
