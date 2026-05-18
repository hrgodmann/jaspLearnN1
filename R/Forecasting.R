#
# Copyright (C) 2025 University of Amsterdam and Netherlands eScience Center
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

Forecasting <- function(jaspResults, dataset = NULL, options) {
  jaspResults$title <- gettext("How Do Symptoms Develop?")

  .ln1Intro(jaspResults, options, .ln1ForeIntroText)

  if (options[["inputType"]] == "loadData") {
    ready <- options[["dependent"]] != ""
  } else {
    ready <- TRUE
  }

  dataset <- .ln1ForeData(jaspResults, dataset, options, ready)

  .ln1ForeCreateDataPlot(jaspResults, dataset, options, .ln1ForeGetDataDependencies, ready)

  .ln1ForeEstimateModel(jaspResults, dataset, options, ready)

  options[["intercept"]] <- TRUE # Required by .tsCreateTableCoefficients

  jaspTimeSeries:::.tsCreateTableCoefficients(
    jaspResults,
    jaspResults[["modelState"]]$object,
    dataset,
    options,
    ready,
    2,
    .ln1ForeGetDataDependencies()
  )

  jaspTimeSeries:::.tsForecastPlot(
    jaspResults,
    jaspResults[["modelState"]]$object,
    dataset,
    dataset,
    options,
    ready,
    3,
    c(.ln1ForeGetDataDependencies(),
      "forecastLength",
      "forecastTimeSeries",
      "forecastTimeSeriesType",
      "forecastTimeSeriesObserved")
  )

  jaspTimeSeries:::.tsCreateTableForecasts(
    jaspResults,
    jaspResults[["modelState"]]$object,
    dataset,
    dataset,
    options,
    ready,
    4,
    c(.ln1ForeGetDataDependencies(),
      "forecastLength",
      "forecastTable")
  )

  jaspTimeSeries:::.tsSaveForecasts(
    jaspResults,
    jaspResults[["modelState"]]$object,
    dataset,
    dataset,
    options,
    ready
  )

  return()
}

.ln1ForeIntroText <- function() {
  return(gettext("Forecasting is an analysis to make predictions on the future development of a dynamic process.  You can think of making predicting on the weather or on the economy. In clinical practice, forecasting can be applied to the symptom develop of a client during treatment. This helps to anticipate new developments. For example when the expectation is that a client wont improve much, a therapist and client can discuss if a different therapeutic strategy is needed. However, if it is expected that more progress will be made, a treatment should of course continue as planned. 

<b>How does it work?</b> 

Forecasting is an umbrella term that covers many different analytical techniques, several of which are implemented in JASP. This tutorial focuses on the autoregressive integrated moving average (ARIMA) model, a widely used approach in forecasting. The autoregressive (AR) part means that the current value of a variable is predicted from its previous values. The moving average (MA) part means that the current value is also influenced by past prediction errors. The integrated (I) part means that the model uses differences between consecutive observations (rather than the raw values) to handle trends in the data.

<b>Practical considerations</b> 

A key practical question in clinical forecasting is how often to collect measurements. In general, more frequent measurements can lead to better predictions. At the same time, it is important to consider what is feasible for a client and at which time scale meaningful changes actually occur. For example, if symptoms do not change much from day to day, daily assessment will not add much information. On the other hand, if measurements are taken only once every three months, important changes in the treatment process may be missed, making predictions less accurate. As a rule of thumb, weekly measurements are often a reasonable starting point. Forecasting models focus on detecting trends, by learning from past data, therapists can adjust treatment plans proactively, rather than only responding after a patient’s condition has worsened. However, the accuracy of these predictions depends strongly on the quality and level of detail (granularity) of the available data. 

<b>Interpretation and limitations</b> 

Forecasting provides an prediction on the future clinical outcome. Note that -just like any prediction- this comes with an uncertainty, or prediction error. Forecasting therefore just gives a likely outcome, but this does not mean it will happen. Also, the further into the future you forecast, the less confident the model becomes, which is reflected in wider uncertainty ranges. Specifically, the ARIMA model only work well with relatively stable data, consistent way over time, and it struggles when something unexpected shifts the trend. It also has no awareness of outside factors that might influence the data. If set up poorly, it can also memorise past data rather than learning genuine patterns, leading to poor real-world predictions. 

"))
} 

.ln1ForeData <- function(jaspResults, dataset, options, ready) {
  if (!ready)
    return(NULL)

  if (options[["inputType"]] == "simulateData") {
    if (is.null(jaspResults[["dataState"]])) {
      dataset <- .ln1ForeSimulateData(options)
      dataState <- createJaspState(object = dataset)
      dataState$dependOn(.ln1ForeGetDataDependencies())
      jaspResults[["dataState"]] <- dataState
    } else {
      dataset <- jaspResults[["dataState"]]$object
    }
  } else {
    columnsNumeric <- options[["dependent"]]
    if (options[["time"]] != "")
      columnsNumeric <- c(columnsNumeric, options[["time"]])
    dataset <- .readDataSetToEnd(
      columns.as.numeric = columnsNumeric
    )
    dataset[["y"]] <- dataset[[options[["dependent"]]]]
    if (options[["time"]] != "") {
      dataset[["t"]] <- dataset[[options[["time"]]]]
    } else {
      dataset[["t"]] <- seq_len(nrow(dataset))
    }
  }

  return(dataset)
}

.ln1ForeGetDataDependencies <- function() {
  return(c(
    "inputType",
    "dependent",
    "time",
    "covariates",
    "noiseSd",
    "simArEffects",
    "simIEffect",
    "simMaEffects",
    "numSamples",
    "seed"
  ))
}

.ln1ForeSimulateData <- function(options) {
  set.seed(options[["seed"]])

  arEffects <- sapply(options[["simArEffects"]], function(x) x[["simArEffect"]])
  maEffects <- sapply(options[["simMaEffects"]], function(x) x[["simMaEffect"]])

  y <- stats::arima.sim(
    model = list(
      "ar" = arEffects,
      "ma" = maEffects,
      "order" = c(length(arEffects), options[["simIEffect"]], length(maEffects))
    ),
    n = options[["numSamples"]],
    sd = options[["noiseSd"]]
  )

  y <- y[-1]

  simData <- data.frame(
    y = as.numeric(y),
    t = seq_along(y),
    phase = 0
  )

  return(simData)
}

.ln1ForeEstimateModel <- function(jaspResults, dataset, options, ready) {
  if (ready && is.null(jaspResults[["modelState"]])) {
    modelObject <- .ln1ForeEstimateModelHelper(dataset, options)
    modelState <- createJaspState(object = modelObject)
    modelState$dependOn(.ln1ForeGetDataDependencies())
    jaspResults[["modelState"]] <- modelState
  }
}

.ln1ForeEstimateModelHelper <- function(dataset, options) {
  mod <- try(forecast::auto.arima(
    dataset[["y"]],
    allowdrift = TRUE,
    allowmean = TRUE
  ))

  if (jaspBase::isTryError(mod)) {
    .quitAnalysis(jaspBase::.extractErrorMessage(mod))
  }

  if (length(mod[["coef"]]) == 0) {
    .quitAnalysis(gettext("No parameters are estimated."))
  }

  return(mod)
}

.ln1ForeCreateDataPlot <- function(jaspResults, dataset, options, dependencyFun, ready) {
  if (options[["plotData"]] && is.null(jaspResults[["dataPlot"]])) {
    dataPlot <- createJaspPlot(
      title = gettext("Data plot"),
      height = 480,
      width = 480,
      position = 1
    )

    dataPlot$dependOn(c("plotData", "plotPoints", "plotLine", dependencyFun()))

    if (ready) {
      dataPlot$plotObject <- .ln1ForeCreateDataPlotFill(dataset, options)
    }

    jaspResults[["dataPlot"]] <- dataPlot
  }
}

.ln1ForeCreateDataPlotFill <- function(dataset, options) {
  yName <- options[["dependent"]]

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[["t"]])
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[["y"]])

  p <- ggplot2::ggplot(
      dataset,
      mapping = ggplot2::aes(
        x = .data[["t"]],
        y = .data[["y"]]
      )
    )

  if (options[["plotLine"]]) {
    p <- p + jaspGraphs::geom_line()
  }
  
  if (options[["plotPoints"]]) {
    p <- p + jaspGraphs::geom_point()
  }
  
  p <- p +
    ggplot2::scale_x_continuous(name = gettext("Time"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
