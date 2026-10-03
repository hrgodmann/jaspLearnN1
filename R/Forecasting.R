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
  .ln1ForeUpgradeState(jaspResults)
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

  fit <- if (ready) jaspResults[["modelState"]]$object else NULL
  .ln1ForeCreateCoefficientTable(jaspResults, fit, options, ready)
  .ln1ForeCreateForecastOutputs(jaspResults, dataset, fit, options, ready)

  return()
}

.ln1ForeIntroText <- function() {
  return(gettext("Forecasting uses past observations to predict future developments. In clinical practice, it can help anticipate possible changes in a client's symptoms. A forecast alone does not establish whether treatment is effective or justify continuing, changing, or stopping it. Forecasts can support discussion between a therapist and client alongside clinical assessment and the client's goals and preferences.

<b>How does it work?</b> 

Forecasting is an umbrella term that covers many different analytical techniques, several of which are implemented in JASP. This tutorial focuses on the autoregressive integrated moving average (ARIMA) model, a widely used approach in forecasting. The autoregressive (AR) part means that the current value of a variable is predicted from its previous values. The moving average (MA) part means that the current value is also influenced by past prediction errors. The integrated (I) part means that the model uses differences between consecutive observations (rather than the raw values) to handle trends in the data. When covariates are selected, their associations with the outcome are estimated jointly, and the ARIMA structure describes the remaining errors. Forecasting then requires a value for each predictor at every future measurement occasion.

<b>Practical considerations</b> 

A key practical question in clinical forecasting is how often to collect measurements. In general, more frequent measurements can lead to better predictions. At the same time, it is important to consider what is feasible for a client and at which time scale meaningful changes actually occur. For example, if symptoms do not change much from day to day, daily assessment will not add much information. On the other hand, if measurements are taken only once every three months, important changes in the treatment process may be missed, making predictions less accurate. As a rule of thumb, weekly measurements are often a reasonable starting point. Forecasting models learn patterns from past data and can help therapists and clients discuss possible future symptom changes. However, the accuracy of these predictions depends strongly on the quality and level of detail (granularity) of the available data.

<b>Interpretation and limitations</b> 

Forecasting provides an prediction on the future clinical outcome. Note that -just like any prediction- this comes with an uncertainty, or prediction error. Forecasting therefore just gives a likely outcome, but this does not mean it will happen. Also, the further into the future you forecast, the less confident the model becomes, which is reflected in wider uncertainty ranges. Specifically, the ARIMA model only work well with relatively stable data, consistent way over time, and it struggles when something unexpected shifts the trend. The model can account for supplied covariates, but cannot anticipate changes in factors that have not been included. Forecasts and prediction intervals with covariates are conditional on the supplied future predictor values; uncertainty about those values is not included. If set up poorly, it can also memorise past data rather than learning genuine patterns, leading to poor real-world predictions.

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
    if (is.null(dataset)) {
      columnsNumeric <- unique(c(options[["dependent"]], options[["time"]],
                                 .ln1ForeCovariates(options)))
      columnsNumeric <- columnsNumeric[nzchar(columnsNumeric)]
      dataset <- .readDataSetToEnd(columns.as.numeric = columnsNumeric)
    }
    dataset <- .ln1ForePrepareData(dataset, options)
  }

  return(dataset)
}

.ln1ForeCovariates <- function(options) {
  if (options[["inputType"]] == "loadData")
    return(as.character(options[["covariates"]]))
  return(character())
}

.ln1ForePrepareData <- function(dataset, options) {
  covariates <- .ln1ForeCovariates(options)
  dependent <- options[["dependent"]]
  time <- options[["time"]]
  columns <- unique(c(dependent, time, covariates))
  columns <- columns[nzchar(columns)]
  if (!all(columns %in% names(dataset)))
    .quitAnalysis(gettext("Some selected variables are unavailable. Assign the outcome, time and covariates again."))
  if (anyDuplicated(covariates) || dependent %in% covariates)
    .quitAnalysis(gettext("Select distinct covariates, excluding the outcome variable."))
  if (!all(vapply(dataset[columns], is.numeric, logical(1))))
    .quitAnalysis(gettext("The outcome, time and covariates must be numeric."))

  # Copy into a new frame so user names cannot overwrite y, t or regressor names.
  result <- data.frame(y = dataset[[dependent]],
                       t = if (length(time) && nzchar(time)) dataset[[time]] else seq_len(nrow(dataset)))
  for (i in seq_along(covariates))
    result[[paste0("xreg", i)]] <- dataset[[covariates[i]]]
  if (any(!is.na(result[["y"]]) & !is.finite(result[["y"]])))
    .quitAnalysis(gettext("The outcome contains infinite values. Replace them with valid measurements or missing values."))
  if (nrow(result) < 2L || sum(!is.na(result[["y"]])) < 2L)
    .quitAnalysis(gettext("At least two observed outcome values are required to fit an ARIMA model."))
  observed <- which(!is.na(result[["y"]]))
  unknownTime <- !is.finite(result[["t"]])
  if (any(unknownTime & seq_len(nrow(result)) <= max(observed)))
    .quitAnalysis(gettext("Historical time values must be finite and complete, including occasions with a missing outcome."))
  lastTime <- max(result[["t"]][observed])
  future <- is.na(result[["y"]]) & (unknownTime | result[["t"]] > lastTime)
  history <- result[!future, , drop = FALSE]
  history <- history[order(history[["t"]]), , drop = FALSE]
  .ln1ForeTimeStep(history)
  futureRows <- result[future, , drop = FALSE]
  # Missing future times are checked only when those rows are used to forecast.
  futureRows <- futureRows[order(!is.finite(futureRows[["t"]]),
                                 futureRows[["t"]], na.last = TRUE), , drop = FALSE]
  result <- rbind(history, futureRows)
  rownames(result) <- NULL
  return(result)
}

.ln1ForeTimeTolerance <- function(time, step) {
  roundoff <- 8 * .Machine$double.eps * max(abs(time))
  if (!is.finite(step) || step <= 0 || !is.finite(roundoff) || roundoff > 0.001 * step)
    .quitAnalysis(gettext("The time values are too large relative to their spacing for reliable numerical precision. Express time relative to the start of the series."))
  # Tolerance follows the measurement interval, even when its numeric unit is
  # very small. Floating-point roundoff also depends on the clock's origin.
  return(max(1e-7 * step, roundoff))
}

.ln1ForeTimeStep <- function(dataset) {
  time <- dataset[["t"]]
  if (length(time) < 2L || any(!is.finite(time)))
    .quitAnalysis(gettext("Historical time values must be finite and complete, including occasions with a missing outcome."))
  steps <- diff(time)
  if (any(steps <= 0))
    .quitAnalysis(gettext("Time values must be unique and equally spaced."))
  step <- (time[length(time)] - time[1L]) / (length(time) - 1L)
  tolerance <- .ln1ForeTimeTolerance(time, step)
  if (any(abs(steps - step) > tolerance))
    .quitAnalysis(gettext("ARIMA requires equally spaced time points. Include rows for missing measurement occasions instead of removing time gaps."))
  return(step)
}

.ln1ForeHistoricalData <- function(dataset) {
  if (is.null(dataset))
    return(NULL)
  observed <- which(!is.na(dataset[["y"]]))
  if (length(observed) < 2L)
    .quitAnalysis(gettext("At least two observed outcome values are required to fit an ARIMA model."))
  return(dataset[seq_len(max(observed)), , drop = FALSE])
}

.ln1ForePredictorMatrix <- function(dataset) {
  columns <- grep("^xreg[0-9]+$", names(dataset), value = TRUE)
  if (!length(columns))
    return(NULL)
  return(as.matrix(dataset[, columns, drop = FALSE]))
}

.ln1ForeCheckPredictors <- function(xreg, differences = 0L, observed = NULL,
                                    includeConstant = differences <= 1L) {
  if (is.null(xreg))
    return(invisible(NULL))
  if (any(!is.finite(xreg)))
    .quitAnalysis(gettext("All historical covariate values must be finite and complete, including occasions with a missing outcome."))
  if (differences >= nrow(xreg) - 1L)
    .quitAnalysis(gettext("There are too few observations for the selected covariates and differencing order."))
  design <- if (differences > 0L) diff(xreg, differences = differences) else xreg
  if (differences == 0L && !is.null(observed))
    design <- design[observed, , drop = FALSE]
  # Center when an intercept/drift is present. Compare against the original
  # scale before normalization: differencing a fractional linear trend can
  # leave roundoff-sized values that must not become a spurious predictor.
  if (includeConstant)
    design <- sweep(design, 2L, colMeans(design), "-")
  scales <- apply(abs(design), 2L, max)
  roundoff <- 32 * .Machine$double.eps * 2^differences * apply(abs(xreg), 2L, max)
  if (any(scales <= roundoff))
    .quitAnalysis(gettext("The covariates are constant or linearly dependent after accounting for the intercept, drift and differencing. Remove redundant covariates or choose a different differencing order."))
  design <- sweep(design, 2L, scales, "/")
  # The default model has an intercept at d=0 and allows drift at d=1.
  if (includeConstant)
    design <- cbind(constant = 1, design)
  if (nrow(design) <= ncol(design))
    .quitAnalysis(gettext("There are too few observations for the selected covariates and differencing order."))
  if (qr(design)$rank < ncol(design))
    .quitAnalysis(gettext("The covariates are constant or linearly dependent after accounting for the intercept, drift and differencing. Remove redundant covariates or choose a different differencing order."))
  return(invisible(NULL))
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
    "modelSpecification",
    "modelSpecificationAutoIc",
    "p",
    "d",
    "q",
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
  dataset <- .ln1ForeHistoricalData(dataset)
  xreg <- .ln1ForePredictorMatrix(dataset)
  .ln1ForeCheckPredictors(xreg, observed = !is.na(dataset[["y"]]))
  if (!is.null(xreg) && length(unique(stats::na.omit(dataset[["y"]]))) < 2L)
    .quitAnalysis(gettext("The outcome must vary to estimate covariate effects."))
  modelSpecification <- options[["modelSpecification"]]
  if (is.null(modelSpecification) || modelSpecification == "")
    modelSpecification <- "auto"

  if (modelSpecification == "custom") {
    .ln1ForeCheckPredictors(xreg, differences = options[["d"]])
    mod <- try(forecast::Arima(
      dataset[["y"]],
      order = c(options[["p"]], options[["d"]], options[["q"]]),
      xreg = xreg,
      include.constant = TRUE
    ), silent = TRUE)
  } else {
    autoIc <- options[["modelSpecificationAutoIc"]]
    if (is.null(autoIc) || autoIc == "")
      autoIc <- "aicc"

    mod <- try(forecast::auto.arima(
      dataset[["y"]],
      xreg = xreg,
      allowdrift = TRUE,
      allowmean = TRUE,
      ic = autoIc
    ), silent = TRUE)
  }

  if (jaspBase::isTryError(mod)) {
    .quitAnalysis(jaspBase::.extractErrorMessage(mod))
  }

  if (!is.null(xreg)) {
    .ln1ForeCheckPredictors(xreg, differences = mod[["arma"]][6L],
                            includeConstant = any(c("intercept", "drift") %in% names(mod[["coef"]])))
    if (!all(colnames(xreg) %in% names(mod[["coef"]])))
      .quitAnalysis(gettext("The fitted model could not estimate every selected covariate. Check the outcome variation and remove redundant covariates."))
  }

  return(mod)
}

.ln1ForeForecastHelper <- function(dataset, fit, options) {
  horizon <- options[["forecastLength"]]
  if (length(horizon) != 1L || !is.finite(horizon) || horizon < 1L || horizon != as.integer(horizon))
    .quitAnalysis(gettext("The number of forecasts must be a positive integer."))
  historical <- .ln1ForeHistoricalData(dataset)
  n <- nrow(historical)
  step <- .ln1ForeTimeStep(historical)
  lastTime <- tail(historical[["t"]], 1L)
  forecastTimes <- lastTime + step * seq_len(horizon)
  if (any(!is.finite(forecastTimes)) || any(diff(c(lastTime, forecastTimes)) <= 0))
    .quitAnalysis(gettext("Future time values cannot be represented reliably at this spacing. Express time relative to the start of the series or request a shorter forecast horizon."))
  tolerance <- .ln1ForeTimeTolerance(c(lastTime, forecastTimes), step)
  if (any(abs(diff(c(lastTime, forecastTimes)) - step) > tolerance))
    .quitAnalysis(gettext("Future time values cannot be represented reliably at this spacing. Express time relative to the start of the series or request a shorter forecast horizon."))
  xreg <- .ln1ForePredictorMatrix(historical)
  futureX <- NULL
  if (!is.null(xreg)) {
    if (nrow(dataset) - n < horizon)
      .quitAnalysis(gettextf("Supply covariate values for all %1$i future rows after the last observed outcome, leaving the future outcome cells empty.", horizon))
    futureRows <- dataset[n + seq_len(horizon), , drop = FALSE]
    if (any(!is.finite(futureRows[["t"]])) || any(diff(c(lastTime, futureRows[["t"]])) <= 0) ||
        any(abs(futureRows[["t"]] - forecastTimes) > tolerance))
      .quitAnalysis(gettext("The requested future time values must continue the historical series at the same spacing. Fill in one consecutive time value for each future covariate row."))
    futureX <- .ln1ForePredictorMatrix(futureRows)
    if (any(!is.finite(futureX)))
      .quitAnalysis(gettext("All covariate values in the requested forecast period must be finite and complete. Enter known future values or an explicit scenario."))
  }
  prediction <- try(forecast::forecast(fit, h = horizon, xreg = futureX,
                                       level = c(80, 95)), silent = TRUE)
  if (jaspBase::isTryError(prediction))
    .quitAnalysis(jaspBase::.extractErrorMessage(prediction))
  return(data.frame(
    t = forecastTimes,
    y = as.numeric(prediction[["mean"]]),
    lower80 = as.numeric(prediction[["lower"]][, "80%"]),
    upper80 = as.numeric(prediction[["upper"]][, "80%"]),
    lower95 = as.numeric(prediction[["lower"]][, "95%"]),
    upper95 = as.numeric(prediction[["upper"]][, "95%"])
  ))
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
      dataPlot$plotObject <- .ln1ForeCreateDataPlotFill(.ln1ForeHistoricalData(dataset), options)
    }

    jaspResults[["dataPlot"]] <- dataPlot
  }
}

.ln1ForeCreateDataPlotFill <- function(dataset, options) {
  yName <- options[["dependent"]]
  if (options[["inputType"]] == "loadData")
    yName <- decodeColNames(yName)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[["t"]])
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[["y"]][is.finite(dataset[["y"]])])

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
