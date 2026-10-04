#
# Copyright (C) 2026 University of Amsterdam and Netherlands eScience Center
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

.ln1ForeUpgradeState <- function(jaspResults) {
  version <- 3L
  marker <- jaspResults[["forecastCacheVersion"]]
  if (!is.null(marker) && identical(marker$object, version))
    return(invisible(NULL))

  # Rebuild earlier calculation/output formats together, including corrected
  # simulated lengths and the current coefficient and prediction explanations.
  keys <- c("dataState", "modelState", "forecastResult", "coefTable",
            "dataPlot", "forecastPlot", "forecastTable", "forecastExport",
            "introText")
  for (key in keys) {
    if (!is.null(jaspResults[[key]]))
      jaspResults[[key]] <- NULL
  }

  # This marker describes the cache format, independently of analysis options.
  jaspResults[["forecastCacheVersion"]] <- createJaspState(object = version)
  return(invisible(NULL))
}

.ln1ForeOutcomeLabel <- function(options) {
  if (options[["inputType"]] == "simulateData" ||
      is.null(options[["dependent"]]) || !nzchar(options[["dependent"]]))
    return(gettext("Outcome"))
  return(decodeColNames(options[["dependent"]]))
}

.ln1ForeModelNote <- function(fit, options = list()) {
  order <- fit[["arma"]]
  if (any(order[c(3, 4, 7)] > 0))
    note <- gettextf("An ARIMA(%1$s, %2$s, %3$s)(%4$s, %5$s, %6$s)[%7$s] model was fitted.",
      order[1], order[6], order[2], order[3], order[7], order[4], order[5])
  else
    note <- gettextf("An ARIMA(%1$s, %2$s, %3$s) model was fitted.", order[1], order[6], order[2])
  coefficients <- names(fit[["coef"]])
  if ("drift" %in% coefficients)
    note <- paste(note, gettext("A linear drift term was included."))
  else if ("intercept" %in% coefficients)
    note <- paste(note, if (length(.ln1ForeCovariates(options)))
      gettext("A regression intercept was included.") else gettext("A process mean was estimated."))
  else
    note <- paste(note, gettext("No mean or drift term was included."))
  if (identical(options[["modelSpecification"]], "auto")) {
    criterion <- options[["modelSpecificationAutoIc"]]
    if (is.null(criterion) || !nzchar(criterion)) criterion <- "aicc"
    label <- switch(criterion, aicc = "AICc", aic = "AIC", bic = "BIC", "AICc")
    note <- paste(note, gettextf("Automatic selection used %1$s, with KPSS tests as the default differencing rule.", label))
  }
  return(note)
}

.ln1ForeCoefficientRows <- function(fit, options) {
  estimate <- fit[["coef"]]
  if (!length(estimate))
    return(NULL)

  # Every displayed row must originate from an estimated coefficient. Predictor
  # selections only supply labels for fitted xreg columns, never additional rows.
  coefficientNames <- names(estimate)
  if (length(coefficientNames) != length(estimate) || any(!nzchar(coefficientNames)))
    stop(gettext("The fitted model did not provide coefficient names."))
  idx <- c(which(coefficientNames == "intercept"),
           which(coefficientNames == "drift"),
           which(!coefficientNames %in% c("intercept", "drift")))
  estimate <- estimate[idx]
  coefficientNames <- coefficientNames[idx]
  se <- sqrt(diag(fit[["var.coef"]]))[idx]
  statistic <- estimate / se
  df <- fit[["nobs"]] - length(estimate)
  level <- options[["coefficientCiLevel"]]
  if (is.null(level))
    level <- 0.95
  pValue <- if (df > 0) 2 * (1 - stats::pt(abs(statistic), df)) else rep(NA_real_, length(estimate))
  margin <- if (df > 0) stats::qt((1 + level) / 2, df = df) * se else rep(NA_real_, length(estimate))

  covariates <- .ln1ForeCovariates(options)
  labels <- coefficientNames
  family <- coefficientNames
  for (i in seq_along(coefficientNames)) {
    name <- coefficientNames[i]
    if (name == "intercept") {
      labels[i] <- if (length(covariates)) gettext("Intercept") else gettext("Mean")
    } else if (name == "drift") {
      labels[i] <- gettext("Drift")
    } else if (grepl("^ar[0-9]+$", name)) {
      labels[i] <- gettextf("AR(%1$i)", as.integer(sub("^ar", "", name)))
      family[i] <- "ar"
    } else if (grepl("^ma[0-9]+$", name)) {
      labels[i] <- gettextf("MA(%1$i)", as.integer(sub("^ma", "", name)))
      family[i] <- "ma"
    } else if (grepl("^sar[0-9]+$", name)) {
      labels[i] <- gettextf("seasonal AR(%1$i)", as.integer(sub("^sar", "", name)))
      family[i] <- "sar"
    } else if (grepl("^sma[0-9]+$", name)) {
      labels[i] <- gettextf("seasonal MA(%1$i)", as.integer(sub("^sma", "", name)))
      family[i] <- "sma"
    } else if (grepl("^xreg[0-9]+$", name)) {
      index <- as.integer(sub("^xreg", "", name))
      if (index < 1 || index > length(covariates))
        stop(gettext("A fitted predictor could not be matched to its selected variable."))
      labels[i] <- decodeColNames(covariates[index])
      family[i] <- "xreg"
    }
  }

  rows <- data.frame(coefficients = labels, estimate = unname(estimate),
    SE = unname(se), t = unname(statistic), p = unname(pValue),
    lower = unname(estimate - margin), upper = unname(estimate + margin),
    .isNewGroup = c(TRUE, family[-1] != family[-length(family)]))
  rownames(rows) <- paste0("row", seq_len(nrow(rows)))
  return(rows)
}

.ln1ForeCreateCoefficientTable <- function(jaspResults, fit, options, ready) {
  if (!is.null(jaspResults[["coefTable"]]))
    return()

  table <- createJaspTable(gettext("Coefficients"))
  table$dependOn(c(.ln1ForeGetDataDependencies(), "coefficientCiLevel"))
  table$position <- 2
  table$showSpecifiedColumnsOnly <- TRUE
  table$addColumnInfo(name = "coefficients", title = "", type = "string")
  table$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  table$addColumnInfo(name = "SE", title = gettext("Standard Error"), type = "number")
  table$addColumnInfo(name = "t", title = gettext("t"), type = "number")
  table$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  level <- options[["coefficientCiLevel"]]
  if (is.null(level))
    level <- 0.95
  overtitle <- gettextf("%1$s%% CI", 100 * level)
  table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
  jaspResults[["coefTable"]] <- table

  if (!ready)
    return()

  rows <- tryCatch(.ln1ForeCoefficientRows(fit, options), error = function(e) e)
  if (inherits(rows, "error")) {
    table$setError(conditionMessage(rows))
    return()
  }
  if (is.null(rows)) {
    table$addFootnote(gettext("This model has no regression, autoregressive, or moving-average coefficients to report. Forecasts can still be computed."))
  } else {
    table$addRows(rows)
    if (fit[["nobs"]] <= length(fit[["coef"]]))
      table$addFootnote(gettext("There are too few observations to compute coefficient p-values and confidence intervals."))
  }
  table$addFootnote(.ln1ForeModelNote(fit, options))
  table$addFootnote(gettext("Coefficient p-values and confidence intervals use an approximate t reference with the number of used observations minus the number of coefficients as degrees of freedom. This is not a validated small-sample correction; results condition on the fitted model."))
  if (length(.ln1ForeCovariates(options)))
    table$addFootnote(gettext("Covariate coefficients describe associations with the outcome, accounting for the other selected covariates and ARIMA errors. They do not establish causal effects."))
}

.ln1ForePredictionNote <- function(options) {
  note <- gettext("Prediction intervals describe future observations under the fitted model. They omit uncertainty in parameter estimates and model selection.")
  if (length(.ln1ForeCovariates(options)))
    note <- paste(note, gettext("They are conditional on the supplied future covariate values and omit uncertainty in those values."))
  return(note)
}

.ln1ForeCreateForecastOutputs <- function(jaspResults, dataset, fit, options, ready) {
  wantPlot <- isTRUE(options[["forecastTimeSeries"]])
  wantTable <- isTRUE(options[["forecastTable"]])
  path <- options[["forecastSave"]]
  wantSave <- !is.null(path) && nzchar(path)
  if (!wantPlot && !wantTable && !wantSave)
    return()

  dependencies <- c(.ln1ForeGetDataDependencies(), "forecastLength")
  # Zero means no request. Other values, including invalid saved/programmatic
  # values, reach the guarded helper for an actionable forecast-specific error.
  hasForecast <- ready && !isTRUE(options[["forecastLength"]] == 0)
  result <- NULL
  if (hasForecast) {
    if (is.null(jaspResults[["forecastResult"]])) {
      result <- tryCatch(
        list(predictions = .ln1ForeForecastHelper(dataset, fit, options), error = NULL),
        error = function(e) list(predictions = NULL, error = conditionMessage(e)))
      state <- createJaspState(object = result)
      state$dependOn(dependencies)
      jaspResults[["forecastResult"]] <- state
    }
    result <- jaspResults[["forecastResult"]]$object
  }

  if (wantPlot && is.null(jaspResults[["forecastPlot"]])) {
    plot <- createJaspPlot(title = gettext("Forecast Time Series Plot"), width = 660, height = 400, position = 3)
    plot$dependOn(c(dependencies, "forecastTimeSeries", "forecastTimeSeriesType", "forecastTimeSeriesObserved"))
    jaspResults[["forecastPlot"]] <- plot
    if (hasForecast) {
      if (!is.null(result[["error"]])) {
        plot$setError(result[["error"]])
      } else {
        figure <- tryCatch(.ln1ForeForecastPlotFill(dataset, result[["predictions"]], options),
          error = function(e) e)
        if (inherits(figure, "error")) plot$setError(conditionMessage(figure)) else plot$plotObject <- figure
      }
    }
  }

  if (wantTable && is.null(jaspResults[["forecastTable"]])) {
    table <- createJaspTable(gettext("Forecasts"))
    table$dependOn(c(dependencies, "forecastTable"))
    table$position <- 4
    table$addColumnInfo(name = "t", title = gettext("Time"), type = "string")
    table$addColumnInfo(name = "y", title = .ln1ForeOutcomeLabel(options), type = "number")
    for (level in c(80, 95)) {
      overtitle <- gettextf("%1$s%% prediction interval", level)
      table$addColumnInfo(name = paste0("lower", level), title = gettext("Lower"), type = "number", overtitle = overtitle)
      table$addColumnInfo(name = paste0("upper", level), title = gettext("Upper"), type = "number", overtitle = overtitle)
    }
    table$addFootnote(.ln1ForePredictionNote(options))
    jaspResults[["forecastTable"]] <- table
    if (hasForecast) {
      if (!is.null(result[["error"]])) {
        table$setError(result[["error"]])
      } else {
        rows <- result[["predictions"]]
        rows[["t"]] <- as.character(rows[["t"]])
        rownames(rows) <- paste0("row", seq_len(nrow(rows)))
        table$addRows(rows)
      }
    }
  }

  return(result)
}

.ln1ForeForecastPlotFill <- function(dataset, predictions, options) {
  observedLabel <- gettext("Observed")
  forecastLabel <- gettext("Forecast")
  predicted <- data.frame(t = predictions[["t"]], y = predictions[["y"]], source = forecastLabel)
  series <- predicted
  if (isTRUE(options[["forecastTimeSeriesObserved"]])) {
    history <- .ln1ForeHistoricalData(dataset)
    observed <- data.frame(t = history[["t"]], y = history[["y"]], source = observedLabel)
    series <- rbind(observed, predicted)
  }
  series <- series[order(series[["t"]]), , drop = FALSE]
  series[["source"]] <- factor(series[["source"]], levels = c(observedLabel, forecastLabel))
  x <- series[["t"]]
  if (length(unique(x)) == 1L) {
    step <- .ln1ForeTimeStep(.ln1ForeHistoricalData(dataset))
    x <- x + c(-0.5, 0.5) * step
  }
  y <- c(series[["y"]], predictions[["lower95"]], predictions[["upper95"]])
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(x[is.finite(x)])
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(y[is.finite(y)])
  colors <- stats::setNames(c("black", "#0072B2"), c(observedLabel, forecastLabel))

  plot <- ggplot2::ggplot(series,
      ggplot2::aes(x = .data[["t"]], y = .data[["y"]], colour = .data[["source"]], group = .data[["source"]])) +
    ggplot2::geom_ribbon(data = predictions,
      mapping = ggplot2::aes(x = .data[["t"]], ymin = .data[["lower95"]], ymax = .data[["upper95"]], fill = "95%"),
      inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = predictions,
      mapping = ggplot2::aes(x = .data[["t"]], ymin = .data[["lower80"]], ymax = .data[["upper80"]], fill = "80%"),
      inherit.aes = FALSE)
  type <- options[["forecastTimeSeriesType"]]
  if (is.null(type))
    type <- "both"
  if (nrow(predictions) == 1) {
    # Ribbons have zero width at a single forecast time; show both intervals as
    # vertical ranges as well so their uncertainty remains visible.
    plot <- plot +
      ggplot2::geom_linerange(data = predictions,
        mapping = ggplot2::aes(x = .data[["t"]], ymin = .data[["lower95"]], ymax = .data[["upper95"]]),
        inherit.aes = FALSE, colour = "#E5E5E5", linewidth = 1) +
      ggplot2::geom_linerange(data = predictions,
        mapping = ggplot2::aes(x = .data[["t"]], ymin = .data[["lower80"]], ymax = .data[["upper80"]]),
        inherit.aes = FALSE, colour = "#BDBDBD", linewidth = 2)
  }
  if (type %in% c("line", "both")) {
    lineData <- if (nrow(predictions) == 1L) series[series[["source"]] == observedLabel, , drop = FALSE] else series
    if (nrow(lineData) > 1L)
      plot <- plot + jaspGraphs::geom_line(data = lineData, na.rm = TRUE)
  }
  if (type %in% c("points", "both")) {
    plot <- plot + jaspGraphs::geom_point(na.rm = TRUE)
  } else if (nrow(predictions) == 1) {
    # A one-step forecast must remain visible when the line display is selected.
    plot <- plot + jaspGraphs::geom_point(data = predicted)
  }
  plot <- plot +
    ggplot2::scale_colour_manual(name = "", values = colors) +
    ggplot2::scale_fill_manual(name = gettext("Prediction interval"), values = c("95%" = "#E5E5E5", "80%" = "#BDBDBD"), breaks = c("80%", "95%")) +
    ggplot2::scale_x_continuous(name = gettext("Time"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = .ln1ForeOutcomeLabel(options), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical")
  if (length(.ln1ForeCovariates(options)))
    plot <- plot + ggplot2::labs(caption = gettext("Conditional on the supplied future covariate values."))
  return(plot)
}
