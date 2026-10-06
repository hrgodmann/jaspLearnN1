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

Treatment <- function(jaspResults, dataset = NULL, options) {
  .ln1TreatUpgradeState(jaspResults)
  .ln1TreatValidateConfidenceLevel(options)
  jaspResults$title <- gettext("Does The Treatment Work?")

  .ln1Intro(jaspResults, options, .ln1TreatIntroText)

  if (options[["inputType"]] == "loadData") {
    ready <- options[["dependent"]] != "" && options[["time"]] != "" && options[["phase"]] != ""
  } else {
    ready <- TRUE
  }

  dataset <- .ln1TreatData(jaspResults, dataset, options, ready)
  .ln1TreatCreateTimeInfo(jaspResults, dataset, options, ready)

  .ln1TreatCreateDataPlot(jaspResults, dataset, options, .ln1TreatGetDataDependencies, ready)
  .ln1TreatEstimateModel(jaspResults, dataset, options, ready)
  .ln1TreatCreateAnalysisPlot(jaspResults, dataset, options, ready)

  # Absent options in older saved analyses retain their existing output choice.
  if (isTRUE(options[["phaseComparisons"]]))
    .ln1TreatCreatePhaseComparisons(jaspResults, dataset, options, ready)
  if (isTRUE(options[["phaseSummary"]]))
    .ln1TreatCreatePhaseSummary(jaspResults, dataset, options, ready)
  if (options[["coefficientsTable"]])
    .ln1TreatCreateCoefficientsTable(jaspResults, options, ready)
  if (options[["autocorrelationTable"]])
    .ln1TreatCreateAutoCorTable(jaspResults, options, ready)

  return()
}

.ln1TreatIntroText <- function() {
  return(gettext("This method is used to examine how symptoms change over time within a single individual and whether these changes differ across treatment phases. It is particularly suitable for clinical questions such as whether symptom improvement is stronger during treatment compared to baseline, or whether specific interventions are associated with changes in symptom trajectories. 

<b>How does it work?</b> 

The model fits a regression line for each phase using generalized least squares (GLS) with AR(1) residual correlation and restricted maximum likelihood (REML). This accounts for the relationship between nearby measurements after allowing for the phase-specific trends. All phase comparisons come from this same fitted model.

<b>Compare treatment with another phase</b>

Choose the compared phase and the reference phase in Phase comparisons. Automatic selections use the second and first phases in chronological order, respectively. Differences are compared phase minus reference phase. The endpoint difference compares the fitted outcome at the end of each phase, using its last scheduled time even if that outcome is missing. It is not an immediate change at treatment onset. The slope difference compares rates of change per time unit moving forward. Phase estimates optionally show each phase's fitted endpoint and slope. Whether a lower or higher outcome represents improvement depends on the measure.

<b>Practical considerations</b> 

Common research questions include whether symptoms decrease more rapidly during treatment than during baseline, or whether different treatment conditions lead to different rates of improvement. For loaded data, select one continuous, equally spaced numeric time variable across all phases; do not restart it at each phase. Rows are sorted by time. Keep a row with its time and phase for every missing outcome. Missing outcomes are not imputed, and their time positions are preserved in the autocorrelation model. Time coding determines the interpretation of regression coefficients: loaded data use the selected time values, while simulations use time starting at 1 within each phase for regression trends. In both cases, autocorrelation follows consecutive measurement occasions across phase boundaries. The model assumes linear change within phases and an AR(1) residual correlation structure.

<b>Interpretation and limitations</b> 

Consider endpoint and slope differences together: an improved endpoint can also reflect a trend already present before treatment. These phase-associated changes do not by themselves establish that treatment caused the improvement. Confidence intervals and tests use approximations that can be unreliable for short series or strong autocorrelation. There is no universal minimum number of observations that guarantees reliable inference. Consider the size and uncertainty of change alongside clinical judgment and other outcome measures.
"))
} 
.ln1TreatData <- function(jaspResults, dataset, options, ready) {
  if (!ready)
    return(NULL)

  if (options[["inputType"]] == "simulateData") {
    if (is.null(jaspResults[["simulatedDataState"]])) {
      dataset <- .ln1TreatSimulateData(options)
      dataState <- createJaspState(object = dataset)
      dataState$dependOn(.ln1TreatGetDataDependencies())
      jaspResults[["simulatedDataState"]] <- dataState
    } else {
      dataset <- jaspResults[["simulatedDataState"]]$object
    }
  } else if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(
      columns.as.numeric = c(options[["dependent"]], options[["time"]]),
      columns.as.factor  = options[["phase"]]
    )
  }

  return(.ln1TreatPrepareData(dataset, options))
}

.ln1TreatSimulateData <- function(options) {
  .ln1TreatValidateSimulation(options)
  set.seed(options[["seed"]])

  phaseN <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseEffectN"]])
  phaseEffects <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseEffectSimple"]])
  phaseIntTime <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseEffectInteraction"]])
  phaseNames <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseName"]])

  totalN <- sum(phaseN)

  yNoise <- tryCatch(stats::arima.sim(
    model = list(ar = options[["simTimeEffectAutocorrelation"]]),
    n = totalN,
    sd = options[["simDependentSd"]]
  ), error = function(e) .quitAnalysis(gettext("The treatment simulation could not be generated. Check the phase lengths, noise standard deviation, and stationary autocorrelation settings.")))

  phaseName <- rep(phaseNames, phaseN)
  phaseBeta <- rep(phaseEffects, phaseN)
  phaseInt <- rep(phaseIntTime, phaseN)

  time <- unlist(lapply(phaseN, seq_len))
  timeEffect <- options[["simTimeEffect"]]

  y <- options[["simDependentMean"]] + timeEffect * time + phaseBeta + time * phaseInt + yNoise
  if (any(!is.finite(y)))
    .quitAnalysis(gettext("The simulation produced nonfinite outcomes. Use smaller finite means, phase effects, trends, or noise values."))

  simData <- data.frame(
    y = y,
    time = time,
    t = seq_along(time),
    phase = phaseName
  )

  return(simData)
}

.ln1TreatGetDataDependencies <- function() {
  return(c(
    "inputType",
    "dependent",
    "time",
    "phase",
    "simDependentMean",
    "simDependentSd",
    "simTimeEffect",
    "simTimeEffectAutocorrelation",
    "simPhaseEffects",
    "seed"
  ))
}

.ln1GetVariableNames <- function(options) {
  varList <- switch(options[["inputType"]],
    "simulateData" = list(
      "time" = "time",
      "dependent" = "y",
      "phase" = "phase",
      "t" = "t"
    ),
    "loadData" = list(
      "time" = options[["time"]],
      "dependent" = options[["dependent"]],
      "phase" = options[["phase"]],
      "t" = options[["time"]]
    )
  )

  return(varList)
}

.ln1TreatEstimateModelHelper <- function(dataset, options) {
  if (identical(options[["inputType"]], "simulateData") &&
      isTRUE(options[["simDependentSd"]] == 0))
    .quitAnalysis(gettext("This zero-noise simulation has no residual variation for model inference. The data plot remains available. Increase the noise standard deviation to estimate uncertainty."))
  variables <- .ln1GetVariableNames(options)
  timing <- attr(dataset, "ln1TreatTime")
  # nlme internally reconstructs formulas without quoting names. Model-only
  # columns protect both supplied names with spaces and internal-name collisions.
  modelData <- data.frame(y = dataset[[variables[["dependent"]]]],
                          time = dataset[[variables[["time"]]]],
                          phase = dataset[[variables[["phase"]]]],
                          occasion = dataset[[timing[["occasion"]]]])
  design <- stats::model.matrix(~ time * phase, modelData[!is.na(modelData[["y"]]), , drop = FALSE])
  if (qr(design)$rank < ncol(design))
    .quitAnalysis(gettext("The phase-specific trends cannot be estimated reliably with this time coding. Express time relative to the start of the series, keeping one continuous clock across phases."))

  # One individual has a fixed intercept and correlated residuals. There is
  # no between-series random-intercept variance to estimate from this series.
  mod <- tryCatch(nlme::gls(
    model = y ~ time * phase,
    data = modelData,
    correlation = nlme::corAR1(form = ~ occasion),
    method = "REML",
    na.action = stats::na.exclude
  ), error = function(e) .quitAnalysis(gettext("The phase-trend model could not be estimated reliably. Check for constant or nearly deterministic outcomes, very short phases, and extreme values. The data plot remains available.")))
  # Build labels from the fitted design; never evaluate decoded user names as
  # formula syntax (a column named '.' would otherwise expand other terms).
  term <- attr(design, "assign")
  labels <- colnames(design)
  timeLabel <- decodeColNames(variables[["time"]])
  phaseLabel <- decodeColNames(variables[["phase"]])
  labels[term == 1L] <- timeLabel
  labels[term == 2L] <- paste0(phaseLabel, substring(labels[term == 2L], nchar("phase") + 1L))
  labels[term == 3L] <- paste0(timeLabel, ":", phaseLabel,
    substring(labels[term == 3L], nchar("time:phase") + 1L))
  attr(mod, "ln1TreatCoefficientNames") <- labels

  return(mod)
}

.ln1TreatEstimateModel <- function(jaspResults, dataset, options, ready) {
  if (ready && is.null(jaspResults[["modelState"]]) && is.null(jaspResults[["modelErrorState"]])) {
    modelObject <- tryCatch(.ln1TreatEstimateModelHelper(dataset, options), error = identity)
    failed <- inherits(modelObject, "error")
    modelState <- createJaspState(object = if (failed) conditionMessage(modelObject) else modelObject)
    modelState$dependOn(.ln1TreatGetDataDependencies())
    jaspResults[[if (failed) "modelErrorState" else "modelState"]] <- modelState
  }
}

.ln1TreatCreateCoefficientsTable <- function(jaspResults, options, ready) {
  if (is.null(jaspResults[["coefTable"]])) {
    table <- createJaspTable(gettext("Coefficients"), position = 7)
    table$dependOn(c(.ln1TreatGetDataDependencies(), "coefficientCiLevel", "coefficientsTable"))

    table$addColumnInfo(name = "name",         title = "",                        type = "string")
    table$addColumnInfo(name = "coef",         title = gettext("Estimate"),       type = "number")
    table$addColumnInfo(name = "SE",           title = gettext("Standard Error"), type = "number")
    table$addColumnInfo(name = "t",            title = gettext("t"),              type = "number")
    table$addColumnInfo(name = "p",            title = gettext("p"),              type = "pvalue")

    overtitle <- .ln1TreatCiTitle(options)

    table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

    table$addFootnote(gettext("Results are based on generalized least squares regression with AR(1) residual correlation, estimated using restricted maximum likelihood (REML). Coefficients describe phase-specific levels and trends for this individual."))

    if (ready && !.ln1TreatSetModelError(table, jaspResults) && !is.null(jaspResults[["modelState"]])) {
      .ln1TreatCoefficientContext(table, jaspResults[["modelState"]]$object, options)
      .ln1TreatFillCoefficientsTable(table, jaspResults[["modelState"]]$object, options)
    }

    jaspResults[["coefTable"]] <- table
  }
}

.ln1TreatFillCoefficientsTable <- function(table, modelObject, options) {
  modelSummary <- summary(modelObject)
  modelCoefficients <- data.frame(stats::coef(modelSummary))

  table[["name"]] <- attr(modelObject, "ln1TreatCoefficientNames")
  table[["coef"]] <- modelCoefficients[["Value"]]
  table[["SE"]] <- modelCoefficients[["Std.Error"]]
  table[["t"]] <- modelCoefficients[["t.value"]]
  table[["p"]] <- modelCoefficients[["p.value"]]

  ci <- nlme::intervals(modelObject, level = options[["coefficientCiLevel"]], which = "coef")

  ciFixed <- data.frame(ci[["coef"]])

  table[["lower"]] <- ciFixed[["lower"]]
  table[["upper"]] <- ciFixed[["upper"]]
}

.ln1TreatCreateAutoCorTable <- function(jaspResults, options, ready) {
  if (is.null(jaspResults[["autoCorTable"]])) {
    table <- createJaspTable(gettext("Autocorrelation"), position = 8)
    table$dependOn(c(.ln1TreatGetDataDependencies(), "coefficientCiLevel", "autocorrelationTable"))

    table$addColumnInfo(name = "name",         title = "",                        type = "string")
    table$addColumnInfo(name = "coef",         title = gettext("Estimate"),       type = "number")

    overtitle <- .ln1TreatCiTitle(options)

    table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

    table$addFootnote(gettext("The AR(1) coefficient is the residual correlation over one measurement interval. Across k intervals, the model uses this coefficient raised to the power k. Missing outcomes preserve their time positions; autocorrelation continues across phase boundaries."))
    table$addFootnote(gettext("The autocorrelation interval uses a transformed-normal approximation. Short series can underestimate autocorrelation and give unreliable intervals."))

    if (ready && !.ln1TreatSetModelError(table, jaspResults) && !is.null(jaspResults[["modelState"]])) {
      .ln1TreatFillAutoCorTable(table, jaspResults[["modelState"]]$object, options)
    }

    jaspResults[["autoCorTable"]] <- table
  }
}

.ln1TreatFillAutoCorTable <- function(table, modelObject, options) {
  table[["name"]] <- "AR(1)"

  # Point estimate is always available from the model
  corStruct <- modelObject[["modelStruct"]][["corStruct"]]
  phi <- as.numeric(stats::coef(corStruct, unconstrained = FALSE))
  table[["coef"]] <- phi

  # Variance-component intervals can be unavailable even away from a boundary.
  ci <- try(nlme::intervals(modelObject, level = options[["coefficientCiLevel"]], which = "var-cov"), silent = TRUE)

  if (!jaspBase::isTryError(ci) && !is.null(ci[["corStruct"]])) {
    ciAutoCor <- data.frame(ci[["corStruct"]])
    table[["lower"]] <- ciAutoCor[["lower"]]
    table[["upper"]] <- ciAutoCor[["upper"]]
  } else {
    table$addFootnote(gettext("The confidence interval for the autocorrelation could not be estimated reliably for this fitted model."))
  }
}

.ln1TreatCreateDataPlot <- function(jaspResults, dataset, options, dependencyFun, ready) {
  if (options[["plotData"]] && is.null(jaspResults[["dataPlot"]])) {
    dataPlot <- createJaspPlot(
      title = gettext("Data plot"),
      height = 480,
      width = 480,
      position = 3
    )
    dataPlot$dependOn(c("plotData", dependencyFun()))
    if (ready) {
      dataPlot$plotObject <- .ln1TreatCreateDataPlotFill(dataset, options)
    }
    jaspResults[["dataPlot"]] <- dataPlot
  }
}

.ln1TreatCreateDataPlotFill <- function(dataset, options) {
  variableNames <- .ln1GetVariableNames(options)

  yName <- variableNames[["dependent"]]
  xName <- variableNames[["t"]]

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[[xName]])
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[[yName]][!is.na(dataset[[yName]])])

  p <- ggplot2::ggplot(
      dataset,
      mapping = ggplot2::aes(
        x = .data[[xName]],
        y = .data[[yName]],
        color = .data[[variableNames[["phase"]]]]
      )
    ) +
    # Preserve JASP styling without fixed colours overriding the phase mapping.
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
    ggplot2::geom_point(size = 3, shape = 21, fill = "grey", stroke = .5, na.rm = TRUE) +
    ggplot2::scale_x_continuous(
      name = if (options[["inputType"]] == "loadData") decodeColNames(xName) else gettext("Time"),
      breaks = xBreaks,
      limits = range(xBreaks)
    ) +
    ggplot2::scale_y_continuous(name = decodeColNames(yName), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}

.ln1TreatCreateAnalysisPlot <- function(jaspResults, dataset, options, ready) {
  if (options[["plotAnalysis"]] && is.null(jaspResults[["analysisPlot"]])) {
    analysisPlot <- createJaspPlot(
      title = gettext("Analysis plot"),
      height = 480,
      width = 480,
      position = 4
    )
    analysisPlot$dependOn(c("plotAnalysis", .ln1TreatGetDataDependencies()))
    if (ready && !.ln1TreatSetModelError(analysisPlot, jaspResults) && !is.null(jaspResults[["modelState"]])) {
      analysisPlot$plotObject <- .ln1TreatCreateAnalysisPlotFill(dataset, jaspResults[["modelState"]]$object, options)
    }
    jaspResults[["analysisPlot"]] <- analysisPlot
  }
}

.ln1TreatCreateAnalysisPlotFill <- function(dataset, modelObject, options) {
  variableNames <- .ln1GetVariableNames(options)

  yName <- variableNames[["dependent"]]
  xName <- variableNames[["t"]]
  phaseName <- variableNames[["phase"]]

  fittedName <- .ln1TreatInternalName(dataset, ".ln1TreatFitted")
  dataset[[fittedName]] <- as.numeric(stats::fitted(modelObject))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[[xName]])
  yValues <- c(dataset[[yName]], dataset[[fittedName]])
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yValues[!is.na(yValues)])

  p <- ggplot2::ggplot(
      dataset,
      mapping = ggplot2::aes(
        x = .data[[xName]],
        y = .data[[yName]],
        color = .data[[phaseName]]
      )
    ) +
    ggplot2::geom_point(size = 3, shape = 21, fill = "grey", stroke = .5,
                       alpha = 0.4, na.rm = TRUE) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(y = .data[[fittedName]]),
      linewidth = 1.2,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(
      name = if (options[["inputType"]] == "loadData") decodeColNames(xName) else gettext("Time"),
      breaks = xBreaks,
      limits = range(xBreaks)
    ) +
    ggplot2::scale_y_continuous(name = decodeColNames(yName), breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::labs(color = gettext("Phase")) +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::theme(legend.position = "bottom")

  return(p)
}
