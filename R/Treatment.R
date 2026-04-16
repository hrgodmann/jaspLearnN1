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
  jaspResults$title <- gettext("Does The Treatment Work?")

  .ln1Intro(jaspResults, options, .ln1TreatIntroText)

  if (options[["inputType"]] == "loadData") {
    ready <- options[["dependent"]] != "" && options[["time"]] != "" && options[["phase"]] != ""
  } else {
    ready <- TRUE
  }

  dataset <- .ln1TreatData(jaspResults, dataset, options, ready)

  .ln1TreatCreateDataPlot(jaspResults, dataset, options, .ln1TreatGetDataDependencies, ready)
  .ln1TreatEstimateModel(jaspResults, dataset, options, ready)
  .ln1TreatCreateCoefficientsTable(jaspResults, options, ready)
  .ln1TreatCreateAutoCorTable(jaspResults, options, ready)

  return()
}

.ln1TreatIntroText <- function() {
  return(gettext("This model helps answer questions about how a patient's symptoms fluctuate over time and what factors predict changes in their mental health on an individual level. It requires repeated measures of symptom severity, behaviors, or other psychological variables over time, along with potential predictors like therapy interventions or daily stressors. A key feature is its ability to account for within-person variability while distinguishing stable patterns from momentary fluctuations. This is particularly useful for therapists aiming to tailor interventions based on a patient's unique response dynamics. Unlike group-level analyses, this approach provides individualized insights rather than assuming uniform effects across patients."))
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
  }

  # Add dummy grouping variable for mixed model
  dataset[["g"]] <- rep(1, nrow(dataset))

  return(dataset)
}

.ln1TreatSimulateData <- function(options) {
  set.seed(options[["seed"]])

  phaseN <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseEffectN"]])
  phaseEffects <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseEffectSimple"]])
  phaseIntTime <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseEffectInteraction"]])
  phaseNames <- sapply(options[["simPhaseEffects"]], function(x) x[["simPhaseName"]])

  totalN <- sum(phaseN)

  yNoise <- stats::arima.sim(
    model = list(ar = options[["simTimeEffectAutocorrelation"]]),
    n = totalN,
    sd = options[["simDependentSd"]]
  )

  phaseName <- rep(phaseNames, phaseN)
  phaseBeta <- rep(phaseEffects, phaseN)
  phaseInt <- rep(phaseIntTime, phaseN)

  time <- unlist(lapply(phaseN, seq_len))
  timeEffect <- options[["simTimeEffect"]]

  y <- options[["simDependentMean"]] + timeEffect * time + phaseBeta + time * phaseInt + yNoise

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

.ln1TreatCreateModelFormula <- function(options) {
  variableNames <- .ln1GetVariableNames(options)

  f <- as.formula(paste(variableNames[["dependent"]], "~", variableNames[["time"]], "*", variableNames[["phase"]]))

  return(f)
}

.ln1TreatEstimateModelHelper <- function(dataset, options) {
  f <- .ln1TreatCreateModelFormula(options)

  mod <- nlme::lme(
    fixed = f,
    data=dataset,
    random = ~ 1 | g,
    correlation = nlme::corAR1()
  )

  return(mod)
}

.ln1TreatEstimateModel <- function(jaspResults, dataset, options, ready) {
  if (ready && is.null(jaspResults[["modelState"]])) {
    modelObject <- .ln1TreatEstimateModelHelper(dataset, options)
    modelState <- createJaspState(object = modelObject)
    modelState$dependOn(.ln1TreatGetDataDependencies())
    jaspResults[["modelState"]] <- modelState
  }
}

.ln1TreatCreateCoefficientsTable <- function(jaspResults, options, ready) {
  if (is.null(jaspResults[["coefTable"]])) {
    table <- createJaspTable(gettext("Coefficients"))
    table$dependOn(c(.ln1TreatGetDataDependencies(), "coefficientCiLevel"))

    table$addColumnInfo(name = "name",         title = "",                        type = "string")
    table$addColumnInfo(name = "coef",         title = gettext("Estimate"),       type = "number")
    table$addColumnInfo(name = "SE",           title = gettext("Standard Error"), type = "number")
    table$addColumnInfo(name = "t",            title = gettext("t"),              type = "number")
    table$addColumnInfo(name = "p",            title = gettext("p"),              type = "pvalue")

    overtitle <- gettextf("%.0f%% CI", 100 * options[["coefficientCiLevel"]])

    table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

    table$addFootnote(gettext("Results are based on a linear mixed-effects model with an AR(1) correlation structure. The model tests whether the level and trend of the dependent variable changed across treatment phases."))

    if (!is.null(jaspResults[["modelState"]]) && ready) {
      .ln1TreatFillCoefficientsTable(table, jaspResults[["modelState"]]$object, options)
    }

    jaspResults[["coefTable"]] <- table
  }
}

.ln1TreatFillCoefficientsTable <- function(table, modelObject, options) {
  modelSummary <- summary(modelObject)
  modelCoefficients <- data.frame(coef(modelSummary))

  table[["name"]] <- row.names(modelCoefficients)
  table[["coef"]] <- modelCoefficients[["Value"]]
  table[["SE"]] <- modelCoefficients[["Std.Error"]]
  table[["t"]] <- modelCoefficients[["t.value"]]
  table[["p"]] <- modelCoefficients[["p.value"]]

  ci <- nlme::intervals(modelObject, level = options[["coefficientCiLevel"]], which = "fixed")

  ciFixed <- data.frame(ci[["fixed"]])

  table[["lower"]] <- ciFixed[["lower"]]
  table[["upper"]] <- ciFixed[["upper"]]
}

.ln1TreatCreateAutoCorTable <- function(jaspResults, options, ready) {
  if (is.null(jaspResults[["autoCorTable"]])) {
    table <- createJaspTable(gettext("Auto-correlation"))
    table$dependOn(c(.ln1TreatGetDataDependencies(), "coefficientCiLevel"))

    table$addColumnInfo(name = "name",         title = "",                        type = "string")
    table$addColumnInfo(name = "coef",         title = gettext("Estimate"),       type = "number")

    overtitle <- gettextf("%.0f%% CI", 100 * options[["coefficientCiLevel"]])

    table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
    table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

    table$addFootnote(gettext("The AR(1) coefficient reflects the first-order autocorrelation of the residuals, indicating how strongly each observation depends on the previous one."))

    if (!is.null(jaspResults[["modelState"]]) && ready) {
      .ln1TreatFillAutoCorTable(table, jaspResults[["modelState"]]$object, options)
    }

    jaspResults[["autoCorTable"]] <- table
  }
}

.ln1TreatFillAutoCorTable <- function(table, modelObject, options) {
  table[["name"]] <- "AR(1)"

  # Point estimate is always available from the model
  corStruct <- modelObject[["modelStruct"]][["corStruct"]]
  phi <- as.numeric(coef(corStruct, unconstrained = FALSE))
  table[["coef"]] <- phi

  # CIs may fail when the estimate is near the boundary
  ci <- try(nlme::intervals(modelObject, level = options[["coefficientCiLevel"]], which = "all"))

  if (!jaspBase::isTryError(ci) && !is.null(ci[["corStruct"]])) {
    ciAutoCor <- data.frame(ci[["corStruct"]])
    table[["lower"]] <- ciAutoCor[["lower"]]
    table[["upper"]] <- ciAutoCor[["upper"]]
  } else {
    table$addFootnote(gettext("Confidence intervals for the auto-correlation could not be computed. The point estimate may be near the boundary of the parameter space."))
  }
}

.ln1TreatCreateDataPlot <- function(jaspResults, dataset, options, dependencyFun, ready) {
  if (options[["plotData"]] && is.null(jaspResults[["dataPlot"]])) {
    dataPlot <- createJaspPlot(
      title = gettext("Data plot"),
      height = 480,
      width = 480,
      position = 2
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
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[[yName]])

  p <- ggplot2::ggplot(
      dataset,
      mapping = ggplot2::aes(
        x = .data[[xName]],
        y = .data[[yName]],
        color = .data[[variableNames[["phase"]]]]
      )
    ) +
    jaspGraphs::geom_line() +
    jaspGraphs::geom_point() +
    ggplot2::scale_x_continuous(
      name = if (options[["inputType"]] == "loadData") xName else gettext("Time"),
      breaks = xBreaks,
      limits = range(xBreaks)
    ) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
