# Phase endpoints and trends are linear contrasts of the existing GLS fit.
# Keep its chronological AR(1) clock and regression parameterization unchanged.
# Data-consuming helpers require .ln1TreatPrepareData() output, which already
# enforces one contiguous episode per phase and valid phase-local clocks.

.ln1TreatComparisonDependencies <- function() {
  c(.ln1TreatGetDataDependencies(), "phaseComparisons", "comparisonPhase",
    "referencePhase", "coefficientCiLevel")
}

.ln1TreatCoefficientContext <- function(table, modelObject, options) {
  coding <- modelObject[["contrasts"]][["phase"]]
  if (is.matrix(coding) && all(coding %in% c(0, 1)) &&
      all(colSums(coding) == 1) && sum(rowSums(coding) == 0) == 1L &&
      all(rowSums(coding) %in% c(0, 1))) {
    reference <- rownames(coding)[rowSums(coding) == 0]
    table$addFootnote(gettextf("Coefficient reference phase: %1$s. The intercept is its fitted outcome at regression time 0. Phase coefficients are differences from this reference at regression time 0; interactions are slope differences. These are not automatically endpoint or treatment-onset comparisons.", reference))
  } else {
    table$addFootnote(gettext("Phase coefficients use contrast coding without a single treatment-coded reference phase. Use Phase comparisons for explicitly named endpoint and slope differences."))
  }
  if (options[["inputType"]] == "simulateData") {
    table$addFootnote(gettext("Simulation regression time starts at 1 in each phase, so regression time 0 is one occasion before each phase's own first measurement."))
  } else {
    table$addFootnote(gettextf("Regression time 0 means %1$s = 0 in the selected time variable. The supplied time origin is retained.", decodeColNames(options[["time"]])))
  }
}

.ln1TreatPhaseDesign <- function(dataset, modelObject, options) {
  variables <- .ln1GetVariableNames(options)
  phase <- dataset[[variables[["phase"]]]]
  phaseNames <- unique(as.character(phase))
  indices <- lapply(phaseNames, function(name) which(as.character(phase) == name))
  ends <- vapply(indices, function(i) utils::tail(i, 1L), integer(1))
  starts <- vapply(indices, function(i) i[1L], integer(1))
  newData <- data.frame(time = dataset[[variables[["time"]]]][ends],
                        phase = factor(phaseNames, levels = levels(phase)))
  design <- function(data) {
    matrix <- stats::model.matrix(~ time * phase, data,
                                  contrasts.arg = modelObject[["contrasts"]])
    matrix[, names(stats::coef(modelObject)), drop = FALSE]
  }
  endpoint <- design(newData)
  newData[["time"]] <- 1
  one <- design(newData)
  newData[["time"]] <- 0
  slope <- one - design(newData)
  list(phases = data.frame(phase = phaseNames,
                           startTime = dataset[[variables[["t"]]]][starts],
                           endTime = dataset[[variables[["t"]]]][ends]),
       endpoint = endpoint, slope = slope)
}

.ln1TreatContrast <- function(weights, modelObject, level) {
  covariance <- stats::vcov(modelObject)
  estimate <- as.numeric(weights %*% stats::coef(modelObject))
  variance <- rowSums((weights %*% covariance) * weights)
  if (any(!is.finite(variance)) || any(variance <= 0))
    stop(gettext("The uncertainty of this phase comparison could not be estimated reliably."), call. = FALSE)
  se <- sqrt(variance)
  df <- modelObject[["dims"]][["N"]] - modelObject[["dims"]][["p"]]
  statistic <- estimate / se
  margin <- stats::qt((1 + level) / 2, df) * se
  data.frame(estimate = estimate, SE = se, df = df, t = statistic,
             p = 2 * stats::pt(-abs(statistic), df),
             lower = estimate - margin, upper = estimate + margin)
}

.ln1TreatSelectComparison <- function(phases, options) {
  select <- function(value, default) {
    if (is.null(value) || identical(value, ""))
      return(default)
    if (!is.character(value) || length(value) != 1L || is.na(value) || !value %in% phases[["phase"]])
      stop(gettext("A selected comparison phase is unavailable. Select the phases again."), call. = FALSE)
    match(value, phases[["phase"]])
  }
  reference <- select(options[["referencePhase"]], 1L)
  comparison <- select(options[["comparisonPhase"]], 2L)
  if (comparison == reference)
    stop(gettext("Select two different phases for the comparison."), call. = FALSE)
  c(comparison = comparison, reference = reference)
}

.ln1TreatComparisonResults <- function(dataset, modelObject, options) {
  design <- .ln1TreatPhaseDesign(dataset, modelObject, options)
  selected <- .ln1TreatSelectComparison(design[["phases"]], options)
  a <- selected[["comparison"]]
  b <- selected[["reference"]]
  weights <- rbind(design[["endpoint"]][a, ] - design[["endpoint"]][b, ],
                   design[["slope"]][a, ] - design[["slope"]][b, ])
  data.frame(quantity = c("endpoint", "slope"),
             comparisonPhase = design[["phases"]][["phase"]][a],
             referencePhase = design[["phases"]][["phase"]][b],
             comparisonEnd = design[["phases"]][["endTime"]][a],
             referenceEnd = design[["phases"]][["endTime"]][b],
             .ln1TreatContrast(weights, modelObject, options[["coefficientCiLevel"]]))
}

.ln1TreatPhaseSummaries <- function(dataset, modelObject, options) {
  design <- .ln1TreatPhaseDesign(dataset, modelObject, options)
  index <- rep(seq_len(nrow(design[["phases"]])), each = 2L)
  weights <- design[["endpoint"]][index, , drop = FALSE]
  weights[seq(2L, nrow(weights), by = 2L), ] <- design[["slope"]]
  data.frame(design[["phases"]][index, c("phase", "startTime", "endTime")],
             quantity = rep(c("endpoint", "slope"), nrow(design[["phases"]])),
             .ln1TreatContrast(weights, modelObject, options[["coefficientCiLevel"]]),
             row.names = NULL)
}

.ln1TreatComparisonFootnotes <- function(table, options) {
  table$addFootnote(gettext("Each endpoint is the fitted outcome at that phase's own last scheduled time, including when its last outcome is missing. Endpoint differences compare two different times; they are not immediate changes at treatment onset. Slopes describe change per time unit moving forward."))
  table$addFootnote(gettext("Estimates use the same GLS model with AR(1) residual correlation and REML. Confidence intervals and tests use an approximate t distribution; they can be unreliable for short series or strong autocorrelation. These results do not by themselves establish causation or clinical importance."))
  if (options[["inputType"]] == "simulateData")
    table$addFootnote(gettext("For simulations, endpoint times use the continuous measurement sequence. Predictions use each phase's own regression clock; slopes are per measurement occasion."))
}

.ln1TreatAddEstimateColumns <- function(table, options, tests) {
  table$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  table$addColumnInfo(name = "SE", title = gettext("Standard Error"), type = "number")
  if (tests) {
    table$addColumnInfo(name = "df", title = gettext("df"), type = "number")
    table$addColumnInfo(name = "t", title = gettext("t"), type = "number")
    table$addColumnInfo(name = "p", title = gettext("p"), type = "pvalue")
  }
  overtitle <- .ln1TreatCiTitle(options)
  table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)
}

.ln1TreatCreatePhaseComparisons <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["phaseComparisons"]]))
    return(invisible(NULL))
  table <- createJaspTable(gettext("Phase comparisons"), position = 5)
  table$dependOn(.ln1TreatComparisonDependencies())
  table$addColumnInfo(name = "comparisonPhase", title = gettext("Compared phase"), type = "string")
  table$addColumnInfo(name = "referencePhase", title = gettext("Reference phase"), type = "string")
  table$addColumnInfo(name = "quantity", title = gettext("Comparison"), type = "string")
  .ln1TreatAddEstimateColumns(table, options, tests = TRUE)
  .ln1TreatComparisonFootnotes(table, options)
  table$addFootnote(gettext("Differences are compared phase minus reference phase. A negative endpoint difference means a lower fitted outcome; whether this represents improvement depends on the outcome scale. The two p-values are unadjusted tests of different questions, not a single test of treatment success."))
  if (ready && !.ln1TreatSetModelError(table, jaspResults) && !is.null(jaspResults[["modelState"]])) {
    tryCatch({
      results <- .ln1TreatComparisonResults(dataset, jaspResults[["modelState"]]$object, options)
      table$addFootnote(gettextf("Endpoint times: %1$s = %2$s; %3$s = %4$s (chronological time).",
                                results[["comparisonPhase"]][1L], format(results[["comparisonEnd"]][1L], digits = 15L, trim = TRUE),
                                results[["referencePhase"]][1L], format(results[["referenceEnd"]][1L], digits = 15L, trim = TRUE)))
      results[["quantity"]] <- c(gettext("Endpoint difference"), gettext("Slope difference"))
      table$addRows(results[, c("comparisonPhase", "referencePhase", "quantity", "estimate", "SE", "df", "t", "p", "lower", "upper")])
    }, error = function(e) table$setError(conditionMessage(e)))
  }
  jaspResults[["phaseComparisons"]] <- table
  return(invisible(NULL))
}

.ln1TreatCreatePhaseSummary <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["phaseSummary"]]))
    return(invisible(NULL))
  table <- createJaspTable(gettext("Phase estimates"), position = 6)
  table$dependOn(c(.ln1TreatGetDataDependencies(), "phaseSummary", "coefficientCiLevel"))
  table$addColumnInfo(name = "phase", title = gettext("Phase"), type = "string")
  table$addColumnInfo(name = "endTime", title = gettext("End time"), type = "number")
  table$addColumnInfo(name = "quantity", title = gettext("Quantity"), type = "string")
  .ln1TreatAddEstimateColumns(table, options, tests = FALSE)
  .ln1TreatComparisonFootnotes(table, options)
  if (ready && !.ln1TreatSetModelError(table, jaspResults) && !is.null(jaspResults[["modelState"]])) {
    tryCatch({
      results <- .ln1TreatPhaseSummaries(dataset, jaspResults[["modelState"]]$object, options)
      results[["quantity"]] <- rep(c(gettext("Fitted endpoint"), gettext("Slope")), nrow(results) / 2L)
      table$addRows(results[, c("phase", "endTime", "quantity", "estimate", "SE", "lower", "upper")])
    }, error = function(e) table$setError(conditionMessage(e)))
  }
  jaspResults[["phaseSummary"]] <- table
  return(invisible(NULL))
}
