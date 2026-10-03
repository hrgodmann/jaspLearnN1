# Time preparation shared by Treatment's model and plots.

.ln1TreatUpgradeState <- function(jaspResults) {
  version <- 3L
  marker <- jaspResults[["treatmentTimeVersion"]]
  if (!is.null(marker) && identical(marker$object, version))
    return(invisible(NULL))

  # Version 1 repaired row-order autocorrelation. Version 2 replaces the
  # unidentified single-group random intercept with GLS. Version 3 adds explicit
  # endpoint comparisons and coefficient context. Rebuild output labels too.
  for (key in c("simulatedDataState", "modelState", "coefTable", "autoCorTable",
                "dataPlot", "analysisPlot", "timeInfo", "introText",
                "phaseComparisons", "phaseSummary")) {
    if (!is.null(jaspResults[[key]]))
      jaspResults[[key]] <- NULL
  }
  jaspResults[["treatmentTimeVersion"]] <- createJaspState(object = version)
  return(invisible(NULL))
}

.ln1TreatInternalName <- function(dataset, name) {
  while (name %in% names(dataset))
    name <- paste0(name, "_")
  return(name)
}

.ln1TreatPrepareData <- function(dataset, options) {
  variables <- .ln1GetVariableNames(options)
  columns <- unique(unlist(variables, use.names = FALSE))
  if (!all(columns %in% names(dataset)))
    .quitAnalysis(gettext("Some selected variables are unavailable. Assign the outcome, time and phase again."))
  if (anyDuplicated(unlist(variables[c("dependent", "time", "phase")], use.names = FALSE)))
    .quitAnalysis(gettext("Select distinct variables for the outcome, time and phase."))
  if (!all(vapply(dataset[unique(unlist(variables[c("dependent", "time", "t")]))], is.numeric, logical(1))))
    .quitAnalysis(gettext("The outcome and time variables must be numeric."))

  time <- dataset[[variables[["t"]]]]
  if (length(time) < 2L || any(!is.finite(time)))
    .quitAnalysis(gettext("Time must contain at least two finite values, with a time value for every row, including missing outcomes."))
  dataset <- dataset[order(time), , drop = FALSE]
  rownames(dataset) <- NULL
  time <- dataset[[variables[["t"]]]]
  steps <- diff(time)
  if (any(steps <= 0))
    .quitAnalysis(gettext("Time values must be unique. Use one continuous time variable across all phases; do not restart time at each phase."))

  step <- (time[length(time)] - time[1L]) / (length(time) - 1L)
  roundoff <- 8 * .Machine$double.eps * max(abs(time))
  if (!is.finite(step) || roundoff > 0.001 * step)
    .quitAnalysis(gettext("The time values are too large relative to their spacing for reliable numerical precision. Express time relative to the start of the series."))
  tolerance <- max(1e-7 * step, roundoff)
  if (any(abs(steps - step) > tolerance))
    .quitAnalysis(gettext("Treatment requires equally spaced time points. Include a row with its time and phase for every missing measurement occasion, leaving only the outcome empty."))

  y <- dataset[[variables[["dependent"]]]]
  if (any(!is.na(y) & !is.finite(y)))
    .quitAnalysis(gettext("The outcome contains infinite values. Replace them with valid measurements or missing values."))
  phase <- dataset[[variables[["phase"]]]]
  if (anyNA(phase) || any(!nzchar(trimws(as.character(phase)))))
    .quitAnalysis(gettext("Every measurement occasion needs a phase, including rows with a missing outcome."))
  # Preserve an assigned factor's level order when sorting. Leave custom
  # contrasts intact when no unused levels need to be removed.
  if (!is.factor(phase))
    phase <- factor(phase)
  if (any(table(phase) == 0L))
    phase <- droplevels(phase)
  if (nlevels(phase) < 2L)
    .quitAnalysis(gettext("Select at least two treatment phases."))
  observed <- !is.na(y)
  if (any(table(phase[observed]) < 2L))
    .quitAnalysis(gettext("Each phase needs at least two observed outcomes at different times to estimate its trend."))
  if (sum(observed) <= 2L * nlevels(phase))
    .quitAnalysis(gettext("There are too few observed outcomes to estimate uncertainty. Provide more than two observations per phase on average."))
  dataset[[variables[["phase"]]]] <- phase

  occasion <- .ln1TreatInternalName(dataset, ".ln1TreatOccasion")
  dataset[[occasion]] <- seq_len(nrow(dataset))
  attr(dataset, "ln1TreatTime") <- list(occasion = occasion, step = step)
  return(dataset)
}

.ln1TreatCreateTimeInfo <- function(jaspResults, dataset, options, ready) {
  if (!ready || !is.null(jaspResults[["timeInfo"]]))
    return(invisible(NULL))
  variables <- .ln1GetVariableNames(options)
  missing <- sum(is.na(dataset[[variables[["dependent"]]]]))
  step <- format(signif(attr(dataset, "ln1TreatTime")[["step"]], 8L), trim = TRUE)
  text <- gettextf("Measurements are ordered by time. Measurement interval (in the selected time units): %1$s. There are %2$i observed outcomes and %3$i missing outcomes. Missing outcomes are not imputed; their time positions are preserved.",
                    step, nrow(dataset) - missing, missing)
  info <- createJaspHtml(text, title = gettext("Measurement timing"), position = 2)
  info$dependOn(.ln1TreatGetDataDependencies())
  jaspResults[["timeInfo"]] <- info
  return(invisible(NULL))
}
