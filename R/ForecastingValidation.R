.ln1ForeFiniteScalar <- function(value) {
  is.numeric(value) && !is.complex(value) && length(value) == 1L && is.finite(value)
}

.ln1ForeWholeNumber <- function(value, minimum = 0L) {
  .ln1ForeFiniteScalar(value) && value >= minimum &&
    value <= .Machine$integer.max && value == floor(value)
}

.ln1ForeValidateSimulation <- function(options) {
  if (!.ln1ForeWholeNumber(options[["numSamples"]], 2L))
    .quitAnalysis(gettext("Simulate at least two observations. N must be a whole number."))
  if (!.ln1ForeWholeNumber(options[["simIEffect"]]))
    .quitAnalysis(gettext("The simulation differencing order must be a nonnegative whole number."))
  if (!.ln1ForeFiniteScalar(options[["noiseSd"]]) || options[["noiseSd"]] < 0)
    .quitAnalysis(gettext("The noise standard deviation must be finite and nonnegative."))
  seed <- options[["seed"]]
  if (!.ln1ForeFiniteScalar(seed) || abs(seed) > .Machine$integer.max || seed != floor(seed))
    .quitAnalysis(gettext("Choose a finite whole-number random seed within the supported integer range."))
  coefficients <- lapply(c("simArEffects", "simMaEffects"), function(key) {
    rows <- options[[key]]
    field <- if (key == "simArEffects") "simArEffect" else "simMaEffect"
    if (!is.null(rows) && (!is.list(rows) || !all(vapply(rows, function(row)
        is.list(row) && .ln1ForeFiniteScalar(row[[field]]), logical(1)))))
      .quitAnalysis(gettext("Every simulation AR and MA coefficient must be a finite number."))
    vapply(rows, function(row) row[[field]], numeric(1))
  })
  ar <- coefficients[[1L]]
  if (length(ar)) {
    roots <- tryCatch(base::polyroot(c(1, -ar)), error = function(e) NULL)
    if (is.null(roots) || any(!is.finite(roots)) || any(Mod(roots) <= 1))
      .quitAnalysis(gettext("The simulation AR coefficients do not define a stationary process. Change the AR coefficients; for one AR lag its magnitude must be below 1."))
  }
  invisible(NULL)
}

.ln1ForeValidateHorizon <- function(horizon) {
  if (!.ln1ForeWholeNumber(horizon, 1L) || horizon > 10000L)
    .quitAnalysis(gettext("Request between 1 and 10000 forecasts. This limit keeps the analysis responsive; it is not a statistical reliability threshold."))
  invisible(NULL)
}

.ln1ForeValidateFit <- function(fit) {
  if (!.ln1ForeFiniteScalar(fit[["code"]]) || fit[["code"]] != 0)
    .quitAnalysis(gettext("The ARIMA model did not converge. Choose simpler AR and MA orders or review the data before interpreting coefficients or forecasts."))

  # forecast::Arima corrects its innovation variance using the number of
  # freely estimated coefficients. Automatic constant models can have a
  # fixed coefficient, so length(coef) is not the appropriate count.
  estimated <- fit[["mask"]]
  if (!is.logical(estimated) || anyNA(estimated) ||
      length(estimated) != length(fit[["coef"]]) ||
      !.ln1ForeWholeNumber(fit[["nobs"]], 1L))
    .quitAnalysis(gettext("The ARIMA model did not provide valid estimation diagnostics. Choose simpler model orders or review the data."))
  if (fit[["nobs"]] <= sum(estimated))
    .quitAnalysis(gettext("There are too few observations for the selected ARIMA model to estimate its residual variance. Choose simpler model orders or provide more observed outcomes."))
  if (!.ln1ForeFiniteScalar(fit[["sigma2"]]) || fit[["sigma2"]] < 0)
    .quitAnalysis(gettext("The ARIMA model produced an invalid residual variance. Choose simpler model orders or review the data before interpreting coefficients or forecasts."))
  invisible(NULL)
}
