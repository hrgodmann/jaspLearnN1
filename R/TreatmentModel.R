# Fit the same REML model from multiple correlation starts. Missing scheduled
# outcomes can make zero a stationary point even when it is not the optimum.
.ln1TreatFitGls <- function(modelData) {
  observed <- !is.na(modelData[["y"]])
  gaps <- diff(modelData[["occasion"]][observed])
  # Starts on the observed-gap scale also reach narrow basins near +/-1.
  # Both extreme gaps matter: one adjacent pair need not represent the series.
  magnitudes <- unique(c(.5, .9,
    as.numeric(outer(c(.5, .9), 1 / unique(range(gaps)), "^"))))
  starts <- unique(c(0, magnitudes, -magnitudes))
  starts <- starts[is.finite(starts) & abs(starts) < 1]
  best <- NULL
  bestLogLik <- -Inf
  for (start in starts) {
    candidate <- tryCatch(nlme::gls(
      model = y ~ time * phase,
      data = modelData,
      correlation = nlme::corAR1(value = start, form = ~ occasion),
      method = "REML",
      na.action = stats::na.exclude
    ), error = function(e) NULL)
    if (is.null(candidate))
      next
    likelihood <- as.numeric(stats::logLik(candidate))
    correlation <- as.numeric(stats::coef(candidate[["modelStruct"]][["corStruct"]],
                                          unconstrained = FALSE))
    if (!is.finite(likelihood) || !is.finite(candidate[["sigma"]]) ||
        candidate[["sigma"]] <= 0 || !is.finite(correlation) || abs(correlation) >= 1 ||
        any(!is.finite(stats::coef(candidate))) || any(!is.finite(stats::vcov(candidate))))
      next
    # Retain the original zero-start fit when improvements are only roundoff.
    # Do not make the substantive tolerance depend on arbitrary outcome units.
    if (!is.null(best)) {
      tolerance <- 1e-7 + 64 * .Machine$double.eps * max(abs(c(likelihood, bestLogLik)))
      if (likelihood <= bestLogLik + tolerance)
        next
    }
    best <- candidate
    bestLogLik <- likelihood
  }
  if (is.null(best))
    .quitAnalysis(gettext("The phase-trend model could not be estimated reliably. Check for constant or nearly deterministic outcomes, very short phases, and extreme values. The data plot remains available."))
  # For even observed gaps, rho and -rho have exactly the same covariance.
  attr(best, "ln1TreatCorrelationSignIdentified") <- !all(gaps %% 2L == 0L)
  return(best)
}
