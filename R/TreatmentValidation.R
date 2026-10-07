.ln1TreatFiniteScalar <- function(value) {
  is.numeric(value) && !is.complex(value) && length(value) == 1L && is.finite(value)
}

.ln1TreatValidateConfidenceLevel <- function(options) {
  level <- options[["coefficientCiLevel"]]
  if (!.ln1TreatFiniteScalar(level) || level <= 0 || level >= 1)
    .quitAnalysis(gettextf("The confidence level must be greater than 0%% and less than 100%%."))
  invisible(NULL)
}

.ln1TreatCiTitle <- function(options) {
  gettextf("%1$s%% CI", format(100 * options[["coefficientCiLevel"]],
                              trim = TRUE, scientific = FALSE, digits = 12L))
}

.ln1TreatValidateSimulation <- function(options) {
  seed <- options[["seed"]]
  if (!.ln1TreatFiniteScalar(seed) || seed < 0 || seed > .Machine$integer.max || seed != floor(seed))
    .quitAnalysis(gettext("The simulation seed must be a whole number from 0 to 2147483647."))
  if (!.ln1TreatFiniteScalar(options[["simDependentMean"]]) ||
      !.ln1TreatFiniteScalar(options[["simTimeEffect"]]))
    .quitAnalysis(gettext("Enter finite values for the simulated mean and phase-local time effect."))
  sd <- options[["simDependentSd"]]
  if (!.ln1TreatFiniteScalar(sd) || sd < 0)
    .quitAnalysis(gettext("The simulated noise standard deviation must be finite and at least zero. Zero shows a deterministic series without model inference."))
  phi <- options[["simTimeEffectAutocorrelation"]]
  if (!.ln1TreatFiniteScalar(phi) || abs(phi) >= 1)
    .quitAnalysis(gettext("The simulated autocorrelation must be greater than -1 and less than 1."))
  phases <- options[["simPhaseEffects"]]
  if (!is.list(phases) || length(phases) < 2L)
    .quitAnalysis(gettext("Define at least two simulation phases."))
  labels <- vapply(seq_along(phases), function(i) {
    phase <- phases[[i]]
    if (!is.list(phase))
      .quitAnalysis(gettextf("Complete the settings for simulation phase %1$i.", i))
    name <- phase[["simPhaseName"]]
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(trimws(name)))
      .quitAnalysis(gettextf("Enter a nonblank name for simulation phase %1$i.", i))
    n <- phase[["simPhaseEffectN"]]
    if (!.ln1TreatFiniteScalar(n) || n < 2 || n > .Machine$integer.max || n != floor(n))
      .quitAnalysis(gettextf("Simulation phase '%1$s' needs at least two time points, entered as a whole number.", name))
    if (!.ln1TreatFiniteScalar(phase[["simPhaseEffectSimple"]]) ||
        !.ln1TreatFiniteScalar(phase[["simPhaseEffectInteraction"]]))
      .quitAnalysis(gettextf("Enter finite phase and phase-by-time effects for simulation phase '%1$s'.", name))
    name
  }, character(1))
  totalN <- sum(vapply(phases, function(phase) phase[["simPhaseEffectN"]], numeric(1)))
  if (totalN > 1000)
    .quitAnalysis(gettext("Simulations are limited to 1,000 time points across all phases to keep fitting responsive. Reduce the time points per phase or remove phases."))
  if (anyDuplicated(labels))
    .quitAnalysis(gettext("Give every simulation phase a distinct name, such as Baseline 1, Treatment, and Baseline 2."))
  invisible(NULL)
}

.ln1TreatSetModelError <- function(output, jaspResults) {
  error <- jaspResults[["modelErrorState"]]
  if (is.null(error))
    return(FALSE)
  output$setError(error$object)
  TRUE
}
