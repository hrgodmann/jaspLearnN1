# Tests for analysis "Does the Treatment Work?"

test_that("Simulated data input works", {
  options <- analysisOptions("Treatment")
  # jaspTools does not resolve dynamic dropdown defaults.
  options$comparisonPhase <- options$referencePhase <- ""
  options$enableIntroText <- FALSE
  options$inputType <- "simulateData"
  options$coefficientsTable <- TRUE
  options$autocorrelationTable <- TRUE
  options$simPhaseEffects <- list(list(simPhaseName = "pre", simPhaseEffectSimple = 0, simPhaseEffectInteraction = 0,
      simPhaseEffectN = 20), list(simPhaseName = "treat", simPhaseEffectSimple = 5,
      simPhaseEffectInteraction = 0, simPhaseEffectN = 20), list(
      simPhaseName = "post", simPhaseEffectSimple = 5, simPhaseEffectInteraction = -0.1,
      simPhaseEffectN = 20))
  set.seed(1)
  results <- runAnalysis("Treatment", NULL, options)
  expect_identical(results$status, "complete")

  plotName <- results[["results"]][["dataPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "data-plot")

  # The generated observations remain unchanged; independently estimate the
  # single-series GLS model using phase-local mean time and continuous AR time.
  reference <- nlme::gls(y ~ time * phase, data = testPlot$data,
                         correlation = nlme::corAR1(form = ~ t),
                         method = "REML", na.action = stats::na.exclude)
  referenceTable <- summary(reference)$tTable
  coefficientCI <- nlme::intervals(reference, level = options$coefficientCiLevel,
                                    which = "coef")$coef
  rows <- function(table) do.call(rbind, lapply(table$data, as.data.frame))
  coefficients <- rows(results$results$coefTable)
  expect_equal(coefficients$name, rownames(referenceTable))
  expect_equal(coefficients$coef, unname(referenceTable[, "Value"]), tolerance = 1e-7)
  expect_equal(coefficients$SE, unname(referenceTable[, "Std.Error"]), tolerance = 1e-7)
  expect_equal(coefficients$t, unname(referenceTable[, "t-value"]), tolerance = 1e-7)
  expect_equal(coefficients$p, unname(referenceTable[, "p-value"]), tolerance = 1e-7)
  expect_equal(coefficients$lower, unname(coefficientCI[, "lower"]), tolerance = 1e-7)
  expect_equal(coefficients$upper, unname(coefficientCI[, "upper"]), tolerance = 1e-7)

  autocorrelation <- rows(results$results$autoCorTable)
  correlationCI <- nlme::intervals(reference, level = options$coefficientCiLevel,
                                    which = "var-cov")$corStruct
  expect_equal(autocorrelation$name, "AR(1)")
  expect_equal(autocorrelation$coef,
               unname(stats::coef(reference$modelStruct$corStruct, unconstrained = FALSE)),
               tolerance = 1e-7)
  expect_equal(autocorrelation$lower, unname(correlationCI[, "lower"]), tolerance = 1e-7)
  expect_equal(autocorrelation$upper, unname(correlationCI[, "upper"]), tolerance = 1e-7)
})
