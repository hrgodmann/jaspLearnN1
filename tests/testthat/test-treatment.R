# Tests for analysis "Does the Treatment Work?"

test_that("Simulated data input works", {
  options <- analysisOptions("Treatment")
  # jaspTools does not resolve dynamic dropdown defaults.
  options$comparisonPhase <- options$referencePhase <- ""
  options$enableIntroText <- FALSE
  options$inputType <- "simulateData"
  options$coefficientsTable <- TRUE
  options$autocorrelationTable <- TRUE
  options$plotData <- options$plotAnalysis <- TRUE
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

  # Both observed points and lines must retain phase colours and separate
  # groups; a fixed geom colour would silently connect phase boundaries.
  for (key in c("dataPlot", "analysisPlot")) {
    plot <- results$state$figures[[results$results[[key]]$data]]$obj
    layers <- ggplot2::ggplot_build(plot)$data[1:2]
    for (layer in layers) {
      expect_length(unique(layer$colour), 3L)
      expect_length(unique(layer$group), 3L)
      expect_equal(unname(vapply(split(layer$x, layer$group),
                                 function(x) diff(range(x)), numeric(1))), rep(19, 3L))
    }
    ordered <- lapply(layers, function(layer) layer[order(layer$x), ])
    expect_equal(ordered[[1L]]$colour, ordered[[2L]]$colour)
  }

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
