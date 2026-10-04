.treatValidFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.treatValidOptions <- function() {
  options <- jaspTools::analysisOptions("Treatment")
  options$enableIntroText <- FALSE
  options$comparisonPhase <- options$referencePhase <- ""
  options$inputType <- "simulateData"
  options$simTimeEffectAutocorrelation <- .3
  options$simDependentMean <- 10
  options$simDependentSd <- .4
  options$simTimeEffect <- -.2
  options$simPhaseEffects <- list(
    list(simPhaseName = "Zulu baseline", simPhaseEffectSimple = 0, simPhaseEffectInteraction = 0, simPhaseEffectN = 20),
    list(simPhaseName = "Active treatment", simPhaseEffectSimple = 2, simPhaseEffectInteraction = -.1, simPhaseEffectN = 25))
  options$plotData <- options$plotAnalysis <- TRUE
  options$coefficientsTable <- options$autocorrelationTable <- TRUE
  options$phaseComparisons <- options$phaseSummary <- TRUE
  options
}

test_that("phase coding follows chronology only when no explicit factor coding exists", {
  options <- .treatValidOptions()
  simulate <- .treatValidFunction(".ln1TreatSimulateData")
  prepare <- .treatValidFunction(".ln1TreatPrepareData")
  fit <- .treatValidFunction(".ln1TreatEstimateModelHelper")
  data <- simulate(options)
  prepared <- prepare(data[45:1, ], options)
  expect_identical(levels(prepared$phase), c("Zulu baseline", "Active treatment"))
  expect_equal(rownames(fit(prepared, options)$contrasts$phase)[
    rowSums(fit(prepared, options)$contrasts$phase) == 0], "Zulu baseline")

  loaded <- options
  loaded$inputType <- "loadData"
  loaded$dependent <- "y"; loaded$time <- "t"; loaded$phase <- "phase"
  characterData <- prepare(data[45:1, ], loaded)
  expect_identical(levels(characterData$phase), c("Zulu baseline", "Active treatment"))
  data$phase <- factor(data$phase, levels = c("Active treatment", "Zulu baseline"))
  contrasts(data$phase) <- stats::contr.sum(2)
  explicit <- prepare(data[45:1, ], loaded)
  expect_identical(levels(explicit$phase), levels(data$phase))
  expect_identical(contrasts(explicit$phase), contrasts(data$phase))
})

test_that("simulation retains its seeded phase-local meaning and saved phase names", {
  options <- .treatValidOptions()
  simulate <- .treatValidFunction(".ln1TreatSimulateData")
  set.seed(options$seed)
  noise <- as.numeric(stats::arima.sim(list(ar = .3), n = 45, sd = .4))
  time <- c(seq_len(20), seq_len(25))
  expected <- 10 - .2 * time + rep(c(0, 2), c(20, 25)) +
    time * rep(c(0, -.1), c(20, 25)) + noise
  expect_equal(as.numeric(simulate(options)$y), expected, tolerance = 0)
  options$simPhaseEffects[[1]]$simPhaseName <- "Pre-treament"
  options$referencePhase <- "Pre-treament"
  options$comparisonPhase <- "Active treatment"
  result <- jaspTools::runAnalysis("Treatment", NULL, options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_equal(result$results$phaseComparisons$data[[1]]$referencePhase, "Pre-treament")
})

test_that("invalid simulation settings fail with actionable messages before generation", {
  simulate <- .treatValidFunction(".ln1TreatSimulateData")
  base <- .treatValidOptions()
  for (value in list(-1, 1, NA_real_, Inf, "0.5", c(.2, .3))) {
    options <- base; options$simTimeEffectAutocorrelation <- value
    expect_error(simulate(options), "autocorrelation")
  }
  for (value in list(-1, Inf, NaN, "1")) {
    options <- base; options$simDependentSd <- value
    expect_error(simulate(options), "standard deviation")
  }
  for (name in c("simDependentMean", "simTimeEffect")) {
    options <- base; options[[name]] <- Inf
    expect_error(simulate(options), "finite")
  }
  for (value in list(-1, .5, Inf, NA_real_)) {
    options <- base; options$seed <- value
    expect_error(simulate(options), "seed")
  }
  for (value in list(0, 1, 2.5, NA_real_, Inf)) {
    options <- base; options$simPhaseEffects[[1]]$simPhaseEffectN <- value
    expect_error(simulate(options), "time points")
  }
  for (name in c("simPhaseEffectSimple", "simPhaseEffectInteraction")) {
    options <- base; options$simPhaseEffects[[1]][[name]] <- Inf
    expect_error(simulate(options), "finite")
  }
  options <- base; options$simPhaseEffects <- options$simPhaseEffects[1]
  expect_error(simulate(options), "at least two")
  options <- base; options$simPhaseEffects[[2]]$simPhaseName <- options$simPhaseEffects[[1]]$simPhaseName
  expect_error(simulate(options), "distinct name")
  options <- base; options$simPhaseEffects[[1]]$simPhaseName <- " "
  expect_error(simulate(options), "nonblank")
  options <- base; options$simPhaseEffects[[1]] <- 1
  expect_error(simulate(options), "Complete the settings")
  options <- base; options$simTimeEffectAutocorrelation <- 1
  result <- jaspTools::runAnalysis("Treatment", NULL, options, view = FALSE)
  expect_identical(result$status, "validationError")
  expect_match(result$results$errorMessage, "greater than -1 and less than 1")
})

test_that("zero-noise demonstrations and real fit failures retain the data plot", {
  for (simulation in c(TRUE, FALSE)) {
    options <- .treatValidOptions()
    options$simDependentSd <- 0
    data <- NULL
    if (!simulation) {
      options$inputType <- "loadData"
      options$dependent <- "y"; options$time <- "clock"; options$phase <- "phase"
      data <- data.frame(y = rep(0, 12), clock = 1:12,
                         phase = factor(rep(c("A", "B"), each = 6)))
    }
    result <- jaspTools::runAnalysis("Treatment", data, options, view = FALSE)
    expect_identical(result$status, "complete")
    expect_identical(result$results$dataPlot$status, "complete")
    for (key in c("analysisPlot", "coefTable", "autoCorTable", "phaseComparisons", "phaseSummary")) {
      expect_identical(result$results[[key]]$status, "error")
      expect_match(result$results[[key]]$error$errorMessage,
                   if (simulation) "zero-noise|no residual variation" else "could not be estimated reliably")
    }
    plot <- result$state$figures[[result$results$dataPlot$data]]$obj
    expect_true(all(is.finite(plot$data$y)))
    if (simulation)
      expect_equal(plot$data$y, 10 - .2 * plot$data$time + rep(c(0, 2), c(20, 25)) +
        plot$data$time * rep(c(0, -.1), c(20, 25)))
  }
})

test_that("fractional confidence levels are preserved in every Treatment table heading", {
  options <- .treatValidOptions()
  options$plotData <- options$plotAnalysis <- FALSE
  for (level in c(.975, .995)) {
    options$coefficientCiLevel <- level
    result <- jaspTools::runAnalysis("Treatment", NULL, options, view = FALSE)
    expect_identical(result$status, "complete")
    for (key in c("coefTable", "autoCorTable", "phaseComparisons", "phaseSummary")) {
      fields <- result$results[[key]]$schema$fields
      intervals <- fields[vapply(fields, function(field) field$name %in% c("lower", "upper"), logical(1))]
      expect_equal(vapply(intervals, `[[`, character(1), "overTitle"), rep(paste0(100 * level, "% CI"), 2))
    }
  }
  validate <- .treatValidFunction(".ln1TreatValidateConfidenceLevel")
  for (level in list(0, 1, -.1, Inf, NA_real_, "95")) {
    options$coefficientCiLevel <- level
    expect_error(validate(options), "confidence level")
  }
})

test_that("timing output shows observed phase counts and qualifies very short series", {
  options <- .treatValidOptions()
  options$inputType <- "loadData"
  options$dependent <- "y"; options$time <- "clock"; options$phase <- "phase"
  options$plotData <- options$plotAnalysis <- FALSE
  data <- data.frame(y = c(1, 3, 2, NA, 5, 2, 6, 3, 7, 2), clock = 1:10,
                     phase = rep(c("A <one>", "B & two"), each = 5))
  result <- jaspTools::runAnalysis("Treatment", data, options, view = FALSE)
  expect_identical(result$status, "complete")
  text <- result$results$timeInfo$rawtext
  expect_match(text, "A &lt;one&gt;: 4", fixed = TRUE)
  expect_match(text, "B &amp; two: 5", fixed = TRUE)
  expect_match(text, "fewer than five", fixed = TRUE)
  expect_match(text, "larger counts do not guarantee", fixed = TRUE)
})
