# Chronology regression tests. Reference models deliberately construct their
# own observation index instead of using the module's preparation helper.

.treatTimeFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.treatTimeOptions <- function() {
  options <- jaspTools::analysisOptions("Treatment")
  # jaspTools does not resolve dynamic dropdown defaults.
  options$comparisonPhase <- options$referencePhase <- ""
  options$enableIntroText <- FALSE
  options$inputType <- "loadData"
  options$dependent <- "symptom"
  options$time <- "clock"
  options$phase <- "stage"
  options$plotData <- FALSE
  options$plotAnalysis <- FALSE
  options$coefficientsTable <- TRUE
  options$autocorrelationTable <- TRUE
  options
}

.treatTimeFixture <- function() {
  set.seed(447)
  occasion <- seq_len(120L)
  noise <- as.numeric(stats::arima.sim(list(ar = .6), n = 120L, sd = .4))
  data.frame(symptom = 1 + .02 * occasion + rep(c(0, 2, 1), each = 40L) + noise,
             clock = occasion,
             stage = factor(rep(c("baseline", "treatment", "followup"), each = 40L)))
}

.treatTimePrepare <- function(data, options = .treatTimeOptions()) {
  .treatTimeFunction(".ln1TreatPrepareData")(data, options)
}

.treatTimeFit <- function(data, options = .treatTimeOptions()) {
  .treatTimeFunction(".ln1TreatEstimateModelHelper")(data, options)
}

.treatTimePhi <- function(model) {
  unname(stats::coef(model$modelStruct$corStruct, unconstrained = FALSE))
}

.treatTimeReference <- function(data) {
  ordered <- data[order(data$clock), , drop = FALSE]
  reference <- data.frame(y = ordered$symptom, time = ordered$clock,
                          phase = ordered$stage,
                          occasion = seq_len(nrow(ordered)))
  nlme::gls(y ~ time * phase, data = reference, method = "REML",
            correlation = nlme::corAR1(form = ~ occasion),
            na.action = stats::na.exclude)
}

.treatTimeRows <- function(table) {
  do.call(rbind, lapply(table$data, function(row) {
    as.data.frame(row, stringsAsFactors = FALSE)
  }))
}

test_that("Treatment sorts the whole record by the selected chronological clock", {
  data <- .treatTimeFixture()
  data$clock <- 100 + 7 * (data$clock - 1L)
  permutation <- c(seq(2L, 120L, 2L), seq(1L, 119L, 2L))
  prepared <- .treatTimePrepare(data[permutation, ])
  timing <- attr(prepared, "ln1TreatTime")
  expect_equal(prepared$symptom, data$symptom)
  expect_equal(prepared$clock, data$clock)
  expect_equal(as.character(prepared$stage), as.character(data$stage))
  expect_identical(rownames(prepared), as.character(seq_len(nrow(data))))
  expect_equal(prepared[[timing$occasion]], seq_len(nrow(data)))
  expect_setequal(names(timing), c("occasion", "step"))
  expect_equal(timing$step, 7)
})

test_that("Treatment AR1 estimates follow time rather than input row order", {
  data <- .treatTimeFixture()
  permutation <- c(seq(2L, 120L, 2L), seq(1L, 119L, 2L))
  sorted <- .treatTimeFit(.treatTimePrepare(data))
  shuffled <- .treatTimeFit(.treatTimePrepare(data[permutation, ]))
  reference <- .treatTimeReference(data)
  for (actual in list(sorted, shuffled)) {
    expect_equal(stats::coef(actual), stats::coef(reference), tolerance = 1e-7)
    expect_equal(stats::vcov(actual), stats::vcov(reference), tolerance = 1e-7)
    expect_equal(.treatTimePhi(actual), .treatTimePhi(reference), tolerance = 1e-7)
    expect_equal(as.numeric(stats::fitted(actual)),
                 as.numeric(stats::fitted(reference)), tolerance = 1e-7)
  }
})

test_that("blank outcomes retain their elapsed intervals and fitted-row alignment", {
  data <- .treatTimeFixture()
  missing <- c(1L, 8L, 9L, 40L, 65L, 120L)
  data$symptom[missing] <- NA_real_
  prepared <- .treatTimePrepare(data[c(120:61, 60:1), ])
  actual <- .treatTimeFit(prepared)
  reference <- .treatTimeReference(data)
  observed <- which(!is.na(data$symptom))
  covarianceClock <- as.numeric(unlist(nlme::getCovariate(actual$modelStruct$corStruct)))
  expect_equal(covarianceClock, observed)
  expect_equal(stats::nobs(actual), length(observed))
  expect_equal(stats::coef(actual), stats::coef(reference), tolerance = 1e-7)
  expect_equal(stats::vcov(actual), stats::vcov(reference), tolerance = 1e-7)
  expect_equal(.treatTimePhi(actual), .treatTimePhi(reference), tolerance = 1e-7)
  fitted <- as.numeric(stats::fitted(actual))
  expect_length(fitted, nrow(data))
  expect_equal(which(is.na(fitted)), missing)
  expect_equal(fitted, as.numeric(stats::fitted(reference)), tolerance = 1e-7)

  correlation <- nlme::corMatrix(actual$modelStruct$corStruct)
  phi <- .treatTimePhi(actual)
  # Occasions 39 and 41 are separated by the blank outcome at occasion 40.
  expect_equal(correlation[match(39L, observed), match(41L, observed)], phi^2,
               tolerance = 1e-12)
  # Two consecutive blanks at 8 and 9 leave three elapsed intervals.
  expect_equal(correlation[match(7L, observed), match(10L, observed)], phi^3,
               tolerance = 1e-12)
  expect_equal(correlation[match(41L, observed), match(42L, observed)], phi,
               tolerance = 1e-12)
})

test_that("changing clock units preserves AR1 and fitted values and rescales slopes", {
  data <- .treatTimeFixture()
  original <- .treatTimeFit(.treatTimePrepare(data))
  shifted <- data
  shifted$clock <- 100 + 7 * shifted$clock
  transformed <- .treatTimeFit(.treatTimePrepare(shifted))
  expect_equal(.treatTimePhi(transformed), .treatTimePhi(original), tolerance = 1e-6)
  expect_equal(as.numeric(stats::fitted(transformed)),
               as.numeric(stats::fitted(original)), tolerance = 1e-6)
  slope <- grepl("^time($|:)", names(stats::coef(original)))
  expect_equal(stats::coef(transformed)[slope], stats::coef(original)[slope] / 7,
               tolerance = 1e-6)
  expect_equal(sqrt(diag(stats::vcov(transformed)))[slope],
               sqrt(diag(stats::vcov(original)))[slope] / 7, tolerance = 1e-6)
})

test_that("simulated phase-local regression time has continuous AR1 chronology", {
  options <- .treatTimeOptions()
  options$inputType <- "simulateData"
  options$seed <- 447L
  options$simTimeEffectAutocorrelation <- .6
  options$simPhaseEffects <- list(
    list(simPhaseName = "baseline", simPhaseEffectSimple = 0,
         simPhaseEffectInteraction = 0, simPhaseEffectN = 40L),
    list(simPhaseName = "treatment", simPhaseEffectSimple = 2,
         simPhaseEffectInteraction = -.02, simPhaseEffectN = 40L),
    list(simPhaseName = "followup", simPhaseEffectSimple = 1,
         simPhaseEffectInteraction = .01, simPhaseEffectN = 40L))
  data <- .treatTimeFunction(".ln1TreatSimulateData")(options)
  prepared <- .treatTimePrepare(data[120:1, ], options)
  timing <- attr(prepared, "ln1TreatTime")
  expect_equal(prepared$time, rep(seq_len(40L), 3L))
  expect_equal(prepared$t, seq_len(120L))
  expect_equal(prepared[[timing$occasion]], seq_len(120L))
  actual <- .treatTimeFit(prepared, options)
  referenceData <- data
  referenceData$phase <- factor(referenceData$phase, levels = unique(referenceData$phase))
  reference <- nlme::gls(y ~ time * phase, data = referenceData, method = "REML",
                         correlation = nlme::corAR1(form = ~ t),
                         na.action = stats::na.exclude)
  expect_equal(stats::coef(actual), stats::coef(reference), tolerance = 1e-7)
  expect_equal(stats::vcov(actual), stats::vcov(reference), tolerance = 1e-7)
  expect_equal(.treatTimePhi(actual), .treatTimePhi(reference), tolerance = 1e-7)
  correlation <- nlme::corMatrix(actual$modelStruct$corStruct)
  expect_equal(correlation[40L, 41L], .treatTimePhi(actual), tolerance = 1e-12)
  expect_equal(correlation[80L, 81L], .treatTimePhi(actual), tolerance = 1e-12)
})

test_that("decimal time grids are accepted but ambiguous and incomplete grids fail", {
  data <- .treatTimeFixture()
  decimal <- data
  decimal$clock <- .1 + .1 * (decimal$clock - 1L)
  prepared <- .treatTimePrepare(decimal)
  expect_equal(attr(prepared, "ln1TreatTime")$step, .1, tolerance = 1e-12)
  expect_equal(prepared$clock, decimal$clock)
  for (badTime in c(data$clock[1], NA_real_, Inf, NaN, data$clock[2] + .25)) {
    invalid <- data
    invalid$clock[2] <- badTime
    expect_error(.treatTimePrepare(invalid), "time|clock|interval|spac|finite|unique",
                 ignore.case = TRUE)
  }
  # Omitted occasions require explicit rows with blank outcomes.
  expect_error(.treatTimePrepare(data[-40L, ]), "time|clock|interval|spac",
               ignore.case = TRUE)
  invalid <- data
  invalid$clock <- rep(seq_len(40L), 3L)
  expect_error(.treatTimePrepare(invalid), "time|clock|unique|duplicat", ignore.case = TRUE)
  invalid$clock <- as.character(data$clock)
  expect_error(.treatTimePrepare(invalid), "time|clock|numeric", ignore.case = TRUE)
})

test_that("Treatment rejects invalid columns, phase labels and insufficient observations", {
  data <- .treatTimeFixture()
  options <- .treatTimeOptions()
  for (column in c("symptom", "clock", "stage")) {
    invalid <- data
    invalid[[column]] <- NULL
    expect_error(.treatTimePrepare(invalid, options), "column|variable|select|missing",
                 ignore.case = TRUE)
  }
  options$phase <- options$time
  expect_error(.treatTimePrepare(data, options), "different|distinct|same|unique",
               ignore.case = TRUE)
  options <- .treatTimeOptions()
  for (badPhase in c(NA_character_, "")) {
    invalid <- data
    invalid$stage <- as.character(invalid$stage)
    invalid$stage[20L] <- badPhase
    expect_error(.treatTimePrepare(invalid), "phase|label|missing|empty", ignore.case = TRUE)
  }
  for (badOutcome in c(Inf, -Inf)) {
    invalid <- data
    invalid$symptom[20L] <- badOutcome
    expect_error(.treatTimePrepare(invalid), "outcome|dependent|finite", ignore.case = TRUE)
  }
  invalid <- data
  invalid$symptom <- as.character(invalid$symptom)
  expect_error(.treatTimePrepare(invalid), "outcome|dependent|numeric", ignore.case = TRUE)
  invalid <- data
  invalid$stage <- "baseline"
  expect_error(.treatTimePrepare(invalid), "phase|two|2", ignore.case = TRUE)
  invalid <- data
  invalid$symptom[41:79] <- NA_real_
  expect_error(.treatTimePrepare(invalid), "phase|observ|two|2", ignore.case = TRUE)
  invalid$symptom[80L] <- NA_real_
  expect_error(.treatTimePrepare(invalid), "phase|observ|two|2", ignore.case = TRUE)
  invalid <- data
  invalid$symptom[-c(1L, 2L, 41L, 42L, 81L, 82L)] <- NA_real_
  expect_error(.treatTimePrepare(invalid), "observ|residual|enough|phase", ignore.case = TRUE)
})

test_that("internal chronology columns never overwrite selected data", {
  data <- .treatTimeFixture()
  first <- .treatTimePrepare(data)
  timing <- attr(first, "ln1TreatTime")
  options <- .treatTimeOptions()
  renamed <- data
  names(renamed) <- c(timing$occasion, "series", "g")
  options$dependent <- timing$occasion
  options$time <- "series"
  options$phase <- "g"
  prepared <- .treatTimePrepare(renamed, options)
  newTiming <- attr(prepared, "ln1TreatTime")
  expect_false(newTiming$occasion %in% names(renamed))
  expect_equal(prepared[[options$dependent]], data$symptom)
  expect_equal(prepared[[options$time]], data$clock)
  expect_equal(as.character(prepared[[options$phase]]), as.character(data$stage))
  fit <- .treatTimeFit(prepared, options)
  reference <- .treatTimeReference(data)
  expect_equal(unname(stats::coef(fit)), unname(stats::coef(reference)), tolerance = 1e-7)
  expect_equal(.treatTimePhi(fit), .treatTimePhi(reference), tolerance = 1e-7)
})

test_that("sorting preserves assigned phase level order and contrasts", {
  data <- .treatTimeFixture()
  data$stage <- factor(data$stage, levels = c("treatment", "followup", "baseline"))
  stats::contrasts(data$stage) <- stats::contr.sum(3L)
  prepared <- .treatTimePrepare(data[120:1, ])
  expect_identical(levels(prepared$stage), levels(data$stage))
  expect_equal(stats::contrasts(prepared$stage), stats::contrasts(data$stage))
  actual <- .treatTimeFit(prepared)
  reference <- .treatTimeReference(data)
  expect_equal(stats::coef(actual), stats::coef(reference), tolerance = 1e-7)
  expect_equal(stats::vcov(actual), stats::vcov(reference), tolerance = 1e-7)
})

test_that("Treatment integration reports identical results after input rows are reordered", {
  data <- .treatTimeFixture()
  options <- .treatTimeOptions()
  original <- jaspTools::runAnalysis("Treatment", data, options, view = FALSE)
  shuffled <- jaspTools::runAnalysis("Treatment", data[120:1, ], options, view = FALSE)
  expect_identical(original$status, "complete")
  expect_identical(shuffled$status, "complete")
  originalRows <- .treatTimeRows(original$results$coefTable)
  shuffledRows <- .treatTimeRows(shuffled$results$coefTable)
  expect_equal(shuffledRows, originalRows, tolerance = 1e-7)
  reference <- .treatTimeReference(data)
  expect_equal(originalRows$coef, unname(stats::coef(reference)), tolerance = 1e-7)
  referenceSE <- unname(sqrt(diag(stats::vcov(reference))))
  expect_equal(originalRows$SE, referenceSE, tolerance = 1e-7)
  expect_equal(.treatTimeRows(original$results$autoCorTable)$coef,
               .treatTimePhi(reference), tolerance = 1e-7)
})

test_that("selected column names with spaces are retained in Treatment output", {
  data <- .treatTimeFixture()
  renamed <- data
  names(renamed) <- c("Symptom score", "Measurement clock", "Treatment stage")
  options <- .treatTimeOptions()
  options$dependent <- names(renamed)[1L]
  options$time <- names(renamed)[2L]
  options$phase <- names(renamed)[3L]
  fit <- .treatTimeFit(.treatTimePrepare(renamed, options), options)
  reference <- .treatTimeReference(data)
  expect_equal(stats::coef(fit), stats::coef(reference), tolerance = 1e-7)
  expect_equal(stats::vcov(fit), stats::vcov(reference), tolerance = 1e-7)
  result <- jaspTools::runAnalysis("Treatment", renamed, options, view = FALSE)
  expect_identical(result$status, "complete")
  rows <- .treatTimeRows(result$results$coefTable)
  expect_equal(rows$coef, unname(stats::coef(reference)), tolerance = 1e-7)
  expect_true(any(grepl("Measurement clock", rows$name, fixed = TRUE)))
  expect_true(any(grepl("Treatment stage", rows$name, fixed = TRUE)))
})

test_that("a literal dot column name cannot expand Treatment's regression design", {
  data <- .treatTimeFixture()
  reference <- .treatTimeReference(data)
  for (column in c("dependent", "time", "phase")) {
    options <- .treatTimeOptions()
    renamed <- data
    names(renamed)[names(renamed) == options[[column]]] <- "."
    options[[column]] <- "."
    prepared <- .treatTimePrepare(renamed, options)
    fit <- .treatTimeFit(prepared, options)
    expect_equal(stats::coef(fit), stats::coef(reference), tolerance = 1e-7)
    result <- jaspTools::runAnalysis("Treatment", renamed, options, view = FALSE)
    expect_identical(result$status, "complete")
    rows <- .treatTimeRows(result$results$coefTable)
    expect_equal(nrow(rows), length(stats::coef(reference)))
    expect_equal(rows$coef, unname(stats::coef(reference)), tolerance = 1e-7)
  }
})

test_that("Treatment integration explains invalid timing instead of estimating a model", {
  data <- .treatTimeFixture()
  data$clock[41L] <- data$clock[40L]
  result <- jaspTools::runAnalysis("Treatment", data, .treatTimeOptions(), view = FALSE)
  expect_identical(result$status, "validationError")
  expect_true(result$results$error)
  expect_match(result$results$errorMessage, "[Tt]ime.*unique")
  expect_match(result$results$errorMessage, "continuous time")
  expect_null(result$results$coefTable)
  expect_null(result$results$autoCorTable)
})
