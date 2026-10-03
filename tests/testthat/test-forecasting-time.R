# Timing tolerances must be relative to the sampling interval, not to an
# arbitrary unit of 1. These tests cover both history and requested future rows.
.foreTimeFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.foreTimeOptions <- function() {
  options <- jaspTools::analysisOptions("Forecasting")
  options$enableIntroText <- FALSE
  options$inputType <- "loadData"
  options$dependent <- "symptom"
  options$time <- "clock"
  options$covariates <- "stress"
  options$modelSpecification <- "custom"
  options$p <- 0L
  options$d <- 0L
  options$q <- 0L
  options$plotData <- FALSE
  options$forecastLength <- 2L
  options$forecastTable <- FALSE
  options$forecastTimeSeries <- FALSE
  options$forecastSave <- ""
  options
}

.foreTimeFixture <- function(step = 1e-9, origin = 0) {
  set.seed(8704)
  stress <- rnorm(63L)
  data.frame(symptom = c(3 + 2 * stress[1:60] + rnorm(60L, sd = .2), rep(NA_real_, 3L)),
             clock = origin + step * seq_len(63L), stress = stress)
}

.foreTimePrepare <- function(data, options) {
  .foreTimeFunction(".ln1ForePrepareData")(data, options)
}

.foreTimeFit <- function(data, options) {
  .foreTimeFunction(".ln1ForeEstimateModelHelper")(data, options)
}

.foreTimePredict <- function(data, fit, options) {
  .foreTimeFunction(".ln1ForeForecastHelper")(data, fit, options)
}

test_that("tiny time units cannot conceal irregular historical sampling", {
  step <- .foreTimeFunction(".ln1ForeTimeStep")
  expect_error(step(data.frame(t = c(0, 1e-9, 4e-9, 1e-8))), "equally spaced")
  for (scale in c(1e-12, .1, 7)) {
    regular <- data.frame(t = scale * seq_len(60L))
    expect_lt(abs(step(regular) / scale - 1), 1e-12)
    regular$t[30L] <- regular$t[30L] + .25 * scale
    expect_error(step(regular), "equally spaced")
  }
  for (invalid in list(c(1, 2, Inf), c(1, NA_real_, 3), c(1, 1, 2)))
    expect_error(step(data.frame(t = invalid)), "finite|unique")
})

test_that("tiny future time units preserve the intended forecast horizon", {
  options <- .foreTimeOptions()
  data <- .foreTimeFixture()
  prepared <- .foreTimePrepare(data, options)
  fit <- .foreTimeFit(prepared, options)
  prediction <- .foreTimePredict(prepared, fit, options)
  expect_lt(max(abs(prediction$t - data$clock[61:62])), 1e-20)

  # These were previously accepted because 50 intervals were less than 1e-7.
  invalid <- data
  invalid$clock[61:63] <- invalid$clock[61:63] + 50e-9
  invalidPrepared <- .foreTimePrepare(invalid, options)
  expect_equal(stats::coef(.foreTimeFit(invalidPrepared, options)), stats::coef(fit))
  expect_error(.foreTimePredict(invalidPrepared, fit, options), "requested future time")
  invalid <- data
  invalid$clock[62L] <- invalid$clock[61L]
  expect_error(.foreTimePredict(.foreTimePrepare(invalid, options), fit, options),
               "requested future time")
})

test_that("fractional clocks tolerate representable roundoff at a large origin", {
  options <- .foreTimeOptions()
  smallOrigin <- .foreTimeFixture(step = .1)
  largeOrigin <- .foreTimeFixture(step = .1, origin = 1e9)
  smallPrepared <- .foreTimePrepare(smallOrigin, options)
  largePrepared <- .foreTimePrepare(largeOrigin, options)
  smallFit <- .foreTimeFit(smallPrepared, options)
  largeFit <- .foreTimeFit(largePrepared, options)
  expect_equal(stats::coef(largeFit), stats::coef(smallFit), tolerance = 1e-12)
  smallPrediction <- .foreTimePredict(smallPrepared, smallFit, options)
  largePrediction <- .foreTimePredict(largePrepared, largeFit, options)
  expect_equal(largePrediction$y, smallPrediction$y, tolerance = 1e-12)
  expect_lt(max(abs(largePrediction$t - largeOrigin$clock[61:62])), 2e-6)
  invalid <- largeOrigin
  invalid$clock[62L] <- invalid$clock[62L] + .01
  expect_error(.foreTimePredict(.foreTimePrepare(invalid, options), largeFit, options),
               "requested future time")
})

test_that("forecast clocks reject inadequate precision and overflowing future times", {
  step <- .foreTimeFunction(".ln1ForeTimeStep")
  # At this origin, adding successive unit steps would repeat a timestamp.
  expect_error(step(data.frame(t = c(2^53 - 2, 2^53 - 1))), "precision|relative")
  options <- .foreTimeOptions()
  options$covariates <- character()
  options$forecastLength <- 2000L
  historical <- data.frame(y = seq_len(60L), t = 1e307 + seq_len(60L) * 1e305)
  # Clock validation happens before forecasting, so no fitted object is needed.
  expect_error(.foreTimePredict(historical, NULL, options), "cannot be represented")
})

test_that("bad future clocks affect only requested forecast outputs", {
  options <- .foreTimeOptions()
  data <- .foreTimeFixture()
  reference <- .foreTimePrepare(data, options)
  fit <- .foreTimeFit(reference, options)
  invalid <- data
  invalid$clock[62L] <- invalid$clock[62L] + .4e-9
  invalid$clock[63L] <- NA_real_
  invalid$stress[63L] <- NA_real_
  prepared <- .foreTimePrepare(invalid, options)
  expect_equal(stats::coef(.foreTimeFit(prepared, options)), stats::coef(fit))
  options$forecastLength <- 1L
  expect_equal(.foreTimePredict(prepared, fit, options),
               .foreTimePredict(reference, fit, options), tolerance = 1e-12)

  options$forecastLength <- 2L
  options$forecastTable <- TRUE
  result <- jaspTools::runAnalysis("Forecasting", invalid, options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_gt(length(result$results$coefTable$data), 0L)
  expect_match(paste(unlist(result$results$forecastTable), collapse = " "),
               "requested future time")
  # Shortening the requested horizon leaves the malformed later rows unused.
  options$forecastLength <- 1L
  shorter <- jaspTools::runAnalysis("Forecasting", invalid, options, view = FALSE)
  expect_identical(shorter$status, "complete")
  expect_length(shorter$results$forecastTable$data, 1L)
})
