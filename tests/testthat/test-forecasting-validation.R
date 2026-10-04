.foreValidationFunction <- function(name) getFromNamespace(name, "jaspLearnN1")
.foreSimulationOptions <- function() list(seed = 1L, noiseSd = 1, numSamples = 100L,
  simArEffects = list(list(simArEffect = .2)), simMaEffects = list(list(simMaEffect = .8)), simIEffect = 1L)

test_that("forecast simulation returns N observations and preserves the seeded d=1 sequence", {
  simulate <- .foreValidationFunction(".ln1ForeSimulateData")
  for (d in 0:2) {
    options <- .foreSimulationOptions()
    options$simIEffect <- d
    actual <- simulate(options)
    set.seed(options$seed)
    reference <- as.numeric(stats::arima.sim(list(ar = .2, ma = .8, order = c(1, d, 1)),
                                             n = options$numSamples, sd = options$noiseSd))
    if (d > 0) reference <- reference[-seq_len(d)]
    expect_identical(nrow(actual), 100L)
    expect_identical(actual$t, seq_len(100L))
    expect_identical(actual$y, reference)
    expect_identical(simulate(options), actual)
  }
  options$numSamples <- 2L
  for (d in 0:2) {
    options$simIEffect <- d
    expect_identical(nrow(simulate(options)), 2L)
  }
})

test_that("simulation input validation handles counts, noise, coefficients and AR stationarity", {
  validate <- .foreValidationFunction(".ln1ForeValidateSimulation")
  simulate <- .foreValidationFunction(".ln1ForeSimulateData")
  for (n in c(0, 1, 2.5, NA, Inf)) {
    options <- .foreSimulationOptions(); options$numSamples <- n
    expect_error(validate(options), "at least two|whole number")
  }
  for (sd in c(-1, Inf, NA)) {
    options <- .foreSimulationOptions(); options$noiseSd <- sd
    expect_error(validate(options), "standard deviation")
  }
  for (d in c(-1, .5, Inf, NA)) {
    options <- .foreSimulationOptions(); options$simIEffect <- d
    expect_error(validate(options), "differencing")
  }
  for (ar in list(c(1), c(-1), c(1.2), c(.9, .9))) {
    options <- .foreSimulationOptions()
    options$simArEffects <- lapply(ar, function(value) list(simArEffect = value))
    expect_error(simulate(options), "stationary")
  }
  for (value in c(Inf, NA)) {
    options <- .foreSimulationOptions(); options$simMaEffects[[1]]$simMaEffect <- value
    expect_error(validate(options), "finite number")
  }
  for (ar in list(c(.5), c(-.5), c(.4, -.2), numeric())) {
    options <- .foreSimulationOptions()
    options$simArEffects <- lapply(ar, function(value) list(simArEffect = value))
    expect_true(all(is.finite(simulate(options)$y)))
  }
  options <- .foreSimulationOptions(); options$noiseSd <- 0
  expect_equal(simulate(options)$y, rep(0, 100L))
  options$seed <- Inf
  expect_error(validate(options), "seed")
})

test_that("forecast horizon resource bounds reject rather than truncate requests", {
  validate <- .foreValidationFunction(".ln1ForeValidateHorizon")
  for (h in c(1, 10000)) expect_silent(validate(h))
  for (h in c(0, -1, 1.5, 10001, Inf, NA))
    expect_error(validate(h), "between 1 and 10000")
})

test_that("forecast labels and caveats describe unchanged coefficient and interval calculations", {
  set.seed(308)
  y <- 8 + as.numeric(stats::arima.sim(list(ar = .3), 80L))
  rows <- .foreValidationFunction(".ln1ForeCoefficientRows")
  note <- .foreValidationFunction(".ln1ForeModelNote")
  options <- list(inputType = "simulateData", coefficientCiLevel = .975, modelSpecification = "custom")
  fit <- forecast::Arima(y, order = c(1, 0, 0), include.constant = TRUE)
  actual <- rows(fit, options)
  expect_identical(actual$coefficients[1L], "Mean")
  expect_match(note(fit, options), "process mean")
  df <- fit$nobs - length(fit$coef)
  expectedOrder <- c(which(names(fit$coef) == "intercept"), which(names(fit$coef) != "intercept"))
  expectedSe <- unname(sqrt(diag(fit$var.coef))[expectedOrder])
  expected <- unname(fit$coef[expectedOrder])
  expect_equal(actual$SE, expectedSe)
  expect_equal(actual$p, 2 * (1 - stats::pt(abs(expected / expectedSe), df)))
  expect_equal(actual$lower, expected - stats::qt(.9875, df) * expectedSe)
  drift <- forecast::Arima(y, order = c(0, 1, 0), include.constant = TRUE)
  expect_match(note(drift, options), "drift term")
  predictor <- matrix(rnorm(80), ncol = 1, dimnames = list(NULL, "xreg1"))
  regression <- forecast::Arima(y, order = c(0, 0, 0), xreg = predictor, include.constant = TRUE)
  options$inputType <- "loadData"; options$covariates <- "stress"
  expect_identical(rows(regression, options)$coefficients[1L], "Intercept")
  expect_match(note(regression, options), "regression intercept")
  options$modelSpecification <- "auto"; options$modelSpecificationAutoIc <- "aicc"
  expect_match(note(regression, options), "AICc")
  predictionNote <- .foreValidationFunction(".ln1ForePredictionNote")(options)
  expect_match(predictionNote, "parameter estimates and model selection")
  expect_match(predictionNote, "future covariate")
})
