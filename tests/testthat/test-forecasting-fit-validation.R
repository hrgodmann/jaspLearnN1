.foreFitFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.foreFitOptions <- function(p = 0L, q = 0L) {
  options <- jaspTools::analysisOptions("Forecasting")
  options$enableIntroText <- FALSE
  options$inputType <- "loadData"
  options$dependent <- "symptom"
  options$time <- "clock"
  options$covariates <- character()
  options$modelSpecification <- "custom"
  options$p <- p; options$d <- 0L; options$q <- q
  options$plotData <- options$forecastTimeSeries <- FALSE
  options$forecastTable <- TRUE
  options$forecastLength <- 2L
  options$forecastSave <- ""
  options$forecastExportSession <- "fit-validation"
  options$forecastExportRequest <- FALSE
  options
}

.foreFitData <- function(seed = 13L, n = 10L, ar = .5, ma = NULL) {
  set.seed(seed)
  data.frame(symptom = as.numeric(stats::arima.sim(list(ar = ar, ma = ma), n = n)),
             clock = seq_len(n))
}

test_that("overparameterized ARIMA models cannot display or export invalid intervals", {
  data <- .foreFitData()
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  for (p in c(4L, 5L)) {
    options <- .foreFitOptions(p = p, q = 5L)
    prepared <- .foreFitFunction(".ln1ForePrepareData")(data, options)
    # These numerical fixtures formerly returned Inf or a negative variance.
    reference <- forecast::Arima(data$symptom, order = c(p, 0L, 5L), include.constant = TRUE)
    expect_lte(reference$nobs, sum(reference$mask))
    expect_error(.foreFitFunction(".ln1ForeEstimateModelHelper")(prepared, options),
                 "did not converge|too few observations.*residual variance")
    options$forecastSave <- path
    options$forecastExportRequest <- TRUE
    writeLines("keep valid export", path)
    result <- jaspTools::runAnalysis("Forecasting", data, options, view = FALSE)
    expect_identical(result$status, "validationError")
    expect_match(result$results$errorMessage, "did not converge|too few observations.*residual variance")
    expect_null(result$results$forecastTable$data)
    expect_identical(readLines(path), "keep valid export")
  }
})

test_that("the numerical convergence fixture follows the optimizer diagnostics", {
  data <- .foreFitData(seed = 17L, n = 12L, ar = .8, ma = -.7)
  options <- .foreFitOptions(p = 3L, q = 3L)
  reference <- forecast::Arima(data$symptom, order = c(3L, 0L, 3L), include.constant = TRUE)
  expect_gt(reference$nobs - sum(reference$mask), 0)
  expect_true(is.finite(reference$sigma2) && reference$sigma2 > 0)
  prepared <- .foreFitFunction(".ln1ForePrepareData")(data, options)
  # This fixture reached the iteration limit in the audit environment. Other
  # supported optimizers/platforms may converge it; both outcomes are checked.
  if (reference$code != 0L) {
    expect_error(.foreFitFunction(".ln1ForeEstimateModelHelper")(prepared, options), "did not converge")
  } else {
    fit <- .foreFitFunction(".ln1ForeEstimateModelHelper")(prepared, options)
    expect_equal(fit$coef, reference$coef)
    actual <- .foreFitFunction(".ln1ForeForecastHelper")(prepared, fit, options)
    expected <- forecast::forecast(reference, h = 2L, level = c(80, 95))
    expect_true(all(is.finite(as.matrix(actual))))
    expect_equal(actual$y, as.numeric(expected$mean))
    expect_equal(actual$lower95, as.numeric(expected$lower[, "95%"]))
  }
})

test_that("nonconvergence diagnostics block inference and export independently of optimizer behavior", {
  data <- .foreFitData(seed = 71L, n = 80L)
  options <- .foreFitOptions(p = 1L)
  failed <- forecast::Arima(data$symptom, order = c(1L, 0L, 0L), include.constant = TRUE)
  expect_identical(failed$code, 0L)
  for (variance in c(-1, Inf, NaN)) {
    invalidVariance <- failed
    invalidVariance$sigma2 <- variance
    expect_error(.foreFitFunction(".ln1ForeValidateFit")(invalidVariance), "invalid residual variance")
  }
  failed$code <- 1L
  expect_error(.foreFitFunction(".ln1ForeValidateFit")(failed), "did not converge")
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  options$forecastSave <- path
  options$forecastExportRequest <- TRUE
  result <- testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Forecasting", data, options, view = FALSE)
  }, Arima = function(...) failed, .package = "forecast")
  expect_identical(result$status, "validationError")
  expect_match(result$results$errorMessage, "did not converge")
  expect_null(result$results$coefTable$data)
  expect_false(file.exists(path))
})

test_that("converged fits and supported zero-variance forecasts keep reference values", {
  data <- .foreFitData(seed = 71L, n = 80L)
  options <- .foreFitOptions(p = 1L)
  prepared <- .foreFitFunction(".ln1ForePrepareData")(data, options)
  fit <- .foreFitFunction(".ln1ForeEstimateModelHelper")(prepared, options)
  reference <- forecast::Arima(data$symptom, order = c(1L, 0L, 0L), include.constant = TRUE)
  expect_equal(fit$coef, reference$coef)
  prediction <- .foreFitFunction(".ln1ForeForecastHelper")(prepared, fit, options)
  expected <- forecast::forecast(reference, h = 2L, level = c(80, 95))
  expect_equal(prediction$y, as.numeric(expected$mean))
  expect_equal(prediction$lower95, as.numeric(expected$lower[, "95%"]))

  for (automatic in c(TRUE, FALSE)) {
    options$modelSpecification <- if (automatic) "auto" else "custom"
    options$p <- options$q <- 0L; options$d <- 2L
    prepared <- data.frame(y = rep(0, 100L), t = seq_len(100L))
    fit <- .foreFitFunction(".ln1ForeEstimateModelHelper")(prepared, options)
    expect_identical(fit$sigma2, 0)
    expect_true(all(!fit$mask))
    prediction <- .foreFitFunction(".ln1ForeForecastHelper")(prepared, fit, options)
    expect_equal(unname(as.matrix(prediction[, -1L])), matrix(0, 2L, 5L))
  }
})

test_that("invalid forecast values preserve coefficients and cannot overwrite a CSV", {
  set.seed(941)
  x <- stats::rnorm(60L)
  data <- data.frame(symptom = c(3 * x + stats::rnorm(60L, sd = .1), NA_real_),
                     clock = seq_len(61L), stress = c(x, 1e308))
  options <- .foreFitOptions()
  options$covariates <- "stress"
  options$forecastLength <- 1L
  prepared <- .foreFitFunction(".ln1ForePrepareData")(data, options)
  fit <- .foreFitFunction(".ln1ForeEstimateModelHelper")(prepared, options)
  expect_error(suppressWarnings(.foreFitFunction(".ln1ForeForecastHelper")(prepared, fit, options)),
                 "could not be computed as finite values")
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines("keep valid export", path)
  options$forecastSave <- path
  options$forecastExportRequest <- TRUE
  result <- suppressWarnings(jaspTools::runAnalysis("Forecasting", data, options, view = FALSE))
  expect_identical(result$status, "complete")
  expect_identical(result$results$coefTable$status, "complete")
  expect_gt(length(result$results$coefTable$data), 0L)
  expect_match(paste(unlist(result$results$forecastTable), collapse = " "),
               "could not be computed as finite values")
  expect_match(result$results$forecastExport$rawtext, "could not be saved")
  expect_identical(readLines(path), "keep valid export")
})

test_that("forecast result shapes cannot silently recycle values across requested times", {
  data <- .foreFitData(seed = 71L, n = 80L)
  options <- .foreFitOptions()
  prepared <- .foreFitFunction(".ln1ForePrepareData")(data, options)
  fit <- .foreFitFunction(".ln1ForeEstimateModelHelper")(prepared, options)
  complete <- forecast::forecast(fit, h = 2L, level = c(80, 95))
  for (field in c("mean", "lower", "upper")) {
    incomplete <- complete
    incomplete[[field]] <- if (field == "mean") complete$mean[1L] else
      complete[[field]][1L, , drop = FALSE]
    testthat::with_mocked_bindings({
      expect_error(.foreFitFunction(".ln1ForeForecastHelper")(prepared, fit, options),
                   "could not be computed as finite values")
    }, forecast = function(...) incomplete, .package = "forecast")
  }
})

test_that("a fit-validation failure consumes its export click until an explicit retry", {
  data <- .foreFitData()
  options <- .foreFitOptions()
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  options$forecastSave <- path
  run <- .foreFitFunction("Forecasting")
  save <- .foreFitFunction(".ln1ForeSaveForecast")
  load <- getFromNamespace("loadJaspResults", "jaspBase")
  native <- NULL
  entered <- FALSE
  testthat::local_mocked_bindings(loadJaspResults = function(name) {
    native <<- load(name)
    native
  }, .package = "jaspBase")
  result <- testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Forecasting", data, options, view = FALSE)
  }, .ln1ForeSaveForecast = function(jaspResults, dataset, options, result, ready, clicked) {
    save(jaspResults, dataset, options, result, ready, clicked)
    if (entered) return(invisible(NULL))
    entered <<- TRUE
    change <- function() native$changeOptions(jsonlite::toJSON(options))
    options$p <- options$q <- 5L
    options$forecastExportRequest <- TRUE
    change()
    expect_error(run(jaspResults, data, options), "did not converge|too few observations")
    expect_true(jaspResults[["forecastExportState"]]$object$observedRequest)
    expect_false(file.exists(path))
    options$p <- options$q <- 0L
    change()
    run(jaspResults, data, options)
    expect_false(file.exists(path))
    expect_identical(jaspResults[["forecastExportState"]]$object$status, "pending")
    options$forecastExportRequest <- FALSE
    change()
    run(jaspResults, data, options)
    expect_true(file.exists(path))
    expect_identical(jaspResults[["forecastExportState"]]$object$status, "success")
  }, .package = "jaspLearnN1")
  expect_true(entered)
  expect_identical(result$status, "complete")
})
