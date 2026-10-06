.foreConstantOptions <- function() {
  options <- jaspTools::analysisOptions("Forecasting")
  options$enableIntroText <- FALSE
  options$inputType <- "simulateData"
  options$noiseSd <- 0
  options$numSamples <- 100L
  options$modelSpecification <- "custom"
  options$p <- 1L; options$d <- options$q <- 0L
  options$plotData <- options$forecastTimeSeries <- FALSE
  options$forecastTable <- TRUE
  options$forecastLength <- 3L
  options$forecastSave <- ""
  options$forecastExportSession <- "constant-series-test"
  options$forecastExportRequest <- FALSE
  options
}

test_that("failed constant-series fits explain the problem without rejecting zero noise", {
  options <- .foreConstantOptions()
  simulate <- getFromNamespace(".ln1ForeSimulateData", "jaspLearnN1")
  estimate <- getFromNamespace(".ln1ForeEstimateModelHelper", "jaspLearnN1")
  data <- simulate(options)
  expect_identical(data$y, rep(0, 100L))
  for (order in list(c(1L, 0L, 0L), c(0L, 0L, 0L), c(0L, 1L, 0L), c(0L, 0L, 1L))) {
    options$p <- order[1L]; options$d <- order[2L]; options$q <- order[3L]
    expect_error(estimate(data, options), "constant series.*positive noise standard deviation")
  }
  options <- .foreConstantOptions()
  result <- jaspTools::runAnalysis("Forecasting", NULL, options, view = FALSE)
  expect_identical(result$status, "validationError")
  expect_match(result$results$errorMessage, "constant series.*positive noise standard deviation")
  expect_false(grepl("optim", result$results$errorMessage, fixed = TRUE))
})

test_that("successful automatic and coefficient-free constant fits remain unchanged", {
  for (automatic in c(TRUE, FALSE)) {
    options <- .foreConstantOptions()
    options$modelSpecification <- if (automatic) "auto" else "custom"
    options$p <- options$q <- 0L; options$d <- 2L
    result <- jaspTools::runAnalysis("Forecasting", NULL, options, view = FALSE)
    expect_identical(result$status, "complete")
    forecasts <- result$results$forecastTable$data
    expect_length(forecasts, 3L)
    for (column in c("y", "lower80", "upper80", "lower95", "upper95"))
      expect_identical(vapply(forecasts, `[[`, numeric(1), column), rep(0, 3L))
  }
})

test_that("a click blocked by constant-series fitting is not replayed after model repair", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  options <- .foreConstantOptions()
  options$modelSpecification <- "auto"
  options$forecastSave <- path
  module <- function(name) getFromNamespace(name, "jaspLearnN1")
  run <- module("Forecasting")
  save <- module(".ln1ForeSaveForecast")
  load <- getFromNamespace("loadJaspResults", "jaspBase")
  native <- NULL
  entered <- FALSE
  testthat::local_mocked_bindings(loadJaspResults = function(name) {
    native <<- load(name)
    native
  }, .package = "jaspBase")
  result <- testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Forecasting", NULL, options, view = FALSE)
  }, .ln1ForeSaveForecast = function(jaspResults, dataset, options, result, ready, clicked) {
    save(jaspResults, dataset, options, result, ready, clicked)
    if (entered) return(invisible(NULL))
    entered <<- TRUE
    change <- function() native$changeOptions(jsonlite::toJSON(options))
    expect_false(file.exists(path))
    options$modelSpecification <- "custom"
    options$forecastExportRequest <- TRUE
    change()
    expect_error(run(jaspResults, NULL, options), "constant series")
    expect_true(jaspResults[["forecastExportState"]]$object$observedRequest)
    expect_false(file.exists(path))
    options$modelSpecification <- "auto"
    change()
    run(jaspResults, NULL, options)
    expect_false(file.exists(path))
    expect_identical(jaspResults[["forecastExportState"]]$object$status, "pending")
    options$forecastExportRequest <- FALSE
    change()
    run(jaspResults, NULL, options)
    expect_true(file.exists(path))
    expect_identical(jaspResults[["forecastExportState"]]$object$status, "success")
  }, .package = "jaspLearnN1")
  expect_identical(result$status, "complete")
  expect_true(entered)
})
