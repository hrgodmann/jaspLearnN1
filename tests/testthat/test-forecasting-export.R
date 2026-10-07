.foreExportFunction <- function(name) getFromNamespace(name, "jaspLearnN1")
.foreExportOptions <- function(path = "") {
  options <- jaspTools::analysisOptions("Forecasting")
  options$enableIntroText <- FALSE
  options$inputType <- "simulateData"
  options$modelSpecification <- "custom"
  options$p <- 0L; options$d <- 1L; options$q <- 0L
  options$plotData <- FALSE
  options$forecastTimeSeries <- FALSE
  options$forecastTable <- TRUE
  options$forecastLength <- 3L
  options$forecastSave <- path
  options$forecastExportSession <- "native-forecast-export"
  options$forecastExportRequest <- FALSE
  options
}

.foreExportStep <- function(results, data, options, prediction, ready = TRUE) {
  clicked <- .foreExportFunction(".ln1ForeBeginExport")(results, options)
  .foreExportFunction(".ln1ForeSaveForecast")(results, data, options, prediction, ready, clicked)
  results[["forecastExportState"]]$object
}

.foreExportRun <- function(options, callback = NULL) {
  original <- .foreExportFunction(".ln1ForeSaveForecast")
  recorded <- new.env(parent = emptyenv())
  result <- testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Forecasting", NULL, options, view = FALSE)
  }, .ln1ForeSaveForecast = function(jaspResults, dataset, options, result, ready, clicked) {
    original(jaspResults, dataset, options, result, ready, clicked)
    recorded$initial <- jaspResults[["forecastExportState"]]$object
    if (!is.null(callback)) testthat::with_mocked_bindings({
      callback(jaspResults, dataset, options, result, recorded)
    }, .ln1ForeSaveForecast = original, .package = "jaspLearnN1")
    recorded$final <- jaspResults[["forecastExportState"]]$object
  }, .package = "jaspLearnN1")
  list(result = result, recorded = as.list(recorded))
}

test_that("selecting or restoring a forecast destination never writes without a click", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines("user file", path)
  options <- .foreExportOptions(path)
  output <- .foreExportRun(options)
  expect_identical(output$result$status, "complete")
  expect_identical(readLines(path), "user file")
  expect_identical(output$recorded$initial$status, "pending")
  for (session in list(NULL, "")) {
    for (request in c(FALSE, TRUE)) {
      options$forecastExportSession <- session
      options$forecastExportRequest <- request
      output <- .foreExportRun(options)
      expect_identical(output$result$status, "complete")
      expect_identical(readLines(path), "user file")
    }
  }
})

test_that("both explicit forecast button transitions save and cosmetic reruns do not", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  options <- .foreExportOptions(path)
  output <- .foreExportRun(options, function(results, data, opts, prediction, recorded) {
    recorded$unwritten <- !file.exists(path)
    opts$forecastExportRequest <- TRUE
    recorded$first <- .foreExportStep(results, data, opts, prediction)
    recorded$csv <- utils::read.csv(path, check.names = FALSE)
    writeLines("external edits", path)
    opts$plotLine <- !isTRUE(opts$plotLine)
    opts$coefficientCiLevel <- .8
    opts$modelSpecificationAutoIc <- "bic" # Inactive in manual mode.
    .foreExportStep(results, data, opts, prediction)
    recorded$preserved <- identical(readLines(path), "external edits")
    opts$forecastExportRequest <- FALSE
    recorded$second <- .foreExportStep(results, data, opts, prediction)
  })
  expect_true(output$recorded$unwritten)
  expect_identical(output$recorded$first$status, "success")
  expect_identical(output$recorded$second$status, "success")
  expect_true(output$recorded$preserved)
  expect_equal(nrow(output$recorded$csv), 3L)
  expect_equal(utils::read.csv(path, check.names = FALSE), output$recorded$csv)
})

test_that("a first forecast click works before a baseline run and fresh sessions do not replay it", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  options <- .foreExportOptions(path)
  options$forecastExportRequest <- TRUE
  output <- .foreExportRun(options, function(results, data, opts, prediction, recorded) {
    recorded$firstWritten <- file.exists(path)
    for (oldToken in c(FALSE, TRUE)) {
      previous <- results[["forecastExportState"]]$object
      previous$session <- "old-form"
      previous$observedRequest <- oldToken
      results[["forecastExportState"]] <- jaspBase::createJaspState(previous)
      writeLines("preserve when reopening", path)
      # Actual QML initialization resets either saved checkbox value FALSE.
      opts$forecastExportSession <- "reopened-form"
      opts$forecastExportRequest <- FALSE
      .foreExportStep(results, data, opts, prediction)
      expect_identical(readLines(path), "preserve when reopening")
      opts$forecastExportRequest <- TRUE
      expect_identical(.foreExportStep(results, data, opts, prediction)$status, "success")
    }
  })
  expect_identical(output$result$status, "complete")
  expect_true(output$recorded$firstWritten)
  expect_identical(output$recorded$initial$status, "success")
})

test_that("forecast data, horizon and path changes stay pending until another click", {
  path <- tempfile(fileext = ".csv")
  nextPath <- tempfile(fileext = ".csv")
  on.exit(unlink(c(path, nextPath)), add = TRUE)
  options <- .foreExportOptions(path)
  options$forecastExportRequest <- TRUE
  output <- .foreExportRun(options, function(results, data, opts, prediction, recorded) {
    writeLines("keep previous export", path)
    changed <- data; changed$y[1] <- changed$y[1] + 1
    expect_identical(.foreExportStep(results, changed, opts, prediction)$status, "pending")
    expect_identical(readLines(path), "keep previous export")
    # Undoing the data change remains pending; it is not a file-write request.
    expect_identical(.foreExportStep(results, data, opts, prediction)$status, "pending")
    opts$forecastLength <- 2L
    opts$forecastSave <- nextPath
    .foreExportStep(results, data, opts, prediction)
    recorded$pathPending <- !file.exists(nextPath)
    opts$forecastExportRequest <- FALSE
    nextPrediction <- prediction
    nextPrediction$predictions <- prediction$predictions[1:2, ]
    .foreExportStep(results, data, opts, nextPrediction)
    recorded$rows <- nrow(utils::read.csv(nextPath))
    opts$forecastSave <- ""
    .foreExportStep(results, data, opts, nextPrediction)
    expect_null(results[["forecastExport"]])
    writeLines("keep after path reselection", nextPath)
    opts$forecastSave <- nextPath
    .foreExportStep(results, data, opts, nextPrediction)
    recorded$reselected <- identical(readLines(nextPath), "keep after path reselection")
  })
  expect_true(output$recorded$pathPending)
  expect_true(output$recorded$reselected)
  expect_identical(output$recorded$rows, 2L)
})

test_that("failed and pathless forecast export clicks are consumed and require explicit retry", {
  directory <- tempfile("forecast-export-missing-")
  path <- file.path(directory, "forecast.csv")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  output <- .foreExportRun(.foreExportOptions(), function(results, data, opts, prediction, recorded) {
    opts$forecastExportRequest <- TRUE
    .foreExportStep(results, data, opts, prediction) # No path: consume this click.
    opts$forecastSave <- path
    expect_identical(.foreExportStep(results, data, opts, prediction)$status, "pending")
    opts$forecastExportRequest <- FALSE
    recorded$failed <- .foreExportStep(results, data, opts, prediction)
    dir.create(directory)
    .foreExportStep(results, data, opts, prediction)
    recorded$notRetried <- !file.exists(path)
    opts$forecastExportRequest <- TRUE
    recorded$retried <- .foreExportStep(results, data, opts, prediction)
  })
  expect_identical(output$result$status, "complete")
  expect_identical(output$recorded$failed$status, "failed")
  expect_true(output$recorded$notRetried)
  expect_identical(output$recorded$retried$status, "success")
})

test_that("forecast requests blocked before output creation cannot replay after repair", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  output <- .foreExportRun(.foreExportOptions(path), function(results, data, opts, prediction, recorded) {
    opts$forecastExportRequest <- TRUE
    # This is the entrypoint's exact order: consume the event, then validate.
    expect_true(.foreExportFunction(".ln1ForeBeginExport")(results, opts))
    invalid <- opts; invalid$noiseSd <- -1
    expect_error(.foreExportFunction(".ln1ForeSimulateData")(invalid), "standard deviation")
    expect_identical(.foreExportStep(results, data, opts, prediction)$status, "pending")
    recorded$notReplayed <- !file.exists(path)
    # A forecast-specific failure also consumes the event while keeping tables.
    opts$forecastExportRequest <- FALSE
    failedPrediction <- list(predictions = NULL, error = "Supply future covariates <required>")
    expect_identical(.foreExportStep(results, data, opts, failedPrediction)$status, "failed")
    expect_match(results[["forecastExport"]]$text, "&lt;required&gt;", fixed = TRUE)
    .foreExportStep(results, data, opts, prediction)
    recorded$noAutomaticRepair <- !file.exists(path)
    opts$forecastExportRequest <- TRUE
    .foreExportStep(results, data, opts, prediction)
  })
  expect_true(output$recorded$notReplayed)
  expect_true(output$recorded$noAutomaticRepair)
  expect_identical(output$recorded$final$status, "success")
  expect_identical(output$result$results$coefTable$status, "complete")
})

test_that("forecast CSV replacement failures retain old bytes and clean temporary files", {
  directory <- tempfile("forecast-rename-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "forecast.csv")
  writeLines("existing complete file", path)
  testthat::with_mocked_bindings({
    expect_error(.foreExportFunction(".ln1ForeWriteCsv")(data.frame(y = 1:3), path), "could not replace")
  }, file.rename = function(from, to) FALSE, .package = "base")
  expect_identical(readLines(path), "existing complete file")
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "forecast.csv")
})

test_that("forecast CSV headers distinguish outcome names from time and interval bounds", {
  set.seed(918)
  data <- data.frame(symptom = stats::rnorm(40L), clock = seq_len(40L))
  reserved <- c("t", "lower80", "upper80", "lower95", "upper95")
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  for (outcome in c(reserved, "symptom", "t_forecast", "daily symptom", "sympt\u00f4me")) {
    options <- .foreExportOptions(path)
    options$inputType <- "loadData"
    options$dependent <- outcome
    options$time <- "clock"
    options$covariates <- character()
    options$p <- options$d <- options$q <- 0L
    options$forecastExportRequest <- TRUE
    renamed <- data
    names(renamed)[1L] <- outcome
    result <- jaspTools::runAnalysis("Forecasting", renamed, options, view = FALSE)
    expect_identical(result$status, "complete")
    expect_match(result$results$forecastExport$rawtext, "Forecasts saved")
    exported <- utils::read.csv(path, check.names = FALSE)
    outcomeHeader <- if (outcome %in% reserved) paste0(outcome, "_forecast") else outcome
    expect_identical(names(exported), c("t", outcomeHeader, reserved[-1L]))
    expect_identical(anyDuplicated(names(exported)), 0L)
    rows <- result$results$forecastTable$data
    expect_equal(exported[[outcomeHeader]], vapply(rows, `[[`, numeric(1), "y"))
    for (bound in reserved[-1L])
      expect_equal(exported[[bound]], vapply(rows, `[[`, numeric(1), bound))
  }
})

test_that("an earlier CSV format becomes pending without replaying the saved click", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  options <- .foreExportOptions(path)
  options$forecastExportRequest <- TRUE
  output <- .foreExportRun(options, function(results, data, opts, prediction, recorded) {
    previous <- results[["forecastExportState"]]$object
    previous$request$formatVersion <- NULL
    results[["forecastExportState"]] <- jaspBase::createJaspState(previous)
    writeLines("keep previous CSV format", path)
    expect_identical(.foreExportStep(results, data, opts, prediction)$status, "pending")
    expect_identical(readLines(path), "keep previous CSV format")
    opts$forecastExportRequest <- FALSE
    expect_identical(.foreExportStep(results, data, opts, prediction)$status, "success")
  })
  expect_identical(output$result$status, "complete")
})
