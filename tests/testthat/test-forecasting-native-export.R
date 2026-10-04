test_that("native forecast invalidation refreshes predictions without replaying CSV exports", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  options <- jaspTools::analysisOptions("Forecasting")
  options$enableIntroText <- FALSE
  options$inputType <- "simulateData"
  options$seed <- 713L
  options$numSamples <- 80L
  options$noiseSd <- 1
  options$simArEffects <- list(list(simArEffect = .7))
  options$simMaEffects <- list()
  options$simIEffect <- 0L
  options$modelSpecification <- "custom"
  options$p <- options$d <- options$q <- 0L
  options$plotData <- FALSE
  options$forecastTimeSeries <- TRUE
  options$forecastTimeSeriesObserved <- TRUE
  options$forecastTimeSeriesType <- "both"
  options$forecastTable <- TRUE
  options$forecastLength <- 3L
  options$forecastSave <- path
  options$forecastExportSession <- "native-invalidation-form"
  options$forecastExportRequest <- FALSE

  module <- function(name) getFromNamespace(name, "jaspLearnN1")
  run <- module("Forecasting")
  save <- module(".ln1ForeSaveForecast")
  estimate <- module(".ln1ForeEstimateModelHelper")
  predict <- module(".ln1ForeForecastHelper")
  plot <- module(".ln1ForeForecastPlotFill")
  write <- module(".ln1ForeWriteCsv")
  load <- getFromNamespace("loadJaspResults", "jaspBase")
  native <- NULL
  recorded <- new.env(parent = emptyenv())
  recorded$entered <- FALSE
  recorded$fits <- recorded$predictions <- recorded$plots <- recorded$writes <- 0L
  # Capture the initialized native context. changeOptions performs the actual
  # dependency invalidation used by JASP; this is not a Desktop lifecycle test.
  testthat::local_mocked_bindings(loadJaspResults = function(name) {
    native <<- load(name)
    native
  }, .package = "jaspBase")
  result <- testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Forecasting", NULL, options, view = FALSE)
  }, .ln1ForeEstimateModelHelper = function(...) {
    recorded$fits <- recorded$fits + 1L
    estimate(...)
  }, .ln1ForeForecastHelper = function(...) {
    recorded$predictions <- recorded$predictions + 1L
    predict(...)
  }, .ln1ForeForecastPlotFill = function(...) {
    recorded$plots <- recorded$plots + 1L
    plot(...)
  }, .ln1ForeWriteCsv = function(data, path) {
    recorded$writes <- recorded$writes + 1L
    write(data, path)
  }, .ln1ForeSaveForecast = function(jaspResults, dataset, options, result, ready, clicked) {
    save(jaspResults, dataset, options, result, ready, clicked)
    if (recorded$entered) return(invisible(NULL))
    recorded$entered <- TRUE
    counts <- function() c(recorded$fits, recorded$predictions, recorded$plots)
    change <- function() {
      # Keep runAnalysis's scalar/array encoding so unchanged options do not
      # spuriously invalidate native dependencies.
      native$changeOptions(jsonlite::toJSON(options))
    }
    journal <- function() jaspResults[["forecastExportState"]]$object
    recorded$initialWrites <- recorded$writes
    recorded$initialAbsent <- !file.exists(path)
    recorded$initialCounts <- counts()

    options$forecastExportRequest <- TRUE
    change()
    run(jaspResults, NULL, options)
    recorded$first <- journal()
    recorded$firstCsv <- utils::read.csv(path, check.names = FALSE)
    recorded$firstCounts <- counts()
    writeLines("external edits must survive recalculation", path)

    options$forecastTimeSeriesObserved <- FALSE
    change()
    recorded$appearanceRetainsModel <- !is.null(jaspResults[["modelState"]])
    recorded$appearanceRetainsPredictions <- !is.null(jaspResults[["forecastResult"]])
    recorded$appearanceClearsPlot <- is.null(jaspResults[["forecastPlot"]])
    recorded$appearanceRetainsEvent <- isTRUE(journal()$observedRequest)
    run(jaspResults, NULL, options)
    recorded$appearanceCounts <- counts()
    recorded$appearanceStatus <- journal()$status

    options$forecastLength <- 5L
    change()
    recorded$horizonRetainsModel <- !is.null(jaspResults[["modelState"]])
    recorded$horizonClearsPredictions <- is.null(jaspResults[["forecastResult"]])
    recorded$horizonRetainsEvent <- isTRUE(journal()$observedRequest)
    run(jaspResults, NULL, options)
    recorded$horizonCounts <- counts()
    recorded$horizonStatus <- journal()$status

    options$p <- 1L
    change()
    recorded$modelClears <- vapply(c("modelState", "forecastResult", "forecastPlot", "forecastTable"),
      function(key) is.null(jaspResults[[key]]), logical(1))
    recorded$modelRetainsEvent <- isTRUE(journal()$observedRequest)
    run(jaspResults, NULL, options)
    recorded$modelCounts <- counts()
    recorded$modelStatus <- journal()$status
    recorded$modelOrder <- jaspResults[["modelState"]]$object$arma[c(1L, 6L, 2L)]
    recorded$updatedPredictions <- jaspResults[["forecastResult"]]$object$predictions
    recorded$beforeSecondWrites <- recorded$writes
    recorded$preserved <- identical(readLines(path), "external edits must survive recalculation")

    # FALSE is the next real button transition, not an unchecked no-op.
    options$forecastExportRequest <- FALSE
    change()
    run(jaspResults, NULL, options)
    recorded$second <- journal()
    recorded$secondCounts <- counts()
    recorded$secondCsv <- utils::read.csv(path, check.names = FALSE)
    recorded$secondWrites <- recorded$writes

    # Leave a TRUE token in the old session, then model the actual form reset:
    # a fresh session and FALSE must not replay that saved export request.
    options$forecastExportRequest <- TRUE
    change()
    run(jaspResults, NULL, options)
    recorded$beforeRestoreWrites <- recorded$writes
    writeLines("preserve while reopening", path)
    options$forecastExportSession <- "reopened-invalidation-form"
    options$forecastExportRequest <- FALSE
    change()
    recorded$restoreRetainsJournal <- !is.null(jaspResults[["forecastExportState"]])
    run(jaspResults, NULL, options)
    recorded$reopened <- journal()
    recorded$reopenedPreserved <- identical(readLines(path), "preserve while reopening")
    recorded$finalCounts <- counts()
  }, .package = "jaspLearnN1")

  expect_identical(result$status, "complete")
  expect_identical(recorded$initialWrites, 0L)
  expect_true(recorded$initialAbsent)
  expect_identical(recorded$initialCounts, c(1L, 1L, 1L))
  expect_identical(recorded$first$status, "success")
  expect_equal(nrow(recorded$firstCsv), 3L)
  expect_identical(recorded$firstCounts, recorded$initialCounts)
  expect_true(recorded$appearanceRetainsModel)
  expect_true(recorded$appearanceRetainsPredictions)
  expect_true(recorded$appearanceClearsPlot)
  expect_true(recorded$appearanceRetainsEvent)
  expect_identical(recorded$appearanceCounts, c(1L, 1L, 2L))
  expect_identical(recorded$appearanceStatus, "success")
  expect_true(recorded$horizonRetainsModel)
  expect_true(recorded$horizonClearsPredictions)
  expect_true(recorded$horizonRetainsEvent)
  expect_identical(recorded$horizonCounts, c(1L, 2L, 3L))
  expect_identical(recorded$horizonStatus, "pending")
  expect_true(all(recorded$modelClears))
  expect_true(recorded$modelRetainsEvent)
  expect_identical(recorded$modelCounts, c(2L, 3L, 4L))
  expect_identical(recorded$modelStatus, "pending")
  expect_equal(recorded$modelOrder, c(1, 0, 0))
  expect_identical(recorded$beforeSecondWrites, 1L)
  expect_true(recorded$preserved)
  expect_identical(recorded$second$status, "success")
  expect_identical(recorded$secondCounts, recorded$modelCounts)
  expect_identical(recorded$secondWrites, 2L)
  expectedCsv <- recorded$updatedPredictions
  names(expectedCsv)[names(expectedCsv) == "y"] <- "Outcome"
  expect_equal(recorded$secondCsv, expectedCsv, ignore_attr = TRUE)
  expect_equal(nrow(recorded$secondCsv), 5L)
  expect_gt(max(abs(recorded$firstCsv$Outcome - recorded$secondCsv$Outcome[1:3])), 1e-4)
  expect_identical(recorded$beforeRestoreWrites, 3L)
  expect_true(recorded$restoreRetainsJournal)
  expect_true(recorded$reopenedPreserved)
  expect_identical(recorded$reopened$session, "reopened-invalidation-form")
  expect_false(recorded$reopened$observedRequest)
  expect_identical(recorded$writes, recorded$beforeRestoreWrites)
  expect_identical(recorded$finalCounts, recorded$modelCounts)
})
