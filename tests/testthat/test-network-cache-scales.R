.netCacheScaleFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.netCacheScaleOptions <- function(path) {
  options <- jaspTools::analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$networkSavePath <- path
  options$networkExportRequest <- FALSE
  options$networkExportSession <- "native-cache-scale-test"
  options$networkSeverityMaximum <- 1
  options$networkConnectionMaximum <- 1
  options$networkConnectionCounts <- FALSE
  options$problems <- list(
    list(problemName = "A", problemSeverity = .8, problemSeverityRated = TRUE),
    list(problemName = "B", problemSeverity = 0, problemSeverityRated = FALSE),
    list(problemName = "C", problemSeverity = 0, problemSeverityRated = TRUE))
  options$connectionList <- list(list(name = "Assessment", allConnections = FALSE,
    allConnectionStrengths = list(), centrality = TRUE, plotNetwork = FALSE,
    edgeWeightTable = TRUE, connections = list(
      list(connectionFrom = "A", connectionTo = "B", connectionStrength = -.7),
      list(connectionFrom = "B", connectionTo = "C", connectionStrength = 0))))
  options
}

.netCacheScaleRun <- function(options, afterSave) {
  save <- .netCacheScaleFunction(".ln1NetSaveNetwork")
  write <- .netCacheScaleFunction(".ln1NetWriteCsv")
  recorded <- new.env(parent = emptyenv())
  recorded$writes <- 0L
  result <- testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
  }, .ln1NetSaveNetwork = function(jaspResults, options) {
    # Construct and reuse the actual native journal within the initialized run.
    save(jaspResults, options)
    recorded$initialWrites <- recorded$writes
    options$networkExportRequest <- TRUE
    save(jaspResults, options)
    recorded$first <- jaspResults[["networkSavePath"]]$object
    afterSave(jaspResults, options, save, recorded)
  }, .ln1NetWriteCsv = function(data, path) {
    recorded$writes <- recorded$writes + 1L
    write(data, path)
  }, .package = "jaspLearnN1")
  list(result = result, recorded = as.list(recorded))
}

test_that("display scale edits stay pending until an explicit CSV export updates metadata", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  output <- .netCacheScaleRun(.netCacheScaleOptions(path),
    function(jaspResults, options, save, recorded) {
      original <- readLines(path)
      options$networkSeverityMaximum <- 100
      options$networkConnectionMaximum <- 10
      save(jaspResults, options)
      recorded$edited <- jaspResults[["networkSavePath"]]$object
      recorded$editedText <- jaspResults[["networkExport"]]$text
      recorded$editedWrites <- recorded$writes
      recorded$editedFileUnchanged <- identical(readLines(path), original)
      save(jaspResults, options)
      recorded$repeatWrites <- recorded$writes

      # The opposite token transition is also an explicit button click.
      options$networkExportRequest <- FALSE
      save(jaspResults, options)
      recorded$clicked <- jaspResults[["networkSavePath"]]$object
      recorded$csv <- utils::read.csv(path, colClasses = "character",
                                      na.strings = character(), stringsAsFactors = FALSE)
    })
  expect_identical(output$result$status, "complete")
  observed <- output$recorded
  expect_identical(observed$initialWrites, 0L)
  expect_identical(observed$first$status, "success")
  expect_identical(observed$edited$status, "pending")
  expect_identical(observed$edited$lastAttempt$status, "success")
  expect_match(observed$editedText, "Changes have not been exported.", fixed = TRUE)
  expect_false(grepl("Exported Assessment", observed$editedText, fixed = TRUE))
  expect_identical(observed$editedWrites, 1L)
  expect_identical(observed$repeatWrites, 1L)
  expect_true(observed$editedFileUnchanged)
  expect_identical(observed$writes, 2L)
  expect_identical(observed$clicked$status, "success")
  csv <- observed$csv
  expect_equal(unique(csv$schemaVersion), "2")
  expect_equal(unique(csv$severityMaximum), "100")
  expect_equal(unique(csv$connectionMaximum), "10")
  expect_equal(csv$severity[csv$type == "node"], c("0.8", "", "0"))
  expect_equal(csv$severityRated[csv$type == "node"], c("true", "false", "true"))
  expect_equal(as.numeric(csv$weight[csv$type == "edge"]), c(-.7, 0))
})

test_that("optional connection count edits never dirty or rewrite an unchanged CSV export", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  output <- .netCacheScaleRun(.netCacheScaleOptions(path),
    function(jaspResults, options, save, recorded) {
      original <- readLines(path)
      originalRequest <- jaspResults[["networkSavePath"]]$object$request
      for (enabled in c(TRUE, FALSE, TRUE)) {
        options$networkConnectionCounts <- enabled
        save(jaspResults, options)
        current <- jaspResults[["networkSavePath"]]$object
        expect_identical(current$status, "success")
        expect_identical(current$request, originalRequest)
        expect_identical(recorded$writes, 1L)
        expect_identical(readLines(path), original)
      }
      recorded$final <- jaspResults[["networkSavePath"]]$object
    })
  expect_identical(output$result$status, "complete")
  expect_identical(output$recorded$initialWrites, 0L)
  expect_identical(output$recorded$writes, 1L)
  expect_identical(output$recorded$final$status, "success")
})
