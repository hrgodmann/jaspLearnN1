.netBlockedFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.netBlockedOptions <- function(path) {
  options <- jaspTools::analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$networkSavePath <- path
  options$networkExportRequest <- FALSE
  options$networkExportSession <- "native-blocked-export"
  options$networkSeverityMaximum <- 1
  options$networkConnectionMaximum <- 1
  options$problems <- list(list(problemName = "A", problemSeverity = .8),
                           list(problemName = "B", problemSeverity = .2))
  options$connectionList <- list(list(name = "Assessment", allConnections = FALSE,
    allConnectionStrengths = list(), centrality = TRUE, plotNetwork = FALSE,
    edgeWeightTable = TRUE, connections = list(
      list(connectionFrom = "A", connectionTo = "B", connectionStrength = -.7))))
  options
}

.netBlockedRun <- function(options, scenario, failure = "validation") {
  run <- .netBlockedFunction("Network")
  save <- .netBlockedFunction(".ln1NetSaveNetwork")
  write <- .netBlockedFunction(".ln1NetWriteCsv")
  plots <- .netBlockedFunction(".ln1NetCreateNetworkPlots")
  observed <- new.env(parent = emptyenv())
  observed$entered <- FALSE
  observed$writes <- 0L
  result <- testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
  }, .ln1NetSaveNetwork = function(jaspResults, options) {
    if (observed$entered)
      return(save(jaspResults, options))
    observed$entered <- TRUE
    save(jaspResults, options)
    if (scenario$previous %in% c("saved", "reopened")) {
      options$networkExportRequest <- TRUE
      save(jaspResults, options)
    }
    if (scenario$previous == "missing") {
      jaspResults[["networkSavePath"]] <- NULL
    } else if (scenario$previous == "legacy") {
      jaspResults[["networkSavePath"]] <- jaspBase::createJaspState(object = list(saved = TRUE))
    } else if (scenario$previous == "reopened") {
      old <- jaspResults[["networkSavePath"]]$object
      old$session <- "previous-form-session"
      jaspResults[["networkSavePath"]] <- jaspBase::createJaspState(object = old)
    }
    observed$beforeWrites <- observed$writes
    observed$beforeLines <- readLines(options$networkSavePath)

    invalid <- options
    invalid$networkExportRequest <- scenario$token
    if (failure == "output") {
      invalid$blockedOutputProbe <- TRUE
    } else if (scenario$invalid == "scale") {
      invalid$networkSeverityMaximum <- 0
    } else if (scenario$invalid == "severity") {
      invalid$problems[[1L]]$problemSeverity <- 2
    } else {
      invalid$problems[[2L]]$problemName <- "A"
    }
    # Invoke the actual analysis entry point on the same native object. The
    # error handler must consume the event before validation/output errors exit.
    observed$error <- tryCatch({ run(jaspResults, NULL, invalid); NULL },
                               error = function(e) conditionMessage(e))
    observed$blocked <- jaspResults[["networkSavePath"]]$object
    observed$blockedStatusHidden <- is.null(jaspResults[["networkExport"]])
    observed$blockedWrites <- observed$writes
    observed$blockedFileUnchanged <- identical(readLines(options$networkSavePath), observed$beforeLines)

    repaired <- options
    repaired$networkExportRequest <- scenario$token
    run(jaspResults, NULL, repaired)
    observed$repaired <- jaspResults[["networkSavePath"]]$object
    observed$repairedText <- jaspResults[["networkExport"]]$text
    observed$repairedWrites <- observed$writes
    observed$repairedFileUnchanged <- identical(readLines(options$networkSavePath), observed$beforeLines)

    repaired$networkExportRequest <- !scenario$token
    run(jaspResults, NULL, repaired)
    observed$clicked <- jaspResults[["networkSavePath"]]$object
    observed$clickedWrites <- observed$writes
  }, .ln1NetWriteCsv = function(data, path) {
    observed$writes <- observed$writes + 1L
    write(data, path)
  }, .ln1NetCreateNetworkPlots = function(jaspResults, dataset, options, dependencies) {
    if (isTRUE(options$blockedOutputProbe))
      stop("A plot preparation failure", call. = FALSE)
    plots(jaspResults, dataset, options, dependencies)
  }, .package = "jaspLearnN1")
  list(result = result, observed = as.list(observed))
}

.netBlockedExpectConsumed <- function(output, scenario) {
  observed <- output$observed
  expect_identical(output$result$status, "complete")
  expect_type(observed$error, "character")
  expect_identical(observed$blocked$version, 3L)
  expect_identical(observed$blocked$session, "native-blocked-export")
  expect_identical(observed$blocked$observedRequest, scenario$token)
  expect_identical(observed$blocked$status, "pending")
  expect_null(observed$blocked$request)
  expect_true(observed$blockedStatusHidden)
  expect_identical(observed$blockedWrites, observed$beforeWrites)
  expect_true(observed$blockedFileUnchanged)
  expect_identical(observed$repaired$status, "pending")
  expect_identical(observed$repairedWrites, observed$beforeWrites)
  expect_true(observed$repairedFileUnchanged)
  expect_match(observed$repairedText, "Changes have not been exported.", fixed = TRUE)
  expect_identical(observed$clicked$status, "success")
  expect_identical(observed$clickedWrites, observed$beforeWrites + 1L)
  if (scenario$previous %in% c("saved", "reopened")) {
    expect_null(observed$blocked$lastAttempt)
    expect_identical(observed$repaired$lastAttempt, observed$blocked$lastAttempt)
  }
}

test_that("validation failures consume export clicks before a later input repair", {
  scenarios <- list(
    list(previous = "baseline", token = TRUE, invalid = "name"),
    list(previous = "saved", token = FALSE, invalid = "scale"),
    list(previous = "missing", token = TRUE, invalid = "severity"),
    # Reopening has a fresh session and the QML-normalized FALSE token.
    list(previous = "reopened", token = FALSE, invalid = "name"),
    list(previous = "legacy", token = TRUE, invalid = "name"),
    list(previous = "baseline", token = FALSE, invalid = "name"))
  for (scenario in scenarios) {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    writeLines("an existing file must survive failed export preparation", path)
    output <- .netBlockedRun(.netBlockedOptions(path), scenario)
    .netBlockedExpectConsumed(output, scenario)
  }
})

test_that("an output preparation failure cannot defer a CSV write until an ordinary rerun", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines("an existing file must survive failed plot preparation", path)
  scenario <- list(previous = "saved", token = FALSE)
  output <- .netBlockedRun(.netBlockedOptions(path), scenario, failure = "output")
  .netBlockedExpectConsumed(output, scenario)
  expect_match(output$observed$error, "A plot preparation failure", fixed = TRUE)
})
