# Export errors are local output results, and retry must work with the same
# native JASP result object and unchanged destination path.
.netExportFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.netExportAssessment <- function(name = "Rated assessment", connections = NULL) {
  if (is.null(connections)) connections <- list(
    list(connectionFrom = "A", connectionTo = "B", connectionStrength = .5),
    list(connectionFrom = "B", connectionTo = "C", connectionStrength = 0))
  list(name = name, connections = connections, allConnections = FALSE,
       allConnectionStrengths = list(), plotNetwork = TRUE,
       centrality = TRUE, edgeWeightTable = TRUE)
}

.netExportOptions <- function(path) {
  options <- jaspTools::analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$networkSavePath <- path
  options$networkExportRequest <- FALSE
  options$networkExportSession <- "native-export-test"
  options$plotLayout <- "linear"
  options$problems <- list(list(problemName = "A", problemSeverity = .8),
                           list(problemName = "B", problemSeverity = .2),
                           list(problemName = "C", problemSeverity = .5))
  options$connectionList <- list(.netExportAssessment())
  options
}

.netExportRun <- function(options, afterFirstSave = NULL, click = TRUE) {
  originalSave <- .netExportFunction(".ln1NetSaveNetwork")
  recorded <- new.env(parent = emptyenv())
  result <- testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
  }, .ln1NetSaveNetwork = function(jaspResults, options) {
    originalSave(jaspResults, options)
    recorded$initialState <- jaspResults[["networkSavePath"]]$object
    recorded$initialText <- if (!is.null(jaspResults[["networkExport"]]))
      jaspResults[["networkExport"]]$text else NULL
    if (click) {
      # Selecting a destination initializes the journal. Only a subsequent
      # button-token transition represents the user's explicit export request.
      options$networkExportRequest <- !isTRUE(options$networkExportRequest)
      originalSave(jaspResults, options)
    }
    recorded$firstState <- jaspResults[["networkSavePath"]]$object
    recorded$firstText <- if (!is.null(jaspResults[["networkExport"]]))
      jaspResults[["networkExport"]]$text else NULL
    if (!is.null(afterFirstSave))
      afterFirstSave(jaspResults, options, originalSave, recorded)
    recorded$finalState <- jaspResults[["networkSavePath"]]$object
  }, .package = "jaspLearnN1")
  list(result = result, recorded = as.list(recorded))
}

.netExportOutput <- function(result, container, assessment = "Rated assessment") {
  result$results[[container]]$collection[[paste0(container, "_", assessment)]]
}

.netExportExpectAnalysisOutputs <- function(result, assessment = "Rated assessment") {
  expect_identical(result$status, "complete")
  plot <- .netExportOutput(result, "networkPlotContainer", assessment)
  expect_identical(plot$status, "complete")
  expect_false(is.null(result$state$figures[[plot$data]]$obj))
  for (container in c("centralityTableContainer", "edgeWeightTableContainer"))
    expect_identical(.netExportOutput(result, container, assessment)$status, "complete")
  expect_equal(result$results$networkExport$title, "Network export")
}

.netExportRead <- function(path) {
  utils::read.csv(path, colClasses = "character", na.strings = character(),
                 stringsAsFactors = FALSE)
}

test_that("selecting a destination with the form's initialized false token never exports", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  options <- .netExportOptions(path)
  # The QML initialization handler resets saved button values to FALSE before
  # the analysis receives options. R tests exercise this normalized contract.
  options$networkExportRequest <- FALSE
  output <- .netExportRun(options, click = FALSE)
  .netExportExpectAnalysisOutputs(output$result)
  expect_identical(output$recorded$firstState$status, "pending")
  expect_identical(output$recorded$firstState$version, 3L)
  expect_identical(output$recorded$firstState$observedRequest, FALSE)
  expect_false(file.exists(path))

  sentinel <- "an existing destination must not be rewritten on opening"
  writeLines(sentinel, path)
  output <- .netExportRun(options, click = FALSE)
  expect_identical(output$recorded$firstState$status, "pending")
  expect_identical(readLines(path), sentinel)
  expect_match(output$recorded$firstText, "Changes have not been exported.", fixed = TRUE)
})

test_that("the first explicit click can export before an initial R baseline run", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  options <- .netExportOptions(path)
  # A fresh session's checkbox starts FALSE in QML, so TRUE means the user
  # clicked even when the backend has not yet received its initial options.
  options$networkExportRequest <- TRUE
  output <- .netExportRun(options, click = FALSE)
  .netExportExpectAnalysisOutputs(output$result)
  expect_identical(output$recorded$firstState$status, "success")
  expect_true(file.exists(path))
  rows <- .netExportRead(path)
  expect_equal(as.numeric(rows$weight[rows$type == "edge"]), c(.5, 0))
})

test_that("button transitions without an initialized form session cannot export", {
  for (session in list(NULL, "")) {
    for (token in c(FALSE, TRUE)) {
      path <- tempfile(fileext = ".csv")
      on.exit(unlink(path), add = TRUE)
      sentinel <- "no form session means no permission to export"
      writeLines(sentinel, path)
      options <- .netExportOptions(path)
      options$networkExportSession <- session
      options$networkExportRequest <- token
      output <- .netExportRun(options)
      .netExportExpectAnalysisOutputs(output$result)
      expect_identical(output$recorded$firstState$status, "pending")
      expect_identical(readLines(path), sentinel)
    }
  }
})

test_that("invalid export destinations leave network plots and tables complete", {
  directory <- tempfile("network-export-errors-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  directoryTarget <- file.path(directory, "directory.csv")
  dir.create(directoryTarget)
  invalidParent <- file.path(directory, "parent-is-a-file")
  writeLines("ordinary file", invalidParent)
  paths <- c(file.path(directory, "missing-parent", "network.csv"),
             directoryTarget, file.path(invalidParent, "network.csv"))
  for (path in paths) {
    output <- .netExportRun(.netExportOptions(path))
    .netExportExpectAnalysisOutputs(output$result)
    expect_identical(output$recorded$firstState$status, "failed")
    expect_true(nzchar(output$recorded$firstState$error))
    expect_match(output$result$results$networkExport$rawtext,
                  "[Ff]ail|[Cc]ould not|[Uu]nable|[Nn]ot saved")
    if (!dir.exists(path)) expect_false(file.exists(path))
  }
  expect_identical(readLines(invalidParent), "ordinary file")
})

test_that("partial export names completed and omitted assessments and preserves the raw CSV schema", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  options <- .netExportOptions(path)
  omittedName <- "Unfinished <C & D>"
  options$connectionList <- list(
    .netExportAssessment("Rated assessment"),
    .netExportAssessment("Empty assessment", connections = list()),
    .netExportAssessment(omittedName, connections = list(
      list(connectionFrom = "A", connectionTo = "", connectionStrength = .7))))
  output <- .netExportRun(options)
  .netExportExpectAnalysisOutputs(output$result)
  .netExportExpectAnalysisOutputs(output$result, "Empty assessment")
  state <- output$recorded$firstState
  expect_identical(state$status, "success")
  expect_equal(state$completed, c("Rated assessment", "Empty assessment"))
  expect_equal(state$omitted, omittedName)
  text <- output$result$results$networkExport$rawtext
  expect_match(text, "Rated assessment", fixed = TRUE)
  expect_match(text, "Empty assessment", fixed = TRUE)
  expect_match(text, "Unfinished &lt;C &amp; D&gt;", fixed = TRUE)
  expect_false(grepl("Unfinished <C & D>", text, fixed = TRUE))
  expect_match(text, "[Oo]mitt|[Ss]kip|[Ii]ncomplete")
  exported <- .netExportRead(path)
  expect_equal(names(exported), c("type", "time", "name", "severity", "from", "to", "weight"))
  nodes <- exported[exported$type == "node", , drop = FALSE]
  expect_equal(nodes$name, c("A", "B", "C"))
  expect_equal(as.numeric(nodes$severity), c(.8, .2, .5))
  edges <- exported[exported$type == "edge", , drop = FALSE]
  expect_equal(edges$time, rep("Rated assessment", 2L))
  expect_equal(edges$from, c("A", "B"))
  expect_equal(edges$to, c("B", "C"))
  expect_equal(as.numeric(edges$weight), c(.5, 0))
  empty <- exported[exported$type == "network", , drop = FALSE]
  expect_equal(empty$time, "Empty assessment")
  expect_true(all(empty[c("name", "severity", "from", "to", "weight")] == ""))
  expect_false(omittedName %in% exported$time)
  expect_equal(nrow(exported), 6L)
})

test_that("all-unfinished assessments leave an existing destination untouched", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  sentinel <- c("existing export", "must survive an incomplete request")
  writeLines(sentinel, path)
  options <- .netExportOptions(path)
  incompleteMatrix <- .netExportAssessment("Unfinished matrix")
  incompleteMatrix$allConnections <- TRUE
  options$connectionList <- list(
    .netExportAssessment("Unfinished row", connections = list(
      list(connectionFrom = "", connectionTo = "B", connectionStrength = .5))),
    incompleteMatrix)
  output <- .netExportRun(options)
  expect_identical(output$result$status, "complete")
  expect_identical(output$recorded$firstState$status, "incomplete")
  expect_length(output$recorded$firstState$completed, 0L)
  expect_equal(output$recorded$firstState$omitted, c("Unfinished row", "Unfinished matrix"))
  expect_identical(readLines(path), sentinel)
  expect_match(output$result$results$networkExport$rawtext, "No CSV was written", fixed = TRUE)
})

test_that("same-path retries use both request-toggle transitions and cosmetic reruns preserve files", {
  directory <- tempfile("network-export-retry-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  parent <- file.path(directory, "initially-missing")
  path <- file.path(parent, "network.csv")
  options <- .netExportOptions(path)
  output <- .netExportRun(options, afterFirstSave = function(jaspResults, options, save, recorded) {
    # All calls use the same initialized native result object and destination.
    recorded$absentBeforeRecovery <- !file.exists(path)
    dir.create(parent)
    save(jaspResults, options)
    recorded$withoutRequestStillAbsent <- !file.exists(path)
    recorded$withoutRequestState <- jaspResults[["networkSavePath"]]$object
    requested <- options
    requested$networkExportRequest <- !isTRUE(requested$networkExportRequest)
    save(jaspResults, requested)
    recorded$retryState <- jaspResults[["networkSavePath"]]$object
    recorded$savedLines <- readLines(path)

    sentinel <- c(recorded$savedLines, "external edit must survive cosmetic rerun")
    writeLines(sentinel, path)
    cosmetic <- requested
    cosmetic$plotStrengthAlpha <- !isTRUE(options$plotStrengthAlpha)
    cosmetic$colorPalette <- "viridis"
    cosmetic$connectionList[[1L]]$centrality <- FALSE
    cosmetic$connectionList[[1L]]$plotNetwork <- FALSE
    cosmetic$connectionList[[1L]]$edgeWeightTable <- FALSE
    # Inactive matrix entries must not change a manual assessment's export.
    cosmetic$connectionList[[1L]]$allConnectionStrengths <- list(
      list(targets = list(list(connectionStrength = -.8))))
    save(jaspResults, cosmetic)
    recorded$cosmeticPreserved <- identical(readLines(path), sentinel)

    requested <- cosmetic
    requested$networkExportRequest <- TRUE
    save(jaspResults, requested)
    recorded$trueRequestState <- jaspResults[["networkSavePath"]]$object
    recorded$trueRequestWrote <- identical(readLines(path), recorded$savedLines)
    writeLines(c(recorded$savedLines, "another external edit"), path)
    requested$networkExportRequest <- FALSE
    save(jaspResults, requested)
    recorded$falseRequestState <- jaspResults[["networkSavePath"]]$object
    recorded$falseRequestWrote <- identical(readLines(path), recorded$savedLines)

    requested$connectionList[[1L]]$connections[[1L]]$connectionStrength <- -.7
    .netExportFunction(".ln1NetData")(jaspResults, NULL, requested)
    save(jaspResults, requested)
    recorded$editedDataState <- jaspResults[["networkSavePath"]]$object
    recorded$editedDataText <- jaspResults[["networkExport"]]$text
    recorded$editedDataPreserved <- identical(readLines(path), recorded$savedLines)
    reverted <- requested
    reverted$connectionList[[1L]]$connections[[1L]]$connectionStrength <- .5
    .netExportFunction(".ln1NetData")(jaspResults, NULL, reverted)
    save(jaspResults, reverted)
    recorded$undoneDataState <- jaspResults[["networkSavePath"]]$object
    recorded$undoneDataPreserved <- identical(readLines(path), recorded$savedLines)
    .netExportFunction(".ln1NetData")(jaspResults, NULL, requested)
    save(jaspResults, requested)
    requested$networkExportRequest <- TRUE
    save(jaspResults, requested)
    recorded$changedRatings <- .netExportRead(path)
    requested$networkSavePath <- file.path(parent, "another.csv")
    save(jaspResults, requested)
    recorded$editedPathState <- jaspResults[["networkSavePath"]]$object
    recorded$editedPathAbsent <- !file.exists(requested$networkSavePath)
    requested$networkExportRequest <- FALSE
    save(jaspResults, requested)
    recorded$changedPathWritten <- file.exists(requested$networkSavePath)
    recorded$changedPathLines <- readLines(requested$networkSavePath)
  })
  .netExportExpectAnalysisOutputs(output$result)
  expect_identical(output$recorded$firstState$status, "failed")
  expect_true(output$recorded$absentBeforeRecovery)
  expect_true(output$recorded$withoutRequestStillAbsent)
  expect_identical(output$recorded$withoutRequestState$status, "failed")
  expect_identical(output$recorded$retryState$status, "success")
  expect_true(output$recorded$cosmeticPreserved)
  expect_identical(output$recorded$trueRequestState$status, "success")
  expect_true(output$recorded$trueRequestWrote)
  expect_identical(output$recorded$falseRequestState$status, "success")
  expect_true(output$recorded$falseRequestWrote)
  expect_identical(output$recorded$editedDataState$status, "pending")
  expect_true(output$recorded$editedDataPreserved)
  expect_match(output$recorded$editedDataText, "Changes have not been exported.", fixed = TRUE)
  expect_false(grepl("Exported Rated assessment", output$recorded$editedDataText, fixed = TRUE))
  expect_identical(output$recorded$editedDataState$lastAttempt$status, "success")
  expect_identical(output$recorded$undoneDataState$status, "pending")
  expect_true(output$recorded$undoneDataPreserved)
  changedRatings <- output$recorded$changedRatings
  expect_equal(as.numeric(changedRatings$weight[changedRatings$type == "edge"]), c(-.7, 0))
  expect_identical(output$recorded$editedPathState$status, "pending")
  expect_true(output$recorded$editedPathAbsent)
  expect_true(output$recorded$changedPathWritten)
  expect_identical(output$recorded$changedPathLines, readLines(path))
  expect_true("networkExportRequest" %in% .netExportFunction(".ln1NetExportDependencies")(options))
  oldOptions <- options
  oldOptions$networkExportRequest <- NULL
  request <- .netExportFunction(".ln1NetExportRequest")
  expect_identical(request(oldOptions), request(options))
})

test_that("clearing and reselecting a destination never queues or triggers an export", {
  directory <- tempfile("network-export-clear-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  firstPath <- file.path(directory, "first.csv")
  secondPath <- file.path(directory, "second.csv")
  sentinel <- "existing file must survive path selection"
  writeLines(sentinel, firstPath)
  writeLines(sentinel, secondPath)
  options <- .netExportOptions("")
  output <- .netExportRun(options, click = FALSE,
    afterFirstSave = function(jaspResults, options, save, recorded) {
      recorded$initialStatusAbsent <- is.null(jaspResults[["networkExport"]])
      selected <- options
      selected$networkSavePath <- firstPath
      save(jaspResults, selected)
      recorded$selectedPreserved <- identical(readLines(firstPath), sentinel)
      selected$networkExportRequest <- TRUE
      save(jaspResults, selected)
      recorded$clickedData <- .netExportRead(firstPath)
      savedLines <- readLines(firstPath)
      writeLines(c(savedLines, "external change"), firstPath)

      selected$networkSavePath <- ""
      save(jaspResults, selected)
      recorded$clearedStatusAbsent <- is.null(jaspResults[["networkExport"]])
      recorded$journalRetained <- !is.null(jaspResults[["networkSavePath"]])
      selected$networkSavePath <- firstPath
      save(jaspResults, selected)
      recorded$reselectedPreserved <- identical(readLines(firstPath),
                                                c(savedLines, "external change"))

      # A token transition with no destination must be consumed, not queued for
      # the next file selection (the UI normally disables the button here).
      selected$networkSavePath <- ""
      save(jaspResults, selected)
      selected$networkExportRequest <- FALSE
      save(jaspResults, selected)
      selected$networkSavePath <- secondPath
      save(jaspResults, selected)
      recorded$emptyClickNotQueued <- identical(readLines(secondPath), sentinel)
      recorded$secondPathState <- jaspResults[["networkSavePath"]]$object
      selected$networkExportRequest <- TRUE
      save(jaspResults, selected)
      recorded$explicitSecondData <- .netExportRead(secondPath)
    })
  .netExportExpectAnalysisOutputs(output$result)
  expect_true(output$recorded$initialStatusAbsent)
  expect_true(output$recorded$selectedPreserved)
  expect_equal(output$recorded$clickedData$type, c(rep("node", 3L), rep("edge", 2L)))
  expect_true(output$recorded$clearedStatusAbsent)
  expect_true(output$recorded$journalRetained)
  expect_true(output$recorded$reselectedPreserved)
  expect_true(output$recorded$emptyClickNotQueued)
  expect_identical(output$recorded$secondPathState$status, "pending")
  expect_identical(output$recorded$explicitSecondData, output$recorded$clickedData)
})

test_that("a write failure cannot replace an existing export with partial CSV content", {
  directory <- tempfile("network-export-partial-write-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "network.csv")
  sentinel <- c("existing complete export", "preserve these bytes")
  writeLines(sentinel, path)
  output <- testthat::with_mocked_bindings({
    .netExportRun(.netExportOptions(path))
  }, write.csv = function(x, file, ...) {
    writeLines("incomplete CSV bytes", file)
    stop("Injected disk-write failure", call. = FALSE)
  }, .package = "utils")
  .netExportExpectAnalysisOutputs(output$result)
  expect_identical(output$recorded$firstState$status, "failed")
  expect_match(output$recorded$firstState$error, "Injected disk-write failure", fixed = TRUE)
  expect_identical(readLines(path), sentinel)
  expect_equal(list.files(directory, all.files = TRUE, no.. = TRUE), "network.csv")
})

test_that("a failed final rename preserves the destination and removes its staging file", {
  directory <- tempfile("network-export-rename-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "network.csv")
  sentinel <- c("existing complete export", "preserve these bytes")
  writeLines(sentinel, path)
  writer <- .netExportFunction(".ln1NetWriteCsv")
  testthat::with_mocked_bindings({
    expect_error(writer(data.frame(type = "node", name = "A"), path),
                 "could not replace", fixed = TRUE)
  }, file.rename = function(from, to) FALSE, .package = "base")
  expect_identical(readLines(path), sentinel)
  expect_equal(list.files(directory, all.files = TRUE, no.. = TRUE), "network.csv")
})

test_that("legacy export markers report unknown status without automatically writing again", {
  for (oldObject in list(NULL, list(saved = TRUE))) {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    sentinel <- "legacy destination must not be rewritten automatically"
    writeLines(sentinel, path)
    options <- .netExportOptions(path)
    originalSave <- .netExportFunction(".ln1NetSaveNetwork")
    recorded <- new.env(parent = emptyenv())
    result <- testthat::with_mocked_bindings({
      jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
    }, .ln1NetSaveNetwork = function(jaspResults, options) {
      # Seed old state only after the analysis has initialized native objects.
      jaspResults[["networkSavePath"]] <- jaspBase::createJaspState(oldObject)
      originalSave(jaspResults, options)
      recorded$legacyState <- jaspResults[["networkSavePath"]]$object
      recorded$legacyText <- jaspResults[["networkExport"]]$text
      recorded$preservedFirst <- identical(readLines(path), sentinel)
      originalSave(jaspResults, options)
      recorded$preservedSecond <- identical(readLines(path), sentinel)
      requested <- options
      requested$networkExportRequest <- TRUE
      originalSave(jaspResults, requested)
      recorded$requestedState <- jaspResults[["networkSavePath"]]$object
      recorded$requestedData <- .netExportRead(path)
    }, .package = "jaspLearnN1")
    .netExportExpectAnalysisOutputs(result)
    expect_identical(recorded$legacyState$status, "legacy")
    expect_true(recorded$preservedFirst)
    expect_true(recorded$preservedSecond)
    expect_match(recorded$legacyText, "[Uu]nknown|[Cc]annot.*confirm|[Nn]ot.*verif")
    expect_match(recorded$legacyText, "Export CSV / Save again", fixed = TRUE)
    expect_identical(recorded$requestedState$status, "success")
    expect_equal(recorded$requestedData$type, c(rep("node", 3L), rep("edge", 2L)))
    unlink(path)
  }
})

test_that("journal migration and same-session reruns never replay their saved export token", {
  for (version in c(1L, 2L, 3L)) {
    for (token in c(FALSE, TRUE)) {
      for (changed in c(FALSE, TRUE)) {
        path <- tempfile(fileext = ".csv")
        on.exit(unlink(path), add = TRUE)
        sentinel <- "restoring a journal must leave the destination untouched"
        writeLines(sentinel, path)
        options <- .netExportOptions(path)
        options$networkExportRequest <- if (version < 3L) FALSE else token
        if (changed)
          options$connectionList[[1L]]$connections[[1L]]$connectionStrength <- -.7
        originalSave <- .netExportFunction(".ln1NetSaveNetwork")
        recorded <- new.env(parent = emptyenv())
        result <- testthat::with_mocked_bindings({
          jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
        }, .ln1NetSaveNetwork = function(jaspResults, options) {
          # A saved native journal contains normalized engine options: JSON
          # converts the fixture's numeric zero rating to an integer zero.
          previousOptions <- options
          if (changed)
            previousOptions$connectionList[[1L]]$connections[[1L]]$connectionStrength <- .5
          previousRequest <- .netExportFunction(".ln1NetExportRequest")(previousOptions)
          oldObject <- list(version = version, status = "success",
                            request = previousRequest, completed = "Rated assessment",
                            omitted = character(), error = NULL)
          if (version == 1L) {
            # The previous policy included the retry token in its payload. A
            # different restored token establishes a baseline, not a replay.
            oldObject$request$retry <- token
          } else {
            oldObject$observedRequest <- token
            oldObject$lastAttempt <- oldObject[c("request", "status", "completed", "omitted", "error")]
            if (version == 3L) oldObject$session <- options$networkExportSession
          }
          jaspResults[["networkSavePath"]] <- jaspBase::createJaspState(oldObject)
          originalSave(jaspResults, options)
          recorded$restoredState <- jaspResults[["networkSavePath"]]$object
          recorded$restoredPreserved <- identical(readLines(path), sentinel)
          originalSave(jaspResults, options)
          recorded$repeatedPreserved <- identical(readLines(path), sentinel)
          options$networkExportRequest <- !isTRUE(options$networkExportRequest)
          originalSave(jaspResults, options)
          recorded$clickedState <- jaspResults[["networkSavePath"]]$object
          recorded$clickedData <- .netExportRead(path)
        }, .package = "jaspLearnN1")
        .netExportExpectAnalysisOutputs(result)
        expect_identical(recorded$restoredState$version, 3L)
        expect_identical(recorded$restoredState$observedRequest, options$networkExportRequest)
        expect_identical(recorded$restoredState$status, if (changed) "pending" else "success")
        expect_true(recorded$restoredPreserved)
        expect_true(recorded$repeatedPreserved)
        expect_identical(recorded$clickedState$status, "success")
        rows <- recorded$clickedData
        expect_equal(as.numeric(rows$weight[rows$type == "edge"]), c(if (changed) -.7 else .5, 0))
        unlink(path)
      }
    }
  }
})

test_that("a new form session ignores saved tokens before allowing a fresh click", {
  for (savedToken in c(FALSE, TRUE)) {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    sentinel <- "opening a form must not replay a persisted button transition"
    writeLines(sentinel, path)
    options <- .netExportOptions(path)
    options$networkExportRequest <- FALSE
    originalSave <- .netExportFunction(".ln1NetSaveNetwork")
    recorded <- new.env(parent = emptyenv())
    result <- testthat::with_mocked_bindings({
      jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
    }, .ln1NetSaveNetwork = function(jaspResults, options) {
      previous <- list(version = 3L, session = "previous-form",
                        observedRequest = savedToken, status = "success",
                        request = .netExportFunction(".ln1NetExportRequest")(options),
                        completed = "Rated assessment", omitted = character(), error = NULL)
      previous$lastAttempt <- previous[c("request", "status", "completed", "omitted", "error")]
      jaspResults[["networkSavePath"]] <- jaspBase::createJaspState(previous)
      earlyClick <- options
      earlyClick$networkExportRequest <- TRUE
      originalSave(jaspResults, earlyClick)
      recorded$earlyClickState <- jaspResults[["networkSavePath"]]$object
      recorded$earlyClickData <- .netExportRead(path)

      # Independently replay opening without clicking from the same saved
      # journal. Only this path receives the QML-initialized FALSE value.
      writeLines(sentinel, path)
      jaspResults[["networkSavePath"]] <- jaspBase::createJaspState(previous)
      originalSave(jaspResults, options)
      recorded$baseline <- jaspResults[["networkSavePath"]]$object
      recorded$preservedOnOpening <- identical(readLines(path), sentinel)
      originalSave(jaspResults, options)
      recorded$preservedOnRerun <- identical(readLines(path), sentinel)
      options$networkExportRequest <- TRUE
      originalSave(jaspResults, options)
      recorded$clickedState <- jaspResults[["networkSavePath"]]$object
      recorded$clickedData <- .netExportRead(path)
    }, .package = "jaspLearnN1")
    .netExportExpectAnalysisOutputs(result)
    expect_identical(recorded$baseline$version, 3L)
    expect_identical(recorded$baseline$session, options$networkExportSession)
    expect_identical(recorded$baseline$observedRequest, FALSE)
    expect_identical(recorded$earlyClickState$status, "success")
    expect_equal(recorded$earlyClickData$type, c(rep("node", 3L), rep("edge", 2L)))
    expect_true(recorded$preservedOnOpening)
    expect_true(recorded$preservedOnRerun)
    expect_identical(recorded$clickedState$status, "success")
    expect_equal(recorded$clickedData$type, c(rep("node", 3L), rep("edge", 2L)))
    unlink(path)
  }
})
