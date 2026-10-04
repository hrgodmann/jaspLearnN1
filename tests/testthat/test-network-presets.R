# File side effects are exercised through the public initialized analysis runner.
# These tests do not simulate Desktop rendering or the frontend Apply operation.
.netPresetFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.netPresetDefinition <- function() {
  list(schemaVersion = 1L, name = "Reusable <example>",
       problems = list(list(problemName = "Z problem", problemDescription = "Repeated description"),
                       list(problemName = "A problem", problemDescription = "Repeated description"),
                       list(problemName = "Énergie", problemDescription = "")),
       scales = list(severityMax = 10L, connectionMax = 100L, connectionInput = "magnitude"))
}

.netPresetOptions <- function() {
  options <- jaspTools::analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$networkExportSession <- "native-preset-test"
  options$networkPresetLoadPath <- ""
  options$networkPresetLoadRequest <- FALSE
  options$networkPresetSavePath <- ""
  options$networkPresetSaveRequest <- FALSE
  options$networkPresetSaveName <- "Reusable definitions"
  options$networkSavePath <- ""
  options$networkExportRequest <- FALSE
  options$networkSeverityMaximum <- 10L
  options$networkConnectionMaximum <- 100L
  options$networkConnectionInput <- "magnitude"
  options$problems <- list(list(problemName = "A", problemDescription = "First definition", problemSeverity = .8),
                           list(problemName = "B", problemDescription = "", problemSeverity = .2),
                           list(problemName = "C", problemDescription = "Third definition", problemSeverity = .5))
  options$connectionList <- list(list(name = "Assessment", connections = list(
    list(connectionFrom = "A", connectionTo = "B", connectionStrength = .5)),
    allConnections = FALSE, allConnectionStrengths = list(),
    plotNetwork = FALSE, centrality = TRUE, edgeWeightTable = TRUE))
  options
}

.netPresetSourceValues <- function(jaspResults) {
  # Native jaspQmlSource$value is serialized JSON. Decode precisely the object
  # that Desktop receives rather than interpreting the getter as an R vector.
  jsonlite::fromJSON(jaspResults[["networkPresetPreview"]]$value, simplifyVector = FALSE)
}

.netPresetRun <- function(options, callback = NULL) {
  original <- .netPresetFunction(".ln1NetPresetFiles")
  recorded <- new.env(parent = emptyenv())
  result <- testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
  }, .ln1NetPresetFiles = function(jaspResults, options) {
    original(jaspResults, options)
    recorded$initialSave <- jaspResults[["networkPresetSaveState"]]$object
    recorded$initialLoad <- jaspResults[["networkPresetLoadState"]]$object
    recorded$initialSources <- .netPresetSourceValues(jaspResults)
    recorded$initialSourceJson <- jaspResults[["networkPresetPreview"]]$value
    if (!is.null(callback)) callback(jaspResults, options, original, recorded)
    recorded$finalSave <- jaspResults[["networkPresetSaveState"]]$object
    recorded$finalLoad <- jaspResults[["networkPresetLoadState"]]$object
    recorded$finalSources <- .netPresetSourceValues(jaspResults)
  }, .package = "jaspLearnN1")
  list(result = result, recorded = as.list(recorded))
}

test_that("preset JSON preserves definitions, scales, order, Unicode and optional descriptions", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  preset <- .netPresetDefinition()
  preset$problems[[1]]$problemName <- "  Z problem  "
  preset$problems[[2]]$problemDescription <- NULL
  preset$problems[[3]]$problemDescription <- "  Text with \"quotes\" & a\nsecond line  "
  preset$scales$connectionInput <- NULL
  .netPresetFunction(".ln1NetWritePreset")(preset, path)
  result <- .netPresetFunction(".ln1NetReadPreset")(path)
  expect_identical(result$name, preset$name)
  expect_identical(vapply(result$problems, `[[`, character(1), "problemName"), c("Z problem", "A problem", "Énergie"))
  expect_null(result$problems[[2]]$problemDescription)
  expect_identical(result$problems[[3]]$problemDescription, "Text with \"quotes\" & a\nsecond line")
  expect_identical(result$scales, list(severityMax = 10L, connectionMax = 100L))
  expect_identical(result$schemaVersion, 1L)
})

test_that("preset validation rejects unsupported, ambiguous and patient-specific data", {
  validate <- .netPresetFunction(".ln1NetValidatePreset")
  changes <- list(
    function(p) { p$schemaVersion <- 2L; p },
    function(p) { p$name <- " "; p },
    function(p) { p$name <- "Two\nlines"; p },
    function(p) { p$connectionList <- list(); p },
    function(p) { p$problems <- p$problems[1]; p },
    function(p) { p$problems <- rep(p$problems, 4); p },
    function(p) { p$problems[[1]]$problemName <- " "; p },
    function(p) { p$problems[[2]]$problemName <- " Z problem "; p },
    function(p) { p$problems[[1]]$problemSeverity <- 0; p },
    function(p) { p$problems[[1]]$problemDescription <- NULL; p$problems[[1]]["problemDescription"] <- list(NULL); p },
    function(p) { p$scales$severityMax <- 0; p },
    function(p) { p$scales$connectionMax <- 1001; p },
    function(p) { p$scales$connectionMax <- 2.5; p },
    function(p) { p$scales$connectionMax <- 2 + 1i; p },
    function(p) { p$scales$severityMax <- "10"; p },
    function(p) { p$scales$connectionInput <- "unknown"; p },
    function(p) { p$scales$weight <- .5; p })
  for (change in changes) expect_error(validate(change(.netPresetDefinition())))
  expect_error(validate(list()))
  duplicateKeys <- .netPresetDefinition()
  duplicateKeys <- c(duplicateKeys, list(name = "Duplicate key"))
  expect_error(validate(duplicateKeys))
  for (maximum in c(1L, 1000L)) {
    preset <- .netPresetDefinition()
    preset$scales$severityMax <- maximum
    preset$scales$connectionMax <- maximum
    expect_equal(validate(preset)$scales$severityMax, maximum)
  }
})

test_that("malformed or oversized imports do not reach the preset preview", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  reader <- .netPresetFunction(".ln1NetReadPreset")
  expect_error(reader(path), "existing")
  expect_error(reader(tempdir()), "existing")
  writeLines("{not JSON}", path)
  expect_error(reader(path), "valid preset JSON")
  writeLines(paste(rep("x", 1024^2 + 1L), collapse = ""), path)
  expect_error(reader(path), "too large")
})

test_that("the saved definition allowlist excludes ratings, tabs and export paths", {
  options <- .netPresetOptions()
  options$problems[[1]]$severityRated <- TRUE
  options$networkSavePath <- "/private/patient/results.csv"
  request <- .netPresetFunction(".ln1NetPresetRequest")(options)
  expect_identical(names(request$preset), c("schemaVersion", "name", "problems", "scales"))
  for (problem in request$preset$problems)
    expect_identical(names(problem), c("problemName", "problemDescription"))
  serialized <- jsonlite::toJSON(request$preset, auto_unbox = TRUE)
  expect_false(grepl("problemSeverity|severityRated|connectionStrength|patient/results|Assessment", serialized))
  changed <- options
  changed$problems[[1]]$problemSeverity <- .1
  changed$connectionList[[1]]$connections[[1]]$connectionStrength <- -.5
  changed$connectionList[[1]]$name <- "Different assessment"
  expect_identical(.netPresetFunction(".ln1NetPresetRequest")(changed), request)
})

test_that("native preset sources preserve order and repeated or empty descriptions", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  .netPresetFunction(".ln1NetWritePreset")(.netPresetDefinition(), path)
  options <- .netPresetOptions()
  options$networkPresetLoadPath <- path
  output <- .netPresetRun(options)
  expect_identical(output$result$status, "complete")
  sources <- output$recorded$initialSources
  expect_true(jsonlite::validate(output$recorded$initialSourceJson))
  expect_type(sources, "list")
  expect_setequal(names(sources), c("names", "descriptions", "name", "severityMaximum",
                                   "connectionMaximum", "connectionInput", "loadedToken"))
  expect_type(sources$names, "list")
  expect_type(sources$descriptions, "list")
  expect_type(sources$name, "character")
  expect_equal(unlist(sources$names), c("Z problem", "A problem", "Énergie"))
  expect_equal(unlist(sources$descriptions), c("Repeated description", "Repeated description", ""))
  expect_equal(unlist(sources$name), "Reusable <example>")
  expect_equal(unlist(sources$severityMaximum), "10")
  expect_equal(unlist(sources$connectionMaximum), "100")
  expect_equal(unlist(sources$connectionInput), "magnitude")
  expect_equal(unlist(sources$loadedToken), paste(options$networkExportSession, path, "false", sep = "\n"))
  expect_match(output$result$results$networkPresetLoadStatus$rawtext, "Reusable &lt;example&gt;", fixed = TRUE)
  expect_identical(output$recorded$initialSave$status, "pending")
})

test_that("preview reload is explicit at the same path and failure clears every source", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writer <- .netPresetFunction(".ln1NetWritePreset")
  writer(.netPresetDefinition(), path)
  options <- .netPresetOptions()
  options$networkPresetLoadPath <- path
  output <- .netPresetRun(options, function(results, opts, original, recorded) {
    writeLines("malformed", path)
    original(results, opts)
    recorded$cached <- .netPresetSourceValues(results)
    opts$networkPresetLoadRequest <- TRUE
    original(results, opts)
    recorded$failed <- .netPresetSourceValues(results)
    recorded$error <- results[["networkPresetLoadStatus"]]$text
    writer(.netPresetDefinition(), path)
    original(results, opts)
    recorded$stillFailed <- .netPresetSourceValues(results)
    opts$networkPresetLoadRequest <- FALSE
    original(results, opts)
    recorded$reloaded <- .netPresetSourceValues(results)
    opts$networkPresetLoadPath <- ""
    original(results, opts)
  })
  expect_identical(output$result$status, "complete")
  expect_equal(unlist(output$recorded$cached$names), c("Z problem", "A problem", "Énergie"))
  # The native getter must contain empty JSON arrays, not empty-string terms.
  # Empty names remove the preview rows; the empty token also blocks Apply.
  for (values in output$recorded$failed) expect_identical(values, list())
  for (values in output$recorded$stillFailed) expect_identical(values, list())
  for (values in output$recorded$finalSources) expect_identical(values, list())
  expect_match(output$recorded$error, "could not be loaded", fixed = TRUE)
  expect_equal(unlist(output$recorded$reloaded$names), c("Z problem", "A problem", "Énergie"))
})

test_that("omitted preset input mode preserves current display settings", {
  preset <- .netPresetDefinition()
  preset$scales$connectionInput <- NULL
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  .netPresetFunction(".ln1NetWritePreset")(preset, path)
  options <- .netPresetOptions()
  options$networkPresetLoadPath <- path
  output <- .netPresetRun(options, function(results, opts, original, recorded) {
    opts$networkConnectionInput <- "signed"
    original(results, opts)
  })
  expect_equal(unlist(output$recorded$initialSources$connectionInput), "magnitude")
  expect_equal(unlist(output$recorded$finalSources$connectionInput), "signed")
})

test_that("a valid preset preview remains available to repair invalid current problem names", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  .netPresetFunction(".ln1NetWritePreset")(.netPresetDefinition(), path)
  options <- .netPresetOptions()
  options$networkPresetLoadPath <- path
  options$problems[[2]]$problemName <- options$problems[[1]]$problemName
  output <- .netPresetRun(options)
  expect_false(identical(output$result$status, "complete"))
  expect_equal(unlist(output$recorded$initialSources$names), c("Z problem", "A problem", "Énergie"))
  expect_equal(unlist(output$recorded$initialSources$loadedToken),
               paste(options$networkExportSession, path, "false", sep = "\n"))
})

test_that("preset saving requires each click and consumes both button transitions", {
  path <- tempfile(fileext = ".json")
  other <- tempfile(fileext = ".json")
  on.exit(unlink(c(path, other)), add = TRUE)
  sentinel <- "existing file"
  writeLines(sentinel, path)
  options <- .netPresetOptions()
  options$networkPresetSavePath <- path
  output <- .netPresetRun(options, function(results, opts, original, recorded) {
    recorded$notSavedInitially <- identical(readLines(path), sentinel)
    opts$networkPresetSaveRequest <- TRUE
    original(results, opts)
    recorded$first <- .netPresetFunction(".ln1NetReadPreset")(path)
    opts$networkPresetSaveName <- "Edited name"
    original(results, opts)
    recorded$pending <- results[["networkPresetSaveState"]]$object$status
    recorded$unchanged <- .netPresetFunction(".ln1NetReadPreset")(path)
    opts$networkPresetSavePath <- other
    original(results, opts)
    recorded$noWriteOnPath <- !file.exists(other)
    opts$networkPresetSaveRequest <- FALSE
    original(results, opts)
    recorded$second <- .netPresetFunction(".ln1NetReadPreset")(other)
    writeLines(sentinel, other)
    opts$connectionList[[1]]$centrality <- FALSE
    original(results, opts)
    recorded$noRepeat <- identical(readLines(other), sentinel)
  })
  expect_identical(output$result$status, "complete")
  expect_true(output$recorded$notSavedInitially)
  expect_identical(output$recorded$first$name, "Reusable definitions")
  expect_identical(output$recorded$pending, "pending")
  expect_identical(output$recorded$unchanged$name, "Reusable definitions")
  expect_true(output$recorded$noWriteOnPath)
  expect_identical(output$recorded$second$name, "Edited name")
  expect_true(output$recorded$noRepeat)
  expect_identical(output$recorded$finalSave$status, "success")
})

test_that("fresh-session saves require the QML-normalized click protocol", {
  for (session in list(NULL, "", "fresh-form")) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    options <- .netPresetOptions()
    options$networkPresetSavePath <- path
    options$networkPresetSaveRequest <- TRUE
    options$networkExportSession <- session
    output <- .netPresetRun(options)
    expect_identical(output$result$status, "complete")
    expect_identical(file.exists(path), identical(session, "fresh-form"))
  }
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  options <- .netPresetOptions()
  options$networkPresetSavePath <- path
  output <- .netPresetRun(options, function(results, opts, original, recorded) {
    opts$networkPresetSaveRequest <- TRUE
    original(results, opts)
    writeLines("preserve on reopening", path)
    opts$networkExportSession <- "reopened-form"
    opts$networkPresetSaveRequest <- FALSE
    original(results, opts)
    recorded$preserved <- identical(readLines(path), "preserve on reopening")
    opts$networkPresetSaveRequest <- TRUE
    original(results, opts)
  })
  expect_true(output$recorded$preserved)
  expect_identical(output$recorded$finalSave$status, "success")
})

test_that("pathless or failed preset saves are consumed without later automatic writes", {
  directory <- tempfile("network-preset-test-")
  path <- file.path(directory, "preset.json")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  options <- .netPresetOptions()
  output <- .netPresetRun(options, function(results, opts, original, recorded) {
    opts$networkPresetSaveRequest <- TRUE
    original(results, opts)
    opts$networkPresetSavePath <- path
    original(results, opts)
    recorded$pathlessConsumed <- results[["networkPresetSaveState"]]$object$status == "pending"
    opts$networkPresetSaveRequest <- FALSE
    original(results, opts)
    recorded$failed <- results[["networkPresetSaveState"]]$object$status
    dir.create(directory)
    original(results, opts)
    recorded$notRetried <- !file.exists(path)
    opts$networkPresetSaveRequest <- TRUE
    original(results, opts)
  })
  expect_identical(output$result$status, "complete")
  expect_true(output$recorded$pathlessConsumed)
  expect_identical(output$recorded$failed, "failed")
  expect_true(output$recorded$notRetried)
  expect_identical(output$recorded$finalSave$status, "success")
  expect_true(file.exists(path))
})

test_that("a failed preset replacement preserves existing bytes and cleans its temporary file", {
  directory <- tempfile("network-preset-rename-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "preset.json")
  writeLines("existing complete preset", path)
  testthat::with_mocked_bindings({
    expect_error(.netPresetFunction(".ln1NetWritePreset")(.netPresetDefinition(), path), "could not replace")
  }, file.rename = function(from, to) FALSE, .package = "base")
  expect_identical(readLines(path), "existing complete preset")
  expect_identical(list.files(directory, all.files = TRUE, no.. = TRUE), "preset.json")
})
