# Independent arithmetic checks for absolute and signed connection summaries.
.netSummaryFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.netSummaryEdges <- function() {
  data.frame(from = c("A", "A", "B", "C"), to = c("B", "C", "C", "B"),
             weight = c(.6, -.6, -.4, -.6), absWeight = c(.6, .6, .4, .6))
}

.netSummaryNodes <- function() {
  data.frame(name = c("A", "B", "C", "D"), strength = c(.8, .2, .5, 1))
}

.netSummaryOptions <- function(edges = .netSummaryEdges()) {
  options <- jaspTools::analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$networkSavePath <- ""
  nodes <- .netSummaryNodes()
  options$problems <- lapply(seq_len(nrow(nodes)), function(i)
    list(problemName = nodes$name[i], problemSeverity = nodes$strength[i]))
  connections <- lapply(seq_len(nrow(edges)), function(i)
    list(connectionFrom = edges$from[i], connectionTo = edges$to[i], connectionStrength = edges$weight[i]))
  # Keep the legacy option key: saved analyses use centrality, not the new label.
  options$connectionList <- list(list(name = "Time 1", connections = connections,
    allConnections = FALSE, allConnectionStrengths = list(),
    centrality = TRUE, plotNetwork = FALSE, edgeWeightTable = TRUE))
  options
}

.netSummaryOracle <- function(edges, nodes = .netSummaryNodes()) {
  incoming <- vapply(nodes$name, function(name) sum(edges$weight[edges$to == name]), numeric(1))
  outgoing <- vapply(nodes$name, function(name) sum(edges$weight[edges$from == name]), numeric(1))
  strengthIn <- vapply(nodes$name, function(name) sum(abs(edges$weight[edges$to == name])), numeric(1))
  strengthOut <- vapply(nodes$name, function(name) sum(abs(edges$weight[edges$from == name])), numeric(1))
  data.frame(name = nodes$name, severity = nodes$strength,
             strengthOut = unname(strengthOut), degreeOut = unname(outgoing),
             strengthIn = unname(strengthIn), degreeIn = unname(incoming))
}

.netSummaryRun <- function(options) {
  jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
}

.netSummaryTable <- function(result, container = "centralityTableContainer") {
  result$results[[container]]$collection[[paste0(container, "_Time 1")]]
}

.netSummaryRows <- function(table) {
  do.call(rbind, lapply(table$data, as.data.frame))
}

.netSummaryExpectRows <- function(actual, expected) {
  expect_equal(nrow(actual), nrow(expected))
  for (column in names(expected))
    expect_equal(actual[[column]], expected[[column]], tolerance = 1e-12, info = column)
}

test_that("connection strengths distinguish cancellation from absent or negative-only links", {
  nodes <- .netSummaryNodes()
  mixed <- .netSummaryEdges()
  positive <- data.frame(from = c("A", "B"), to = c("B", "C"),
                          weight = c(.3, .7), absWeight = c(.3, .7))
  empty <- mixed[FALSE, , drop = FALSE]
  for (edges in list(mixed, positive, empty)) {
    actual <- .netSummaryFunction(".ln1NetCentralitySingle")(edges, nodes, .netSummaryOptions(edges))
    expected <- .netSummaryOracle(edges, nodes)
    expect_equal(names(actual), names(expected))
    .netSummaryExpectRows(actual, expected)
  }
  cancellation <- .netSummaryFunction(".ln1NetCentralitySingle")(mixed, nodes, .netSummaryOptions())
  expect_equal(cancellation$strengthOut[1L], 1.2)
  expect_equal(cancellation$degreeOut[1L], 0)
  expect_equal(cancellation$strengthIn[2L], 1.2)
  expect_equal(cancellation$degreeIn[2L], 0)
  expect_equal(cancellation$strengthOut[2L], .4)
  expect_equal(cancellation$degreeOut[2L], -.4)
  expect_equal(cancellation$strengthIn[3L], 1)
  expect_equal(cancellation$degreeIn[3L], -1)
  expect_equal(unlist(cancellation[4L, c("strengthOut", "degreeOut", "strengthIn", "degreeIn")],
                       use.names = FALSE), rep(0, 4L))
})

test_that("native connection summaries expose severity and clearly grouped metrics", {
  options <- .netSummaryOptions()
  result <- .netSummaryRun(options)
  expect_identical(result$status, "complete")
  expect_equal(result$results$centralityTableContainer$title, "Connection Summaries")
  table <- .netSummaryTable(result)
  expect_identical(table$status, "complete")
  expect_equal(table$title, "Time 1")
  .netSummaryExpectRows(.netSummaryRows(table), .netSummaryOracle(.netSummaryEdges()))
  fields <- table$schema$fields
  expect_equal(vapply(fields, `[[`, character(1), "name"),
               c("name", "severity", "strengthOut", "degreeOut", "strengthIn", "degreeIn"))
  expect_equal(vapply(fields, `[[`, character(1), "title"),
               c("Problem", "Severity", "Absolute strength", "Signed sum", "Absolute strength", "Signed sum"))
  expect_equal(vapply(fields[3:6], `[[`, character(1), "overTitle"),
               c("Outgoing", "Outgoing", "Incoming", "Incoming"))
  footnotes <- paste(vapply(table$footnotes, `[[`, character(1), "text"), collapse = " ")
  expect_match(footnotes, "[Aa]bsolute")
  expect_match(footnotes, "cancel")
  expect_match(footnotes, "[Ss]everity")
  expect_match(footnotes, "perceiv|perception")
  expect_match(footnotes, "benefit")
})

test_that("severity changes are displayed without changing connection metrics", {
  options <- .netSummaryOptions()
  first <- .netSummaryRun(options)
  changedSeverity <- c(0, 1, .4, .7)
  for (i in seq_along(options$problems))
    options$problems[[i]]$problemSeverity <- changedSeverity[i]
  second <- .netSummaryRun(options)
  expect_identical(first$status, "complete")
  expect_identical(second$status, "complete")
  firstRows <- .netSummaryRows(.netSummaryTable(first))
  secondRows <- .netSummaryRows(.netSummaryTable(second))
  expect_equal(secondRows$severity, changedSeverity)
  expect_equal(firstRows$severity, .netSummaryNodes()$strength)
  for (column in c("name", "strengthOut", "degreeOut", "strengthIn", "degreeIn"))
    expect_equal(secondRows[[column]], firstRows[[column]], info = column)
})

test_that("the existing centrality checkbox still controls connection-summary output", {
  options <- .netSummaryOptions()
  enabled <- .netSummaryRun(options)
  expect_false(is.null(.netSummaryTable(enabled)))
  for (flag in list(FALSE, NULL)) {
    options$connectionList[[1L]]$centrality <- flag
    result <- .netSummaryRun(options)
    expect_identical(result$status, "complete")
    expect_null(.netSummaryTable(result))
    edges <- .netSummaryTable(result, "edgeWeightTableContainer")
    expect_identical(edges$status, "complete")
    expect_equal(.netSummaryRows(edges)$weight, .netSummaryEdges()$weight)
  }
})

test_that("manual and all-connections inputs give the same connection summaries", {
  edges <- .netSummaryEdges()
  options <- .netSummaryOptions(edges)
  manual <- .netSummaryRun(options)
  nodes <- .netSummaryNodes()$name
  options$connectionList[[1L]]$allConnections <- TRUE
  options$connectionList[[1L]]$allConnectionStrengths <- lapply(nodes, function(from)
    list(targets = lapply(nodes, function(to) {
      selected <- edges$from == from & edges$to == to
      list(connectionStrength = sum(edges$weight[selected]))
    })))
  allMode <- .netSummaryRun(options)
  expect_identical(allMode$status, "complete")
  expect_equal(.netSummaryRows(.netSummaryTable(allMode)),
               .netSummaryRows(.netSummaryTable(manual)), tolerance = 1e-12)
  expect_length(.netSummaryTable(allMode, "edgeWeightTableContainer")$data, 12L)

  options$connectionList[[1L]]$allConnectionStrengths <- lapply(nodes, function(from)
    list(targets = lapply(nodes, function(to) list(connectionStrength = 0))))
  allZero <- .netSummaryRun(options)
  expect_identical(allZero$status, "complete")
  .netSummaryExpectRows(.netSummaryRows(.netSummaryTable(allZero)),
                        .netSummaryOracle(edges[FALSE, , drop = FALSE]))
  expect_length(.netSummaryTable(allZero, "edgeWeightTableContainer")$data, 12L)
  empty <- .netSummaryRun(.netSummaryOptions(edges[FALSE, , drop = FALSE]))
  expect_identical(empty$status, "complete")
  expect_equal(.netSummaryRows(.netSummaryTable(empty)), .netSummaryRows(.netSummaryTable(allZero)))
  expect_length(.netSummaryTable(empty, "edgeWeightTableContainer")$data, 0L)
})

test_that("unmarked and version-one caches rebuild the new connection-summary schema", {
  options <- .netSummaryOptions()
  expected <- .netSummaryOracle(.netSummaryEdges())
  originalUpgrade <- .netSummaryFunction(".ln1NetUpgradeState")
  for (oldVersion in list(NULL, 1L)) {
    recorded <- new.env(parent = emptyenv())
    result <- testthat::with_mocked_bindings({
      .netSummaryRun(options)
    }, .ln1NetUpgradeState = function(jaspResults) {
      # This callback runs inside the initialized native runAnalysis context.
      oldSummary <- expected[, c("name", "degreeIn", "degreeOut")]
      states <- jaspBase::createJaspContainer()
      states[["Time 1"]] <- jaspBase::createJaspState(oldSummary)
      jaspResults[["centralityContainer"]] <- states
      oldTable <- jaspBase::createJaspTable("Time 1")
      oldTable$addColumnInfo(name = "name", type = "string")
      oldTable$addColumnInfo(name = "degreeIn", type = "number")
      oldTable$addColumnInfo(name = "degreeOut", type = "number")
      oldTable$addRows(oldSummary)
      tables <- jaspBase::createJaspContainer(title = "Centrality")
      tables[["Time 1"]] <- oldTable
      jaspResults[["centralityTableContainer"]] <- tables
      if (is.null(oldVersion)) {
        jaspResults[["networkNodesVersion"]] <- NULL
      } else {
        jaspResults[["networkNodesVersion"]] <- jaspBase::createJaspState(oldVersion)
      }
      originalUpgrade(jaspResults)
      recorded$cleared <- is.null(jaspResults[["centralityContainer"]]) &&
        is.null(jaspResults[["centralityTableContainer"]])
      recorded$version <- jaspResults[["networkNodesVersion"]]$object
    }, .package = "jaspLearnN1")
    expect_true(recorded$cleared)
    expect_identical(recorded$version, 4L)
    expect_identical(result$status, "complete")
    expect_equal(result$results$centralityTableContainer$title, "Connection Summaries")
    .netSummaryExpectRows(.netSummaryRows(.netSummaryTable(result)), expected)
    expect_equal(vapply(.netSummaryTable(result)$schema$fields, `[[`, character(1), "name"), names(expected))
  }
})
