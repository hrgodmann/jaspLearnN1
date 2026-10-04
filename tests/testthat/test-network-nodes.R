# Networks retain every defined problem, including problems without edges.
.netNodesFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.netNodesEdge <- function(from = "A", to = "B", strength = .5) {
  list(connectionFrom = from, connectionTo = to, connectionStrength = strength)
}

.netNodesOccasion <- function(name = "Time 1", connections = list(.netNodesEdge())) {
  list(name = name, connections = connections, allConnections = FALSE,
       allConnectionStrengths = list(), plotNetwork = TRUE,
       centrality = TRUE, edgeWeightTable = TRUE)
}

.netNodesOptions <- function(connections = list(.netNodesEdge()), layout = "linear") {
  options <- jaspTools::analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$networkSavePath <- ""
  options$networkExportSession <- "native-network-nodes"
  options$problems <- list(list(problemName = "A", problemSeverity = .8),
                           list(problemName = "B", problemSeverity = .2),
                           list(problemName = "C", problemSeverity = .5))
  options$connectionList <- list(.netNodesOccasion(connections = connections))
  options$plotLayout <- layout
  options$plotSeverityFill <- options$plotSeveritySize <- TRUE
  options$plotSeverityAlpha <- FALSE
  options$plotStrengthColor <- options$plotStrengthWidth <- TRUE
  options$plotStrengthAlpha <- FALSE
  options
}

.netNodesAttributes <- function(options) {
  data.frame(name = vapply(options$problems, `[[`, character(1), "problemName"),
             strength = vapply(options$problems, `[[`, numeric(1), "problemSeverity"))
}

.netNodesEdges <- function(connections) {
  data.frame(from = vapply(connections, `[[`, character(1), "connectionFrom"),
             to = vapply(connections, `[[`, character(1), "connectionTo"),
             weight = vapply(connections, `[[`, numeric(1), "connectionStrength"),
             absWeight = abs(vapply(connections, `[[`, numeric(1), "connectionStrength")))
}

.netNodesRows <- function(table) {
  if (!length(table$data)) return(data.frame())
  do.call(rbind, lapply(table$data, as.data.frame))
}

.netNodesOutput <- function(result, container, occasion = "Time 1") {
  result$results[[container]]$collection[[paste0(container, "_", occasion)]]
}

.netNodesRun <- function(options) {
  jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
}

.netNodesRunExport <- function(options) {
  save <- .netNodesFunction(".ln1NetSaveNetwork")
  testthat::with_mocked_bindings({
    .netNodesRun(options)
  }, .ln1NetSaveNetwork = function(jaspResults, options) {
    save(jaspResults, options)
    options$networkExportRequest <- !isTRUE(options$networkExportRequest)
    save(jaspResults, options)
  }, .package = "jaspLearnN1")
}

.netNodesExpectPlot <- function(result, options, occasion = "Time 1") {
  output <- .netNodesOutput(result, "networkPlotContainer", occasion)
  expect_identical(output$status, "complete")
  plot <- result$state$figures[[output$data]]$obj
  nodes <- .netNodesAttributes(options)
  expect_setequal(plot$data$name, nodes$name)
  expect_equal(plot$data$strength[match(nodes$name, plot$data$name)], nodes$strength)
  expect_true(all(is.finite(plot$data$x) & is.finite(plot$data$y)))
  graph <- attr(plot$data, "graph")
  expect_equal(igraph::vcount(graph), nrow(nodes))
  expect_setequal(igraph::V(graph)$name, nodes$name)
  built <- ggplot2::ggplot_build(plot)
  labelLayers <- Filter(function(layer)
    "label" %in% names(layer) && any(!is.na(layer$label)), built$data)
  expect_length(labelLayers, 1L)
  expect_setequal(labelLayers[[1L]]$label, nodes$name)
  graph
}

test_that("network centrality retains isolates and uses only specified directed weights", {
  options <- .netNodesOptions()
  nodes <- .netNodesAttributes(options)
  for (connections in list(list(.netNodesEdge()), list(),
                           list(.netNodesEdge(strength = -.4), .netNodesEdge("B", "A", .7)))) {
    edges <- .netNodesEdges(connections)
    centrality <- .netNodesFunction(".ln1NetCentralitySingle")(edges, nodes, options)
    expectedIn <- vapply(nodes$name, function(name) sum(edges$weight[edges$to == name]), numeric(1))
    expectedOut <- vapply(nodes$name, function(name) sum(edges$weight[edges$from == name]), numeric(1))
    expect_equal(centrality$name, nodes$name)
    expect_equal(centrality$degreeIn, unname(expectedIn))
    expect_equal(centrality$degreeOut, unname(expectedOut))
    expect_equal(centrality$degreeIn[centrality$name == "C"], 0)
    expect_equal(centrality$degreeOut[centrality$name == "C"], 0)
  }
})

test_that("native networks keep an unconnected problem in plots and centrality", {
  for (layout in c("linear", "sugiyama", "fr")) {
    options <- .netNodesOptions(layout = layout)
    result <- .netNodesRun(options)
    expect_identical(result$status, "complete")
    graph <- .netNodesExpectPlot(result, options)
    expect_equal(igraph::ecount(graph), 1L)
    expect_equal(as.vector(igraph::ends(graph, igraph::E(graph), names = TRUE)), c("A", "B"))
    centrality <- .netNodesRows(.netNodesOutput(result, "centralityTableContainer"))
    expect_equal(centrality$name, c("A", "B", "C"))
    expect_equal(centrality$degreeIn, c(0, .5, 0))
    expect_equal(centrality$degreeOut, c(.5, 0, 0))
    edges <- .netNodesRows(.netNodesOutput(result, "edgeWeightTableContainer"))
    expect_equal(edges$from, "A")
    expect_equal(edges$to, "B")
    expect_equal(edges$weight, .5)
  }
})

test_that("an explicitly empty edge list produces a valid all-isolate network", {
  for (layout in c("linear", "sugiyama", "fr")) {
    options <- .netNodesOptions(connections = list(), layout = layout)
    result <- .netNodesRun(options)
    expect_identical(result$status, "complete")
    graph <- .netNodesExpectPlot(result, options)
    expect_equal(igraph::ecount(graph), 0L)
    centrality <- .netNodesRows(.netNodesOutput(result, "centralityTableContainer"))
    expect_equal(centrality$name, c("A", "B", "C"))
    expect_equal(centrality$degreeIn, rep(0, 3L))
    expect_equal(centrality$degreeOut, rep(0, 3L))
    edges <- .netNodesOutput(result, "edgeWeightTableContainer")
    expect_identical(edges$status, "complete")
    expect_length(edges$data, 0L)
    expect_setequal(vapply(edges$schema$fields, `[[`, character(1), "name"),
                     c("from", "to", "weight"))
  }
})

test_that("a fully connected two-node network keeps its original numerical results", {
  options <- .netNodesOptions(connections = list(.netNodesEdge(strength = -.4),
                                                .netNodesEdge("B", "A", .7)))
  options$problems <- options$problems[1:2]
  result <- .netNodesRun(options)
  expect_identical(result$status, "complete")
  graph <- .netNodesExpectPlot(result, options)
  expect_equal(igraph::ecount(graph), 2L)
  centrality <- .netNodesRows(.netNodesOutput(result, "centralityTableContainer"))
  expect_equal(centrality$name, c("A", "B"))
  expect_equal(centrality$degreeIn, c(.7, -.4))
  expect_equal(centrality$degreeOut, c(-.4, .7))
  edges <- .netNodesRows(.netNodesOutput(result, "edgeWeightTableContainer"))
  expect_equal(edges$from, c("A", "B"))
  expect_equal(edges$to, c("B", "A"))
  expect_equal(edges$weight, c(-.4, .7))
})

test_that("all-connections mode retains zero ratings without plotting arrows", {
  options <- .netNodesOptions(connections = list())
  options$connectionList[[1L]]$allConnections <- TRUE
  options$connectionList[[1L]]$allConnectionStrengths <- lapply(seq_len(3L), function(i)
    list(targets = lapply(seq_len(3L), function(j) list(connectionStrength = 0))))
  expect_true(.netNodesFunction(".ln1NetEdgelistReady")(options$connectionList[[1L]], c("A", "B", "C")))
  result <- .netNodesRun(options)
  expect_identical(result$status, "complete")
  graph <- .netNodesExpectPlot(result, options)
  expect_equal(igraph::vcount(graph), 3L)
  expect_equal(igraph::ecount(graph), 0L)
  edges <- .netNodesRows(.netNodesOutput(result, "edgeWeightTableContainer"))
  expect_equal(nrow(edges), 6L)
  expect_equal(edges$weight, rep(0, 6L))
  expect_true(all(edges$from != edges$to))
  expect_setequal(paste(edges$from, edges$to), c("A B", "A C", "B A", "B C", "C A", "C B"))
  centrality <- .netNodesRows(.netNodesOutput(result, "centralityTableContainer"))
  expect_equal(centrality$name, c("A", "B", "C"))
  expect_equal(centrality$degreeIn, rep(0, 3L))
  expect_equal(centrality$degreeOut, rep(0, 3L))
})

test_that("missing and unfinished edge inputs are distinct from an explicit empty list", {
  nodes <- c("A", "B", "C")
  check <- .netNodesFunction(".ln1NetCheckEdgelist")
  expect_true(check(.netNodesOccasion(connections = list()), nodes))
  missing <- .netNodesOccasion()
  missing$connections <- NULL
  expect_false(check(missing, nodes))
  for (connection in list(.netNodesEdge(from = ""), .netNodesEdge(to = ""),
                          .netNodesEdge(from = "unknown"), .netNodesEdge(to = "A"))) {
    expect_false(check(.netNodesOccasion(connections = list(connection)), nodes))
  }
  allMode <- .netNodesOccasion(connections = list())
  allMode$allConnections <- TRUE
  ready <- .netNodesFunction(".ln1NetEdgelistReady")
  expect_false(ready(allMode, nodes))
  allMode$allConnectionStrengths <- NULL
  expect_false(ready(allMode, nodes))
})

test_that("network CSV preserves every node and explicitly records empty occasions", {
  for (onlyEmpty in c(FALSE, TRUE)) {
    options <- .netNodesOptions()
    empty <- .netNodesOccasion("No connections", connections = list())
    incomplete <- .netNodesOccasion("Unfinished", connections = list(.netNodesEdge(to = "")))
    options$connectionList <- if (onlyEmpty) list(empty, incomplete) else
      list(.netNodesOccasion(), empty, incomplete)
    options$networkSavePath <- tempfile(fileext = ".csv")
    on.exit(unlink(options$networkSavePath), add = TRUE)
    result <- .netNodesRunExport(options)
    expect_identical(result$status, "complete")
    expect_true(file.exists(options$networkSavePath))
    exported <- utils::read.csv(options$networkSavePath, stringsAsFactors = FALSE,
                                na.strings = character(), colClasses = "character")
    expect_equal(names(exported), c("type", "time", "name", "severity", "from", "to", "weight",
                                     "schemaVersion", "severityMaximum", "connectionMaximum", "severityRated"))
    nodes <- exported[exported$type == "node", , drop = FALSE]
    expect_equal(nodes$name, c("A", "B", "C"))
    expect_equal(as.numeric(nodes$severity), c(.8, .2, .5))
    expect_true(all(nodes$time == "" & nodes$from == "" & nodes$to == "" & nodes$weight == ""))
    occasions <- exported[exported$type == "network", , drop = FALSE]
    expect_equal(occasions$time, "No connections")
    expect_true(all(occasions[c("name", "severity", "from", "to", "weight")] == ""))
    edges <- exported[exported$type == "edge", , drop = FALSE]
    if (onlyEmpty) {
      expect_equal(nrow(edges), 0L)
      expect_equal(nrow(exported), 4L)
    } else {
      expect_equal(edges$time, "Time 1")
      expect_equal(edges$from, "A")
      expect_equal(edges$to, "B")
      expect_equal(as.numeric(edges$weight), .5)
      expect_equal(nrow(exported), 5L)
    }
    expect_false("Unfinished" %in% exported$time)
    unlink(options$networkSavePath)
  }
})

test_that("unfinished network input is not presented as a completed empty network", {
  options <- .netNodesOptions(connections = list())
  unfinished <- .netNodesOccasion("Unfinished", connections = list(.netNodesEdge(to = "")))
  missing <- .netNodesOccasion("Missing")
  missing$connections <- NULL
  options$connectionList <- list(options$connectionList[[1L]], unfinished, missing)
  result <- .netNodesRun(options)
  expect_identical(result$status, "complete")
  expect_identical(.netNodesOutput(result, "networkPlotContainer")$status, "complete")
  for (occasion in c("Unfinished", "Missing")) {
    expect_null(.netNodesOutput(result, "centralityTableContainer", occasion))
    expect_null(.netNodesOutput(result, "edgeWeightTableContainer", occasion))
    output <- .netNodesOutput(result, "networkPlotContainer", occasion)
    if (!is.null(output)) {
      hasFigure <- is.character(output$data) && length(output$data) == 1L &&
        nzchar(output$data) && output$data %in% names(result$state$figures)
      expect_false(hasFigure)
    }
  }
})

test_that("legacy network caches are rebuilt with previously hidden isolated nodes", {
  options <- .netNodesOptions()
  options$networkSavePath <- tempfile(fileext = ".csv")
  on.exit(unlink(options$networkSavePath), add = TRUE)
  savedContents <- "existing export must not be rewritten by an analysis upgrade"
  writeLines(savedContents, options$networkSavePath)
  nodes <- .netNodesAttributes(options)
  edges <- .netNodesEdges(options$connectionList[[1L]]$connections)
  oldCentrality <- data.frame(name = c("A", "B"), degreeIn = c(0, .5), degreeOut = c(.5, 0))
  originalUpgrade <- .netNodesFunction(".ln1NetUpgradeState")
  recorded <- new.env(parent = emptyenv())
  recorded$called <- FALSE
  result <- testthat::with_mocked_bindings({
    .netNodesRun(options)
  }, .ln1NetUpgradeState = function(jaspResults) {
    # Native constructors are valid here because runAnalysis initialized JASP.
    recorded$called <- TRUE
    jaspResults[["nodeAttributesState"]] <- jaspBase::createJaspState(nodes)
    edgelists <- jaspBase::createJaspContainer()
    edgelists[["Time 1"]] <- jaspBase::createJaspState(edges)
    jaspResults[["edgelistContainer"]] <- edgelists
    centralities <- jaspBase::createJaspContainer()
    centralities[["Time 1"]] <- jaspBase::createJaspState(oldCentrality)
    jaspResults[["centralityContainer"]] <- centralities
    oldTable <- jaspBase::createJaspTable("Time 1")
    oldTable$addColumnInfo(name = "name", type = "string")
    oldTable$addColumnInfo(name = "degreeIn", type = "number")
    oldTable$addColumnInfo(name = "degreeOut", type = "number")
    oldTable$addRows(oldCentrality)
    tables <- jaspBase::createJaspContainer()
    tables[["Time 1"]] <- oldTable
    jaspResults[["centralityTableContainer"]] <- tables
    oldEdges <- jaspBase::createJaspTable("Time 1")
    oldEdges$addColumnInfo(name = "weight", type = "number")
    oldEdges$addRows(data.frame(weight = 999))
    edgeTables <- jaspBase::createJaspContainer()
    edgeTables[["Time 1"]] <- oldEdges
    jaspResults[["edgeWeightTableContainer"]] <- edgeTables
    oldPlot <- .netNodesFunction(".ln1NetCreateNetworkPlotFill")(edges, nodes[1:2, ], options)
    plots <- jaspBase::createJaspContainer()
    plots[["Time 1"]] <- jaspBase::createJaspPlot(plot = oldPlot, title = "Time 1")
    jaspResults[["networkPlotContainer"]] <- plots
    jaspResults[["networkSavePath"]] <- jaspBase::createJaspState(list(saved = TRUE))
    jaspResults[["networkNodesVersion"]] <- NULL

    originalUpgrade(jaspResults)
    keys <- c("nodeAttributesState", "edgelistContainer", "centralityContainer",
              "centralityTableContainer", "edgeWeightTableContainer", "networkPlotContainer")
    recorded$cleared <- vapply(keys, function(key) is.null(jaspResults[[key]]), logical(1))
    recorded$exportPreserved <- !is.null(jaspResults[["networkSavePath"]])
    recorded$version <- jaspResults[["networkNodesVersion"]]$object
    sentinel <- list(marker = "current cache")
    jaspResults[["nodeAttributesState"]] <- jaspBase::createJaspState(sentinel)
    originalUpgrade(jaspResults)
    recorded$idempotent <- identical(jaspResults[["nodeAttributesState"]]$object, sentinel)
    jaspResults[["nodeAttributesState"]] <- NULL
  }, .package = "jaspLearnN1")
  expect_true(recorded$called)
  expect_true(all(recorded$cleared))
  expect_true(recorded$exportPreserved)
  expect_identical(recorded$version, 4L)
  expect_true(recorded$idempotent)
  expect_identical(result$status, "complete")
  graph <- .netNodesExpectPlot(result, options)
  expect_equal(igraph::vcount(graph), 3L)
  centrality <- .netNodesRows(.netNodesOutput(result, "centralityTableContainer"))
  expect_equal(centrality$name, c("A", "B", "C"))
  expect_equal(centrality$degreeIn, c(0, .5, 0))
  expect_equal(centrality$degreeOut, c(.5, 0, 0))
  expect_equal(.netNodesRows(.netNodesOutput(result, "edgeWeightTableContainer"))$weight, .5)
  expect_true(file.exists(options$networkSavePath))
  expect_identical(readLines(options$networkSavePath), savedContents)
})

test_that("version-two caches rebuild for scales without rewriting existing exports", {
  options <- .netNodesOptions(connections = list(.netNodesEdge(strength = 0)))
  options$networkSavePath <- tempfile(fileext = ".csv")
  on.exit(unlink(options$networkSavePath), add = TRUE)
  savedContents <- "existing export must not be rewritten by a result upgrade"
  writeLines(savedContents, options$networkSavePath)
  originalUpgrade <- .netNodesFunction(".ln1NetUpgradeState")
  recorded <- new.env(parent = emptyenv())
  result <- testthat::with_mocked_bindings({
    .netNodesRun(options)
  }, .ln1NetUpgradeState = function(jaspResults) {
    # Seed native version-2 caches inside the initialized analysis context.
    .netNodesFunction(".ln1NetData")(jaspResults, NULL, options)
    .netNodesFunction(".ln1NetCentrality")(jaspResults, options)
    .netNodesFunction(".ln1NetEdgeWeightTables")(jaspResults, options)
    oldPlot <- .netNodesFunction(".ln1NetCreateNetworkPlotFill")(
      .netNodesEdges(list(.netNodesEdge())), .netNodesAttributes(options), options)
    plots <- jaspBase::createJaspContainer()
    plots[["Time 1"]] <- jaspBase::createJaspPlot(plot = oldPlot, title = "Time 1")
    jaspResults[["networkPlotContainer"]] <- plots
    savedState <- list(saved = TRUE)
    jaspResults[["networkSavePath"]] <- jaspBase::createJaspState(savedState)
    jaspResults[["networkNodesVersion"]] <- jaspBase::createJaspState(2L)

    originalUpgrade(jaspResults)
    recorded$plotCleared <- is.null(jaspResults[["networkPlotContainer"]])
    preservedKeys <- c("nodeAttributesState", "edgelistContainer", "centralityContainer",
                       "centralityTableContainer", "edgeWeightTableContainer", "networkSavePath")
    recorded$preserved <- vapply(preservedKeys, function(key)
      !is.null(jaspResults[[key]]), logical(1))
    recorded$savedState <- jaspResults[["networkSavePath"]]$object
    recorded$version <- jaspResults[["networkNodesVersion"]]$object
    .netNodesFunction(".ln1NetData")(jaspResults, NULL, options)
    .netNodesFunction(".ln1NetCreateNetworkPlots")(
      jaspResults, NULL, options, .netNodesFunction(".ln1NetGetDataDependencies"))
    originalUpgrade(jaspResults)
    recorded$currentPlotPreserved <- !is.null(jaspResults[["networkPlotContainer"]])
  }, .package = "jaspLearnN1")
  expect_true(recorded$plotCleared)
  expect_true(recorded$preserved[["networkSavePath"]])
  expect_false(any(recorded$preserved[setdiff(names(recorded$preserved), "networkSavePath")]))
  expect_identical(recorded$savedState, list(saved = TRUE))
  expect_identical(recorded$version, 4L)
  expect_true(recorded$currentPlotPreserved)
  expect_identical(result$status, "complete")
  graph <- .netNodesExpectPlot(result, options)
  expect_equal(igraph::ecount(graph), 0L)
  expect_equal(.netNodesRows(.netNodesOutput(result, "edgeWeightTableContainer"))$weight, 0)
  expect_equal(.netNodesRows(.netNodesOutput(result, "centralityTableContainer"))$degreeOut,
               rep(0, 3L))
  expect_identical(readLines(options$networkSavePath), savedContents)
})
