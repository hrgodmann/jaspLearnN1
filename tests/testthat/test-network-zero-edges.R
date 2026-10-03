# Zero ratings remain data, but must not become arrows in a network plot.
.netZeroFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.netZeroNodes <- function() {
  data.frame(name = c("A", "B", "C", "D"), strength = c(.8, .2, .5, .9))
}

.netZeroEdges <- function(zeroOnly = FALSE) {
  weights <- if (zeroOnly) rep(0, 4L) else c(.7, -.4, 0, -0)
  data.frame(from = c("A", "B", "C", "D"), to = c("B", "C", "A", "A"),
             weight = weights, absWeight = abs(weights))
}

.netZeroOptions <- function(edges, layout = "linear", allMode = FALSE) {
  options <- jaspTools::analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$networkSavePath <- ""
  options$networkExportSession <- "native-network-zero-edges"
  options$plotLayout <- layout
  options$plotStrengthColor <- options$plotStrengthWidth <- TRUE
  options$plotStrengthAlpha <- FALSE
  options$plotSeverityFill <- options$plotSeveritySize <- TRUE
  options$plotSeverityAlpha <- FALSE
  nodes <- .netZeroNodes()
  options$problems <- lapply(seq_len(nrow(nodes)), function(i)
    list(problemName = nodes$name[i], problemSeverity = nodes$strength[i]))
  connections <- lapply(seq_len(nrow(edges)), function(i)
    list(connectionFrom = edges$from[i], connectionTo = edges$to[i], connectionStrength = edges$weight[i]))
  strengths <- lapply(nodes$name, function(from)
    list(targets = lapply(nodes$name, function(to)
      list(connectionStrength = sum(edges$weight[edges$from == from & edges$to == to])))))
  options$connectionList <- list(list(name = "Time 1", connections = connections,
    allConnections = allMode, allConnectionStrengths = strengths,
    plotNetwork = TRUE, centrality = TRUE, edgeWeightTable = TRUE))
  options
}

.netZeroExpectedAllEdges <- function(edges) {
  names <- .netZeroNodes()$name
  do.call(rbind, lapply(names, function(from) {
    targets <- names[names != from]
    weights <- vapply(targets, function(to)
      sum(edges$weight[edges$from == from & edges$to == to]), numeric(1))
    data.frame(from = from, to = targets, weight = unname(weights), absWeight = abs(unname(weights)))
  }))
}

.netZeroOutput <- function(result, container) {
  result$results[[container]]$collection[[paste0(container, "_Time 1")]]
}

.netZeroRows <- function(table) {
  do.call(rbind, lapply(table$data, as.data.frame))
}

.netZeroRunExport <- function(options) {
  save <- .netZeroFunction(".ln1NetSaveNetwork")
  testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
  }, .ln1NetSaveNetwork = function(jaspResults, options) {
    save(jaspResults, options)
    options$networkExportRequest <- !isTRUE(options$networkExportRequest)
    save(jaspResults, options)
  }, .package = "jaspLearnN1")
}

.netZeroExpectPlot <- function(plot, expectedEdges) {
  nodes <- .netZeroNodes()
  graph <- attr(plot$data, "graph")
  expect_equal(igraph::vcount(graph), nrow(nodes))
  expect_setequal(igraph::V(graph)$name, nodes$name)
  expect_equal(plot$data$strength[match(nodes$name, plot$data$name)], nodes$strength)
  expect_true(all(is.finite(plot$data$x) & is.finite(plot$data$y)))
  expect_equal(igraph::ecount(graph), nrow(expectedEdges))
  if (nrow(expectedEdges)) {
    endpoints <- igraph::ends(graph, igraph::E(graph), names = TRUE)
    actualPairs <- paste(endpoints[, 1L], endpoints[, 2L], sep = "->")
    expectedPairs <- paste(expectedEdges$from, expectedEdges$to, sep = "->")
    expect_setequal(actualPairs, expectedPairs)
    expect_identical(as.numeric(igraph::E(graph)$weight[match(expectedPairs, actualPairs)]),
                     as.numeric(expectedEdges$weight))
    expect_true(all(igraph::E(graph)$weight != 0))
  }

  built <- ggplot2::ggplot_build(plot)
  edgeLayers <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomEdgePath"), logical(1)))
  expect_length(edgeLayers, 1L)
  edgeData <- built$data[[edgeLayers]]
  # Checking the built geometry catches an invisible-looking but still drawn
  # zero edge. Merely checking color, width or alpha would miss that defect.
  expect_equal(length(unique(edgeData$group)), nrow(expectedEdges))
  if (!nrow(expectedEdges)) expect_equal(nrow(edgeData), 0L)
  pointLayers <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1)))
  expect_length(pointLayers, 1L)
  expect_equal(nrow(built$data[[pointLayers]]), nrow(nodes))
  labelLayers <- Filter(function(layer) "label" %in% names(layer) && any(!is.na(layer$label)), built$data)
  expect_length(labelLayers, 1L)
  expect_setequal(labelLayers[[1L]]$label, nodes$name)
}

test_that("zero arrows are omitted independently of opacity, width and color settings", {
  styles <- list(c(color = TRUE, width = TRUE, alpha = FALSE),
                 c(color = FALSE, width = FALSE, alpha = FALSE),
                 c(color = TRUE, width = TRUE, alpha = TRUE),
                 c(color = FALSE, width = FALSE, alpha = TRUE))
  for (layout in c("linear", "sugiyama")) {
    for (zeroOnly in c(FALSE, TRUE)) {
      edges <- .netZeroEdges(zeroOnly)
      expected <- edges[edges$weight != 0, , drop = FALSE]
      for (style in styles) {
        options <- .netZeroOptions(edges, layout)
        options$plotStrengthColor <- unname(style[["color"]])
        options$plotStrengthWidth <- unname(style[["width"]])
        options$plotStrengthAlpha <- unname(style[["alpha"]])
        original <- edges
        plot <- .netZeroFunction(".ln1NetCreateNetworkPlotFill")(edges, .netZeroNodes(), options)
        .netZeroExpectPlot(plot, expected)
        expect_identical(edges, original)
      }
    }
  }
})

test_that("small nonzero ratings keep their arrows without an implicit threshold", {
  edges <- .netZeroEdges()
  edges$weight <- c(.Machine$double.eps, -.Machine$double.eps, 0, -0)
  edges$absWeight <- abs(edges$weight)
  expected <- edges[1:2, , drop = FALSE]
  for (layout in c("linear", "sugiyama")) {
    options <- .netZeroOptions(edges, layout)
    options$plotStrengthAlpha <- options$plotStrengthWidth <- options$plotStrengthColor <- FALSE
    plot <- .netZeroFunction(".ln1NetCreateNetworkPlotFill")(edges, .netZeroNodes(), options)
    .netZeroExpectPlot(plot, expected)
  }
})

test_that("native manual and all-mode plots omit zero arrows while retaining raw ratings and exports", {
  for (layout in c("linear", "sugiyama")) {
    for (allMode in c(FALSE, TRUE)) {
      for (zeroOnly in c(FALSE, TRUE)) {
        edges <- .netZeroEdges(zeroOnly)
        rawEdges <- if (allMode) .netZeroExpectedAllEdges(edges) else edges
        options <- .netZeroOptions(edges, layout, allMode)
        options$networkSavePath <- tempfile(fileext = ".csv")
        on.exit(unlink(options$networkSavePath), add = TRUE)
        result <- .netZeroRunExport(options)
        expect_identical(result$status, "complete")
        output <- .netZeroOutput(result, "networkPlotContainer")
        expect_identical(output$status, "complete")
        plot <- result$state$figures[[output$data]]$obj
        .netZeroExpectPlot(plot, rawEdges[rawEdges$weight != 0, , drop = FALSE])

        table <- .netZeroOutput(result, "edgeWeightTableContainer")
        expect_identical(table$status, "complete")
        rows <- .netZeroRows(table)
        expect_equal(rows$from, rawEdges$from)
        expect_equal(rows$to, rawEdges$to)
        expect_equal(rows$weight, rawEdges$weight, tolerance = 1e-14)
        expect_equal(sum(rows$weight == 0), sum(rawEdges$weight == 0))
        summaries <- .netZeroRows(.netZeroOutput(result, "centralityTableContainer"))
        expect_equal(summaries$name, .netZeroNodes()$name)
        expect_equal(summaries$severity, .netZeroNodes()$strength)
        expectedOut <- vapply(.netZeroNodes()$name,
          function(node) sum(rawEdges$weight[rawEdges$from == node]), numeric(1))
        expect_equal(summaries$degreeOut, unname(expectedOut), tolerance = 1e-14)

        expect_true(file.exists(options$networkSavePath))
        exported <- utils::read.csv(options$networkSavePath, colClasses = "character",
                                    na.strings = character())
        exportedEdges <- exported[exported$type == "edge", , drop = FALSE]
        expect_equal(exportedEdges$time, rep("Time 1", nrow(rawEdges)))
        expect_equal(exportedEdges$from, rawEdges$from)
        expect_equal(exportedEdges$to, rawEdges$to)
        expect_equal(as.numeric(exportedEdges$weight), rawEdges$weight, tolerance = 1e-14)
        expect_equal(exported$name[exported$type == "node"], .netZeroNodes()$name)
        # Explicit zero ratings are still edge records, not an unentered tab.
        expect_false(any(exported$type == "network"))
        unlink(options$networkSavePath)
      }
    }
  }
})
