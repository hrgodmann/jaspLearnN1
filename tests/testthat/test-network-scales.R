.netScaleFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.netScaleOptions <- function(maximum = 1) {
  options <- jaspTools::analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$networkSavePath <- ""
  options$networkSeverityMaximum <- maximum
  options$networkConnectionMaximum <- maximum
  options$problems <- list(
    list(problemName = "A", problemSeverity = .25, problemSeverityRated = TRUE),
    list(problemName = "B", problemSeverity = 0, problemSeverityRated = FALSE),
    list(problemName = "C", problemSeverity = 0, problemSeverityRated = TRUE))
  options$connectionList <- list(list(name = "Assessment", allConnections = FALSE,
    connections = list(list(connectionFrom = "A", connectionTo = "B", connectionStrength = -.7),
      list(connectionFrom = "B", connectionTo = "C", connectionStrength = 0)),
    allConnectionStrengths = list(), plotNetwork = TRUE, centrality = TRUE, edgeWeightTable = TRUE))
  options
}

.netScaleTable <- function(result, key) {
  table <- result$results[[key]]$collection[[paste0(key, "_Assessment")]]
  do.call(rbind, lapply(table$data, as.data.frame))
}

test_that("display scales rescale tables while unknown severity remains distinct from zero", {
  for (maximum in c(1, 10, 100, 1000)) {
    options <- .netScaleOptions(maximum)
    result <- jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
    expect_identical(result$status, "complete")
    summaries <- .netScaleTable(result, "centralityTableContainer")
    expect_equal(as.numeric(summaries$severity[c(1, 3)]), c(.25 * maximum, 0))
    expect_identical(summaries$severity[2], "")
    expect_equal(summaries$degreeOut, c(-.7, 0, 0) * maximum)
    expect_equal(summaries$strengthIn, c(0, .7, 0) * maximum)
    edges <- .netScaleTable(result, "edgeWeightTableContainer")
    expect_equal(edges$weight, c(-.7, 0) * maximum)
  }
})

test_that("unrated severity retains all plotted nodes and the nonzero arrow", {
  options <- .netScaleOptions(100)
  nodes <- data.frame(name = c("A", "B", "C"), strength = c(.25, NA_real_, 0))
  edges <- data.frame(from = c("A", "B"), to = c("B", "C"), weight = c(-.7, 0), absWeight = c(.7, 0))
  for (layout in c("linear", "sugiyama")) {
    options$plotLayout <- layout
    options$plotSeverityFill <- TRUE
    options$plotSeveritySize <- TRUE
    options$plotSeverityAlpha <- TRUE
    plot <- .netScaleFunction(".ln1NetCreateNetworkPlotFill")(edges, nodes, options)
    graph <- attr(plot$data, "graph")
    expect_equal(igraph::vcount(graph), 3)
    expect_equal(igraph::ecount(graph), 1)
    expect_equal(plot$data$plotLabel, c("A", "B (not rated)", "C"))
    built <- ggplot2::ggplot_build(plot)
    # Every node has a point; the unknown node has a separate neutral overlay.
    expect_equal(nrow(built$data[[2L]]), 3)
    expect_equal(nrow(built$data[[3L]]), 1)
    expect_true(all(is.finite(built$data[[2L]]$size)))
    expect_equal(plot$scales$get_scales("fill")$labels, c(0, 50, 100))
  }
})

test_that("CSV scale metadata preserves canonical values and explicit unrated severity", {
  options <- .netScaleOptions(100)
  captured <- new.env(parent = emptyenv())
  result <- testthat::with_mocked_bindings({
    jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
  }, .ln1NetSaveNetwork = function(jaspResults, options) {
    captured$data <- .netScaleFunction(".ln1NetExportData")(jaspResults, options)
  }, .package = "jaspLearnN1")
  expect_identical(result$status, "complete")
  data <- captured$data
  expect_equal(unique(data$schemaVersion), 2L)
  expect_equal(unique(data$severityMaximum), 100)
  expect_equal(unique(data$connectionMaximum), 100)
  expect_equal(as.numeric(data$severity[data$type == "node"]), c(.25, NA_real_, 0))
  expect_equal(data$severityRated[data$type == "node"], c("true", "false", "true"))
  expect_equal(as.numeric(data$weight[data$type == "edge"]), c(-.7, 0))
})
