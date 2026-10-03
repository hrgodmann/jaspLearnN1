# Tests for analysis "How Are Symptoms Connected?"

test_that("Network specification works", {
  options <- analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$networkSavePath <- ""
  options$problems <- list(list(problemName = "A", problemSeverity = 0.8), list(problemName = "B",
      problemSeverity = 0.2), list(problemName = "C", problemSeverity = 0.5))
  options$connectionList <- list(list(connections = list(list(connectionFrom = "A", connectionTo = "B",
      connectionStrength = 0.5), list(connectionFrom = "A", connectionTo = "C",
      connectionStrength = -0.5), list(connectionFrom = "C", connectionTo = "B",
      connectionStrength = 0.5)), name = "Time 1", plotNetwork = TRUE,
      centrality = TRUE), list(connections = list(list(connectionFrom = "A",
      connectionTo = "B", connectionStrength = -0.5), list(connectionFrom = "A",
      connectionTo = "C", connectionStrength = 0.5), list(connectionFrom = "C",
      connectionTo = "B", connectionStrength = -0.5)), name = "Time 2",
      plotNetwork = TRUE, centrality = TRUE))
  # Explicit nested defaults keep this regression independent of QML parsing.
  options$connectionList <- lapply(options$connectionList, function(occasion) {
    occasion$allConnections <- FALSE
    occasion$allConnectionStrengths <- list()
    occasion$edgeWeightTable <- FALSE
    occasion
  })
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("Network", dataset, options)

  tableRows <- function(name) {
    table <- results$results$centralityTableContainer$collection[[paste0("centralityTableContainer_", name)]]
    do.call(rbind, lapply(table$data, as.data.frame))
  }
  first <- tableRows("Time 1")
  second <- tableRows("Time 2")
  # Preserve the original signed sums; the new columns make cancellation explicit.
  expect_equal(first$degreeIn, c(0, 1, -.5))
  expect_equal(first$degreeOut, c(0, 0, .5))
  expect_equal(second$degreeIn, c(0, -1, .5))
  expect_equal(second$degreeOut, c(0, 0, -.5))
  for (rows in list(first, second)) {
    expect_equal(rows$name, c("A", "B", "C"))
    expect_equal(rows$severity, c(.8, .2, .5))
    expect_equal(rows$strengthOut, c(1, 0, .5))
    expect_equal(rows$strengthIn, c(0, 1, .5))
  }

	plotName <- results[["results"]][["networkPlotContainer"]][["collection"]][["networkPlotContainer_Time 1"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "time-1")

	plotName <- results[["results"]][["networkPlotContainer"]][["collection"]][["networkPlotContainer_Time 2"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "time-2")
})
