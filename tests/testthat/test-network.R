# Tests for analysis "How Are Symptoms Connected?"

test_that("Network specification works", {
  options <- analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$problemSavePath <- ""
  options$connectionSavePath <- ""
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
  set.seed(1)
  dataset <- NULL
  results <- runAnalysis("Network", dataset, options)

  table <- results[["results"]][["centralityTableContainer"]][["collection"]][["centralityTableContainer_Time 1"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0, 0, "A", 1, 0, "B", -0.5, 0.5, "C"))

	table <- results[["results"]][["centralityTableContainer"]][["collection"]][["centralityTableContainer_Time 2"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(0, 0, "A", -1, 0, "B", 0.5, -0.5, "C"))

	plotName <- results[["results"]][["networkPlotContainer"]][["collection"]][["networkPlotContainer_Time 1"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "time-1")

	plotName <- results[["results"]][["networkPlotContainer"]][["collection"]][["networkPlotContainer_Time 2"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "time-2")
})
