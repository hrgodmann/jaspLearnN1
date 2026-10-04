.netCountsFunction <- function() getFromNamespace(".ln1NetConnectionCounts", "jaspLearnN1")

.netCountsNodes <- function() {
  data.frame(name = c("A", "B", "C", "D"), strength = c(.8, 0, NA_real_, 1))
}

.netCountsEdges <- function() {
  data.frame(from = c("A", "A", "B", "C"), to = c("B", "C", "C", "B"),
             weight = c(.6, -.6, -.4, -.6), stringsAsFactors = FALSE)
}

test_that("connection counts distinguish breadth from signed cancellation and weight magnitude", {
  nodes <- .netCountsNodes()
  edges <- .netCountsEdges()
  expected <- data.frame(name = nodes$name, countIn = c(0L, 2L, 2L, 0L),
                         countOut = c(2L, 1L, 1L, 0L))
  original <- edges
  actual <- .netCountsFunction()(edges, nodes)
  expect_identical(actual, expected)
  expect_identical(edges, original)
  edges$weight <- c(.Machine$double.eps, -.Machine$double.eps, -1, 1)
  expect_identical(.netCountsFunction()(edges, nodes), expected)
  nodes$strength <- c(0, .7, 1, NA_real_)
  expect_identical(.netCountsFunction()(edges, nodes), expected)
})

test_that("zero and empty networks retain all problems with zero counts", {
  nodes <- .netCountsNodes()
  edges <- .netCountsEdges()
  expected <- data.frame(name = nodes$name, countIn = integer(4L), countOut = integer(4L))
  expect_identical(.netCountsFunction()(NULL, nodes), expected)
  expect_identical(.netCountsFunction()(edges[FALSE, ], nodes), expected)
  edges$weight <- c(0, -0, 0, -0)
  expect_identical(.netCountsFunction()(edges, nodes), expected)
  expect_identical(.netCountsFunction()(edges, nodes[FALSE, ]),
                    data.frame(name = character(), countIn = integer(), countOut = integer()))
})

test_that("counts use unique valid directed pairs and preserve selected node order", {
  nodes <- .netCountsNodes()[c(4, 2, 1, 3), ]
  edges <- .netCountsEdges()
  duplicate <- edges[c(1, 2, 2), ]
  duplicate$weight <- c(-.5, .3, 0)
  invalid <- data.frame(from = c("A", "unknown", "A", "A", "A", "A", NA, "A"),
                        to = c("A", "D", "D", "D", "D", "D", "D", NA),
                        weight = c(.5, .5, Inf, NA, 2, -2, .5, .5))
  expected <- data.frame(name = nodes$name, countIn = c(0L, 2L, 0L, 2L),
                         countOut = c(0L, 1L, 2L, 1L))
  expect_identical(.netCountsFunction()(rbind(edges, duplicate, invalid), nodes), expected)
  expect_identical(.netCountsFunction()(edges[nrow(edges):1, ], nodes), expected)
  edges$weight <- as.character(edges$weight)
  expect_equal(.netCountsFunction()(edges, nodes)$countOut, integer(4L))
})

test_that("all-mode zero cells and manual omissions give the same nonzero connection counts", {
  nodes <- .netCountsNodes()
  manual <- .netCountsEdges()
  allMode <- expand.grid(from = nodes$name, to = nodes$name, stringsAsFactors = FALSE)
  allMode <- allMode[allMode$from != allMode$to, ]
  allMode$weight <- 0
  for (i in seq_len(nrow(manual))) {
    match <- allMode$from == manual$from[i] & allMode$to == manual$to[i]
    allMode$weight[match] <- manual$weight[i]
  }
  expect_identical(.netCountsFunction()(allMode, nodes), .netCountsFunction()(manual, nodes))
  # Direction is independent: A -> B and B -> A each count once.
  pair <- data.frame(from = c("A", "B"), to = c("B", "A"), weight = c(.5, -.5))
  expect_identical(.netCountsFunction()(pair, nodes),
                    data.frame(name = nodes$name, countIn = c(1L, 1L, 0L, 0L),
                               countOut = c(1L, 1L, 0L, 0L)))
})

test_that("native optional counts stay unscaled while connection sums use display units", {
  nodes <- .netCountsNodes()
  edges <- rbind(.netCountsEdges(), data.frame(from = "D", to = "A", weight = 0))
  for (enabled in c(FALSE, TRUE)) {
    for (maximum in c(1, 100)) {
      options <- jaspTools::analysisOptions("Network")
      options$enableIntroText <- FALSE
      options$networkSavePath <- ""
      options$networkExportSession <- "native-network-counts"
      options$networkSeverityMaximum <- maximum
      options$networkConnectionMaximum <- maximum
      options$networkConnectionCounts <- enabled
      options$problems <- lapply(seq_len(nrow(nodes)), function(i)
        list(problemName = nodes$name[i],
             problemSeverity = if (is.na(nodes$strength[i])) 0 else nodes$strength[i],
             problemSeverityRated = !is.na(nodes$strength[i])))
      connections <- lapply(seq_len(nrow(edges)), function(i)
        list(connectionFrom = edges$from[i], connectionTo = edges$to[i],
             connectionStrength = edges$weight[i]))
      options$connectionList <- list(list(name = "Baseline", connections = connections,
        allConnections = FALSE, allConnectionStrengths = list(),
        centrality = TRUE, plotNetwork = FALSE, edgeWeightTable = FALSE))
      result <- jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
      expect_identical(result$status, "complete")
      table <- result$results$centralityTableContainer$collection[["centralityTableContainer_Baseline"]]
      expect_identical(table$status, "complete")
      rows <- do.call(rbind, lapply(table$data, as.data.frame))
      expect_equal(rows$name, nodes$name)
      expect_equal(rows$degreeOut, c(0, -.4, -.6, 0) * maximum)
      fields <- vapply(table$schema$fields, `[[`, character(1), "name")
      expect_identical(all(c("countOut", "countIn") %in% fields), enabled)
      if (enabled) {
        expect_equal(rows$countOut, c(2L, 1L, 1L, 0L))
        expect_equal(rows$countIn, c(0L, 2L, 2L, 0L))
      }
    }
  }
})
