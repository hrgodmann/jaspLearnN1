.netKeyFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.netKeyRows <- function() {
  list(list(value = "B", targets = list(list(value = "A", connectionStrength = -.3),
                                       list(value = "B", connectionStrength = Inf))),
       list(value = "A", targets = list(list(value = "B", connectionStrength = .7),
                                       list(value = "A", connectionStrength = Inf))))
}

.netKeyOptions <- function(rows = .netKeyRows()) {
  options <- jaspTools::analysisOptions("Network")
  options$enableIntroText <- FALSE
  options$networkSavePath <- ""
  options$problems <- list(list(problemName = "A", problemSeverity = .8),
                           list(problemName = "B", problemSeverity = .2))
  options$connectionList <- list(list(name = "Assessment", allConnections = TRUE,
    allConnectionStrengths = rows, connections = list(), plotNetwork = FALSE,
    centrality = TRUE, edgeWeightTable = TRUE))
  options
}

test_that("native-shaped keyed matrices follow identities despite independent row order", {
  options <- .netKeyOptions()
  expect_true(.netKeyFunction(".ln1NetEdgelistReady")(options$connectionList[[1]], c("A", "B")))
  expect_null(.netKeyFunction(".ln1NetValidateOptions")(options))
  result <- jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
  expect_identical(result$status, "complete")
  table <- result$results$edgeWeightTableContainer$collection$edgeWeightTableContainer_Assessment
  rows <- do.call(rbind, lapply(table$data, as.data.frame))
  expect_equal(rows$from, c("A", "B"))
  expect_equal(rows$to, c("B", "A"))
  expect_equal(as.numeric(rows$weight), c(.7, -.3))
})

test_that("all-pairs identity rejects partial, invalid, duplicate and unknown keys", {
  changes <- list(
    function(x) { x[[1]]$value <- NULL; x },
    function(x) { x[[1]]$targets[[1]]$value <- NULL; x },
    function(x) { x[[1]]$value <- "A"; x },
    function(x) { x[[1]]$targets[[1]]$value <- "B"; x },
    function(x) { x[[1]]$value <- "Unknown"; x },
    function(x) { x[[1]]$targets[[1]]$value <- "Unknown"; x },
    function(x) { x[[1]]$value <- 1; x },
    function(x) { x[[1]]$targets[[1]]$value <- NA_character_; x })
  for (change in changes) {
    options <- .netKeyOptions(change(.netKeyRows()))
    expect_false(.netKeyFunction(".ln1NetEdgelistReady")(options$connectionList[[1]], c("A", "B")))
    expect_error(.netKeyFunction(".ln1NetValidateOptions")(options), "problem keys", fixed = TRUE)
  }
})

test_that("wholly unkeyed legacy matrices retain positional meaning and incomplete lists wait", {
  normalize <- .netKeyFunction(".ln1NetAllConnectionMatrix")
  keyed <- normalize(.netKeyRows(), c("A", "B"))$rows
  unkeyed <- lapply(keyed, function(row) {
    row$value <- NULL
    row$targets <- lapply(row$targets, function(target) { target$value <- NULL; target })
    row
  })
  expect_identical(normalize(unkeyed, c("A", "B"))$rows, unkeyed)
  for (rows in list(.netKeyRows()[1], list(), NULL)) {
    matrix <- normalize(rows, c("A", "B"))
    expect_false(matrix$invalidKeys)
    expect_null(matrix$rows)
  }
})

test_that("preset Unicode normalization follows the same fixtures as the QML validator", {
  fixtures <- jsonlite::fromJSON(test_path("..", "fixtures", "network-preset-normalization.json"),
                                 simplifyVector = FALSE)
  validate <- .netKeyFunction(".ln1NetValidatePreset")
  for (fixture in fixtures) {
    if (fixture$valid)
      expect_equal(validate(fixture$input), fixture$expected, info = fixture$description)
    else
      expect_error(validate(fixture$input), info = fixture$description)
  }
})
