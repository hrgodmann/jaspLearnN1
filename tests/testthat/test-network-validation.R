.netValidationFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.netValidationEdge <- function(from = "A", to = "B", weight = .5) {
  list(connectionFrom = from, connectionTo = to, connectionStrength = weight)
}

.netValidationOptions <- function() {
  list(problems = list(list(problemName = "A", problemSeverity = .8),
                        list(problemName = "B", problemSeverity = 0),
                        list(problemName = "C", problemSeverity = 1)),
       connectionList = list(list(name = "Baseline", allConnections = FALSE,
         connections = list(.netValidationEdge()), allConnectionStrengths = list())))
}

.netValidate <- function(options) .netValidationFunction(".ln1NetValidateOptions")(options)

test_that("legacy Network ratings validate without changing names or canonical values", {
  options <- .netValidationOptions()
  options$problems[[1L]]$problemName <- " A & <B> "
  options$connectionList[[1L]]$connections[[1L]]$connectionFrom <- " A & <B> "
  original <- options
  expect_null(.netValidate(options))
  expect_identical(options, original)
  rated <- .netValidationFunction(".ln1NetSeverityRated")
  severity <- .netValidationFunction(".ln1NetSeverityValue")
  expect_true(all(vapply(options$problems, rated, logical(1))))
  expect_equal(vapply(options$problems, severity, numeric(1)), c(.8, 0, 1))
})

test_that("display scale maxima are whole numbers and never rescale stored ratings", {
  maximum <- .netValidationFunction(".ln1NetScaleMaximum")
  for (optionName in c("networkSeverityMaximum", "networkConnectionMaximum")) {
    expect_identical(maximum(list(), optionName), 1)
    for (value in c(1, 10, 100, 1000)) {
      options <- .netValidationOptions()
      options[[optionName]] <- value
      original <- options
      expect_equal(maximum(options, optionName), value)
      expect_null(.netValidate(options))
      expect_identical(options, original)
    }
    for (value in list(0, -1, .5, 1.5, 1001, Inf, -Inf, NA_real_, NaN,
                       "100", TRUE, numeric(), c(1, 2), 1 + 0i)) {
      options <- .netValidationOptions()
      options[[optionName]] <- value
      expect_error(.netValidate(options), "maximum.*whole number.*1.*1000")
    }
  }
})

test_that("problem and assessment names are nonblank and unique after trimming", {
  for (name in list(NULL, "", " \t\r\n", "\u00a0", NA_character_, 12, c("A", "B"))) {
    options <- .netValidationOptions()
    options$problems[[1L]]$problemName <- name
    expect_error(.netValidate(options), "nonblank name for problem 1", fixed = TRUE)
    options <- .netValidationOptions()
    options$connectionList[[1L]]$name <- name
    expect_error(.netValidate(options), "nonblank name for assessment 1", fixed = TRUE)
  }
  for (name in c("A", " A ", "\tA\n", "\u00a0A\u00a0")) {
    options <- .netValidationOptions()
    options$problems[[2L]]$problemName <- name
    expect_error(.netValidate(options), "Problem names must be unique", fixed = TRUE)
  }
  options <- .netValidationOptions()
  options$connectionList[[2L]] <- options$connectionList[[1L]]
  options$connectionList[[2L]]$name <- " Baseline "
  expect_error(.netValidate(options), "Assessment names must be unique", fixed = TRUE)
  options$connectionList[[2L]]$name <- "Follow-up"
  options$problems[[2L]]$problemName <- "a"
  expect_null(.netValidate(options))
})

test_that("the backend enforces the supported range of two to ten problems", {
  for (n in c(0L, 1L, 2L, 10L, 11L)) {
    options <- .netValidationOptions()
    options$problems <- lapply(seq_len(n), function(i)
      list(problemName = paste("Problem", i), problemSeverity = 0))
    if (n >= 2L && n <= 10L)
      expect_null(.netValidate(options))
    else
      expect_error(.netValidate(options), "between 2 and 10 problems", fixed = TRUE)
  }
})

test_that("unrated severity is missing rather than an entered zero", {
  rated <- .netValidationFunction(".ln1NetSeverityRated")
  severity <- .netValidationFunction(".ln1NetSeverityValue")
  options <- .netValidationOptions()
  options$problems[[1L]]$problemSeverityRated <- FALSE
  expect_false(rated(options$problems[[1L]]))
  expect_identical(severity(options$problems[[1L]]), NA_real_)
  options$problems[[1L]]$problemSeverity <- NULL
  expect_identical(severity(options$problems[[1L]]), NA_real_)
  expect_null(.netValidate(options))
  expect_identical(severity(options$problems[[2L]]), 0)
  options$problems[[1L]]$problemSeverityRated <- TRUE
  expect_error(.netValidate(options), "Enter a severity for problem 'A'", fixed = TRUE)

  for (flag in list(NA, 0, "false", logical(), c(TRUE, FALSE), NULL)) {
    options <- .netValidationOptions()
    options$problems[[1L]]["problemSeverityRated"] <- list(flag)
    expect_error(.netValidate(options), "Choose whether severity has been rated", fixed = TRUE)
  }
  for (value in list(-.1, 1.1, Inf, -Inf, NA_real_, NaN, "0.5", TRUE, c(.1, .2), .5 + 0i)) {
    for (flag in c(FALSE, TRUE)) {
      options <- .netValidationOptions()
      options$problems[[1L]]$problemSeverityRated <- flag
      options$problems[[1L]]$problemSeverity <- value
      expect_error(.netValidate(options), "Severity for problem 'A'.*finite value")
    }
  }
})

test_that("duplicate selected directed pairs are rejected even when their ratings are zero", {
  for (weight in list(.2, 0, NULL)) {
    options <- .netValidationOptions()
    options$connectionList[[1L]]$connections <- list(
      .netValidationEdge(weight = 0), .netValidationEdge(weight = weight))
    expect_error(.netValidate(options), "Baseline.*more than one connection from 'A' to 'B'")
  }
  options <- .netValidationOptions()
  options$connectionList[[1L]]$connections <- list(
    .netValidationEdge(), .netValidationEdge("B", "A", -.8))
  expect_null(.netValidate(options))
  # Duplicates are assessed independently in each assessment.
  options$connectionList[[2L]] <- options$connectionList[[1L]]
  options$connectionList[[2L]]$name <- "Follow-up"
  expect_null(.netValidate(options))
  # Existing readiness handles blank/unknown endpoints and self-links.
  for (edge in list(.netValidationEdge(to = ""), .netValidationEdge(to = "unknown"),
                    .netValidationEdge(to = "A"))) {
    options$connectionList[[1L]]$connections <- list(edge, edge)
    expect_null(.netValidate(options))
  }
})

test_that("active out-of-range ratings fail but incomplete ratings stay readiness concerns", {
  for (weight in c(-1, 0, 1)) {
    options <- .netValidationOptions()
    options$connectionList[[1L]]$connections[[1L]]$connectionStrength <- weight
    expect_null(.netValidate(options))
  }
  for (weight in c(-1.01, 1.01, 100)) {
    options <- .netValidationOptions()
    options$networkConnectionMaximum <- 100
    options$connectionList[[1L]]$connections[[1L]]$connectionStrength <- weight
    expect_error(.netValidate(options), "Baseline.*outside the selected connection scale")
  }
  for (weight in list(NULL, NA_real_, NaN, Inf, -Inf, "0.5", c(.1, .2), .5 + 0i)) {
    options <- .netValidationOptions()
    options$connectionList[[1L]]$connections[[1L]]$connectionStrength <- weight
    expect_null(.netValidate(options))
  }
  options <- .netValidationOptions()
  options$connectionList[[1L]]$allConnectionStrengths <- list(
    list(targets = list(list(connectionStrength = 0), list(connectionStrength = 9))))
  expect_null(.netValidate(options))
  options$connectionList[[1L]]$allConnections <- TRUE
  expect_error(.netValidate(options), "outside the selected connection scale", fixed = TRUE)
  options$connectionList[[1L]]$allConnectionStrengths <- lapply(1:3, function(i)
    list(targets = lapply(1:3, function(j) list(connectionStrength = if (i == j) Inf else 0))))
  options$connectionList[[1L]]$connections[[1L]]$connectionStrength <- 9
  expect_null(.netValidate(options))
  options$connectionList[[1L]]$allConnectionStrengths[[2L]]$targets[[3L]]$connectionStrength <- -1.1
  expect_error(.netValidate(options), "outside the selected connection scale", fixed = TRUE)
})

test_that("connection entry style accepts only the documented modes", {
  options <- .netValidationOptions()
  for (mode in c("signed", "magnitude")) {
    options$networkConnectionInput <- mode
    expect_null(.netValidate(options))
  }
  for (mode in list("", "absolute", "SIGNED", NA_character_, TRUE, c("signed", "magnitude"))) {
    options$networkConnectionInput <- mode
    expect_error(.netValidate(options), "Choose signed or magnitude entry", fixed = TRUE)
  }
})

test_that("native Network stops ambiguous identifiers before creating keyed results", {
  for (kind in c("problem", "assessment", "connection", "strength")) {
    options <- jaspTools::analysisOptions("Network")
    fixture <- .netValidationOptions()
    for (key in names(fixture)) options[[key]] <- fixture[[key]]
    options$enableIntroText <- FALSE
    options$networkSavePath <- ""
    options$networkExportSession <- "native-network-validation"
    options$connectionList[[1L]]$plotNetwork <- FALSE
    options$connectionList[[1L]]$centrality <- FALSE
    options$connectionList[[1L]]$edgeWeightTable <- FALSE
    if (kind == "problem") {
      options$problems[[2L]]$problemName <- " A "
      message <- "Problem names must be unique"
    } else if (kind == "assessment") {
      options$connectionList[[2L]] <- options$connectionList[[1L]]
      options$connectionList[[2L]]$name <- " Baseline "
      message <- "Assessment names must be unique"
    } else if (kind == "connection") {
      options$connectionList[[1L]]$connections[[2L]] <- .netValidationEdge(weight = 0)
      message <- "more than one connection"
    } else {
      options$connectionList[[1L]]$connections[[1L]]$connectionStrength <- 100
      message <- "outside the selected connection scale"
    }
    result <- jaspTools::runAnalysis("Network", NULL, options, view = FALSE)
    expect_identical(result$status, "validationError")
    expect_true(result$results$error)
    expect_match(result$results$errorMessage, message, fixed = TRUE)
  }
})
