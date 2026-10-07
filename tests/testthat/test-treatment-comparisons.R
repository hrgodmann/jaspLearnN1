# Endpoint and trend references use the paper's reversed within-phase time:
# zero at the scheduled endpoint, positive values for earlier occasions.
# The module instead fits global time, so agreement checks a change of basis.

.treatCompFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.treatCompOptions <- function() {
  options <- jaspTools::analysisOptions("Treatment")
  options$enableIntroText <- FALSE
  options$inputType <- "loadData"
  options$dependent <- "symptom"
  options$time <- "clock"
  options$phase <- "stage"
  options$plotData <- FALSE
  options$plotAnalysis <- FALSE
  options$coefficientsTable <- FALSE
  options$autocorrelationTable <- FALSE
  options$coefficientCiLevel <- .95
  options$phaseComparisons <- TRUE
  options$phaseSummary <- TRUE
  options$comparisonPhase <- ""
  options$referencePhase <- ""
  options
}

.treatCompFixture <- function() {
  set.seed(7134)
  n <- 90L
  clock <- 100 + 2 * (seq_len(n) - 1L)
  stage <- rep(c("Zulu baseline", "Act: treatment", "Later / follow-up"), each = 30L)
  end <- ave(clock, stage, FUN = max)
  y <- rep(c(12, 8, 9), each = 30L) +
    rep(c(.05, -.10, .02), each = 30L) * (clock - end) +
    as.numeric(stats::arima.sim(list(ar = .45), n = n, sd = .4))
  data.frame(symptom = y, clock = clock, stage = factor(stage))
}

.treatCompPrepare <- function(data, options) {
  .treatCompFunction(".ln1TreatPrepareData")(data, options)
}

.treatCompFit <- function(prepared, options) {
  .treatCompFunction(".ln1TreatEstimateModelHelper")(prepared, options)
}

.treatCompOracle <- function(data, options) {
  simulation <- identical(options$inputType, "simulateData")
  yName <- if (simulation) "y" else options$dependent
  timeName <- if (simulation) "time" else options$time
  phaseName <- if (simulation) "phase" else options$phase
  chronology <- if (simulation) "t" else options$time
  ordered <- data[order(data[[chronology]]), , drop = FALSE]
  time <- ordered[[timeName]]
  phaseNames <- unique(as.character(ordered[[phaseName]]))
  phase <- factor(ordered[[phaseName]], levels = phaseNames)
  end <- vapply(phaseNames, function(label) max(time[phase == label]), numeric(1))
  referenceData <- data.frame(y = ordered[[yName]], phase = phase,
    reversed = unname(end[as.character(phase)]) - time,
    occasion = seq_len(nrow(ordered)))
  reference <- nlme::gls(y ~ 0 + phase + phase:reversed, data = referenceData,
    correlation = nlme::corAR1(form = ~ occasion), method = "REML",
    na.action = stats::na.exclude)
  endpoint <- stats::model.matrix(~ 0 + phase + phase:reversed,
    data.frame(phase = factor(phaseNames, levels = phaseNames), reversed = 0))
  # Forward-time slopes have the opposite sign from reversed-time slopes.
  slope <- stats::model.matrix(~ 0 + phase + phase:reversed,
    data.frame(phase = factor(phaseNames, levels = phaseNames), reversed = -1)) - endpoint
  rownames(endpoint) <- rownames(slope) <- phaseNames
  list(model = reference, endpoint = endpoint, slope = slope,
       phases = phaseNames, end = unname(end),
       chronologicalEnd = vapply(phaseNames, function(label)
         max(ordered[[chronology]][phase == label]), numeric(1)))
}

.treatCompContrast <- function(oracle, contrast, level = .95) {
  estimate <- as.numeric(contrast %*% stats::coef(oracle$model))
  SE <- sqrt(as.numeric(contrast %*% stats::vcov(oracle$model) %*% contrast))
  df <- stats::nobs(oracle$model) - length(stats::coef(oracle$model))
  statistic <- estimate / SE
  critical <- stats::qt((1 + level) / 2, df = df)
  c(estimate = estimate, SE = SE, df = df, t = statistic,
    p = 2 * stats::pt(-abs(statistic), df = df),
    lower = estimate - critical * SE, upper = estimate + critical * SE)
}

.treatCompRows <- function(table) {
  do.call(rbind, lapply(table$data, function(row)
    as.data.frame(row, stringsAsFactors = FALSE)))
}

.treatCompNativeText <- function(value) {
  # Native JASP serializes string cells as escaped HTML for safe display.
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  gsub(">", "&gt;", value, fixed = TRUE)
}

.treatCompSummary <- function(prepared, model, options) {
  .treatCompFunction(".ln1TreatPhaseSummaries")(prepared, model, options)
}

.treatCompResults <- function(prepared, model, options) {
  .treatCompFunction(".ln1TreatComparisonResults")(prepared, model, options)
}

.treatCompExpectContrast <- function(row, oracle, contrast, level = .95) {
  expected <- .treatCompContrast(oracle, contrast, level)
  for (column in names(expected))
    expect_equal(row[[column]], unname(expected[[column]]), tolerance = 1e-5,
                 info = paste("Independent reversed-time reference:", column))
}

test_that("phase endpoints and forward trends agree with reversed-time GLS", {
  data <- .treatCompFixture()
  options <- .treatCompOptions()
  prepared <- .treatCompPrepare(data[90:1, ], options)
  model <- .treatCompFit(prepared, options)
  oracle <- .treatCompOracle(data, options)
  summary <- .treatCompSummary(prepared, model, options)
  expect_equal(summary$phase, rep(oracle$phases, each = 2L))
  expect_equal(summary$quantity, rep(c("endpoint", "slope"), 3L))
  expect_equal(summary$startTime, rep(data$clock[c(1L, 31L, 61L)], each = 2L))
  expect_equal(summary$endTime, rep(data$clock[c(30L, 60L, 90L)], each = 2L))
  for (i in seq_along(oracle$phases)) {
    .treatCompExpectContrast(summary[2L * i - 1L, ], oracle, oracle$endpoint[i, ])
    .treatCompExpectContrast(summary[2L * i, ], oracle, oracle$slope[i, ])
  }

  result <- .treatCompResults(prepared, model, options)
  expect_equal(result$quantity, c("endpoint", "slope"))
  # Defaults follow the observed chronology despite alphabetical factor levels.
  expect_equal(result$comparisonPhase, rep("Act: treatment", 2L))
  expect_equal(result$referencePhase, rep("Zulu baseline", 2L))
  expect_equal(result$comparisonEnd, rep(data$clock[60L], 2L))
  expect_equal(result$referenceEnd, rep(data$clock[30L], 2L))
  .treatCompExpectContrast(result[1L, ], oracle, oracle$endpoint[2L, ] - oracle$endpoint[1L, ])
  .treatCompExpectContrast(result[2L, ], oracle, oracle$slope[2L, ] - oracle$slope[1L, ])
})

test_that("selected phase comparisons are invariant to factor reference and contrast coding", {
  data <- .treatCompFixture()
  options <- .treatCompOptions()
  options$comparisonPhase <- "Later / follow-up"
  options$referencePhase <- "Act: treatment"
  options$coefficientCiLevel <- .80
  prepared <- .treatCompPrepare(data, options)
  model <- .treatCompFit(prepared, options)
  original <- .treatCompResults(prepared, model, options)
  originalSummary <- .treatCompSummary(prepared, model, options)
  oracle <- .treatCompOracle(data, options)
  .treatCompExpectContrast(original[1L, ], oracle,
    oracle$endpoint[3L, ] - oracle$endpoint[2L, ], level = .80)
  .treatCompExpectContrast(original[2L, ], oracle,
    oracle$slope[3L, ] - oracle$slope[2L, ], level = .80)

  # A small recording table checks contextual text without constructing native
  # JASP objects outside the initialized runAnalysis context.
  contextText <- function(fit) {
    footnotes <- character()
    table <- list(addFootnote = function(text) footnotes <<- c(footnotes, text))
    .treatCompFunction(".ln1TreatCoefficientContext")(table, fit, options)
    paste(footnotes, collapse = " ")
  }
  expect_match(contextText(model), "Coefficient reference phase: Act: treatment", fixed = TRUE)

  for (sumContrasts in c(FALSE, TRUE)) {
    changed <- data
    changed$stage <- factor(changed$stage,
      levels = c("Later / follow-up", "Zulu baseline", "Act: treatment"))
    if (sumContrasts) stats::contrasts(changed$stage) <- stats::contr.sum(3L)
    prepared <- .treatCompPrepare(changed[90:1, ], options)
    fit <- .treatCompFit(prepared, options)
    expect_equal(.treatCompResults(prepared, fit, options), original, tolerance = 1e-5)
    expect_equal(.treatCompSummary(prepared, fit, options), originalSummary, tolerance = 1e-5)
    if (sumContrasts) {
      expect_match(contextText(fit), "without a single treatment-coded reference phase", fixed = TRUE)
    } else {
      expect_match(contextText(fit), "Coefficient reference phase: Later / follow-up", fixed = TRUE)
    }
  }
})

test_that("a blank final outcome does not move a phase's scheduled endpoint", {
  data <- .treatCompFixture()
  data$symptom[c(8L, 30L, 60L, 90L)] <- NA_real_
  options <- .treatCompOptions()
  prepared <- .treatCompPrepare(data, options)
  model <- .treatCompFit(prepared, options)
  oracle <- .treatCompOracle(data, options)
  summary <- .treatCompSummary(prepared, model, options)
  result <- .treatCompResults(prepared, model, options)
  expect_equal(summary$endTime, rep(data$clock[c(30L, 60L, 90L)], each = 2L))
  expect_equal(result$comparisonEnd, rep(data$clock[60L], 2L))
  expect_equal(result$referenceEnd, rep(data$clock[30L], 2L))
  for (i in seq_along(oracle$phases))
    .treatCompExpectContrast(summary[2L * i - 1L, ], oracle, oracle$endpoint[i, ])
  .treatCompExpectContrast(result[1L, ], oracle, oracle$endpoint[2L, ] - oracle$endpoint[1L, ])
})

test_that("time offsets preserve endpoints and changing units rescales only trends", {
  data <- .treatCompFixture()
  options <- .treatCompOptions()
  prepared <- .treatCompPrepare(data, options)
  fit <- .treatCompFit(prepared, options)
  original <- .treatCompResults(prepared, fit, options)
  originalSummary <- .treatCompSummary(prepared, fit, options)
  data$clock <- 1000 + 7 * data$clock
  prepared <- .treatCompPrepare(data, options)
  changedFit <- .treatCompFit(prepared, options)
  changed <- .treatCompResults(prepared, changedFit, options)
  changedSummary <- .treatCompSummary(prepared, changedFit, options)
  for (column in c("estimate", "SE", "lower", "upper")) {
    expect_equal(changed[[column]][1L], original[[column]][1L], tolerance = 1e-5)
    expect_equal(changed[[column]][2L], original[[column]][2L] / 7, tolerance = 1e-5)
    endpoint <- originalSummary$quantity == "endpoint"
    expect_equal(changedSummary[[column]][endpoint], originalSummary[[column]][endpoint],
                 tolerance = 1e-5)
    expect_equal(changedSummary[[column]][!endpoint], originalSummary[[column]][!endpoint] / 7,
                 tolerance = 1e-5)
  }
  for (column in c("df", "t", "p"))
    expect_equal(changed[[column]], original[[column]], tolerance = 1e-5)
  expect_equal(changed$comparisonEnd, 1000 + 7 * original$comparisonEnd)
  expect_equal(changed$referenceEnd, 1000 + 7 * original$referenceEnd)
})

test_that("simulation comparisons use phase-local regression time and continuous endpoint labels", {
  loaded <- .treatCompFixture()
  data <- data.frame(y = loaded$symptom, time = rep(seq_len(30L), 3L),
                     t = seq_len(90L), phase = loaded$stage)
  options <- .treatCompOptions()
  options$inputType <- "simulateData"
  prepared <- .treatCompPrepare(data[90:1, ], options)
  fit <- .treatCompFit(prepared, options)
  oracle <- .treatCompOracle(data, options)
  summary <- .treatCompSummary(prepared, fit, options)
  result <- .treatCompResults(prepared, fit, options)
  expect_equal(summary$startTime, rep(c(1L, 31L, 61L), each = 2L))
  expect_equal(summary$endTime, rep(c(30L, 60L, 90L), each = 2L))
  expect_equal(result$comparisonEnd, rep(60L, 2L))
  expect_equal(result$referenceEnd, rep(30L, 2L))
  for (i in seq_along(oracle$phases)) {
    .treatCompExpectContrast(summary[2L * i - 1L, ], oracle, oracle$endpoint[i, ])
    .treatCompExpectContrast(summary[2L * i, ], oracle, oracle$slope[i, ])
  }
  .treatCompExpectContrast(result[1L, ], oracle, oracle$endpoint[2L, ] - oracle$endpoint[1L, ])
  .treatCompExpectContrast(result[2L, ], oracle, oracle$slope[2L, ] - oracle$slope[1L, ])
})

test_that("supplied data and special column and phase names are compared literally", {
  data <- .treatCompFixture()
  phaseNames <- c("<baseline & α>", "Tx: 'A' / β", "later . [x]")
  data$stage <- factor(rep(phaseNames, each = 30L))
  names(data) <- c("Symptom value", ".", "Phase key")
  options <- .treatCompOptions()
  options$dependent <- names(data)[1L]
  options$time <- names(data)[2L]
  options$phase <- names(data)[3L]
  options$comparisonPhase <- phaseNames[2L]
  options$referencePhase <- phaseNames[1L]
  prepared <- .treatCompFunction(".ln1TreatData")(list(), data[90:1, ], options, ready = TRUE)
  expect_equal(prepared[[options$dependent]], data[[options$dependent]])
  fit <- .treatCompFit(prepared, options)
  oracle <- .treatCompOracle(data, options)
  actual <- .treatCompResults(prepared, fit, options)
  expect_equal(actual$comparisonPhase, rep(phaseNames[2L], 2L))
  expect_equal(actual$referencePhase, rep(phaseNames[1L], 2L))
  .treatCompExpectContrast(actual[1L, ], oracle, oracle$endpoint[2L, ] - oracle$endpoint[1L, ])
  .treatCompExpectContrast(actual[2L, ], oracle, oracle$slope[2L, ] - oracle$slope[1L, ])

  result <- jaspTools::runAnalysis("Treatment", data[90:1, ], options, view = FALSE)
  expect_identical(result$status, "complete")
  comparisons <- .treatCompRows(result$results$phaseComparisons)
  summaries <- .treatCompRows(result$results$phaseSummary)
  expect_equal(comparisons$comparisonPhase, .treatCompNativeText(actual$comparisonPhase))
  expect_equal(comparisons$referencePhase, .treatCompNativeText(actual$referencePhase))
  expect_equal(comparisons$estimate, actual$estimate, tolerance = 1e-6)
  expect_equal(comparisons$SE, actual$SE, tolerance = 1e-6)
  expect_equal(summaries$phase, .treatCompNativeText(rep(phaseNames, each = 2L)))
})

test_that("phase comparison selection is explicit and rejects missing or identical phases", {
  data <- .treatCompFixture()
  options <- .treatCompOptions()
  prepared <- .treatCompPrepare(data, options)
  fit <- .treatCompFit(prepared, options)
  default <- .treatCompResults(prepared, fit, options)
  options$comparisonPhase <- options$referencePhase <- NULL
  expect_equal(.treatCompResults(prepared, fit, options), default)
  for (key in c("comparisonPhase", "referencePhase")) {
    for (badSelection in list("A phase from another dataset", NA_character_,
                             c("Zulu baseline", "Act: treatment"), 1)) {
      invalid <- options
      invalid[[key]] <- badSelection
      expect_error(.treatCompResults(prepared, fit, invalid), "phase|select|available",
                   ignore.case = TRUE)
    }
  }
  options$comparisonPhase <- options$referencePhase <- "Act: treatment"
  expect_error(.treatCompResults(prepared, fit, options), "different|distinct|same|phase",
               ignore.case = TRUE)
})

test_that("a reference-only collision explains the chronological automatic choices", {
  # Deliberately nonalphabetical order: automatic choices follow chronology.
  phases <- data.frame(phase = c("Zulu baseline", "Act: treatment", "Later / follow-up"))
  select <- .treatCompFunction(".ln1TreatSelectComparison")
  options <- .treatCompOptions()
  expect_identical(select(phases, options), c(comparison = 2L, reference = 1L))

  options$referencePhase <- phases$phase[2L]
  message <- tryCatch(select(phases, options), error = conditionMessage)
  expect_match(message, "Select two different phases", fixed = TRUE)
  expect_match(message, "second phase in chronological order for Compared phase", fixed = TRUE)
  expect_match(message, "first for Reference phase", fixed = TRUE)

  # Fixing the explicit selection keeps the automatic compared phase at two.
  options$referencePhase <- phases$phase[3L]
  expect_identical(select(phases, options), c(comparison = 2L, reference = 3L))
  options$comparisonPhase <- phases$phase[1L]
  expect_identical(select(phases, options), c(comparison = 1L, reference = 3L))
})

test_that("native coefficient and endpoint footnotes escape user-provided labels", {
  data <- .treatCompFixture()
  labels <- c('Baseline <α> & "A"', "Treatment > β & 'B'", "Later <γ>")
  data$stage <- factor(rep(labels, each = 30L), levels = labels)
  timeName <- 'Clock <day> & "visit"'
  names(data)[names(data) == "clock"] <- timeName
  options <- .treatCompOptions()
  options$time <- timeName
  options$coefficientsTable <- TRUE
  options$comparisonPhase <- labels[2L]
  options$referencePhase <- labels[1L]

  result <- jaspTools::runAnalysis("Treatment", data, options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_identical(result$results$coefTable$status, "complete")
  expect_identical(result$results$phaseComparisons$status, "complete")
  footnotes <- function(table)
    paste(vapply(table$footnotes, `[[`, character(1), "text"), collapse = " ")
  coefficientText <- footnotes(result$results$coefTable)
  endpointText <- footnotes(result$results$phaseComparisons)
  expect_match(coefficientText,
    "Coefficient reference phase: Baseline &lt;α&gt; &amp; &quot;A&quot;.", fixed = TRUE)
  expect_match(coefficientText,
    "Regression time 0 means Clock &lt;day&gt; &amp; &quot;visit&quot; = 0", fixed = TRUE)
  expect_match(endpointText,
    "Endpoint times: Treatment &gt; β &amp; &#39;B&#39; = 218; Baseline &lt;α&gt; &amp; &quot;A&quot; = 158",
    fixed = TRUE)
  expect_false(grepl("<α>|<day>|&amp;lt;|&amp;quot;", coefficientText))
  expect_false(grepl("<α>|&amp;lt;|&amp;#39;", endpointText))
})

test_that("repeated phase episodes are rejected before fitting any pooled model", {
  data <- .treatCompFixture()
  data$stage <- factor(rep(c("A", "B", "A", "C", "D"), each = 18L))
  options <- .treatCompOptions()
  # Even an unselected repeated phase would otherwise alter the shared fit.
  options$comparisonPhase <- "D"
  options$referencePhase <- "C"
  expect_error(.treatCompPrepare(data, options), "repeat|contiguous|episode|phase",
               ignore.case = TRUE)
  options$coefficientsTable <- options$autocorrelationTable <- TRUE
  options$plotData <- options$plotAnalysis <- TRUE
  result <- jaspTools::runAnalysis("Treatment", data, options, view = FALSE)
  expect_identical(result$status, "validationError")
  expect_match(result$results$errorMessage, "distinct names", fixed = TRUE)
  expect_null(result$results$coefTable)
  expect_null(result$results$analysisPlot)
})

test_that("a comparison-selection error preserves other valid Treatment results", {
  data <- .treatCompFixture()
  options <- .treatCompOptions()
  options$comparisonPhase <- "A phase from another dataset"
  options$coefficientsTable <- options$autocorrelationTable <- TRUE
  result <- jaspTools::runAnalysis("Treatment", data, options, view = FALSE)
  expect_identical(result$status, "complete")
  expect_identical(result$results$phaseComparisons$status, "error")
  expect_match(result$results$phaseComparisons$error$errorMessage, "phase.*unavailable")
  expect_length(result$results$phaseComparisons$data, 0L)
  expect_length(result$results$coefTable$data, 6L)
  expect_length(result$results$autoCorTable$data, 1L)
  expect_length(result$results$phaseSummary$data, 6L)
  expect_true(all(is.finite(.treatCompRows(result$results$coefTable)$coef)))
})

test_that("native phase-comparison intervals honor CI level and name their coefficient reference", {
  data <- .treatCompFixture()
  options <- .treatCompOptions()
  options$comparisonPhase <- "Later / follow-up"
  options$referencePhase <- "Zulu baseline"
  options$coefficientsTable <- TRUE
  tables <- list()
  for (level in c(.8, .95)) {
    options$coefficientCiLevel <- level
    result <- jaspTools::runAnalysis("Treatment", data, options, view = FALSE)
    expect_identical(result$status, "complete")
    expect_identical(result$results$phaseComparisons$status, "complete")
    rows <- .treatCompRows(result$results$phaseComparisons)
    expect_equal(rows$comparisonPhase, rep(options$comparisonPhase, 2L))
    expect_equal(rows$referencePhase, rep(options$referencePhase, 2L))
    margin <- stats::qt((1 + level) / 2, df = rows$df) * rows$SE
    expect_equal(rows$lower, rows$estimate - margin, tolerance = 1e-7)
    expect_equal(rows$upper, rows$estimate + margin, tolerance = 1e-7)
    footnotes <- paste(vapply(result$results$coefTable$footnotes, `[[`, character(1), "text"),
                        collapse = " ")
    # Raw coefficients retain their own coding; the selected reference above is
    # for endpoint/slope comparisons and must not relabel those coefficients.
    expect_match(footnotes, "Coefficient reference phase: Act: treatment", fixed = TRUE)
    expect_match(footnotes, "regression time 0", fixed = TRUE)
    tables[[as.character(level)]] <- rows
  }
  expect_equal(tables[["0.8"]]$estimate, tables[["0.95"]]$estimate, tolerance = 1e-7)
  expect_equal(tables[["0.8"]]$SE, tables[["0.95"]]$SE, tolerance = 1e-7)
  expect_true(all(tables[["0.95"]]$lower < tables[["0.8"]]$lower))
  expect_true(all(tables[["0.95"]]$upper > tables[["0.8"]]$upper))
})

test_that("adjacent simulated episodes sharing a label are not treated as one endpoint", {
  loaded <- .treatCompFixture()
  data <- data.frame(y = loaded$symptom, time = rep(seq_len(30L), 3L),
                     t = seq_len(90L), phase = rep(c("A", "A", "B"), each = 30L))
  options <- .treatCompOptions()
  options$inputType <- "simulateData"
  options$comparisonPhase <- "B"
  options$referencePhase <- "A"
  expect_error(.treatCompPrepare(data, options), "restart|distinct|episode|phase",
               ignore.case = TRUE)
})
