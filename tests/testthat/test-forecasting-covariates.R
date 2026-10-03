# Regression and integration tests for regression with ARIMA errors.
# Numerical reference fits deliberately bypass the module's data/model helpers.

.foreCovFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.foreCovOptions <- function(covariates = "stress", d = 0L, p = 0L, q = 0L) {
  options <- jaspTools::analysisOptions("Forecasting")
  options$enableIntroText <- FALSE
  options$inputType <- "loadData"
  options$dependent <- "symptom"
  options$time <- "clock"
  options$covariates <- covariates
  options$modelSpecification <- "custom"
  options$p <- p
  options$d <- d
  options$q <- q
  options$plotData <- FALSE
  options$forecastLength <- 0L
  options$forecastTable <- FALSE
  options$forecastTimeSeries <- FALSE
  options$forecastSave <- ""
  options
}

.foreCovFixture <- function(n = 90L, h = 4L, d = 0L) {
  set.seed(7041 + d)
  stress <- rnorm(n + h)
  sleep <- rnorm(n + h)
  noise <- as.numeric(stats::arima.sim(list(ar = .35), n = n, sd = .3))
  if (d > 0L)
    for (i in seq_len(d)) noise <- cumsum(noise)
  data.frame(symptom = c(10 + 3 * stress[seq_len(n)] - 2 * sleep[seq_len(n)] + noise,
                         rep(NA_real_, h)),
             clock = 100 + 7 * (seq_len(n + h) - 1L),
             stress = stress, sleep = sleep)
}

.foreCovPrepare <- function(data, options) {
  .foreCovFunction(".ln1ForePrepareData")(data, options)
}

.foreCovFit <- function(data, options) {
  .foreCovFunction(".ln1ForeEstimateModelHelper")(data, options)
}

.foreCovPredict <- function(data, fit, options) {
  .foreCovFunction(".ln1ForeForecastHelper")(data, fit, options)
}

.foreCovRows <- function(table) {
  do.call(rbind, lapply(table$data, function(row) {
    as.data.frame(row, stringsAsFactors = FALSE)
  }))
}

.foreCovRun <- function(data, options, encoded = FALSE) {
  if (encoded) {
    options$dependent.types <- "scale"
    options$time.types <- if (nzchar(options$time)) "scale" else character()
    options$covariates.types <- rep("scale", length(options$covariates))
    encodedInput <- jaspTools:::encodeOptionsAndDataset(options, data)
    expect_gt(nrow(encodedInput$encodingMap), 0L)
    expect_setequal(encodedInput$encodingMap$original,
                    unique(c(options$dependent, options$time, options$covariates)))
    expect_false(identical(encodedInput$dataset, data))

    # jaspTools returns an encoding map but does not install a name decoder.
    # Emulate Desktop's callback contract for this run; this is not GUI testing.
    decoderName <- ".decodeColNamesLax"
    hadDecoder <- exists(decoderName, envir = .GlobalEnv, inherits = FALSE)
    oldDecoder <- if (hadDecoder) get(decoderName, envir = .GlobalEnv) else NULL
    on.exit({
      if (hadDecoder) assign(decoderName, oldDecoder, envir = .GlobalEnv)
      else rm(list = decoderName, envir = .GlobalEnv)
    }, add = TRUE)
    encodingMap <- encodedInput$encodingMap
    assign(decoderName, function(value) {
      index <- match(value, encodingMap$encoded)
      matched <- !is.na(index)
      value[matched] <- encodingMap$original[index[matched]]
      value
    }, envir = .GlobalEnv)
    expect_equal(jaspBase::decodeColNames(encodedInput$options$covariates),
                 options$covariates)
    options <- encodedInput$options
    data <- encodedInput$dataset
  }
  jaspTools::runAnalysis("Forecasting", data, options, view = FALSE,
                         encodedDataset = encoded)
}

test_that("forecast data preserve predictor values while sorting the time grid", {
  data <- .foreCovFixture()
  options <- .foreCovOptions(c("stress", "sleep"))
  scrambled <- data[c(seq(2, nrow(data), 2), seq(1, nrow(data), 2)), ]
  prepared <- .foreCovPrepare(scrambled, options)
  expect_equal(prepared$y, data$symptom)
  expect_equal(prepared$t, data$clock)
  expect_equal(prepared$xreg1, data$stress)
  expect_equal(prepared$xreg2, data$sleep)
  historical <- .foreCovFunction(".ln1ForeHistoricalData")(prepared)
  expect_equal(nrow(historical), 90L)
  expect_equal(tail(historical$t, 1), data$clock[90])

  options$time <- ""
  rowOrder <- .foreCovPrepare(scrambled, options)
  expect_equal(rowOrder$t, seq_len(nrow(scrambled)))
  expect_equal(rowOrder$y, scrambled$symptom)
})

test_that("zero, one and multiple predictors match independent manual ARIMA fits", {
  data <- .foreCovFixture(h = 0L)
  for (predictors in list(character(), "stress", c("stress", "sleep"))) {
    options <- .foreCovOptions(predictors, p = 1L)
    fit <- .foreCovFit(.foreCovPrepare(data, options), options)
    xreg <- if (length(predictors)) as.matrix(data[predictors]) else NULL
    if (!is.null(xreg)) colnames(xreg) <- paste0("xreg", seq_along(predictors))
    reference <- forecast::Arima(data$symptom, order = c(1, 0, 0),
                                 xreg = xreg, include.constant = TRUE)
    expect_equal(stats::coef(fit), stats::coef(reference), tolerance = 1e-7)
    expect_equal(fit$var.coef, reference$var.coef, tolerance = 1e-7)
    expect_equal(as.numeric(stats::fitted(fit)),
                 as.numeric(stats::fitted(reference)), tolerance = 1e-7)
  }
})

test_that("automatic model selection includes all selected predictors", {
  data <- .foreCovFixture(h = 0L)
  options <- .foreCovOptions(c("stress", "sleep"))
  options$modelSpecification <- "auto"
  options$modelSpecificationAutoIc <- "aicc"
  fit <- .foreCovFit(.foreCovPrepare(data, options), options)
  xreg <- as.matrix(data[c("stress", "sleep")])
  colnames(xreg) <- c("xreg1", "xreg2")
  reference <- forecast::auto.arima(data$symptom, xreg = xreg,
                                   allowdrift = TRUE, allowmean = TRUE, ic = "aicc")
  expect_equal(forecast::arimaorder(fit), forecast::arimaorder(reference))
  expect_equal(stats::coef(fit), stats::coef(reference), tolerance = 1e-7)
  expect_true(all(c("xreg1", "xreg2") %in% names(stats::coef(fit))))
})

test_that("internal missing outcomes preserve occasions and predictor alignment", {
  data <- .foreCovFixture()
  data$symptom[c(8, 37, 64)] <- NA_real_
  options <- .foreCovOptions(c("stress", "sleep"), p = 1L)
  options$forecastLength <- 4L
  prepared <- .foreCovPrepare(data, options)
  historical <- .foreCovFunction(".ln1ForeHistoricalData")(prepared)
  expect_equal(nrow(historical), 90L)
  expect_equal(historical$t, data$clock[1:90])
  expect_equal(which(is.na(historical$y)), c(8L, 37L, 64L))
  xreg <- as.matrix(data[1:90, c("stress", "sleep")])
  colnames(xreg) <- c("xreg1", "xreg2")
  reference <- forecast::Arima(data$symptom[1:90], order = c(1, 0, 0),
                               xreg = xreg, include.constant = TRUE)
  fit <- .foreCovFit(prepared, options)
  expect_equal(stats::coef(fit), stats::coef(reference), tolerance = 1e-7)
  expect_equal(fit$var.coef, reference$var.coef, tolerance = 1e-7)
  originalPrediction <- .foreCovPredict(prepared, fit, options)

  options$covariates <- rev(options$covariates)
  reversedData <- .foreCovPrepare(data, options)
  reversedFit <- .foreCovFit(reversedData, options)
  expect_equal(unname(stats::coef(reversedFit)[c("xreg2", "xreg1")]),
               unname(stats::coef(fit)[c("xreg1", "xreg2")]), tolerance = 1e-5)
  expect_equal(.foreCovPredict(reversedData, reversedFit, options)$y,
               originalPrediction$y, tolerance = 1e-5)
})

test_that("valid differenced predictor models match reference fits and forecasts", {
  for (d in c(1L, 2L)) {
    data <- .foreCovFixture(d = d)
    options <- .foreCovOptions(c("stress", "sleep"), d = d)
    options$forecastLength <- 4L
    prepared <- .foreCovPrepare(data, options)
    fit <- .foreCovFit(prepared, options)
    trainingX <- as.matrix(data[1:90, c("stress", "sleep")])
    futureX <- as.matrix(data[91:94, c("stress", "sleep")])
    colnames(trainingX) <- colnames(futureX) <- c("xreg1", "xreg2")
    reference <- forecast::Arima(data$symptom[1:90], order = c(0, d, 0),
                                 xreg = trainingX, include.constant = TRUE)
    prediction <- forecast::forecast(reference, xreg = futureX, level = c(80, 95))
    actual <- .foreCovPredict(prepared, fit, options)
    expect_equal(stats::coef(fit), stats::coef(reference), tolerance = 1e-7)
    expect_equal(actual$y, as.numeric(prediction$mean), tolerance = 1e-7)
    expect_equal(actual$lower80, as.numeric(prediction$lower[, "80%"]), tolerance = 1e-7)
    expect_equal(actual$upper95, as.numeric(prediction$upper[, "95%"]), tolerance = 1e-7)
  }
})

test_that("future predictor scenarios change forecasts without changing the fit", {
  data <- .foreCovFixture()
  options <- .foreCovOptions(c("stress", "sleep"))
  options$forecastLength <- 4L
  changed <- data
  changed$stress[91:94] <- changed$stress[91:94] + 2
  first <- .foreCovPrepare(data, options)
  second <- .foreCovPrepare(changed, options)
  firstFit <- .foreCovFit(first, options)
  secondFit <- .foreCovFit(second, options)
  expect_equal(stats::coef(firstFit), stats::coef(secondFit))
  expect_equal(firstFit$var.coef, secondFit$var.coef)
  firstForecast <- .foreCovPredict(first, firstFit, options)
  secondForecast <- .foreCovPredict(second, secondFit, options)
  expect_equal(secondForecast$y - firstForecast$y,
               rep(2 * unname(stats::coef(firstFit)["xreg1"]), 4), tolerance = 1e-7)

  changed$stress[1:90] <- changed$stress[1:90] * 2
  changedFit <- .foreCovFit(.foreCovPrepare(changed, options), options)
  expect_equal(unname(stats::coef(changedFit)["xreg1"]),
               unname(stats::coef(firstFit)["xreg1"]) / 2, tolerance = 1e-5)
})

test_that("forecast horizons use future rows and selected numeric time units", {
  data <- .foreCovFixture()
  options <- .foreCovOptions("stress")
  prepared <- .foreCovPrepare(data, options)
  fit <- .foreCovFit(prepared, options)
  for (h in c(1L, 4L)) {
    options$forecastLength <- h
    actual <- .foreCovPredict(prepared, fit, options)
    futureX <- matrix(data$stress[90 + seq_len(h)], ncol = 1L,
                      dimnames = list(NULL, "xreg1"))
    reference <- forecast::forecast(fit, xreg = futureX, level = c(80, 95))
    expect_equal(nrow(actual), h)
    expect_equal(actual$t, data$clock[90 + seq_len(h)])
    expect_equal(actual$y, as.numeric(reference$mean), tolerance = 1e-7)
    expect_equal(actual$lower95, as.numeric(reference$lower[, "95%"]), tolerance = 1e-7)
  }

  options$covariates <- character()
  options$forecastLength <- 6L
  noPredictors <- .foreCovPrepare(data, options)
  noPredictorFit <- .foreCovFit(noPredictors, options)
  actual <- .foreCovPredict(noPredictors, noPredictorFit, options)
  reference <- forecast::forecast(noPredictorFit, h = 6, level = c(80, 95))
  expect_equal(actual$t, data$clock[90] + 7 * seq_len(6))
  expect_equal(actual$y, as.numeric(reference$mean), tolerance = 1e-7)
})

test_that("unsupported historical data are rejected before misleading estimation", {
  data <- .foreCovFixture(h = 0L)
  options <- .foreCovOptions("stress")
  for (badValue in c(NA_real_, Inf, -Inf)) {
    invalid <- data
    invalid$stress[12] <- badValue
    expect_error(.foreCovFit(.foreCovPrepare(invalid, options), options),
                 "missing|finite|covariate|predictor", ignore.case = TRUE)
  }
  invalid <- data
  invalid$symptom <- 5
  options$modelSpecification <- "auto"
  expect_error(.foreCovFit(.foreCovPrepare(invalid, options), options),
               "constant|vary|variation", ignore.case = TRUE)
  options$covariates <- "symptom"
  expect_error(.foreCovFit(.foreCovPrepare(data, options), options),
               "dependent|outcome|covariate|predictor", ignore.case = TRUE)
})

test_that("rank and differencing checks reject unidentifiable predictors", {
  data <- .foreCovFixture(h = 0L)
  options <- .foreCovOptions(c("stress", "sleep"))
  invalid <- data
  invalid$sleep <- 2 * invalid$stress + 3
  expect_error(.foreCovFit(.foreCovPrepare(invalid, options), options),
               "rank|dependent|collinear|identif", ignore.case = TRUE)
  invalid <- data
  invalid$stress <- 1
  expect_error(.foreCovFit(.foreCovPrepare(invalid, options), options),
               "constant|vary|variation|rank|collinear", ignore.case = TRUE)
  for (d in c(1L, 2L)) {
    options <- .foreCovOptions("stress", d = d)
    invalid <- data
    invalid$stress <- seq_len(nrow(invalid))
    expect_error(.foreCovFit(.foreCovPrepare(invalid, options), options),
                 "differ|rank|drift|identif|collinear", ignore.case = TRUE)
  }
  options <- .foreCovOptions("stress", d = 2L)
  for (offset in c(100, 1e6)) {
    invalid <- data
    invalid$stress <- offset + (seq_len(nrow(data)) - 1L) / 10
    expect_error(.foreCovFit(.foreCovPrepare(invalid, options), options),
                 "differ|rank|drift|identif|collinear", ignore.case = TRUE)
  }
})

test_that("ambiguous time grids and inadequate future predictors fail clearly", {
  data <- .foreCovFixture()
  options <- .foreCovOptions("stress")
  for (badTime in c(data$clock[1], NA_real_, Inf, data$clock[2] + .5)) {
    invalid <- data
    invalid$clock[2] <- badTime
    expect_error(.foreCovPrepare(invalid, options), "time|interval|spac", ignore.case = TRUE)
  }
  prepared <- .foreCovPrepare(data, options)
  fit <- .foreCovFit(prepared, options)
  options$forecastLength <- 5L
  expect_error(.foreCovPredict(prepared, fit, options),
               "future|forecast|covariate|predictor", ignore.case = TRUE)
  options$forecastLength <- 4L
  for (badValue in c(NA_real_, Inf)) {
    invalid <- data
    invalid$stress[92] <- badValue
    expect_error(.foreCovPredict(.foreCovPrepare(invalid, options), fit, options),
                 "future|missing|finite|covariate|predictor", ignore.case = TRUE)
  }
})

test_that("unused future rows do not invalidate historical estimation or shorter forecasts", {
  data <- .foreCovFixture()
  options <- .foreCovOptions("stress")
  valid <- .foreCovPrepare(data, options)
  referenceFit <- .foreCovFit(valid, options)

  invalid <- data
  invalid$clock[92] <- NA_real_
  prepared <- .foreCovPrepare(invalid, options)
  fit <- .foreCovFit(prepared, options)
  expect_equal(stats::coef(fit), stats::coef(referenceFit))
  result <- .foreCovRun(invalid, options)
  expect_identical(result$status, "complete")
  expect_equal(nrow(.foreCovRows(result$results$coefTable)), 2L)
  options$forecastLength <- 2L
  expect_error(.foreCovPredict(prepared, fit, options), "time|future", ignore.case = TRUE)

  # The malformed second future row is unused for a one-step forecast.
  options$forecastLength <- 1L
  expect_equal(.foreCovPredict(prepared, fit, options),
               .foreCovPredict(valid, referenceFit, options), tolerance = 1e-7)
  invalid$stress[92:94] <- NA_real_
  invalid$clock[93:94] <- c(Inf, NA_real_)
  extraInvalid <- .foreCovPrepare(invalid, options)
  expect_equal(.foreCovPredict(extraInvalid, .foreCovFit(extraInvalid, options), options),
               .foreCovPredict(valid, referenceFit, options), tolerance = 1e-7)

  options$forecastTimeSeries <- TRUE
  options$forecastTimeSeriesObserved <- FALSE
  options$forecastTimeSeriesType <- "line"
  result <- .foreCovRun(invalid, options)
  expect_identical(result$status, "complete")
  plot <- result$state$figures[[result$results$forecastPlot$data]]$obj
  built <- ggplot2::ggplot_build(plot)
  expect_true(all(is.finite(built$layout$panel_params[[1]]$x.range)))
  pointLayers <- Filter(function(layer) all(c("x", "y") %in% names(layer)), built$data)
  expect_true(any(vapply(pointLayers, function(layer)
    any(is.finite(layer$x) & layer$x == data$clock[91]), logical(1))))

  # An unused missing time must not disable sorting of the requested rows.
  unsorted <- data[c(seq_len(90), 92, 91, 93, 94), ]
  unsorted$clock[94] <- NA_real_
  options$forecastLength <- 2L
  unsortedPrepared <- .foreCovPrepare(unsorted, options)
  expect_equal(.foreCovPredict(unsortedPrepared, .foreCovFit(unsortedPrepared, options), options),
               .foreCovPredict(valid, referenceFit, options), tolerance = 1e-7)
})

test_that("reserved and nonsyntactic predictor names preserve values and labels", {
  data <- .foreCovFixture(h = 0L)
  predictorNames <- c("y", "t", "intercept", "drift", "ar1", "xreg1", "daily stress", "sleep-score")
  for (predictor in predictorNames) {
    renamed <- data
    names(renamed)[names(renamed) == "stress"] <- predictor
    options <- .foreCovOptions(predictor)
    prepared <- .foreCovPrepare(renamed, options)
    expect_equal(prepared$xreg1, data$stress)
    expected <- .foreCovFit(prepared, options)
    for (encoded in c(FALSE, TRUE)) {
      result <- .foreCovRun(renamed, options, encoded = encoded)
      expect_identical(result$status, "complete")
      rows <- .foreCovRows(result$results$coefTable)
      expect_equal(nrow(rows), length(stats::coef(expected)))
      expect_equal(sum(rows$coefficients == predictor), 1L)
      expect_equal(rows$estimate[rows$coefficients == predictor],
                   unname(stats::coef(expected)["xreg1"]), tolerance = 1e-7)
    }
  }
})

test_that("public coefficient tables report the fitted effects and requested intervals", {
  set.seed(25)
  x <- rnorm(100)
  data <- data.frame(symptom = 10 + 8 * x + rnorm(100, sd = .1),
                     clock = seq_len(100), stress = x)
  options <- .foreCovOptions("stress")
  reference <- forecast::Arima(data$symptom, order = c(0, 0, 0),
                               xreg = matrix(x, ncol = 1, dimnames = list(NULL, "xreg1")))
  tables <- lapply(c(.80, .99), function(level) {
    options$coefficientCiLevel <- level
    result <- .foreCovRun(data, options)
    expect_identical(result$status, "complete")
    .foreCovRows(result$results$coefTable)
  })
  expect_equal(tables[[1]]$estimate, unname(stats::coef(reference)), tolerance = 1e-7)
  expect_equal(tables[[1]]$SE, unname(sqrt(diag(reference$var.coef))), tolerance = 1e-7)
  expect_equal(tables[[1]]$estimate, tables[[2]]$estimate)
  expect_true(all(tables[[2]]$upper - tables[[2]]$lower >
                  tables[[1]]$upper - tables[[1]]$lower))
  expect_equal(tables[[1]]$estimate[tables[[1]]$coefficients == "stress"], 8, tolerance = .02)
  expect_gt(abs(diff(tables[[1]]$estimate)), 1)
})

test_that("forecast table plot and CSV share the same conditional predictions", {
  data <- .foreCovFixture()
  options <- .foreCovOptions(c("stress", "sleep"))
  options$forecastLength <- 4L
  options$forecastTable <- TRUE
  options$forecastTimeSeries <- TRUE
  options$forecastTimeSeriesType <- "both"
  options$forecastSave <- tempfile(fileext = ".csv")
  on.exit(unlink(options$forecastSave), add = TRUE)
  result <- .foreCovRun(data, options)
  expect_identical(result$status, "complete")
  prepared <- .foreCovPrepare(data, options)
  reference <- .foreCovPredict(prepared, .foreCovFit(prepared, options), options)
  rows <- .foreCovRows(result$results$forecastTable)
  for (column in names(reference))
    expect_equal(as.numeric(rows[[column]]), reference[[column]], tolerance = 1e-7)
  expect_true(file.exists(options$forecastSave))
  exported <- utils::read.csv(options$forecastSave, check.names = FALSE)
  for (column in names(reference)) {
    exportedColumn <- if (column == "y") options$dependent else column
    expect_equal(exported[[exportedColumn]], reference[[column]], tolerance = 1e-7)
  }

  plotName <- result$results$forecastPlot$data
  plot <- result$state$figures[[plotName]]$obj
  layers <- ggplot2::ggplot_build(plot)$data
  containsForecast <- function(layer) {
    if (!all(c("x", "y") %in% names(layer))) return(FALSE)
    selected <- layer[layer$x %in% reference$t, , drop = FALSE]
    selected <- selected[order(selected$x), , drop = FALSE]
    nrow(selected) == nrow(reference) &&
      isTRUE(all.equal(as.numeric(selected$x), reference$t, tolerance = 1e-7)) &&
      isTRUE(all.equal(as.numeric(selected$y), reference$y, tolerance = 1e-7))
  }
  expect_true(any(vapply(layers, containsForecast, logical(1))))
})

test_that("unavailable future predictors leave valid coefficients visible", {
  data <- .foreCovFixture(h = 0L)
  options <- .foreCovOptions("stress")
  options$forecastLength <- 3L
  options$forecastTable <- TRUE
  options$forecastTimeSeries <- TRUE
  result <- .foreCovRun(data, options)
  expect_equal(nrow(.foreCovRows(result$results$coefTable)), 2L)
  expect_identical(result$results$coefTable$status, "complete")
  tableText <- paste(unlist(result$results$forecastTable, use.names = FALSE), collapse = " ")
  expect_match(tableText, "future|covariate|predictor", ignore.case = TRUE)
  expect_true(is.null(result$results$forecastTable$data) ||
                length(result$results$forecastTable$data) == 0L)
})

test_that("an unavailable export location preserves the fitted and forecast tables", {
  data <- .foreCovFixture()
  options <- .foreCovOptions("stress")
  options$forecastLength <- 2L
  options$forecastTable <- TRUE
  options$forecastSave <- file.path(tempfile("missing-forecast-directory-"), "forecasts.csv")
  result <- .foreCovRun(data, options)
  expect_identical(result$status, "complete")
  expect_identical(result$results$coefTable$status, "complete")
  expect_identical(result$results$forecastTable$status, "complete")
  expect_equal(length(result$results$forecastTable$data), 2L)
  exportText <- paste(unlist(result$results$forecastExport, use.names = FALSE), collapse = " ")
  expect_match(exportText, "could not be saved|writable", ignore.case = TRUE)
  expect_false(file.exists(options$forecastSave))
})

test_that("simulation ignores stale covariates and valid coefficient-free models forecast", {
  options <- .foreCovOptions("previous predictor")
  options$inputType <- "simulateData"
  expect_length(.foreCovFunction(".ln1ForeCovariates")(options), 0L)
  stale <- .foreCovRun(NULL, options)
  options$covariates <- character()
  clean <- .foreCovRun(NULL, options)
  expect_identical(stale$status, "complete")
  expect_equal(stale$results$coefTable$data, clean$results$coefTable$data)

  data <- .foreCovFixture(h = 0L, d = 2L)
  options <- .foreCovOptions(character(), d = 2L)
  options$forecastLength <- 2L
  options$forecastTable <- TRUE
  prepared <- .foreCovPrepare(data, options)
  fit <- .foreCovFit(prepared, options)
  expect_length(stats::coef(fit), 0L)
  expect_equal(nrow(.foreCovPredict(prepared, fit, options)), 2L)
  result <- .foreCovRun(data, options)
  expect_identical(result$status, "complete")
  expect_equal(length(result$results$forecastTable$data), 2L)
})
