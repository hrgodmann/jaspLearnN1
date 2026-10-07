.treatMissingFunction <- function(name) getFromNamespace(name, "jaspLearnN1")

.treatMissingOptions <- function() {
  options <- jaspTools::analysisOptions("Treatment")
  options$inputType <- "loadData"
  options$dependent <- "y"
  options$time <- "time"
  options$phase <- "phase"
  options$comparisonPhase <- options$referencePhase <- ""
  options$enableIntroText <- FALSE
  options$plotData <- options$plotAnalysis <- FALSE
  options$coefficientsTable <- options$autocorrelationTable <- TRUE
  options$phaseComparisons <- TRUE
  options
}

.treatMissingData <- function(rho, gap, n = 240L) {
  set.seed(51)
  data <- data.frame(y = as.numeric(stats::arima.sim(list(ar = rho), n = n, sd = .4)),
                     time = seq_len(n), phase = factor(rep(c("A", "B"), each = n / 2)))
  data$y[-seq.int(1L, n, by = gap)] <- NA_real_
  data
}

# Independent dense-matrix REML, using neither gls nor its initialization.
# Refine all grid-local optima on both sides of zero, including near-boundary
# values on a transformed scale, rather than assuming a unimodal likelihood.
.treatMissingOracle <- function(data) {
  observed <- !is.na(data$y)
  X <- stats::model.matrix(~ time * phase, data[observed, ])
  y <- data$y[observed]
  lag <- abs(outer(data$time[observed], data$time[observed], "-"))
  df <- length(y) - ncol(X)
  evaluate <- function(phi, details = FALSE) {
    V <- phi^lag
    inverseX <- solve(V, X)
    information <- crossprod(X, inverseX)
    beta <- solve(information, crossprod(inverseX, y))
    residual <- y - as.numeric(X %*% beta)
    sigma2 <- as.numeric(crossprod(residual, solve(V, residual))) / df
    logLik <- -.5 * (as.numeric(determinant(V, logarithm = TRUE)$modulus) +
      as.numeric(determinant(information, logarithm = TRUE)$modulus) +
      df * (1 + log(2 * pi * sigma2)))
    if (!details) return(logLik)
    list(phi = phi, logLik = logLik, beta = as.numeric(beta), covariance = sigma2 * solve(information))
  }
  grid <- sort(unique(c(seq(-.99, .99, length.out = 81L), tanh(seq(-7, 7, by = .2)))))
  values <- vapply(grid, evaluate, numeric(1))
  local <- which(values[2:(length(values) - 1L)] >= values[1:(length(values) - 2L)] &
                   values[2:(length(values) - 1L)] >= values[3:length(values)]) + 1L
  # optimize's relative tolerance on raw rho loses covariance precision near
  # +/-1; refine on atanh(rho), then return to the correlation scale.
  candidates <- c(grid[which.max(values)], vapply(local, function(i)
    tanh(stats::optimize(function(eta) evaluate(tanh(eta)),
      interval = atanh(grid[c(i - 1L, i + 1L)]), maximum = TRUE,
      tol = 1e-10)$maximum), numeric(1)))
  best <- candidates[which.max(vapply(candidates, evaluate, numeric(1)))]
  evaluate(best, details = TRUE)
}

test_that("periodic missingness fits match independent REML for both correlation signs", {
  options <- .treatMissingOptions()
  for (gap in c(3L, 9L)) {
    for (sign in c(-1, 1)) {
      data <- .treatMissingData(sign * if (gap == 3L) .95 else .98, gap)
      oracle <- .treatMissingOracle(data)
      prepared <- .treatMissingFunction(".ln1TreatPrepareData")(data, options)
      fit <- .treatMissingFunction(".ln1TreatEstimateModelHelper")(prepared, options)
      expect_equal(as.numeric(stats::logLik(fit)), oracle$logLik, tolerance = 1e-7)
      expect_equal(unname(stats::coef(fit)), oracle$beta, tolerance = 1e-5)
      expect_equal(unname(stats::vcov(fit)), unname(oracle$covariance), tolerance = 1e-5)
      expect_equal(as.numeric(stats::coef(fit$modelStruct$corStruct, unconstrained = FALSE)),
                   oracle$phi, tolerance = 1e-6)
      # Independently define B-at-240 minus A-at-120 and B minus A slope.
      contrast <- rbind(c(0, 120, 1, 240), c(0, 0, 0, 1))
      expectedSE <- sqrt(diag(contrast %*% oracle$covariance %*% t(contrast)))
      result <- .treatMissingFunction(".ln1TreatComparisonResults")(prepared, fit, options)
      expect_equal(result$estimate, drop(contrast %*% oracle$beta), tolerance = 1e-5)
      expect_equal(result$SE, unname(expectedSE), tolerance = 1e-5)
    }
  }
})

test_that("a single adjacent pair does not hide the correlation basin of large gaps", {
  set.seed(43)
  occasion <- c(1L, 2L, seq.int(1001L, 78001L, by = 1000L))
  observed <- data.frame(y = as.numeric(stats::arima.sim(list(ar = .8), n = 80L, sd = .5)),
                         time = occasion, phase = factor(rep(c("A", "B"), each = 40L)))
  observed$y[2L] <- observed$y[1L] + .15
  oracle <- .treatMissingOracle(observed)
  options <- .treatMissingOptions()
  data <- data.frame(y = NA_real_, time = seq_len(max(occasion)),
                     phase = factor(ifelse(seq_len(max(occasion)) < occasion[41L], "A", "B")))
  data$y[occasion] <- observed$y
  prepared <- .treatMissingFunction(".ln1TreatPrepareData")(data, options)
  fit <- .treatMissingFunction(".ln1TreatEstimateModelHelper")(prepared, options)
  expect_gt(as.numeric(stats::logLik(fit)), -95)
  expect_equal(as.numeric(stats::logLik(fit)), oracle$logLik, tolerance = 1e-7)
  expect_equal(unname(stats::coef(fit)), oracle$beta, tolerance = 1e-5)
  expect_equal(unname(stats::vcov(fit)), unname(oracle$covariance), tolerance = 1e-5)
})

test_that("native Treatment reports corrected uncertainty for the audit missingness fixture", {
  data <- .treatMissingData(.95, 3L)
  oracle <- .treatMissingOracle(data)
  result <- jaspTools::runAnalysis("Treatment", data, .treatMissingOptions(), view = FALSE)
  expect_identical(result$status, "complete")
  expect_equal(result$results$autoCorTable$data[[1L]]$coef, oracle$phi, tolerance = 1e-6)
  rows <- do.call(rbind, lapply(result$results$phaseComparisons$data, as.data.frame))
  contrast <- rbind(c(0, 120, 1, 240), c(0, 0, 0, 1))
  expect_equal(rows$estimate, drop(contrast %*% oracle$beta), tolerance = 1e-5)
  expect_equal(rows$SE, unname(sqrt(diag(contrast %*% oracle$covariance %*% t(contrast)))),
               tolerance = 1e-5)
  expect_gt(rows$SE[1L], .79)
})

test_that("even observed gaps suppress an unidentified signed AR1 estimate but retain phase inference", {
  data <- .treatMissingData(.9, 2L, n = 120L)
  options <- .treatMissingOptions()
  prepared <- .treatMissingFunction(".ln1TreatPrepareData")(data, options)
  fit <- .treatMissingFunction(".ln1TreatEstimateModelHelper")(prepared, options)
  phi <- as.numeric(stats::coef(fit$modelStruct$corStruct, unconstrained = FALSE))
  lag <- abs(outer(data$time[!is.na(data$y)], data$time[!is.na(data$y)], "-"))
  expect_equal(phi^lag, (-phi)^lag, tolerance = 0)
  expect_false(attr(fit, "ln1TreatCorrelationSignIdentified"))
  result <- jaspTools::runAnalysis("Treatment", data, options, view = FALSE)
  expect_identical(result$status, "complete")
  row <- result$results$autoCorTable$data[[1L]]
  for (key in c("coef", "lower", "upper"))
    expect_true(is.null(row[[key]]) || identical(row[[key]], "") ||
                  (is.numeric(row[[key]]) && is.na(row[[key]])))
  notes <- paste(unlist(result$results$autoCorTable$footnotes), collapse = " ")
  expect_match(notes, "sign.*not identifiable")
  expect_match(notes, "even numbers")
  comparisons <- result$results$phaseComparisons
  expect_identical(comparisons$status, "complete")
  expect_true(all(vapply(comparisons$data, function(row) is.finite(row$SE), logical(1))))

  # One additional even-position outcome breaks the sign alias.
  data$y[2L] <- data$y[1L] + .1
  fit <- .treatMissingFunction(".ln1TreatEstimateModelHelper")(
    .treatMissingFunction(".ln1TreatPrepareData")(data, options), options)
  expect_true(attr(fit, "ln1TreatCorrelationSignIdentified"))
})
