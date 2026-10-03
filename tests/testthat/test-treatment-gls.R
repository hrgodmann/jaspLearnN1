# Independent dense-matrix REML calculations: this oracle does not call nlme.
.treatGlsOracle <- function(data) {
  observed <- !is.na(data$y)
  X <- stats::model.matrix(~ time * phase, data[observed, ])
  y <- data$y[observed]
  lag <- abs(outer(data$occasion[observed], data$occasion[observed], "-"))
  df <- length(y) - ncol(X)
  evaluate <- function(phi, details = FALSE) {
    R <- phi^lag
    inverseX <- solve(R, X)
    information <- crossprod(X, inverseX)
    beta <- solve(information, crossprod(inverseX, y))
    residual <- y - as.numeric(X %*% beta)
    sigma2 <- as.numeric(crossprod(residual, solve(R, residual))) / df
    objective <- .5 * (as.numeric(determinant(R, logarithm = TRUE)$modulus) +
      as.numeric(determinant(information, logarithm = TRUE)$modulus) +
      df * (1 + log(2 * pi * sigma2)))
    if (!details)
      return(objective)
    covariance <- sigma2 * solve(information)
    se <- sqrt(diag(covariance))
    list(beta = as.numeric(beta), covariance = covariance, se = se, sigma2 = sigma2,
         logLik = -objective, phi = phi, df = df,
         p = 2 * stats::pt(-abs(as.numeric(beta) / se), df = df))
  }
  optimum <- stats::optimize(evaluate, interval = c(-.98, .98), tol = 1e-10)
  evaluate(optimum$minimum, details = TRUE)
}

.treatGlsFixture <- function(rho, missing = FALSE) {
  set.seed(if (rho > 0) 981L else 982L)
  occasion <- seq_len(120L)
  phase <- factor(rep(c("baseline", "treatment", "followup"), each = 40L))
  y <- 3 + .02 * occasion + rep(c(0, -2, -1), each = 40L) +
    as.numeric(stats::arima.sim(list(ar = rho), n = 120L, sd = .4))
  if (missing)
    y[c(1L, 8L, 9L, 40L, 65L, 120L)] <- NA_real_
  data.frame(y = y, time = occasion, phase = phase, occasion = occasion)
}

.treatGlsOptions <- function() {
  options <- jaspTools::analysisOptions("Treatment")
  # jaspTools does not resolve dynamic dropdown defaults.
  options$comparisonPhase <- options$referencePhase <- ""
  options$inputType <- "loadData"
  options$dependent <- "y"
  options$time <- "time"
  options$phase <- "phase"
  options$enableIntroText <- FALSE
  options$coefficientsTable <- options$autocorrelationTable <- TRUE
  options$plotData <- options$plotAnalysis <- FALSE
  options
}

test_that("Treatment estimates an identifiable single-series GLS model", {
  prepare <- getFromNamespace(".ln1TreatPrepareData", "jaspLearnN1")
  estimate <- getFromNamespace(".ln1TreatEstimateModelHelper", "jaspLearnN1")
  for (rho in c(.55, -.45)) {
    data <- .treatGlsFixture(rho, missing = rho < 0)
    oracle <- .treatGlsOracle(data)
    fit <- estimate(prepare(data, .treatGlsOptions()), .treatGlsOptions())
    expect_s3_class(fit, "gls")
    expect_false(inherits(fit, "lme"))
    expect_null(fit$modelStruct$reStruct)
    expect_identical(fit$method, "REML")
    expect_equal(unname(stats::coef(fit)), oracle$beta, tolerance = 1e-6)
    expect_equal(unname(stats::vcov(fit)), unname(oracle$covariance), tolerance = 1e-6)
    expect_equal(fit$sigma^2, oracle$sigma2, tolerance = 1e-6)
    expect_equal(as.numeric(stats::logLik(fit)), oracle$logLik, tolerance = 1e-7)
    expect_equal(as.numeric(stats::coef(fit$modelStruct$corStruct, unconstrained = FALSE)),
                 oracle$phi, tolerance = 1e-6)
    expect_true(is.matrix(fit$apVar))
    expect_equal(dim(fit$apVar), c(2L, 2L))
    expect_true(all(is.finite(fit$apVar)))
    expect_gt(min(eigen(fit$apVar, symmetric = TRUE, only.values = TRUE)$values), 0)

    summaryRows <- data.frame(stats::coef(summary(fit)))
    expect_equal(summaryRows$Std.Error, unname(oracle$se), tolerance = 1e-6)
    expect_equal(summaryRows$p.value, unname(oracle$p), tolerance = 1e-6)
    ci <- nlme::intervals(fit, which = "coef", level = .95)$coef
    margin <- stats::qt(.975, oracle$df) * oracle$se
    expect_equal(unname(ci[, "lower"]), unname(oracle$beta - margin), tolerance = 1e-6)
    expect_equal(unname(ci[, "upper"]), unname(oracle$beta + margin), tolerance = 1e-6)
  }
})

test_that("GLS intercept uncertainty excludes an arbitrary shared offset variance", {
  data <- .treatGlsFixture(.55)
  options <- .treatGlsOptions()
  prepare <- getFromNamespace(".ln1TreatPrepareData", "jaspLearnN1")
  fit <- getFromNamespace(".ln1TreatEstimateModelHelper", "jaspLearnN1")(prepare(data, options), options)
  X <- stats::model.matrix(~ time * phase, data)
  phi <- as.numeric(stats::coef(fit$modelStruct$corStruct, unconstrained = FALSE))
  covariance <- fit$sigma^2 * phi^abs(outer(data$occasion, data$occasion, "-"))
  correct <- solve(crossprod(X, solve(covariance, X)))
  expect_equal(stats::vcov(fit), correct, tolerance = 1e-7)

  # Reproduce exactly what the old artificial variance adds to uncertainty.
  for (tau2 in c(.1, 10)) {
    oldCovariance <- covariance + tau2
    oldBetaCovariance <- solve(crossprod(X, solve(oldCovariance, X)))
    expect_equal(oldBetaCovariance[1L, 1L] - stats::vcov(fit)[1L, 1L],
                 tau2, tolerance = 1e-7)
    expect_equal(oldBetaCovariance[-1L, -1L], stats::vcov(fit)[-1L, -1L], tolerance = 1e-7)
  }
})

test_that("native GLS tables report corrected uncertainty and honor CI level", {
  data <- .treatGlsFixture(.55, missing = TRUE)
  oracle <- .treatGlsOracle(data)
  options <- .treatGlsOptions()
  tables <- list()
  for (level in c(.8, .95)) {
    options$coefficientCiLevel <- level
    result <- jaspTools::runAnalysis("Treatment", data, options, view = FALSE)
    expect_identical(result$status, "complete")
    rows <- do.call(rbind, lapply(result$results$coefTable$data, as.data.frame))
    margin <- stats::qt((1 + level) / 2, oracle$df) * oracle$se
    expect_equal(rows$coef, oracle$beta, tolerance = 1e-6)
    expect_equal(rows$SE, unname(oracle$se), tolerance = 1e-6)
    expect_equal(rows$t, unname(oracle$beta / oracle$se), tolerance = 1e-6)
    expect_equal(rows$p, unname(oracle$p), tolerance = 1e-6)
    expect_equal(rows$lower, unname(oracle$beta - margin), tolerance = 1e-6)
    expect_equal(rows$upper, unname(oracle$beta + margin), tolerance = 1e-6)
    expect_match(paste(unlist(result$results$coefTable$footnotes), collapse = " "),
                  "generalized least squares")
    ar <- result$results$autoCorTable$data[[1L]]
    expect_equal(ar$coef, oracle$phi, tolerance = 1e-6)
    expect_true(is.finite(ar$lower) && is.finite(ar$upper))
    expect_lt(ar$lower, ar$coef)
    expect_gt(ar$upper, ar$coef)
    expect_gte(ar$lower, -1)
    expect_lte(ar$upper, 1)
    tables[[as.character(level)]] <- ar
  }
  expect_lt(tables[["0.95"]]$lower, tables[["0.8"]]$lower)
  expect_gt(tables[["0.95"]]$upper, tables[["0.8"]]$upper)
})
