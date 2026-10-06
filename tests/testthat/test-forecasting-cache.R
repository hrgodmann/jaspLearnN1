test_that("legacy forecast states and displayed results are rebuilt with covariates", {
  set.seed(713)
  n <- 80L
  h <- 3L
  x <- c(stats::rnorm(n), -1, 0, 1)
  y <- 12 + 3 * x[seq_len(n)] + stats::rnorm(n, sd = .1)
  data <- data.frame(symptom = c(y, rep(NA_real_, h)),
                     time = 100 + 7 * (seq_len(n + h) - 1L), stress = x)
  options <- jaspTools::analysisOptions("Forecasting")
  options$enableIntroText <- FALSE
  options$inputType <- "loadData"
  options$dependent <- "symptom"
  options$time <- "time"
  options$covariates <- "stress"
  options$modelSpecification <- "custom"
  options$p <- options$d <- options$q <- 0L
  options$plotData <- FALSE
  options$forecastLength <- h
  options$forecastTable <- TRUE
  options$forecastTimeSeries <- FALSE
  options$forecastSave <- ""

  originalUpgrade <- getFromNamespace(".ln1ForeUpgradeState", "jaspLearnN1")
  for (previousVersion in list(NULL, 3L)) {
    recorded <- new.env(parent = emptyenv())
    recorded$called <- FALSE
    testthat::local_mocked_bindings(.ln1ForeUpgradeState = function(jaspResults) {
      # These constructors run inside runAnalysis's initialized native context.
      # Seed the old outcome-only fit and old dataframe-shaped forecast cache,
      # together with displayed results that would otherwise skip recomputation.
      recorded$called <- TRUE
      oldFit <- forecast::Arima(y, order = c(0, 0, 0), include.constant = TRUE)
      jaspResults[["modelState"]] <- jaspBase::createJaspState(object = oldFit)
      jaspResults[["forecastResult"]] <- jaspBase::createJaspState(object =
        data.frame(t = 999, y = -999, lower80 = -1000, upper80 = -998,
                   lower95 = -1001, upper95 = -997))
      oldCoefficients <- jaspBase::createJaspTable("Legacy coefficients")
      oldCoefficients$addColumnInfo(name = "coefficients", type = "string")
      oldCoefficients$addColumnInfo(name = "estimate", type = "number")
      oldCoefficients$addRows(data.frame(coefficients = c("Intercept", "stress"),
                                        estimate = rep(unname(oldFit$coef), 2L)))
      jaspResults[["coefTable"]] <- oldCoefficients
      oldForecasts <- jaspBase::createJaspTable("Legacy forecasts")
      oldForecasts$addColumnInfo(name = "t", type = "string")
      oldForecasts$addColumnInfo(name = "y", type = "number")
      oldForecasts$addRows(data.frame(t = "999", y = -999))
      jaspResults[["forecastTable"]] <- oldForecasts
      jaspResults[["forecastPlot"]] <- jaspBase::createJaspPlot(title = "Legacy forecast plot")
      journal <- list(version = 1L, session = "preserved-session", observedRequest = TRUE,
                      request = list(path = "saved.csv"), status = "success")
      jaspResults[["forecastExportState"]] <- jaspBase::createJaspState(object = journal)
      jaspResults[["forecastCacheVersion"]] <- if (is.null(previousVersion)) NULL else
        jaspBase::createJaspState(object = previousVersion)

      originalUpgrade(jaspResults)
      recorded$cleared <- vapply(c("modelState", "forecastResult", "coefTable", "forecastTable", "forecastPlot"),
        function(key) is.null(jaspResults[[key]]), logical(1))
      recorded$version <- jaspResults[["forecastCacheVersion"]]$object
      recorded$journalPreserved <- identical(jaspResults[["forecastExportState"]]$object, journal)
    }, .package = "jaspLearnN1")

    result <- jaspTools::runAnalysis("Forecasting", data, options, view = FALSE)
    expect_true(recorded$called)
    expect_true(all(recorded$cleared))
    expect_identical(recorded$version, 4L)
    expect_true(recorded$journalPreserved)
    expect_identical(result$status, "complete")

    reference <- forecast::Arima(y, order = c(0, 0, 0),
      xreg = matrix(x[seq_len(n)], ncol = 1L, dimnames = list(NULL, "xreg1")),
      include.constant = TRUE)
    referenceForecast <- forecast::forecast(reference,
      xreg = matrix(x[n + seq_len(h)], ncol = 1L, dimnames = list(NULL, "xreg1")))
    tableRows <- function(table) do.call(rbind, lapply(table$data, as.data.frame))
    coefficients <- tableRows(result$results$coefTable)
    predictions <- tableRows(result$results$forecastTable)
    expect_equal(coefficients$coefficients, c("Intercept", "stress"))
    expect_equal(coefficients$estimate, unname(reference$coef), tolerance = 1e-7)
    expect_equal(as.numeric(predictions$t), data$time[n + seq_len(h)])
    expect_equal(predictions$y, as.numeric(referenceForecast$mean), tolerance = 1e-7)
  }
})
