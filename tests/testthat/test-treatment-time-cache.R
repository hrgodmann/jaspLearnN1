test_that("legacy Treatment models and outputs are rebuilt with phase comparisons", {
  set.seed(447)
  n <- 120L
  clock <- seq_len(n)
  data <- data.frame(
    symptom = 1 + .02 * clock + rep(c(0, 2, 1), each = 40L) +
      as.numeric(stats::arima.sim(list(ar = .6), n = n, sd = .4)),
    clock = clock,
    stage = factor(rep(c("baseline", "treatment", "followup"), each = 40L)))
  permutation <- c(seq(2L, 120L, 2L), seq(1L, 119L, 2L))
  options <- jaspTools::analysisOptions("Treatment")
  options$enableIntroText <- FALSE
  options$inputType <- "loadData"
  options$dependent <- "symptom"
  options$time <- "clock"
  options$phase <- "stage"
  options$plotData <- FALSE
  options$plotAnalysis <- FALSE
  options$coefficientsTable <- TRUE
  options$autocorrelationTable <- TRUE
  options$phaseComparisons <- TRUE
  options$phaseSummary <- TRUE
  options$comparisonPhase <- ""
  options$referencePhase <- ""

  referenceData <- data.frame(y = data$symptom, time = data$clock,
                              phase = data$stage, occasion = clock)
  reference <- nlme::gls(y ~ time * phase, data = referenceData, method = "REML",
                         correlation = nlme::corAR1(form = ~ occasion),
                         na.action = stats::na.exclude)
  referencePhi <- unname(stats::coef(reference$modelStruct$corStruct, unconstrained = FALSE))
  rows <- function(table) do.call(rbind, lapply(table$data, as.data.frame))

  originalUpgrade <- getFromNamespace(".ln1TreatUpgradeState", "jaspLearnN1")
  originalEstimate <- getFromNamespace(".ln1TreatEstimateModel", "jaspLearnN1")
  for (oldVersion in list(NULL, 1L, 2L, 3L, 4L, 5L, 6L)) {
    recorded <- new.env(parent = emptyenv())
    recorded$called <- FALSE
    result <- testthat::with_mocked_bindings({
      jaspTools::runAnalysis("Treatment", data[permutation, ], options, view = FALSE)
    }, .ln1TreatUpgradeState = function(jaspResults) {
      # Construct native objects only inside runAnalysis's initialized context.
      recorded$called <- TRUE
      # Version 1 fixed chronology but retained the random intercept; version 2
      # already used GLS. Older unmarked caches also used input row order.
      oldData <- if (is.null(oldVersion)) data[permutation, , drop = FALSE] else data
      oldData$g <- 1L
      oldData$occasion <- oldData$clock
      correlation <- if (is.null(oldVersion)) nlme::corAR1() else nlme::corAR1(form = ~ occasion)
      oldFit <- if (!is.null(oldVersion) && oldVersion >= 2L) {
        nlme::gls(symptom ~ clock * stage, data = oldData, correlation = correlation,
                   method = "REML", na.action = stats::na.exclude)
      } else {
        nlme::lme(symptom ~ clock * stage, data = oldData,
                    random = ~ 1 | g, correlation = correlation)
      }
      recorded$oldPhi <- unname(stats::coef(oldFit$modelStruct$corStruct, unconstrained = FALSE))
      jaspResults[["modelState"]] <- jaspBase::createJaspState(object = oldFit)
      jaspResults[["modelErrorState"]] <- jaspBase::createJaspState(object = "Earlier fit failure")
      jaspResults[["simulatedDataState"]] <- jaspBase::createJaspState(object = oldData)
      for (key in c("coefTable", "autoCorTable", "phaseComparisons", "phaseSummary")) {
        table <- jaspBase::createJaspTable(paste("Legacy", key))
        table$addColumnInfo(name = "coef", type = "number")
        table$addRows(data.frame(coef = -999))
        jaspResults[[key]] <- table
      }
      for (key in c("dataPlot", "analysisPlot")) {
        jaspResults[[key]] <- jaspBase::createJaspPlot(
          plot = ggplot2::ggplot(oldData, ggplot2::aes(x = clock, y = symptom)) +
            ggplot2::geom_point(), title = paste("Legacy", key))
      }
      for (key in c("timeInfo", "introText"))
        jaspResults[[key]] <- jaspBase::createJaspHtml("Legacy timing", title = key)
      if (is.null(oldVersion)) {
        jaspResults[["treatmentTimeVersion"]] <- NULL
      } else {
        jaspResults[["treatmentTimeVersion"]] <- jaspBase::createJaspState(object = oldVersion)
      }

      originalUpgrade(jaspResults)
      keys <- c("simulatedDataState", "modelState", "modelErrorState", "coefTable", "autoCorTable",
                "dataPlot", "analysisPlot", "timeInfo", "introText", "phaseComparisons", "phaseSummary")
      recorded$cleared <- vapply(keys, function(key) is.null(jaspResults[[key]]), logical(1))
      recorded$version <- jaspResults[["treatmentTimeVersion"]]$object

      # Once the current marker exists, another entry must keep valid states.
      sentinel <- list(marker = "already rebuilt")
      jaspResults[["modelState"]] <- jaspBase::createJaspState(object = sentinel)
      originalUpgrade(jaspResults)
      recorded$idempotent <- identical(jaspResults[["modelState"]]$object, sentinel)
      recorded$secondVersion <- jaspResults[["treatmentTimeVersion"]]$object
      jaspResults[["modelState"]] <- NULL
    }, .ln1TreatEstimateModel = function(jaspResults, dataset, options, ready) {
      originalEstimate(jaspResults, dataset, options, ready)
      recorded$rebuilt <- jaspResults[["modelState"]]$object
    }, .package = "jaspLearnN1")

    expect_true(recorded$called)
    expect_true(all(recorded$cleared))
    expect_identical(recorded$version, 7L)
    expect_true(recorded$idempotent)
    expect_identical(recorded$secondVersion, 7L)
    expect_identical(result$status, "complete")
    expect_s3_class(recorded$rebuilt, "gls")
    expect_null(recorded$rebuilt$modelStruct$reStruct)
    expect_false(inherits(recorded$rebuilt, "lme"))

    coefficients <- rows(result$results$coefTable)
    autocorrelation <- rows(result$results$autoCorTable)
    expect_equal(coefficients$coef, unname(stats::coef(reference)), tolerance = 1e-7)
    expect_equal(coefficients$SE, unname(sqrt(diag(stats::vcov(reference)))), tolerance = 1e-7)
    expect_equal(autocorrelation$coef, referencePhi, tolerance = 1e-7)
    if (is.null(oldVersion))
      expect_gt(abs(recorded$oldPhi - referencePhi), .1)
    expect_null(result$results$dataPlot)
    expect_null(result$results$analysisPlot)
    expect_null(result$results$introText)
    expect_false(is.null(result$results$timeInfo))
    comparisons <- rows(result$results$phaseComparisons)
    summaries <- rows(result$results$phaseSummary)
    expect_equal(comparisons$comparisonPhase, rep("treatment", 2L))
    expect_equal(comparisons$referencePhase, rep("baseline", 2L))
    expect_equal(nrow(summaries), 6L)
    expect_true(all(is.finite(comparisons$estimate)))
    expect_true(all(is.finite(summaries$estimate)))
  }
})
