test_that("Treatment plots preserve missing positions and decode selected labels", {
  set.seed(447)
  n <- 120L
  occasion <- seq_len(n)
  phase <- factor(rep(c("baseline", "treatment", "followup"), each = 40L))
  y <- 1 + .02 * occasion + rep(c(0, 2, 1), each = 40L) +
    as.numeric(stats::arima.sim(list(ar = .6), n = n, sd = .4))
  missing <- c(1L, 8L, 9L, 40L, 65L, 120L)
  y[missing] <- NA_real_
  # 'fitted' was previously overwritten when constructing the analysis plot.
  data <- data.frame(fitted = y, check.names = FALSE)
  data[["Measurement clock"]] <- 100 + 7 * (occasion - 1L)
  data[["Treatment phase"]] <- phase
  options <- jaspTools::analysisOptions("Treatment")
  # jaspTools does not resolve dynamic dropdown defaults.
  options$comparisonPhase <- options$referencePhase <- ""
  options$inputType <- "loadData"
  options$enableIntroText <- FALSE
  options$dependent <- "fitted"
  options$time <- "Measurement clock"
  options$phase <- "Treatment phase"
  options$dependent.types <- options$time.types <- "scale"
  options$phase.types <- "nominal"
  options$plotData <- options$plotAnalysis <- TRUE
  options$coefficientsTable <- TRUE
  options$autocorrelationTable <- FALSE

  encoded <- jaspTools:::encodeOptionsAndDataset(options, data[n:1, ])
  expect_equal(nrow(encoded$encodingMap), 3L)
  expect_false(identical(encoded$dataset, data[n:1, ]))
  decoderName <- ".decodeColNamesLax"
  hadDecoder <- exists(decoderName, envir = .GlobalEnv, inherits = FALSE)
  oldDecoder <- if (hadDecoder) get(decoderName, envir = .GlobalEnv) else NULL
  on.exit({
    if (hadDecoder) assign(decoderName, oldDecoder, envir = .GlobalEnv)
    else rm(list = decoderName, envir = .GlobalEnv)
  }, add = TRUE)
  # Emulate Desktop's name-decoder callback for this native engine test.
  # jaspTools returns the map but does not install the callback itself.
  encodingMap <- encoded$encodingMap
  assign(decoderName, function(value) {
    index <- match(value, encodingMap$encoded)
    matched <- !is.na(index)
    value[matched] <- encodingMap$original[index[matched]]
    value
  }, envir = .GlobalEnv)
  expect_equal(jaspBase::decodeColNames(encoded$options$time), options$time)
  result <- jaspTools::runAnalysis("Treatment", encoded$dataset, encoded$options,
                                   encodedDataset = TRUE, view = FALSE)
  expect_identical(result$status, "complete")
  expect_match(result$results$timeInfo$rawtext, "114 observed outcomes and 6 missing")
  expect_match(result$results$timeInfo$rawtext, "time units\\): 7")
  expect_match(result$results$timeInfo$rawtext, "not imputed")
  labels <- vapply(result$results$coefTable$data, `[[`, character(1), "name")
  expect_equal(labels, c("(Intercept)", "Measurement clock",
    "Treatment phasefollowup", "Treatment phasetreatment",
    "Measurement clock:Treatment phasefollowup", "Measurement clock:Treatment phasetreatment"))

  referenceData <- data.frame(y = y, time = data[[options$time]], phase = phase,
                              occasion = occasion)
  reference <- nlme::gls(y ~ time * phase, data = referenceData, method = "REML",
    correlation = nlme::corAR1(form = ~ occasion), na.action = stats::na.exclude)
  for (key in c("dataPlot", "analysisPlot")) {
    expect_identical(result$results[[key]]$status, "complete")
    plot <- result$state$figures[[result$results[[key]]$data]]$obj
    expect_equal(plot$data[[encoded$options$time]], data[[options$time]])
    expect_equal(plot$data[[encoded$options$dependent]], y)
    expect_equal(plot$scales$get_scales("x")$name, options$time)
    expect_equal(plot$scales$get_scales("y")$name, options$dependent)
    built <- ggplot2::ggplot_build(plot)
    points <- built$data[[if (key == "dataPlot") 2L else 1L]]
    points <- points[order(points$x), ]
    expect_equal(points$x, data[[options$time]])
    expect_equal(points$y, y)
    if (key == "analysisPlot") {
      expect_equal(plot$theme$legend.position, "bottom")
      expect_equal(plot$labels$colour, "Phase")
      expect_setequal(as.character(built$plot$scales$get_scales("colour")$get_labels()),
                       levels(phase))
      line <- built$data[[2L]]
      line <- line[order(line$x), ]
      expect_equal(line$x, data[[options$time]])
      expect_equal(which(is.na(line$y)), missing)
      expect_equal(line$y, as.numeric(stats::fitted(reference)), tolerance = 1e-6)
    }
  }
})

test_that("Treatment explains numerically unestimable absolute time values", {
  set.seed(9)
  data <- data.frame(y = rnorm(120), clock = 1.7e9 + seq_len(120),
                     phase = factor(rep(c("baseline", "treatment", "followup"), each = 40)))
  options <- jaspTools::analysisOptions("Treatment")
  # jaspTools does not resolve dynamic dropdown defaults.
  options$comparisonPhase <- options$referencePhase <- ""
  options$inputType <- "loadData"
  options$dependent <- "y"
  options$time <- "clock"
  options$phase <- "phase"
  options$enableIntroText <- FALSE
  options$plotData <- options$plotAnalysis <- FALSE
  result <- jaspTools::runAnalysis("Treatment", data, options, view = FALSE)
  expect_identical(result$status, "validationError")
  expect_match(result$results$errorMessage, "time relative to the start")
})
