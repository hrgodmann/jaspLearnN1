# Inspect the rendered guide, not just a theme declaration: JASP's raw theme
# suppresses legends even when the phase colour mapping and scale are present.
.treatLegendText <- function(grob) {
  if (inherits(grob, "text"))
    return(as.character(grob$label))
  children <- c(grob$grobs, as.list(grob$children))
  unlist(lapply(children, .treatLegendText), use.names = FALSE)
}

test_that("Treatment raw-data plots identify every phase in a vertical legend", {
  phases <- c("Baseline without treatment", "Treatment with support",
              "Treatment without support", "Follow-up after treatment")
  data <- data.frame(y = c(1, 3, 2, 4, 3, 5, 2, 3, 1, 2, 1, 3),
                     t = seq_len(12),
                     phase = factor(rep(phases, each = 3), levels = phases))
  plotFunction <- getFromNamespace(".ln1TreatCreateDataPlotFill", "jaspLearnN1")

  for (mode in c("simulateData", "loadData")) {
    options <- list(inputType = mode, dependent = "Outcome", time = "Clock",
                    phase = "Treatment stage")
    plotData <- data
    if (mode == "loadData")
      names(plotData) <- c("Outcome", "Clock", "Treatment stage")
    plot <- plotFunction(plotData, options)
    built <- ggplot2::ggplot_build(plot)
    figure <- ggplot2::ggplotGrob(plot)
    guides <- figure$grobs[grepl("^guide-box", figure$layout$name)]
    guides <- Filter(function(grob) inherits(grob, "gtable"), guides)
    expect_equal(length(guides), 1L, info = mode)
    legend <- guides[[1L]]
    expect_equal(sort(.treatLegendText(legend)), sort(c("Phase", phases)), info = mode)
    expect_equal(plot$theme$legend.position, "bottom", info = mode)

    # Long phase names occupy separate rows instead of sharing a crowded row.
    keys <- legend$grobs[[which(legend$layout$name == "guides")]]
    labelPositions <- keys$layout[grepl("^label", keys$layout$name), ]
    expect_equal(nrow(labelPositions), length(phases), info = mode)
    expect_equal(length(unique(labelPositions$l)), 1L, info = mode)
    expect_equal(length(unique(labelPositions$t)), length(phases), info = mode)

    # The legend is backed by the same phase scale as both observed layers.
    scale <- built$plot$scales$get_scales("colour")
    expect_equal(as.character(scale$get_labels()), phases, info = mode)
    phaseColumn <- if (mode == "loadData") "Treatment stage" else "phase"
    expectedColours <- unname(scale$map(plotData[[phaseColumn]]))
    for (layer in built$data[1:2])
      expect_equal(layer$colour, expectedColours, info = mode)
  }
})
