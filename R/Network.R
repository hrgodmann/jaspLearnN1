#
# Copyright (C) 2025 University of Amsterdam and Netherlands eScience Center
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

Network <- function(jaspResults, dataset = NULL, options) {
  jaspResults$title <- gettext("How Are Symptoms Connected?")

  .ln1Intro(jaspResults, options, .ln1NetIntroText)

  .ln1NetData(jaspResults, dataset, options)

  .ln1NetCreateNetworkPlots(jaspResults, dataset, options, .ln1NetGetDataDependencies)

  .ln1NetCentrality(jaspResults, options)

  .ln1NetSaveProblems(jaspResults, options)
  .ln1NetSaveConnections(jaspResults, options)
}

.ln1NetIntroText <- function() {
  return(gettext("VAR models allow therapists to examine how different psychological and behavioral variables influence each other dynamically over time, helping to uncover reciprocal relationships between symptoms and external factors. These models require multiple time series variables recorded over time, such as mood ratings, sleep patterns, and coping strategies. A distinguishing feature of VAR is its ability to model bidirectional influences, revealing feedback loops that might sustain or alleviate mental health issues. This can be particularly useful in identifying maladaptive cycles—such as anxiety leading to avoidance, which in turn reinforces anxiety—that might not be apparent through traditional analyses. The insights from VAR models can help personalize interventions by targeting the most influential nodes in a patient’s symptom network."))
} 

.ln1NetGetDataDependencies <- function() {
  return(c("problems", "connections"))
}

.ln1NetData <- function(jaspResults, dataset, options) {
  nodeAttributes <- data.frame(t(sapply(options[["problems"]], function(problem) {
    return(c(problem[["problemName"]], problem[["problemSeverity"]]))
  })))
  names(nodeAttributes) <- c("name", "strength")
  nodeAttributes[["strength"]] <- as.numeric(nodeAttributes[["strength"]])

  jaspResults[["nodeAttributesState"]] <- createJaspState(nodeAttributes)

  .ln1NetEdgelists(jaspResults, options)
}

.ln1NetEdgelists <- function(jaspResults, options) {
  if(is.null(jaspResults[["edgelistContainer"]])) {
    edgelistContainer <- createJaspContainer()
    edgelistContainer$dependOn("problems")
    jaspResults[["edgelistContainer"]] <- edgelistContainer
  }

  nodeNames <- .ln1NetNodeNames(options)
  for (i in seq_along(options[["connectionList"]])) {
    edgelistOptions <- options[["connectionList"]][[i]]
    if (.ln1NetCheckEdgelist(edgelistOptions, nodeNames)) {
      edgelistName <- edgelistOptions[["name"]]
      edgelistState <- .ln1NetSingleEdgelist(edgelistOptions)
      edgelistState$dependOn(nestedOptions = list(c("connectionList", i, "connections")))
      jaspResults[["edgelistContainer"]][[edgelistName]] <- edgelistState
    }
  }
}

.ln1NetNodeNames <- function(options) {
  sapply(options[["problems"]], function(p) p[["problemName"]])
}

.ln1NetCheckEdgelist <- function(edgelistOptions, nodeNames = NULL) {
  return(all(sapply(edgelistOptions[["connections"]], function(path) {
    from <- path[["connectionFrom"]]
    to   <- path[["connectionTo"]]
    valid <- from != "" && to != "" && from != to
    if (valid && !is.null(nodeNames))
      valid <- from %in% nodeNames && to %in% nodeNames
    return(valid)
  })))
}

.ln1NetSingleEdgelist <- function(edgelistOptions) {
  edgelist <- data.frame(t(sapply(edgelistOptions[["connections"]], function(path) {
    return(c(path[["connectionFrom"]], path[["connectionTo"]], path[["connectionStrength"]]))
  })))
  names(edgelist) <- c("from", "to", "weight")
  edgelist[["weight"]] <- as.numeric(edgelist[["weight"]])
  edgelist[["absWeight"]] <- abs(edgelist[["weight"]])
  return(createJaspState(edgelist))
}

.ln1NetCentrality <- function(jaspResults, options) {
  if (is.null(jaspResults[["centralityContainer"]])) {
    jaspResults[["centralityContainer"]] <- createJaspContainer()
  }

  if (is.null(jaspResults[["centralityTableContainer"]])) {
    jaspResults[["centralityTableContainer"]] <- createJaspContainer(title = gettext("Centrality"))
  }

  if (!is.null(jaspResults[["edgelistContainer"]]) && length(jaspResults[["edgelistContainer"]]) > 0) {
    for (i in seq_along(options[["connectionList"]])) {
      edgelistOptions <- options[["connectionList"]][[i]]
      edgelistName <- edgelistOptions[["name"]]
      if (is.null(jaspResults[["centralityContainer"]][[edgelistName]])) {
        if (.ln1NetCheckEdgelist(edgelistOptions, .ln1NetNodeNames(options))) {
          centralityState <- createJaspState(
            .ln1NetCentralitySingle(
              jaspResults[["edgelistContainer"]][[edgelistName]]$object,
              jaspResults[["nodeAttributesState"]]$object,
              options
            )
          )
          centralityState$dependOn(
            nestedOptions = list(
              c("connectionList", i, "connections"),
              c("connectionList", i, "centrality")
            )
          )
          jaspResults[["centralityContainer"]][[edgelistName]] <- centralityState
        }
      }

      if (edgelistOptions[["centrality"]] && is.null(jaspResults[["centralityTableContainer"]][[edgelistName]]) && 
        !is.null(jaspResults[["centralityContainer"]][[edgelistName]])) {
        centralityTable <- createJaspTable(edgelistName)
        centralityTable$dependOn(
          nestedOptions = list(
            c("connectionList", i, "connections"),
            c("connectionList", i, "centrality")
          )
        )
        jaspResults[["centralityTableContainer"]][[edgelistName]] <- .ln1NetFillCentralityTable(
          centralityTable,
          jaspResults[["centralityContainer"]][[edgelistName]]$object,
          options
        )
      }
    }
  }
}

.ln1NetCentralitySingle <- function(edgelist, nodeAttributes, options) {
  centrality <- tidygraph::tbl_graph(nodes=nodeAttributes, edges=edgelist, directed = TRUE) |>
    tidygraph::activate(nodes) |>
    dplyr::mutate(
      degreeIn = tidygraph::centrality_degree(weights = weight, mode = "in"),
      degreeOut = tidygraph::centrality_degree(weights = weight, mode = "out")
    ) |>
    as.data.frame()
  
  return(centrality[, c("name", "degreeIn", "degreeOut")])
}

.ln1NetFillCentralityTable <- function(table, centrality, options) {
  degreeOvertitle <- gettext("Degree")

  table$addColumnInfo(name = "name", title = gettext("Problem"), type = "string")
  table$addColumnInfo(name = "degreeIn", title = gettext("In"), type = "number", overtitle = degreeOvertitle)
  table$addColumnInfo(name = "degreeOut", title = gettext("Out"), type = "number", overtitle = degreeOvertitle)

  table[["name"]] <- centrality[["name"]]
  table[["degreeIn"]] <- centrality[["degreeIn"]]
  table[["degreeOut"]] <- centrality[["degreeOut"]]

  return(table)
}

.ln1NetCreateNetworkPlots <- function(jaspResults, dataset, options, dependencyFun) {
  if(is.null(jaspResults[["networkPlotContainer"]])) {
    jaspResults[["networkPlotContainer"]] <- createJaspContainer(title = gettext("Network Plots"))
  }

  if (!is.null(jaspResults[["edgelistContainer"]]) && length(jaspResults[["edgelistContainer"]]) > 0) {
    for (i in seq_along(options[["connectionList"]])) {
      edgelistOptions <- options[["connectionList"]][[i]]
      if (edgelistOptions[["plotNetwork"]] && .ln1NetCheckEdgelist(edgelistOptions, .ln1NetNodeNames(options))) {
        edgelistName <- edgelistOptions[["name"]]
        dataPlot <- createJaspPlot(
          title = edgelistName,
          height = 480,
          width = 480,
          position = 2
        )
        dataPlot$dependOn(
          options = c(
            "plotLayout", "colorPalette", "plotSeverityFill", "plotSeveritySize", "plotSeverityAlpha", 
            "plotStrengthColor", "plotStrengthWidth", "plotStrengthAlpha"
          ),
          nestedOptions = list(
            c("connectionList", i, "connections"),
            c("connectionList", i, "plotNetwork")
          )
        )
        dataPlot$plotObject <- .ln1NetCreateNetworkPlotFill(
          jaspResults[["edgelistContainer"]][[edgelistName]]$object,
          jaspResults[["nodeAttributesState"]]$object,
          options
        )
        jaspResults[["networkPlotContainer"]][[edgelistName]] <- dataPlot
      }
    }
  }
}

.ln1NetCreateNetworkPlotFill <- function(edgelist, nodeAttributes, options) {
  gr <- tidygraph::tbl_graph(nodes=nodeAttributes, edges=edgelist, directed = TRUE)

  p <- ggraph::ggraph(
    gr,
    layout = options[["plotLayout"]],
    circular = options[["plotLayout"]] == "linear"
  )

  strengthQuosure <- "strength"
  weightQuosure <- "weight"
  absWeightQuosure <- "absWeight"

  nodeArgs <- list()
  textArgs <- list(label = "name")
  edgeArgs <- list()

  if (options[["plotSeverityFill"]]) {
    nodeArgs[["fill"]] <- strengthQuosure
  }

  if (options[["plotSeveritySize"]]) {
    nodeArgs[["size"]] <- strengthQuosure
  }

  if (options[["plotSeverityAlpha"]]) {
    nodeArgs[["alpha"]] <- strengthQuosure
  }

  if (options[["plotStrengthColor"]]) {
    edgeArgs[["edge_color"]] <- weightQuosure
  }

  if (options[["plotStrengthWidth"]]) {
    edgeArgs[["edge_width"]] <- absWeightQuosure
  }

  if (options[["plotStrengthAlpha"]]) {
    edgeArgs[["edge_alpha"]] <- absWeightQuosure
  }

  getAes <- function(...) {
    args <- lapply(list(...), function(x) if (!is.null(x)) ggplot2::sym(x))

    return(ggplot2::aes(!!!args))
  }

  # Edges: curved fan with filled arrowheads
  p <- p +
    ggraph::geom_edge_fan(
      mapping = do.call(getAes, args = edgeArgs),
      arrow = grid::arrow(type = "closed", angle = 25, length = grid::unit(3, "mm")),
      end_cap = ggraph::circle(14, "mm"),
      start_cap = ggraph::circle(14, "mm"),
      strength = 0.3,
      show.legend = FALSE
    )

  # Nodes: filled colored circles
  nodeStaticArgs <- list(
    mapping   = do.call(getAes, args = nodeArgs),
    shape     = 21,
    color     = "grey30",
    stroke    = 0.8,
    show.legend = !is.null(nodeArgs[["fill"]])
  )
  if (is.null(nodeArgs[["fill"]])) {
    nodeStaticArgs[["fill"]] <- "#B3BAC5"
  }
  if (is.null(nodeArgs[["size"]])) {
    nodeStaticArgs[["size"]] <- 12
  }
  p <- p + do.call(ggraph::geom_node_point, nodeStaticArgs)

  # Labels: repelled away from nodes and edges
  p <- p +
    ggraph::geom_node_text(
      mapping = do.call(getAes, args = textArgs),
      repel = TRUE,
      size = 5.5,
      fontface = "bold",
      bg.color = "white",
      bg.r = 0.15,
      point.padding = grid::unit(15, "mm"),
      box.padding = grid::unit(5, "mm"),
      min.segment.length = grid::unit(0, "mm"),
      force = 2,
      force_pull = 0.5,
      max.overlaps = Inf,
      show.legend = FALSE
    )

  p <- p +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.4)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.4)) +
    jaspGraphs::scale_JASPfill_continuous(
      name = gettext("Severity"),
      palette = options[["colorPalette"]],
      limits = c(0, 1),
      breaks = seq(0, 1, 0.5)
    ) +
    ggraph::scale_edge_width_continuous(limits = c(0, 1), range = c(0.3, 1.5), guide = "none") +
    ggraph::scale_edge_alpha_continuous(limits = c(0, 1), range = c(0, 1), guide = "none") +
    ggraph::scale_edge_color_gradient2(limits = c(-1, 1), low = "#D55E00", mid = "grey80", high = "#0072B2", guide = "none") +
    ggplot2::scale_size_continuous(limits = c(0, 1), range = c(10, 20), guide = "none") +
    ggplot2::scale_alpha_continuous(limits = c(0, 1), range = c(0.3, 1), guide = "none") +
    ggraph::theme_graph() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12)
    )

  return(p)
}

.ln1NetConcatenateEdgelists <- function(edgelistContainer, options) {
  edgelistList <- list()

  for (i in seq_along(options[["connectionList"]])) {
    edgelistOptions <- options[["connectionList"]][[i]]
    if (.ln1NetCheckEdgelist(edgelistOptions, .ln1NetNodeNames(options))) {
      edgelistName <- edgelistOptions[["name"]]
      edgelistList[[edgelistName]] <- edgelistContainer[[edgelistName]]$object
      edgelistList[[edgelistName]][["name"]] <- edgelistName
    }
  }

  return(Reduce(rbind, edgelistList))
}

.ln1NetSaveProblems <- function(jaspResults, options) {
  if (is.null(jaspResults[["problemSavePath"]])) {
    problemSavePath <- createJaspState()
    problemSavePath$dependOn(c("problems", "problemSavePath"))
    jaspResults[["problemSavePath"]] <- problemSavePath

    if (options[["problemSavePath"]] != "") {
      nodeAttributes <- jaspResults[["nodeAttributesState"]]$object
      utils::write.csv(nodeAttributes, file = options[["problemSavePath"]], row.names = FALSE)
    }
  }
}

.ln1NetSaveConnections <- function(jaspResults, options) {
  if (is.null(jaspResults[["connectionSavePath"]])) {
    connectionSavePath <- createJaspState()
    connectionSavePath$dependOn(c("connectionList", "connectionSavePath"))
    jaspResults[["connectionSavePath"]] <- connectionSavePath

    if (options[["connectionSavePath"]] != "") {
      edgelistDf <- .ln1NetConcatenateEdgelists(jaspResults[["edgelistContainer"]], options)
      utils::write.csv(edgelistDf[, c("name", "from", "to", "weight")], file = options[["connectionSavePath"]], row.names = FALSE)
    }
  }
}
