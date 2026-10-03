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

  .ln1NetUpgradeState(jaspResults)

  .ln1Intro(jaspResults, options, .ln1NetIntroText)

  .ln1NetData(jaspResults, dataset, options)

  .ln1NetCreateNetworkPlots(jaspResults, dataset, options, .ln1NetGetDataDependencies)

  .ln1NetCentrality(jaspResults, options)

  .ln1NetEdgeWeightTables(jaspResults, options)

  .ln1NetSaveNetwork(jaspResults, options)
}

.ln1NetIntroText <- function() {
  return(gettext("PECAN (Perceived Causal Networks) is an approach used to map how individuals believe their symptoms, emotions, and behaviors influence one another. Instead of relying purely on statistical relationships, PECAN focuses on a client’s (or their therapists) own perception of causal connections within their psychological system. In clinical practice, PECAN can help make these perceived relationships explicit and structured. For example, a client may believe that poor sleep leads to increased anxiety, which in turn leads to avoidance behavior. By visualizing such connections, therapists and clients can gain insight into how problems are maintained and identify potential targets for intervention. 

<b>How does it work?</b> 

PECAN is typically based on self-reported data, where clients indicate how strongly they believe different variables influence each other. These variables can include symptoms (e.g., anxiety, mood), behaviors (e.g., avoidance), or contextual factors (e.g., stress). The result is a network structure in which nodes represent variables and connections represent perceived causal effects. Unlike purely data-driven models, PECAN captures subjective beliefs about causality. This makes it especially useful in therapy, as these beliefs often guide behavior and coping strategies. By mapping them, therapists can work together with clients to evaluate whether certain perceived relationships are helpful, accurate, or potentially maladaptive.
PECAN can also be combined with time series data, allowing a comparison between perceived relationships and statistically estimated relationships (e.g., from VAR models). This can reveal discrepancies between what a client believes and what is observed in their data.

<b>Practical considerations</b> 

PECAN relies on the client’s insight and ability to reflect on their own experiences. As such, the quality of the network depends on how well the client can articulate perceived relationships. It may be helpful to guide clients through the process using structured prompts or examples. Because PECAN reflects subjective beliefs, it does not require large amounts of repeated measurements. However, it can be enriched by integrating it with longitudinal data, especially when used alongside other analytical approaches. 

<b>Interpretation and limitations</b> 

It is important to recognize that PECAN represents perceived causality, not necessarily actual causal mechanisms. Clients’ (and therapists’) beliefs may be incomplete, biased, or influenced by current mood or recent experiences. Nevertheless, these perceptions are clinically meaningful, as they can shape behavior and emotional responses. PECAN should therefore be used as a tool for discussion and hypothesis generation, rather than as definitive evidence about causal processes. When combined with other methods—such as forecasting or VAR models—it can provide a more comprehensive understanding of both subjective experience and observed dynamics.
"))
} 


.ln1NetGetDataDependencies <- function() {
  return(c("problems", "connectionList"))
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
    edgelistContainer$dependOn(.ln1NetGetDataDependencies())
    jaspResults[["edgelistContainer"]] <- edgelistContainer
  }

  nodeNames <- .ln1NetNodeNames(options)
  for (i in seq_along(options[["connectionList"]])) {
    edgelistOptions <- options[["connectionList"]][[i]]
    edgelistName <- edgelistOptions[["name"]]
    if (!.ln1NetEdgelistReady(edgelistOptions, nodeNames))
      next
    if (isTRUE(edgelistOptions[["allConnections"]])) {
      edgelistState <- .ln1NetAllEdges(edgelistOptions[["allConnectionStrengths"]], nodeNames)
    } else {
      edgelistState <- .ln1NetSingleEdgelist(edgelistOptions)
    }
    jaspResults[["edgelistContainer"]][[edgelistName]] <- edgelistState
  }
}

.ln1NetNodeNames <- function(options) {
  sapply(options[["problems"]], function(p) p[["problemName"]])
}

.ln1NetAllEdges <- function(allConnStrengths, nodeNames) {
  edges <- .ln1NetEmptyEdgelist()
  for (i in seq_along(allConnStrengths)) {
    fromName <- nodeNames[i]
    if (is.na(fromName) || fromName == "") next
    targets <- allConnStrengths[[i]][["targets"]]
    for (j in seq_along(targets)) {
      toName <- nodeNames[j]
      if (is.na(toName) || toName == "") next
      if (fromName != toName) {
        strength <- as.numeric(targets[[j]][["connectionStrength"]])
        edges <- rbind(edges, data.frame(
          from = fromName, to = toName,
          weight = strength, absWeight = abs(strength),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  return(createJaspState(edges))
}

.ln1NetFindSelfLoops <- function(edgelistOptions) {
  loops <- character(0)
  for (path in edgelistOptions[["connections"]]) {
    from <- path[["connectionFrom"]]
    to   <- path[["connectionTo"]]
    if (isTRUE(nzchar(from)) && isTRUE(nzchar(to)) && isTRUE(from == to))
      loops <- c(loops, from)
  }
  return(unique(loops))
}

.ln1NetCheckEdgelist <- function(edgelistOptions, nodeNames = NULL) {
  connections <- edgelistOptions[["connections"]]
  if (is.null(connections))
    return(FALSE)
  return(all(vapply(connections, function(path) {
    from <- path[["connectionFrom"]]
    to   <- path[["connectionTo"]]
    valid <- isTRUE(nzchar(from)) && isTRUE(nzchar(to)) &&
      isTRUE(from != to) && .ln1NetValidStrength(path[["connectionStrength"]])
    if (valid && !is.null(nodeNames))
      valid <- from %in% nodeNames && to %in% nodeNames
    return(valid)
  }, logical(1))))
}

.ln1NetSingleEdgelist <- function(edgelistOptions) {
  if (length(edgelistOptions[["connections"]]) == 0L)
    return(createJaspState(.ln1NetEmptyEdgelist()))

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
    jaspResults[["centralityContainer"]]$dependOn(.ln1NetGetDataDependencies())
  }

  if (is.null(jaspResults[["centralityTableContainer"]])) {
    jaspResults[["centralityTableContainer"]] <- createJaspContainer(title = gettext("Connection Summaries"))
    jaspResults[["centralityTableContainer"]]$dependOn(.ln1NetGetDataDependencies())
  }

  if (!is.null(jaspResults[["edgelistContainer"]]) && length(jaspResults[["edgelistContainer"]]) > 0) {
    for (i in seq_along(options[["connectionList"]])) {
      edgelistOptions <- options[["connectionList"]][[i]]
      edgelistName <- edgelistOptions[["name"]]
      if (is.null(jaspResults[["centralityContainer"]][[edgelistName]])) {
        if (.ln1NetEdgelistReady(edgelistOptions, .ln1NetNodeNames(options))) {
          edgelist <- jaspResults[["edgelistContainer"]][[edgelistName]]$object
          nodeAttrs <- jaspResults[["nodeAttributesState"]]$object
          centralityState <- createJaspState(
            .ln1NetCentralitySingle(
              edgelist,
              nodeAttrs,
              options
            )
          )
          jaspResults[["centralityContainer"]][[edgelistName]] <- centralityState
        }
      }

      if (isTRUE(edgelistOptions[["centrality"]]) && is.null(jaspResults[["centralityTableContainer"]][[edgelistName]]) &&
        !is.null(jaspResults[["centralityContainer"]][[edgelistName]])) {
        centralityTable <- createJaspTable(edgelistName)
        edges <- jaspResults[["edgelistContainer"]][[edgelistName]]$object
        if (any(!.ln1NetNodeNames(options) %in% c(edges[["from"]], edges[["to"]])))
          centralityTable$addFootnote(gettext("Problems without entered connections have zero incoming and outgoing values. This does not establish the absence of real-world influence."))
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
  # Keep the existing degreeIn/degreeOut IDs for saved output compatibility.
  # These are signed sums, not counts; the displayed labels state that explicitly.
  centrality <- tidygraph::tbl_graph(nodes=nodeAttributes, edges=edgelist, directed = TRUE) |>
    tidygraph::activate(nodes) |>
    dplyr::mutate(
      severity = strength,
      strengthIn = tidygraph::centrality_degree(weights = abs(weight), mode = "in"),
      strengthOut = tidygraph::centrality_degree(weights = abs(weight), mode = "out"),
      degreeIn = tidygraph::centrality_degree(weights = weight, mode = "in"),
      degreeOut = tidygraph::centrality_degree(weights = weight, mode = "out")
    ) |>
    as.data.frame()

  return(centrality[, c("name", "severity", "strengthOut", "degreeOut", "strengthIn", "degreeIn")])
}

.ln1NetFillCentralityTable <- function(table, centrality, options) {
  table$addColumnInfo(name = "name", title = gettext("Problem"), type = "string")
  table$addColumnInfo(name = "severity", title = gettext("Severity"), type = "number")
  table$addColumnInfo(name = "strengthOut", title = gettext("Absolute strength"), type = "number", overtitle = gettext("Outgoing"))
  table$addColumnInfo(name = "degreeOut", title = gettext("Signed sum"), type = "number", overtitle = gettext("Outgoing"))
  table$addColumnInfo(name = "strengthIn", title = gettext("Absolute strength"), type = "number", overtitle = gettext("Incoming"))
  table$addColumnInfo(name = "degreeIn", title = gettext("Signed sum"), type = "number", overtitle = gettext("Incoming"))

  for (column in names(centrality))
    table[[column]] <- centrality[[column]]

  table$addFootnote(gettext("Outgoing summarizes connections from a problem; incoming summarizes connections to it. Absolute strength sums the absolute ratings. Signed sum adds the ratings with their signs."))
  table$addFootnote(gettext("Ratings of +0.7 and -0.7 give an absolute strength of 1.4 and a signed sum of 0. A zero signed sum can reflect cancellation, even when connections are present."))
  table$addFootnote(gettext("Severity ranges from 0 to 1, is shared across time points, and does not weight these sums. These summaries describe perceived connections, not predicted treatment benefit or treatment priorities."))

  return(table)
}

.ln1NetEdgeWeightTables <- function(jaspResults, options) {
  if (is.null(jaspResults[["edgeWeightTableContainer"]])) {
    jaspResults[["edgeWeightTableContainer"]] <- createJaspContainer(title = gettext("Edge Weights"))
    jaspResults[["edgeWeightTableContainer"]]$dependOn(.ln1NetGetDataDependencies())
  }

  if (!is.null(jaspResults[["edgelistContainer"]]) && length(jaspResults[["edgelistContainer"]]) > 0) {
    nodeNames <- .ln1NetNodeNames(options)
    for (i in seq_along(options[["connectionList"]])) {
      edgelistOptions <- options[["connectionList"]][[i]]
      edgelistName <- edgelistOptions[["name"]]
      if (isTRUE(edgelistOptions[["edgeWeightTable"]]) && is.null(jaspResults[["edgeWeightTableContainer"]][[edgelistName]])) {
        if (.ln1NetEdgelistReady(edgelistOptions, nodeNames)) {
          edgelist <- jaspResults[["edgelistContainer"]][[edgelistName]]$object
          edgeTable <- createJaspTable(edgelistName)
          edgeTable$addColumnInfo(name = "from",   title = gettext("From"),   type = "string")
          edgeTable$addColumnInfo(name = "to",     title = gettext("To"),     type = "string")
          edgeTable$addColumnInfo(name = "weight", title = gettext("Weight"), type = "number")

          edgeTable[["from"]]   <- edgelist[["from"]]
          edgeTable[["to"]]     <- edgelist[["to"]]
          edgeTable[["weight"]] <- edgelist[["weight"]]
          if (nrow(edgelist) == 0L)
            edgeTable$addFootnote(gettext("No connections have been entered for this time point. All selected problems remain in the network."))

          jaspResults[["edgeWeightTableContainer"]][[edgelistName]] <- edgeTable
        }
      }
    }
  }
}

.ln1NetCreateNetworkPlots <- function(jaspResults, dataset, options, dependencyFun) {
  if(is.null(jaspResults[["networkPlotContainer"]])) {
    jaspResults[["networkPlotContainer"]] <- createJaspContainer(title = gettext("Network Plots"))
    jaspResults[["networkPlotContainer"]]$dependOn(dependencyFun())
  }

  if (!is.null(jaspResults[["edgelistContainer"]])) {
    nodeNames <- .ln1NetNodeNames(options)
    for (i in seq_along(options[["connectionList"]])) {
      edgelistOptions <- options[["connectionList"]][[i]]
      if (isTRUE(edgelistOptions[["plotNetwork"]])) {
        edgelistName <- edgelistOptions[["name"]]
        useAllConnections <- isTRUE(edgelistOptions[["allConnections"]])
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
          )
        )

        edgelistReady <- .ln1NetEdgelistReady(edgelistOptions, nodeNames)
        selfLoops <- if (useAllConnections) character(0) else .ln1NetFindSelfLoops(edgelistOptions)

        if (length(selfLoops) > 0) {
          dataPlot$setError(gettextf(
            "A problem cannot be connected to itself. Please fix: %s.",
            paste(selfLoops, collapse = ", ")
          ))
        } else if (edgelistReady) {
          edgelist <- jaspResults[["edgelistContainer"]][[edgelistName]]$object
          nodeAttrs <- jaspResults[["nodeAttributesState"]]$object
          dataPlot$plotObject <- .ln1NetCreateNetworkPlotFill(
            edgelist,
            nodeAttrs,
            options
          )
        } else if (useAllConnections) {
          dataPlot$setError(gettext("Complete the connection strengths for all selected problems."))
        } else {
          dataPlot$setError(gettext("Complete each connection with two different selected problems and a strength, or remove the unfinished row. To show a network without connections, remove all connection rows."))
        }

        jaspResults[["networkPlotContainer"]][[edgelistName]] <- dataPlot
      }
    }
  }
}

.ln1NetCreateNetworkPlotFill <- function(edgelist, nodeAttributes, options) {
  # Zero is an entered rating of no perceived relationship. Keep the raw rating
  # in tables and exports, but exclude it from the plotted topology and arrows.
  edgelist <- edgelist[edgelist[["weight"]] != 0, , drop = FALSE]
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
    if (.ln1NetEdgelistReady(edgelistOptions, .ln1NetNodeNames(options))) {
      edgelistName <- edgelistOptions[["name"]]
      edgelistList[[edgelistName]] <- edgelistContainer[[edgelistName]]$object
      edgelistList[[edgelistName]][["name"]] <- rep(edgelistName, nrow(edgelistList[[edgelistName]]))
    }
  }

  return(Reduce(rbind, edgelistList))
}
