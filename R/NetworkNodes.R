# Preserve the full set of selected problems, including isolated problems.

.ln1NetUpgradeState <- function(jaspResults) {
  version <- 3L
  marker <- jaspResults[["networkNodesVersion"]]
  if (!is.null(marker) && identical(marker$object, version))
    return(invisible(NULL))

  # Version 1 retained isolated nodes. Version 2 adds severity and absolute
  # strength alongside signed sums and replaces the old Degree table labels.
  # Version 3 omits zero-rated plot edges. Preserve version-2 data, tables and
  # export state so this plot-only upgrade does not trigger another file write.
  keys <- "networkPlotContainer"
  if (is.null(marker) || !identical(marker$object, 2L))
    keys <- c("nodeAttributesState", "edgelistContainer", "centralityContainer",
              "centralityTableContainer", "edgeWeightTableContainer",
              "networkPlotContainer")
  # Keep earlier export-attempt markers: the export helper reports their unknown
  # outcome instead of rewriting an old destination during a cache upgrade.
  for (key in keys) {
    if (!is.null(jaspResults[[key]]))
      jaspResults[[key]] <- NULL
  }
  jaspResults[["networkNodesVersion"]] <- createJaspState(object = version)
  return(invisible(NULL))
}

.ln1NetEmptyEdgelist <- function() {
  data.frame(from = character(0), to = character(0),
             weight = numeric(0), absWeight = numeric(0),
             stringsAsFactors = FALSE)
}

.ln1NetValidStrength <- function(strength) {
  length(strength) == 1L && is.numeric(strength) && is.finite(strength)
}

.ln1NetEdgelistReady <- function(edgelistOptions, nodeNames) {
  if (!isTRUE(edgelistOptions[["allConnections"]]))
    return(.ln1NetCheckEdgelist(edgelistOptions, nodeNames))

  # The all-connections control supplies one row and target per selected node,
  # including hidden diagonal entries. An unfinished matrix is not an empty graph.
  rows <- edgelistOptions[["allConnectionStrengths"]]
  if (is.null(rows) || length(rows) != length(nodeNames))
    return(FALSE)
  return(all(vapply(seq_along(rows), function(i) {
    targets <- rows[[i]][["targets"]]
    if (is.null(targets) || length(targets) != length(nodeNames))
      return(FALSE)
    all(vapply(setdiff(seq_along(nodeNames), i), function(j) {
      .ln1NetValidStrength(targets[[j]][["connectionStrength"]])
    }, logical(1)))
  }, logical(1))))
}

.ln1NetEmptyNetworkRows <- function(edgelistContainer, options) {
  rows <- list()
  for (tab in options[["connectionList"]]) {
    if (!.ln1NetEdgelistReady(tab, .ln1NetNodeNames(options)))
      next
    edges <- edgelistContainer[[tab[["name"]]]]$object
    if (nrow(edges) == 0L) {
      rows[[length(rows) + 1L]] <- data.frame(
        type = "network", time = tab[["name"]], name = "", severity = "",
        from = "", to = "", weight = "", stringsAsFactors = FALSE
      )
    }
  }
  return(do.call(rbind, rows))
}
