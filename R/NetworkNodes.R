# Preserve the full set of selected problems, including isolated problems.

.ln1NetUpgradeState <- function(jaspResults) {
  version <- 5L
  marker <- jaspResults[["networkNodesVersion"]]
  if (!is.null(marker) && identical(marker$object, version))
    return(invisible(NULL))

  # Rebuild results with the current keyed-edge schema and dependencies.
  # Export intent is separate: rebuilding cached output never requests a write.
  keys <- c("introText", "nodeAttributesState", "edgelistContainer", "centralityContainer",
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
  .ln1NetFiniteScalar(strength) && strength >= -1 && strength <= 1
}

.ln1NetAllConnectionMatrix <- function(rows, nodeNames) {
  result <- list(rows = NULL, invalidKeys = FALSE)
  if (!is.list(rows) || !all(vapply(rows, is.list, logical(1))))
    return(result)
  allRows <- c(rows, unlist(lapply(rows, function(row) row[["targets"]]), recursive = FALSE))
  if (!all(vapply(allRows, is.list, logical(1))))
    return(result)
  hasKey <- vapply(allRows, function(row) "value" %in% names(row), logical(1))
  keyed <- any(hasKey)
  # Only wholly unkeyed legacy/programmatic matrices use position. Never mix
  # positional identity with the explicit problem names supplied by JASP.
  if (keyed && !all(hasKey)) {
    result[["invalidKeys"]] <- TRUE
    return(result)
  }
  orderRows <- function(items) {
    if (!is.list(items)) return(NULL)
    if (keyed && length(items)) {
      valid <- vapply(items, function(item) {
        key <- item[["value"]]
        is.character(key) && length(key) == 1L && !is.na(key) && key %in% nodeNames
      }, logical(1))
      keys <- if (all(valid)) vapply(items, `[[`, character(1), "value") else character()
      if (!all(valid) || anyDuplicated(keys)) {
        result[["invalidKeys"]] <<- TRUE
        return(NULL)
      }
      if (length(keys) != length(nodeNames)) return(NULL)
      return(items[match(nodeNames, keys)])
    }
    if (length(items) != length(nodeNames)) return(NULL)
    items
  }
  ordered <- orderRows(rows)
  # Check target keys even if the outer list is not complete yet.
  targets <- lapply(rows, function(row) orderRows(row[["targets"]]))
  if (result[["invalidKeys"]] || is.null(ordered) || any(vapply(targets, is.null, logical(1))))
    return(result)
  if (keyed)
    targets <- targets[match(nodeNames, vapply(rows, `[[`, character(1), "value"))]
  for (i in seq_along(ordered)) ordered[[i]][["targets"]] <- targets[[i]]
  result[["rows"]] <- ordered
  result
}

.ln1NetEdgelistReady <- function(edgelistOptions, nodeNames) {
  if (!isTRUE(edgelistOptions[["allConnections"]]))
    return(.ln1NetCheckEdgelist(edgelistOptions, nodeNames))

  # The all-connections control supplies one row and target per selected node,
  # including hidden diagonal entries. An unfinished matrix is not an empty graph.
  rows <- .ln1NetAllConnectionMatrix(edgelistOptions[["allConnectionStrengths"]], nodeNames)[["rows"]]
  if (is.null(rows))
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
