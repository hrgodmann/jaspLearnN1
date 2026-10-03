# CSV export is optional: file errors must not invalidate the network results.

.ln1NetExportDependencies <- function(options) {
  dependencies <- c(.ln1NetGetDataDependencies(), "networkSavePath")
  if ("networkExportRequest" %in% names(options))
    dependencies <- c(dependencies, "networkExportRequest")
  if ("networkExportSession" %in% names(options))
    dependencies <- c(dependencies, "networkExportSession")
  return(dependencies)
}

.ln1NetExportRequest <- function(options) {
  assessments <- lapply(options[["connectionList"]], function(tab) {
    allConnections <- isTRUE(tab[["allConnections"]])
    list(name = tab[["name"]], allConnections = allConnections,
         ratings = if (allConnections) tab[["allConnectionStrengths"]] else tab[["connections"]])
  })
  return(list(path = options[["networkSavePath"]], problems = options[["problems"]],
              assessments = assessments))
}

.ln1NetExportData <- function(jaspResults, options) {
  nodeAttributes <- jaspResults[["nodeAttributesState"]]$object
  edgelistDf <- .ln1NetConcatenateEdgelists(jaspResults[["edgelistContainer"]], options)
  nodeRows <- data.frame(
    type = "node", time = "", name = nodeAttributes[["name"]],
    severity = nodeAttributes[["strength"]], from = "", to = "", weight = "",
    stringsAsFactors = FALSE
  )
  edgeRows <- NULL
  if (!is.null(edgelistDf) && nrow(edgelistDf) > 0L) {
    edgeRows <- data.frame(
      type = "edge", time = edgelistDf[["name"]], name = "", severity = "",
      from = edgelistDf[["from"]], to = edgelistDf[["to"]], weight = edgelistDf[["weight"]],
      stringsAsFactors = FALSE
    )
  }
  emptyNetworkRows <- .ln1NetEmptyNetworkRows(jaspResults[["edgelistContainer"]], options)
  return(rbind(nodeRows, edgeRows, emptyNetworkRows))
}

.ln1NetWriteCsv <- function(data, path) {
  if (dir.exists(path))
    stop(gettext("The destination is a folder. Choose a CSV file name."), call. = FALSE)
  if (!dir.exists(dirname(path)))
    stop(gettext("The destination folder does not exist."), call. = FALSE)
  if (file.exists(path) && file.access(path, mode = 2L) != 0L)
    stop(gettext("The destination file is not writable."), call. = FALSE)

  # Serialize fully before replacing an existing export. A same-directory rename
  # avoids cross-filesystem moves; a failed write/rename leaves the old file alone.
  temporary <- tempfile(pattern = ".jasp-network-", tmpdir = dirname(path), fileext = ".csv")
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(data, file = temporary, row.names = FALSE)
  if (!file.rename(temporary, path))
    stop(gettext("The CSV could not replace the destination file."), call. = FALSE)
  return(invisible(NULL))
}

.ln1NetExportEscape <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  return(gsub("'", "&#39;", text, fixed = TRUE))
}

.ln1NetExportStatus <- function(jaspResults, record, options) {
  if (record[["status"]] == "pending") {
    messages <- gettext("Changes have not been exported. Click Export CSV / Save again to write the current networks.")
  } else if (record[["status"]] == "legacy") {
    messages <- gettext("This saved analysis contains an earlier export request whose outcome is unknown. Click Export CSV / Save again to save the current networks.")
  } else if (record[["status"]] == "failed") {
    messages <- c(gettext("The CSV could not be saved. Check the destination folder and permissions, then click Export CSV / Save again. Choosing another destination does not save the file until you click the button."),
                  record[["error"]])
  } else if (record[["status"]] == "incomplete") {
    messages <- gettext("No CSV was written because no assessments are complete. Complete or remove unfinished connections, then click Export CSV / Save again. Any existing file was left unchanged.")
  } else {
    messages <- c(gettextf("Saved CSV to %1$s.", record[["request"]][["path"]]),
                  gettextf("Exported %1$s.", paste(record[["completed"]], collapse = ", ")))
  }
  if (record[["status"]] %in% c("success", "incomplete")) {
    for (name in record[["omitted"]])
      messages <- c(messages, gettextf("%1$s was omitted because it contains unfinished or invalid connections.", name))
  }
  messages <- c(gettextf("CSV destination: %1$s.", options[["networkSavePath"]]), messages)
  text <- paste0("<p>", .ln1NetExportEscape(messages), "</p>", collapse = "")
  status <- createJaspHtml(text, elementType = "div", title = gettext("Network export"), position = 5)
  status$dependOn(.ln1NetExportDependencies(options))
  jaspResults[["networkExport"]] <- status
  return(invisible(NULL))
}

.ln1NetSaveNetwork <- function(jaspResults, options) {
  request <- .ln1NetExportRequest(options)
  observedRequest <- isTRUE(options[["networkExportRequest"]])
  session <- options[["networkExportSession"]]
  hasSession <- is.character(session) && length(session) == 1L &&
    !is.na(session) && nzchar(session)
  state <- jaspResults[["networkSavePath"]]
  record <- if (!is.null(state)) state$object else NULL
  current <- hasSession && is.list(record) && identical(record[["version"]], 3L) &&
    identical(record[["session"]], session)
  clicked <- FALSE

  if (!current) {
    previous <- record
    record <- list(version = 3L, session = session, observedRequest = observedRequest,
                   request = request, status = if (is.null(state)) "pending" else "legacy",
                   lastAttempt = NULL)
    # QML resets the button to FALSE and creates a fresh session after restoring
    # controls. TRUE in that fresh session is the first actual click, even if
    # opening a completed analysis did not run R before the user pressed it.
    clicked <- hasSession && observedRequest
    if (is.list(previous) && isTRUE(previous[["version"]] %in% c(1L, 2L, 3L))) {
      if (identical(previous[["version"]], 1L))
        previous[["request"]][["retry"]] <- NULL
      record[["lastAttempt"]] <- previous[["lastAttempt"]]
      if (is.null(record[["lastAttempt"]]) &&
          previous[["status"]] %in% c("success", "failed", "incomplete"))
        record[["lastAttempt"]] <- previous[c("request", "status", "completed", "omitted", "error")]
      if (identical(previous[["request"]], request)) {
        for (key in c("status", "completed", "omitted", "error"))
          record[[key]] <- previous[[key]]
      } else {
        record[["status"]] <- "pending"
      }
    }
  } else {
    clicked <- "networkExportRequest" %in% names(options) &&
      !identical(record[["observedRequest"]], observedRequest)
    # Consume the click even if no file can be written. Fixing inputs later must
    # not turn an earlier failed or incomplete request into an automatic write.
    record[["observedRequest"]] <- observedRequest
    if (!identical(record[["request"]], request)) {
      record[["request"]] <- request
      record[["status"]] <- "pending"
      for (key in c("completed", "omitted", "error"))
        record[[key]] <- NULL
    }
  }

  path <- options[["networkSavePath"]]
  hasPath <- !is.null(path) && isTRUE(nzchar(path))
  if (clicked && hasPath) {
    tabs <- options[["connectionList"]]
    ready <- vapply(tabs, .ln1NetEdgelistReady, logical(1), nodeNames = .ln1NetNodeNames(options))
    assessmentNames <- vapply(tabs, `[[`, character(1), "name")
    record[["status"]] <- "incomplete"
    record[["completed"]] <- assessmentNames[ready]
    record[["omitted"]] <- assessmentNames[!ready]
    record[["error"]] <- NULL
    if (any(ready)) {
      error <- tryCatch({
        .ln1NetWriteCsv(.ln1NetExportData(jaspResults, options), path)
        NULL
      }, error = function(e) conditionMessage(e), warning = function(w) conditionMessage(w))
      record[["status"]] <- if (is.null(error)) "success" else "failed"
      record[["error"]] <- error
    }
    record[["lastAttempt"]] <- record[c("request", "status", "completed", "omitted", "error")]
  }

  # Persist the observed button value across every rerun and path clearing.
  # This side-effect journal intentionally has no option dependencies. The
  # current payload is compared above; only a new button transition may write.
  jaspResults[["networkSavePath"]] <- createJaspState(object = record)
  if (hasPath)
    .ln1NetExportStatus(jaspResults, record, options)
  else
    jaspResults[["networkExport"]] <- NULL
  return(invisible(NULL))
}
