# An export click must be consumed even when analysis preparation fails before
# the export helper runs. Repairing the input is not another export request.
.ln1NetConsumeBlockedExportRequest <- function(jaspResults, options) {
  state <- jaspResults[["networkSavePath"]]
  previous <- if (is.null(state)) NULL else state$object
  lastAttempt <- NULL
  if (is.list(previous)) {
    lastAttempt <- previous[["lastAttempt"]]
    if (is.null(lastAttempt) &&
        isTRUE(previous[["status"]] %in% c("success", "failed", "incomplete")))
      lastAttempt <- previous[c("request", "status", "completed", "omitted", "error")]
  }

  # Use the raw session/token: the scale or rating options may themselves be
  # invalid. A missing request forces the next successful run to show pending.
  record <- list(version = 3L, session = options[["networkExportSession"]],
                 observedRequest = isTRUE(options[["networkExportRequest"]]),
                 request = NULL, status = "pending", lastAttempt = lastAttempt)
  jaspResults[["networkSavePath"]] <- createJaspState(object = record)
  jaspResults[["networkExport"]] <- NULL
  invisible(NULL)
}
