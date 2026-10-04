# An export click must be consumed even when analysis preparation fails before
# the export helper runs. Repairing the input is not another export request.
.ln1NetConsumeBlockedExportRequest <- function(jaspResults, options) {
  # Use the raw session/token: the scale or rating options may themselves be
  # invalid. A missing request forces the next successful run to show pending.
  record <- list(version = 3L, session = options[["networkExportSession"]],
                 observedRequest = isTRUE(options[["networkExportRequest"]]),
                 request = NULL, status = "pending")
  jaspResults[["networkSavePath"]] <- createJaspState(object = record)
  jaspResults[["networkExport"]] <- NULL
  invisible(NULL)
}
