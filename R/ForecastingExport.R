# Explicit CSV writes use the same initialized-session/button contract as
# Network. The consumed event persists independently of calculation caches.

.ln1ForeBeginExport <- function(jaspResults, options) {
  session <- options[["forecastExportSession"]]
  hasSession <- is.character(session) && length(session) == 1L && !is.na(session) && nzchar(session)
  observed <- isTRUE(options[["forecastExportRequest"]])
  state <- jaspResults[["forecastExportState"]]
  record <- if (is.null(state)) NULL else state$object
  recognized <- is.list(record) && identical(record[["version"]], 1L)
  current <- recognized && hasSession && identical(record[["session"]], session)
  if (!current) {
    if (!recognized)
      record <- list(version = 1L, request = NULL, status = "pending")
    record[["session"]] <- session
    # QML resets restored request values to FALSE and supplies a fresh session
    # before R dispatch. A fresh TRUE therefore represents the first real click.
    clicked <- hasSession && observed
  } else {
    clicked <- "forecastExportRequest" %in% names(options) &&
      !identical(record[["observedRequest"]], observed)
  }
  record[["observedRequest"]] <- observed
  if (clicked) {
    record[["request"]] <- NULL
    record[["status"]] <- "pending"
    record[["error"]] <- NULL
  }
  # Persist before any model/data operation can fail. No automatic dependencies:
  # clearing this journal on an ordinary edit could replay a consumed click.
  jaspResults[["forecastExportState"]] <- createJaspState(object = record)
  return(clicked)
}

.ln1ForeExportRequest <- function(dataset, options) {
  model <- if (identical(options[["modelSpecification"]], "custom"))
    options[c("modelSpecification", "p", "d", "q")] else
    options[c("modelSpecification", "modelSpecificationAutoIc")]
  input <- if (identical(options[["inputType"]], "simulateData"))
    options[c("inputType", "noiseSd", "simArEffects", "simIEffect", "simMaEffects", "numSamples", "seed")] else
    options[c("inputType", "dependent", "time", "covariates")]
  # Changed CSV naming makes a previously saved request pending, without
  # changing the consumed button event or automatically rewriting the file.
  return(list(formatVersion = 2L, path = options[["forecastSave"]], input = input, model = model,
              horizon = options[["forecastLength"]], data = dataset))
}

.ln1ForeWriteCsv <- function(data, path) {
  .ln1WriteCsv(data, path)
}

.ln1ForeSaveForecast <- function(jaspResults, dataset, options, result, ready, clicked) {
  record <- jaspResults[["forecastExportState"]]$object
  request <- .ln1ForeExportRequest(dataset, options)
  if (!identical(record[["request"]], request)) {
    record[["request"]] <- request
    record[["status"]] <- "pending"
    record[["error"]] <- NULL
  }
  path <- options[["forecastSave"]]
  hasPath <- is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path)
  if (clicked && hasPath) {
    record[["status"]] <- "incomplete"
    if (isTRUE(ready) && !is.null(result)) {
      error <- result[["error"]]
      if (is.null(error) && !is.null(result[["predictions"]])) {
        exported <- result[["predictions"]]
        outcome <- .ln1ForeOutcomeLabel(options)
        if (outcome %in% setdiff(names(exported), "y"))
          outcome <- paste0(outcome, "_forecast")
        names(exported)[names(exported) == "y"] <- outcome
        error <- tryCatch({ .ln1ForeWriteCsv(exported, path); NULL },
          error = function(e) conditionMessage(e), warning = function(w) conditionMessage(w))
        record[["status"]] <- if (is.null(error)) "success" else "failed"
      } else if (!is.null(error)) {
        record[["status"]] <- "failed"
      }
      record[["error"]] <- error
    }
  }
  jaspResults[["forecastExportState"]] <- createJaspState(object = record)
  if (!hasPath) {
    jaspResults[["forecastExport"]] <- NULL
    return(invisible(NULL))
  }
  message <- switch(record[["status"]],
    success = gettextf("Forecasts saved to %1$s.", path),
    failed = c(gettext("The forecasts could not be saved. Correct the inputs or choose a writable CSV destination, then click Export CSV / Save again."), record[["error"]]),
    incomplete = gettext("No CSV was written. Select an outcome and request at least one forecast, then click Export CSV / Save again."),
    gettext("Changes have not been exported. Click Export CSV / Save again to write the current forecasts."))
  text <- paste0("<p>", .ln1EscapeHtml(message), "</p>", collapse = "")
  status <- createJaspHtml(text, elementType = "div", title = gettext("Forecast export"), position = 5)
  dependencies <- c(.ln1ForeGetDataDependencies(), "forecastLength", "forecastSave")
  dependencies <- c(dependencies, intersect(c("forecastExportRequest", "forecastExportSession"), names(options)))
  status$dependOn(dependencies)
  jaspResults[["forecastExport"]] <- status
  invisible(NULL)
}
