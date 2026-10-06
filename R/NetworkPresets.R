# Presets are reusable definitions, never patient ratings or assessment data.
# Loading prepares QML sources; applying them is a separate explicit UI action.

.ln1NetPresetString <- function(value, nonempty = FALSE) {
  is.character(value) && length(value) == 1L && !is.na(value) &&
    (!nonempty || nzchar(value))
}

# Explicit cross-runtime contract, including NEL, Mongolian vowel separator and
# BOM. Keep identical to _trimPresetText in NetworkPresetData.js and its fixtures.
.ln1NetPresetTrim <- function(value) {
  trimws(value, whitespace = "[\u0009-\u000d\u0020\u0085\u00a0\u1680\u180e\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]")
}

.ln1NetPresetObject <- function(value, allowed) {
  is.list(value) && !is.null(names(value)) &&
    !anyDuplicated(names(value)) && all(names(value) %in% allowed)
}

.ln1NetPresetName <- function(value) {
  .ln1NetPresetString(value) && nzchar(.ln1NetPresetTrim(value)) &&
    !grepl("[\\x{0000}-\\x{001f}\\x{007f}]", value, perl = TRUE)
}

.ln1NetPresetMaximum <- function(value) {
  is.numeric(value) && !is.complex(value) && length(value) == 1L && is.finite(value) &&
    value >= 1 && value <= 1000 && value == floor(value)
}

.ln1NetValidatePreset <- function(preset) {
  if (!.ln1NetPresetObject(preset, c("schemaVersion", "name", "problems", "scales")))
    stop(gettext("The preset must contain only its version, name, problem definitions, and scales."), call. = FALSE)
  if (!is.numeric(preset[["schemaVersion"]]) || is.complex(preset[["schemaVersion"]]) ||
      length(preset[["schemaVersion"]]) != 1L ||
      !isTRUE(preset[["schemaVersion"]] == 1))
    stop(gettext("The preset format version is unsupported. Version 1 is required."), call. = FALSE)
  if (!.ln1NetPresetName(preset[["name"]]))
    stop(gettext("The preset needs a nonempty name without control characters."), call. = FALSE)
  problems <- preset[["problems"]]
  if (!is.list(problems) || !is.null(names(problems)) ||
      length(problems) < 2L || length(problems) > 10L)
    stop(gettext("A preset must contain between 2 and 10 problem definitions."), call. = FALSE)
  problems <- lapply(seq_along(problems), function(i) {
    problem <- problems[[i]]
    if (!.ln1NetPresetObject(problem, c("problemName", "problemDescription")))
      stop(gettextf("Problem %1$i must contain only a name and an optional description.", i), call. = FALSE)
    if (!.ln1NetPresetName(problem[["problemName"]]))
      stop(gettextf("Problem %1$i needs a nonempty name without control characters.", i), call. = FALSE)
    result <- list(problemName = .ln1NetPresetTrim(problem[["problemName"]]))
    if ("problemDescription" %in% names(problem)) {
      if (!.ln1NetPresetString(problem[["problemDescription"]]))
        stop(gettextf("The description for problem %1$i must be text.", i), call. = FALSE)
      result[["problemDescription"]] <- .ln1NetPresetTrim(problem[["problemDescription"]])
    }
    result
  })
  if (anyDuplicated(vapply(problems, `[[`, character(1), "problemName")))
    stop(gettext("Problem names in a preset must be unique."), call. = FALSE)
  scales <- preset[["scales"]]
  if (!.ln1NetPresetObject(scales, c("severityMax", "connectionMax", "connectionInput")) ||
      !.ln1NetPresetMaximum(scales[["severityMax"]]) ||
      !.ln1NetPresetMaximum(scales[["connectionMax"]]))
    stop(gettext("Preset scale maxima must be whole numbers from 1 to 1000."), call. = FALSE)
  normalizedScales <- list(severityMax = as.integer(scales[["severityMax"]]),
                           connectionMax = as.integer(scales[["connectionMax"]]))
  if ("connectionInput" %in% names(scales)) {
    if (!.ln1NetPresetString(scales[["connectionInput"]]) ||
        !scales[["connectionInput"]] %in% c("signed", "magnitude"))
      stop(gettext("The preset connection input must be signed or magnitude."), call. = FALSE)
    normalizedScales[["connectionInput"]] <- scales[["connectionInput"]]
  }
  list(schemaVersion = 1L, name = .ln1NetPresetTrim(preset[["name"]]),
       problems = problems, scales = normalizedScales)
}

.ln1NetReadPreset <- function(path) {
  if (!.ln1NetPresetString(path, nonempty = TRUE) || !file.exists(path) || dir.exists(path))
    stop(gettext("Choose an existing preset JSON file."), call. = FALSE)
  size <- file.info(path)[["size"]]
  if (!is.finite(size) || size > 1024^2)
    stop(gettext("The preset file is too large. The maximum size is 1 MiB."), call. = FALSE)
  bytes <- readBin(path, what = "raw", n = 1024^2 + 1L)
  if (length(bytes) > 1024^2)
    stop(gettext("The preset file is too large. The maximum size is 1 MiB."), call. = FALSE)
  text <- rawToChar(bytes)
  preset <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
                     error = function(e) stop(gettext("The selected file is not valid preset JSON."), call. = FALSE))
  .ln1NetValidatePreset(preset)
}

.ln1NetWritePreset <- function(preset, path) {
  preset <- .ln1NetValidatePreset(preset)
  if (!.ln1NetPresetString(path, nonempty = TRUE) || dir.exists(path))
    stop(gettext("Choose a JSON file name for the preset."), call. = FALSE)
  text <- jsonlite::toJSON(preset, auto_unbox = TRUE, pretty = TRUE, null = "null")
  if (nchar(text, type = "bytes") > 1024^2)
    stop(gettext("The preset file is too large. The maximum size is 1 MiB."), call. = FALSE)
  .ln1WriteExport(path, function(temporary)
    writeLines(enc2utf8(text), temporary, useBytes = TRUE))
}

.ln1NetPresetRequest <- function(options) {
  # Deliberate allowlist: severity, connection ratings, tabs and export paths
  # cannot enter the reusable definition or affect its pending/saved status.
  problems <- lapply(options[["problems"]], function(problem) {
    result <- list(problemName = problem[["problemName"]])
    if ("problemDescription" %in% names(problem))
      result[["problemDescription"]] <- problem[["problemDescription"]]
    result
  })
  input <- options[["networkConnectionInput"]]
  if (is.null(input)) input <- "signed"
  severityMaximum <- options[["networkSeverityMaximum"]]
  if (is.null(severityMaximum)) severityMaximum <- 1L
  connectionMaximum <- options[["networkConnectionMaximum"]]
  if (is.null(connectionMaximum)) connectionMaximum <- 1L
  list(path = options[["networkPresetSavePath"]],
       preset = list(schemaVersion = 1L, name = options[["networkPresetSaveName"]],
                     problems = problems,
                     scales = list(severityMax = severityMaximum,
                                   connectionMax = connectionMaximum,
                                   connectionInput = input)))
}

.ln1NetPresetLoadToken <- function(options) {
  session <- options[["networkExportSession"]]
  path <- options[["networkPresetLoadPath"]]
  if (!.ln1NetPresetString(session, nonempty = TRUE) || !.ln1NetPresetString(path, nonempty = TRUE))
    return("")
  paste(session, path, if (isTRUE(options[["networkPresetLoadRequest"]])) "true" else "false", sep = "\n")
}

.ln1NetPresetSources <- function(jaspResults, preset, token, options) {
  # Native JASP serializes character(0) as "", which can create a blank term.
  # Empty lists are genuine JSON arrays and remove every preview term instead.
  values <- list(names = list(), descriptions = list(), name = list(),
                 severityMaximum = list(), connectionMaximum = list(),
                 connectionInput = list(), loadedToken = list())
  if (!is.null(preset)) {
    values[["name"]] <- preset[["name"]]
    values[["names"]] <- vapply(preset[["problems"]], `[[`, character(1), "problemName")
    values[["descriptions"]] <- vapply(preset[["problems"]], function(problem) {
      if (is.null(problem[["problemDescription"]])) "" else problem[["problemDescription"]]
    }, character(1))
    values[["severityMaximum"]] <- as.character(preset[["scales"]][["severityMax"]])
    values[["connectionMaximum"]] <- as.character(preset[["scales"]][["connectionMax"]])
    input <- preset[["scales"]][["connectionInput"]]
    if (is.null(input)) input <- options[["networkConnectionInput"]]
    if (is.null(input)) input <- "signed"
    values[["connectionInput"]] <- input
    values[["loadedToken"]] <- token
  }
  # One structured source keeps the definition and freshness token in the same
  # JSON revision. QML reads named/indexed paths and checks readiness before Apply.
  jaspResults[["networkPresetPreview"]] <- createJaspQmlSource(sourceID = "networkPresetPreview", value = values)
  invisible(NULL)
}

.ln1NetPresetStatus <- function(jaspResults, key, title, messages) {
  if (length(messages) == 0L) {
    jaspResults[[key]] <- NULL
  } else {
    text <- paste0("<p>", .ln1EscapeHtml(messages), "</p>", collapse = "")
    jaspResults[[key]] <- createJaspHtml(text, elementType = "div", title = title, position = 6)
  }
  invisible(NULL)
}

.ln1NetLoadPreset <- function(jaspResults, options) {
  token <- .ln1NetPresetLoadToken(options)
  state <- jaspResults[["networkPresetLoadState"]]
  record <- if (is.null(state)) NULL else state$object
  if (!is.list(record) || !identical(record[["token"]], token)) {
    record <- list(token = token, preset = NULL, error = NULL)
    if (nzchar(token)) {
      record[["preset"]] <- tryCatch(.ln1NetReadPreset(options[["networkPresetLoadPath"]]),
        error = function(e) { record[["error"]] <<- conditionMessage(e); NULL },
        warning = function(w) { record[["error"]] <<- conditionMessage(w); NULL })
    }
    jaspResults[["networkPresetLoadState"]] <- createJaspState(object = record)
  }
  .ln1NetPresetSources(jaspResults, record[["preset"]], token, options)
  messages <- character()
  if (!is.null(record[["error"]]))
    messages <- c(gettext("The preset could not be loaded. Correct the file or choose another file, then load it again. The current network inputs were not changed."), record[["error"]])
  else if (!is.null(record[["preset"]]))
    messages <- gettextf("Loaded preset %1$s for preview. Apply it explicitly to replace the current problem definitions.", record[["preset"]][["name"]])
  .ln1NetPresetStatus(jaspResults, "networkPresetLoadStatus", gettext("Preset preview"), messages)
  invisible(NULL)
}

.ln1NetSavePreset <- function(jaspResults, options) {
  request <- .ln1NetPresetRequest(options)
  observedRequest <- isTRUE(options[["networkPresetSaveRequest"]])
  session <- options[["networkExportSession"]]
  hasSession <- .ln1NetPresetString(session, nonempty = TRUE)
  state <- jaspResults[["networkPresetSaveState"]]
  record <- if (is.null(state)) NULL else state$object
  current <- hasSession && is.list(record) && identical(record[["version"]], 1L) &&
    identical(record[["session"]], session)
  if (!current) {
    previous <- record
    record <- list(version = 1L, session = session, observedRequest = observedRequest,
                   request = request, status = "pending")
    # The bound button is reset to FALSE during QML initialization, before R
    # dispatch. In a fresh session TRUE is therefore the first explicit click.
    clicked <- hasSession && observedRequest
    if (is.list(previous) && identical(previous[["version"]], 1L)) {
      if (identical(previous[["request"]], request)) {
        record[["status"]] <- previous[["status"]]
        record[["error"]] <- previous[["error"]]
      }
    }
  } else {
    clicked <- "networkPresetSaveRequest" %in% names(options) &&
      !identical(record[["observedRequest"]], observedRequest)
    record[["observedRequest"]] <- observedRequest
    if (!identical(record[["request"]], request)) {
      record[["request"]] <- request
      record[["status"]] <- "pending"
      record[["error"]] <- NULL
    }
  }
  path <- request[["path"]]
  hasPath <- .ln1NetPresetString(path, nonempty = TRUE)
  if (clicked && hasPath) {
    error <- tryCatch({ .ln1NetWritePreset(request[["preset"]], path); NULL },
                       error = function(e) conditionMessage(e), warning = function(w) conditionMessage(w))
    record[["status"]] <- if (is.null(error)) "success" else "failed"
    record[["error"]] <- error
  }
  record[["lastAttempt"]] <- NULL
  # An event journal must survive data/path changes and path clearing. Giving
  # it automatic option dependencies could replay an already consumed click.
  jaspResults[["networkPresetSaveState"]] <- createJaspState(object = record)
  messages <- character()
  if (hasPath) {
    messages <- if (record[["status"]] == "success") {
      gettextf("Saved reusable preset to %1$s. Patient ratings and assessment connections were not included.", path)
    } else if (record[["status"]] == "failed") {
      c(gettext("The preset could not be saved. Check the definitions and destination, then click Save symptom preset again. An existing file was left unchanged."), record[["error"]])
    } else {
      gettext("The current preset definitions have not been saved. Click Save symptom preset to write them. This replaces an existing file at the selected destination.")
    }
  }
  .ln1NetPresetStatus(jaspResults, "networkPresetSaveStatus", gettext("Save preset"), messages)
  invisible(NULL)
}

.ln1NetPresetFiles <- function(jaspResults, options) {
  .ln1NetLoadPreset(jaspResults, options)
  .ln1NetSavePreset(jaspResults, options)
  invisible(NULL)
}
