# Validate identifiers and stored rating domains before constructing keyed results.
# Display scales change presentation; stored ratings stay on their original scale.

.ln1NetFiniteScalar <- function(value) {
  is.numeric(value) && !is.complex(value) && length(value) == 1L && is.finite(value)
}

.ln1NetScaleMaximum <- function(options, optionName) {
  maximum <- options[[optionName]]
  if (is.null(maximum))
    return(1)
  if (!.ln1NetFiniteScalar(maximum) || maximum < 1 || maximum > 1000 || maximum != floor(maximum)) {
    if (identical(optionName, "networkSeverityMaximum"))
      .quitAnalysis(gettext("The severity scale maximum must be a whole number from 1 to 1000."))
    else
      .quitAnalysis(gettext("The connection scale maximum must be a whole number from 1 to 1000."))
  }
  return(as.numeric(maximum))
}

.ln1NetSeverityRated <- function(problem) {
  if (!"problemSeverityRated" %in% names(problem))
    return(TRUE)
  rated <- problem[["problemSeverityRated"]]
  if (!is.logical(rated) || length(rated) != 1L || is.na(rated))
    .quitAnalysis(gettext("Choose whether severity has been rated for each problem."))
  return(rated)
}

.ln1NetSeverityValue <- function(problem) {
  rated <- .ln1NetSeverityRated(problem)
  severity <- problem[["problemSeverity"]]
  name <- problem[["problemName"]]
  if (!is.null(severity) && (!.ln1NetFiniteScalar(severity) || severity < 0 || severity > 1))
    .quitAnalysis(gettextf("Severity for problem '%1$s' must be a finite value within the selected severity scale.", name))
  if (!rated)
    return(NA_real_)
  if (is.null(severity))
    .quitAnalysis(gettextf("Enter a severity for problem '%1$s', or mark its severity as not rated.", name))
  return(as.numeric(severity))
}

.ln1NetValidateNames <- function(rows, field, problems = FALSE) {
  labels <- vapply(seq_along(rows), function(i) {
    name <- if (is.list(rows[[i]])) rows[[i]][[field]] else NULL
    valid <- is.character(name) && length(name) == 1L && !is.na(name)
    if (valid)
      name <- trimws(name, whitespace = "[\\h\\v]")
    if (!valid || !nzchar(name)) {
      if (problems)
        .quitAnalysis(gettextf("Enter a nonblank name for problem %1$i.", i))
      else
        .quitAnalysis(gettextf("Enter a nonblank name for assessment %1$i.", i))
    }
    return(name)
  }, character(1))
  repeated <- which(duplicated(labels))
  if (length(repeated)) {
    name <- labels[repeated[1L]]
    if (problems)
      .quitAnalysis(gettextf("Problem names must be unique. Rename the repeated problem '%1$s'; spaces at the beginning or end do not distinguish names.", name))
    else
      .quitAnalysis(gettextf("Assessment names must be unique. Rename the repeated assessment '%1$s'; spaces at the beginning or end do not distinguish names.", name))
  }
  return(invisible(NULL))
}

.ln1NetValidateConnectionStrength <- function(strength, assessmentName) {
  # Missing or nonfinite ratings make an assessment unfinished through the
  # readiness helper, so other completed assessments can still be displayed.
  if (.ln1NetFiniteScalar(strength) && (strength < -1 || strength > 1))
    .quitAnalysis(gettextf("A connection in assessment '%1$s' is outside the selected connection scale. Enter a value within that scale.", assessmentName))
  return(invisible(NULL))
}

.ln1NetValidateOptions <- function(options) {
  .ln1NetScaleMaximum(options, "networkSeverityMaximum")
  .ln1NetScaleMaximum(options, "networkConnectionMaximum")
  input <- options[["networkConnectionInput"]]
  if (!is.null(input) && !(is.character(input) && length(input) == 1L &&
                           !is.na(input) && input %in% c("signed", "magnitude")))
    .quitAnalysis(gettext("Choose signed or magnitude entry for connection ratings."))

  problems <- options[["problems"]]
  assessments <- options[["connectionList"]]
  if (length(problems) < 2L || length(problems) > 10L)
    .quitAnalysis(gettext("Enter between 2 and 10 problems for the network."))
  .ln1NetValidateNames(problems, "problemName", problems = TRUE)
  .ln1NetValidateNames(assessments, "name")
  for (problem in problems)
    .ln1NetSeverityValue(problem)
  nodeNames <- vapply(problems, `[[`, character(1), "problemName")

  for (assessment in assessments) {
    name <- assessment[["name"]]
    if (isTRUE(assessment[["allConnections"]])) {
      rows <- assessment[["allConnectionStrengths"]]
      for (i in seq_along(rows)) {
        targets <- rows[[i]][["targets"]]
        for (j in setdiff(seq_along(targets), i))
          .ln1NetValidateConnectionStrength(targets[[j]][["connectionStrength"]], name)
      }
    } else {
      pairs <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
      for (connection in assessment[["connections"]]) {
        .ln1NetValidateConnectionStrength(connection[["connectionStrength"]], name)
        from <- connection[["connectionFrom"]]
        to <- connection[["connectionTo"]]
        validPair <- is.character(from) && length(from) == 1L && !is.na(from) &&
          is.character(to) && length(to) == 1L && !is.na(to) && from != to &&
          from %in% nodeNames && to %in% nodeNames
        if (validPair)
          pairs <- rbind(pairs, data.frame(from = from, to = to, stringsAsFactors = FALSE))
      }
      repeated <- which(duplicated(pairs))
      if (length(repeated)) {
        pair <- pairs[repeated[1L], ]
        .quitAnalysis(gettextf("Assessment '%1$s' contains more than one connection from '%2$s' to '%3$s'. Keep one rating for this direction; the opposite direction can be rated separately.",
                               name, pair[["from"]], pair[["to"]]))
      }
    }
  }
  return(invisible(NULL))
}
