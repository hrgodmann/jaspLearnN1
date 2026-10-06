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

# The .data pronoun is evaluated by ggplot2/dplyr, not as a package variable.
utils::globalVariables(".data", package = environment())

.ln1Intro <- function(jaspResults, options, textFun) {
  if (options[["enableIntroText"]] && is.null(jaspResults[["introText"]])) {
    introText <- createJaspHtml(
      textFun(),
      title = gettext("Introduction"),
      position = 1
    )
    introText$dependOn("enableIntroText")

    jaspResults[["introText"]] <- introText
  }
}

# Serialize in the destination directory before replacing an existing export.
# Callers supply the file format; save intent and status stay in each analysis.
.ln1WriteExport <- function(path, writer) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    stop(gettext("Choose a file name for the export."), call. = FALSE)
  if (dir.exists(path))
    stop(gettext("The destination is a folder. Choose a file name."), call. = FALSE)
  if (!dir.exists(dirname(path)))
    stop(gettext("The destination folder does not exist."), call. = FALSE)
  if (file.exists(path) && file.access(path, mode = 2L) != 0L)
    stop(gettext("The destination file is not writable."), call. = FALSE)

  temporary <- tempfile(pattern = ".jasp-learnn1-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  writer(temporary)
  if (!file.rename(temporary, path))
    stop(gettext("The export could not replace the destination file."), call. = FALSE)
  invisible(NULL)
}

.ln1WriteCsv <- function(data, path, na = "NA") {
  .ln1WriteExport(path, function(temporary)
    utils::write.csv(data, file = temporary, row.names = FALSE, na = na))
}

.ln1EscapeHtml <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  gsub("'", "&#39;", text, fixed = TRUE)
}
