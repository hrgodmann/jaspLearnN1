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
