#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @import tidyr
#' @importFrom purrr map
#' @import stringr
#' @import ggplot2
#' @import isoreader
#' @importFrom stats setNames
#' @importFrom methods is
#' @importFrom utils packageVersion
#' @importFrom readxl read_excel
NULL

# quiets concerns of R CMD check about . that appears in pipelines
# and some very commonly used variable names used in NSE commands
utils::globalVariables(c(".", "file_id", "mass"))

# release questions
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}
