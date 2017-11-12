#' @keywords internal
"_PACKAGE"

#' @import rlang
#' @importFrom tidyselect vars_select starts_with ends_with
#' @importFrom glue glue collapse
#' @importFrom dplyr mutate select filter as_data_frame
#' @importFrom tidyr nest
#' @importFrom purrr map map_lgl map_int map_chr map2 map2_lgl safely
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
