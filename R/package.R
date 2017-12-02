#' @keywords internal
"_PACKAGE"

#' @importFrom rlang enquo quo quos UQ UQE !! !!! quo_expr quo_text quo_is_null quo_is_symbol quo_is_lang is_quosure is_list is_empty is_integerish eval_tidy sym lang_head lang_args
#' @importFrom tidyselect vars_select starts_with ends_with everything
#' @importFrom glue glue collapse
#' @importFrom dplyr mutate select filter as_data_frame left_join data_frame %>% bind_rows
#' @importFrom tidyr nest unnest
#' @importFrom purrr map map_lgl map_int map_chr map2 map2_lgl map2_chr safely
#' @importFrom stringr str_c
#' @importFrom broom tidy glance
#' @importFrom modelr geom_ref_line add_residuals
#' @importFrom investr invest
#' @importFrom isoreader iso_turn_info_messages_on iso_turn_info_messages_off
#' @importFrom stats setNames
#' @importFrom methods is
#' @importFrom utils packageVersion
#' @importFrom readxl read_excel
NULL

# quiets concerns of R CMD check about . that appears in pipelines
# and some very commonly used variable names used in NSE commands
utils::globalVariables(c("."))

# release questions
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}
