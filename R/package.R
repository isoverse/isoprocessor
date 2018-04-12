#' @keywords internal
"_PACKAGE"

#' @importFrom rlang enquo quo quos !! !!! ensyms get_expr quo_expr quo_text quo_is_null quo_is_missing quo_is_symbol quo_is_lang is_quosure is_list is_empty is_integerish eval_tidy sym lang_head lang_args sym
#' @importFrom tidyselect vars_select starts_with ends_with everything
#' @importFrom glue glue collapse
#' @importFrom dplyr mutate select rename filter as_data_frame inner_join left_join right_join anti_join data_frame %>% bind_rows group_by ungroup arrange summarize do case_when row_number
#' @importFrom tidyr nest unnest gather
#' @importFrom purrr map map_lgl map_int map_dbl map_chr map2 map2_lgl map2_chr safely pmap
#' @importFrom ggplot2 ggplot aes aes_string geom_bar geom_point geom_hline theme_bw theme labs element_text %+% facet_wrap is.ggplot
#' @importFrom stringr str_c str_replace fixed str_detect
#' @importFrom forcats as_factor
#' @importFrom broom tidy glance
#' @importFrom modelr geom_ref_line add_residuals
#' @importFrom investr invest
#' @importFrom isoreader iso_turn_info_messages_on iso_turn_info_messages_off extract_substring extract_word parse_number parse_double parse_integer parse_logical parse_datetime
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
