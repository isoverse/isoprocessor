#' @keywords internal
"_PACKAGE"

#' @importFrom rlang enquo enexpr quo quos !! !!! sym ensyms get_expr as_label quo_is_null quo_is_missing quo_is_symbol quo_is_call is_quosure is_list is_empty is_integerish eval_tidy sym
#' @importFrom tidyselect vars_select starts_with ends_with everything
#' @importFrom glue glue
#' @importFrom dplyr mutate select rename filter as_data_frame inner_join left_join right_join anti_join data_frame %>% bind_rows group_by ungroup arrange summarize do case_when row_number
#' @importFrom tidyr nest unnest gather
#' @importFrom purrr map map_lgl map_int map_dbl map_chr map2 map2_lgl map2_chr safely pmap
#' @importFrom ggplot2 ggplot aes aes_string aes_q geom_bar geom_smooth geom_point geom_line geom_hline geom_rect geom_vline theme_bw theme labs element_text %+% scale_x_continuous scale_x_datetime scale_linetype_manual scale_y_continuous expand_limits facet_grid facet_wrap is.ggplot aes_ coord_cartesian
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_c str_replace fixed str_detect
#' @importFrom forcats as_factor
#' @importFrom broom tidy glance
#' @importFrom investr invest
#' @importFrom stats setNames
#' @importFrom methods is
#' @importFrom utils packageVersion
#' @importFrom readxl read_excel
#' @import isoreader
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
