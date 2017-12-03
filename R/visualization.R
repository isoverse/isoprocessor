
#' Visualize reference peaks
#'
#' @note: refine and write tests
#' @param group_id first column in group id is used for x axis
#' @param within_group whether to visualize the deviation within the specified group or across all groups
#' @export
iso_visualize_ref_peaks <- function(dt, is_ref_condition, ratio = default(ratio), group_id = default(group_id), within_group = FALSE) {

  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  if (missing(is_ref_condition)) stop("no condition as to what constitutes a reference peak provided", call. = FALSE)
  dt_cols <- get_column_names(!!enquo(dt), ratio = enquo(ratio), group_id = enquo(group_id), n_reqs = list(group_id = "+"))
  ref_quo <- enquo(is_ref_condition)

  refs <- dt %>%
    filter(!!ref_quo) %>%
    rename(ratio = !!as.name(dt_cols$ratio)) %>%
    mutate(delta_deviation = (ratio/mean(ratio) - 1) * 1000) %>%
    # calculate deviation from interrun ratio
    group_by(!!!cols_to_quos(dt_cols$group_id)) %>%
    mutate(
      run_delta_deviation = (ratio/mean(ratio) - 1) * 1000,
      ref_peak_nr = 1:n()) %>% ungroup() %>%
    mutate(ref_peak_nr = factor(ref_peak_nr))

  # plot reference peaks relative to total average
  p <- refs %>%
    ggplot() +
    aes_string(dt_cols$group_id[1], "delta_deviation", fill = "ref_peak_nr") +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0) +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Analysis", fill = "Reference peak", y = "Deviation from total average [permil]")

  if (!within_group) {
    # across all samples
    return(p)
  } else {
    # inter-group average
    p %+%
      aes(y = run_delta_deviation) +
      labs(y = "Deviation within group [permil]")
  }
}
