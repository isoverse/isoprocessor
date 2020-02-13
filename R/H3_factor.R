# H3 factor functions =====

#' Summarize H3 factors
#' @param dt data table
#' @param is_H3_factor_file_condition condition to find H3 factor files
#' @export
summarize_H3_factors <- function(dt, is_H3_factor_file_condition, is_H3_factor_file = default(is_H3_factor_file),
                                 file_id = default(file_id), file_datetime = default(file_datetime),
                                 H3_factor = default(H3_factor), ampl = default(ampl), keep = default(NULL)) {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)

  dt_cols <- get_column_names(dt, file_id = enquo(file_id), file_datetime = enquo(file_datetime),
                              H3_factor = enquo(H3_factor), ampl = enquo(ampl), keep = enquo(keep),
                              n_reqs = list(keep = "*"))
  dt_new_cols <- get_new_column_names(is_H3_factor_file = enquo(is_H3_factor_file))

  if (missing(is_H3_factor_file_condition))
    quo_is_H3_factor_file_condition <- quo(TRUE)
  else
    quo_is_H3_factor_file_condition <- enquo(is_H3_factor_file_condition)

  dt %>%
    # identify H3 factor files and make sure H3 factor is numeric
    mutate(
      !!dt_new_cols$is_H3_factor_file := !!quo_is_H3_factor_file_condition,
      !!dt_cols$H3_factor := as.numeric(.data[[dt_cols$H3_factor]])
    ) %>%
    # nest the data
    nest_data(group_by = c(dt_new_cols$is_H3_factor_file, dt_cols[c("file_id", "file_datetime", "H3_factor", "keep")]) %>% unlist(use.names=FALSE)) %>%
    # H3 factor groups
    arrange(!!sym(dt_cols$file_datetime)) %>%
    mutate(H3_factor_group = cumsum(c(0, diff(!!sym(dt_new_cols$is_H3_factor_file))) > 0)) %>%
    # n# analyses per H3 factor
    group_by(H3_factor_group) %>%
    mutate(n_analyses = dplyr::n()) %>%
    ungroup() %>%
    arrange(!!sym(dt_cols$file_datetime)) %>%
    # calculate max and min amplitudes
    mutate(
      low_amp = map_dbl(nested_data, ~min(.x[[dt_cols$ampl]])),
      high_amp = map_dbl(nested_data, ~max(.x[[dt_cols$ampl]]))
    )
}

#' Print H3 factor summary
#' @export
print_H3_factors <- function(dt, file_id = default(file_id), file_datetime = default(file_datetime),
                             H3_factor = default(H3_factor), is_H3_factor_file = default(is_H3_factor_file), ...) {
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(dt, file_id = enquo(file_id), file_datetime = enquo(file_datetime), H3_factor = enquo(H3_factor))
  iso_print_data_table(dt, select = c(!!file_id, !!file_datetime, !!H3_factor, n_analyses, low_amp, high_amp), filter = !!is_H3_factor_file, ...)
}

#' Plot H3 factor
#'
#' @param dt the data table
#' @export
plot_H3_factors <- function(dt, file_datetime = default(file_datetime), H3_factor = default(H3_factor), is_H3_factor_file = default(is_H3_factor_file), color = low_amp, label = default(file_id),
                           date_breaks = "12 hours", date_labels = "%b %d - %H:%m") {

  # safety checks
  if (missing(dt)) stop("no data table supplied", call. = FALSE)
  dt_cols <- get_column_names(dt, file_datetime = enquo(file_datetime),
                              H3_factor = enquo(H3_factor), is_H3_factor_file = enquo(is_H3_factor_file),
                              color = enquo(color), label = enquo(label))

  # plot
  dt %>%
    ggplot() +
    aes_q(x = sym(dt_cols$file_datetime), y = sym(dt_cols$H3_factor), color = sym(dt_cols$color), text = sym(dt_cols$label)) +
    # linear fit to the data with SE error range
    geom_smooth(data = function(df) filter(df, is_H3_factor_file), map = aes(color = NULL, text = NULL), method="lm", fullrange = TRUE) +
    #add data point with the Analysis number as text (vjust to offset from data point)
    geom_point(map = aes(color = NULL)) +
    geom_point(data = function(df) filter(df, !!sym(dt_cols$is_H3_factor_file)), size = 3) +
    geom_text (data = function(df) filter(df, !!sym(dt_cols$is_H3_factor_file)), aes_string(label = dt_cols$label), vjust = 1.5 ) +
    # add scales (datetime scale with appropriate breaks and formatted as Month Day - Hour : Minute)
    scale_x_datetime (date_breaks = date_breaks, date_labels = date_labels) +
    scale_y_continuous(breaks = seq(0,10, by = 0.01)) +
    scale_colour_continuous(low="red", high="olivedrab3", na.value="black") +
    # add theme
    theme_bw() + theme(legend.position = "right") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    # add labels
    labs(color = "lowest amplitude [mV]", x = "", y = "H3 factor [ppm/nA]")
}

