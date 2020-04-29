# calculate ratios =====


#' Calculate ratios
#'
#' Calculate ratios from raw data. Note that these are raw ratios and are not normalized against any standards yet. The ratios are calculated straight from the raw data without any unit conversions, i.e. if the raw data is in mV, the ratio is mV/mV, if in nA the ratio is nA/nA, etc. These raw ratios are subsequently NOT identical to absolute ratios, in fact they are usually not even close (especially if raw data recorded as voltages with different resistors). If raw data is first converted to identical current units (\code{\link{iso_convert_signals}}), the ratios may be close to their true values (+instrument fractionation), however, isotope ratios should always be calibrated against reference ratios measured in the same data file.
#'
#' @param ... S3 method placeholder parameters, see class specific functions for details on parameters
#' @return the passed in iso_file(s) with ratios added
#' @export
iso_calculate_ratios <- function(...) {
  UseMethod("iso_calculate_ratios")
}

#' @export
iso_calculate_ratios.default <- function(...) {
  if(length(list(...)) == 0) stop("missing parameters", call. = FALSE)
  stop("this function is not defined for objects of type '",
       class(..1)[1], "'", call. = FALSE)
}

#' @export
iso_calculate_ratios.iso_file <- function(iso_files, ...) {
  return(iso_calculate_ratios(iso_as_file_list(iso_files), ...)[[1]])
}

#' @rdname iso_calculate_ratios
#' @param iso_files collection of iso_file objects
#' @param ratios which ratios to calculate (e.g. c("45/44", "46/44"))
#' @inheritParams iso_show_default_processor_parameters
#' @export
iso_calculate_ratios.iso_file_list <- function(iso_files, ratios, quiet = default(quiet)) {

  # get raw data
  raw_data <- iso_get_raw_data(iso_files, quiet = TRUE) %>%
    iso_calculate_ratios(ratios = ratios, quiet = quiet) %>%
    { split(., .$file_id) }

  # update iso files
  ratio_cols <- generate_ratio_column_names(ratios)$column
  iso_files <- map(
    iso_files,
    ~{
      if (.x$file_info$file_id %in% names(raw_data)) {
        # make sure only to keep original cols and ratio columns
        original_cols <- names(.x$raw_data)
        .x$raw_data <- raw_data[[.x$file_info$file_id]][unique(c(original_cols, ratio_cols))]
      }
      .x
    })

  return(iso_as_file_list(iso_files))
}

#' @rdname iso_calculate_ratios
#' @param df a data frame of raw continuous flow or dual inlet data, must have at minimum the columns 'file_id' and the referenced mass columns required to calculate the \code{ratios}. Assumes that mass columns have either 'v' (for voltages) or 'i' (for currents) as the column name prefix and a ".<unit>" suffix, but otherwise makes no safety checks and will let you calculate any two ratios.
#' @export
iso_calculate_ratios.data.frame <- function(df, ratios, quiet = default(quiet)) {

  # check for data
  if (nrow(df) == 0) stop("no data provided", call. = FALSE)
  if(missing(ratios) || is.null(ratios)) stop("no ratios provided for ratio calculations", call. = FALSE)
  if(!is.logical(quiet)) stop("quiet must be TRUE or FALSE - make sure to pass ratios as a vector, not separate arguments", call. = FALSE)

  # ratios
  ratio_pattern <- "^(\\d+)/(\\d+)$"
  if (!all(ok <- stringr::str_detect(ratios, ratio_pattern))) {
    glue::glue("invalid ratio(s): '{paste(ratios[!ok], collapse = \"', '\")}'. ",
               "Supported format example: c('45/44', '46/44').") %>%
      stop(call. = FALSE)
  }

  # ratio columns
  ratio_columns <- generate_ratio_column_names(ratios)

  # available mass columns
  mass_column_pattern <- "^([vi])C?(\\d+)\\.(.*)$"
  mass_lookup_df <- names(df) %>%
    stringr::str_subset(mass_column_pattern) %>%
    stringr::str_match(mass_column_pattern) %>%
    {colnames(.) <- c("col", "type", "mass", "units"); .} %>%
    as_tibble()

  # calculation columns
  calc_columns <-
    ratio_columns %>%
    left_join(select(mass_lookup_df, type_top = type, col_top = col, units_top = units, top = mass), by = "top") %>%
    left_join(select(mass_lookup_df, type_bot = type, col_bot = col, units_bot = units, bot = mass), by = "bot")
  ok_calc_columns <- filter(calc_columns, type_top == type_bot)

  # check for ratios with missing columns
  prob_ratios <- filter(calc_columns, !column %in% ok_calc_columns$column)$ratio %>% unique()
  if (length(prob_ratios) > 0) {
    glue::glue("missing intensity column(s) for the following ratio(s): '{paste(prob_ratios, collapse = \"', '\")}'. ",
               "The columns for each ratio must exist as current or voltage intensity columns.") %>%
      stop(call. = FALSE)
  }

  # check other columns
  grp_cols <- c("file_id")
  if (!all(ok <- grp_cols %in% names(df))) {
    glue::glue("missing key column(s): '{paste(grp_cols[!ok], collapse = \"', '\")}'.") %>%
      stop(call. = FALSE)
  }

  # information
  if (!quiet) {
    glue::glue(
      "Info: calculating ratio(s) in {length(unique(df$file_id))} data file(s): ",
      "{paste(ratio_columns$column, collapse = ', ')}") %>%
      message()
  }

  # ratios quo function:
  generate_ratio_quo <- function(col_top, col_bot) {
    if (length(col_top) == 1) {
      return(quo(!!sym(col_top[1]) / !!sym(col_bot[1])))
    } else if (length(col_top) == 2) {
      return(quo(case_when(
        !is.na(!!sym(col_top[1])) ~ !!sym(col_top[1]) / !!sym(col_bot[1]),
        TRUE ~ !!sym(col_top[2]) / !!sym(col_bot[2])
      )))
    } else {
      glue::glue("cannot deal with ratios that have {length(col_top)} valid column combinations", call. = FALSE)
    }
  }

  # ratio value quos
  ratio_quos <-
    ok_calc_columns %>%
    dplyr::group_by(column) %>%
    dplyr::summarize(
      quo = list(generate_ratio_quo(col_top, col_bot))
    )
  ratio_quos <- setNames(ratio_quos$quo, ratio_quos$column)

  # calculation
  df <- mutate(df, !!!ratio_quos)

  # finalize
  return(df)
}

# convenience function for ratio column names
generate_ratio_column_names <- function(ratios) {
  ratio_pattern <- "^(\\d+)/(\\d+)$"
  ratios %>%
    stringr::str_match(ratio_pattern) %>%
    { tibble(column = str_c("r",.[,1]), ratio = .[,1], top = .[,2], bot = .[,3]) }
}

# iso_calculate_ratios <- function(iso_files, ratios, quiet = default(quiet)) {
#
#   # safety checks
#   if(!iso_is_object(iso_files)) stop("can only calculate ratios for iso files", call. = FALSE)
#   if(missing(ratios) || is.null(ratios)) stop("no ratios provided for ratio calculations", call. = FALSE)
#   if(!is.logical(quiet)) stop("quiet must be TRUE or FALSE - make sure to pass ratios as a vector, not separate arguments", call. = FALSE)
#   single_file <- iso_is_file(iso_files) # to make sure return is the same as supplied
#   iso_files <- iso_as_file_list(iso_files)
#
#   # ratios
#   ratio_pattern <- "^(\\d+)/(\\d+)$"
#   if (!all(ok <- str_detect(ratios, ratio_pattern))) {
#     stop("invalid ratio(s): ", str_c(ratios[!ok], collapse = ", "), call. = FALSE)
#   }
#   ratio_columns <- ratios %>%
#     stringr::str_match(ratio_pattern) %>%
#     { tibble(column = str_c("r",.[,1]), ratio = .[,1], top = .[,2], bot = .[,3]) }
#
#   # information
#   if (!quiet) {
#     stringr::str_interp("Info: calculating ratio(s) in $[d]{n} data file(s): ${ratios}",
#                list(n = length(iso_files), ratios = str_c(ratios, collapse =", "))) %>% message()
#   }
#
#   # make sure data is provided
#   isoreader:::check_read_options(iso_files, "raw_data")
#
#   # calculate ratios for all iso_files
#   mass_column_pattern <- "^[vi](\\d+)\\.(.*)$"
#   calculate_iso_file_ratios <- function(iso_file) {
#
#     if (iso_file$read_options$raw_data && nrow(iso_file$raw_data) > 0) {
#       # generate mass lookup
#       mass_lookup <- names(iso_file$raw_data) %>%
#         stringr::str_subset(mass_column_pattern) %>%
#         stringr::str_match(mass_column_pattern) %>%
#         { setNames(.[,1], .[,2]) }
#
#       # generate ratios
#       for (i in 1:nrow(ratio_columns)) {
#         if (!is.na(top <- mass_lookup[ratio_columns$top[i]]) && !is.na(bot <- mass_lookup[ratio_columns$bot[i]]))
#           iso_file$raw_data[[ratio_columns$column[i]]] <- iso_file$raw_data[[top]] / iso_file$raw_data[[bot]]
#       }
#     }
#
#     return(iso_file)
#   }
#
#   # apply calculations
#   iso_files <- iso_files %>% lapply(calculate_iso_file_ratios) %>% iso_as_file_list()
#
#   # return single (if passed in as single)
#   if (single_file) return (iso_files[[1]])
#   return(iso_files)
# }


# calculate deltas =====

#' Calculate deltas
#'
#' Calculate delta values from recorded data in dual inlet files (continuous flow not yet supported). Note that these are recorded delta values of sample vs. standard in each dual inlet file and NOT normalized against any external standard or reported values of the standard. They usually need at least a frame shift correction afterwards. Also note that the delta values are only multipled by 1000 if \code{in_permil=TRUE} (the default), otherwise returns raw values without unit multiplication.
#'
#' If \code{bracket=TRUE} (the default), the delta values are calculated from the raw data ratios of the sample and the two bracketing standards in the same cycle and cycle before it (this implicitly assumes that the pre-standard has cycle number 0) If \code{bracket=FALSE}, the delta values are calculated straight from the corresponding raw data ratios of the sample and the standard in the same cycle. Delta values are only stored in the \code{sample} rows of the raw data, not in the \code{standard} rows.
#'
#' @param ... S3 method placeholder parameters, see class specific functions for details on parameters
#' @return the passed in data with deltas added
#' @export
iso_calculate_deltas <- function(...) {
  UseMethod("iso_calculate_deltas")
}

#' @export
iso_calculate_deltas.default <- function(...) {
  if(length(list(...)) == 0) stop("missing parameters", call. = FALSE)
  stop("this function is not defined for objects of type '",
       class(..1)[1], "'", call. = FALSE)
}

#' @export
iso_calculate_deltas.iso_file <- function(iso_files, ...) {
  iso_calculate_deltas(iso_as_file_list(iso_files), ...)[[1]]
}

# Note: could make this for dual_inlet_list instead but want a more helpful error message
#' @rdname iso_calculate_deltas
#' @param iso_files collection of dual inlet iso_file objects
#' @param deltas which deltas to calculate from the raw data, must be in the complete format with prefix 'd' and both numerator and denominator mass, e.g. \code{c("d45/44", "d46/44")} to calculate both delta 45/44 and delta 46/44. Deltas can only be calculated if the corresponding ratio columns already exist.
#' @inheritParams iso_show_default_processor_parameters
#' @export
iso_calculate_deltas.iso_file_list <- function(iso_files, deltas, bracket = TRUE, in_permil = TRUE, quiet = default(quiet)) {

  # calculating deltas from raw data only makes sense in dual inlet files
  if(!iso_is_dual_inlet(iso_files)) {
    if (iso_is_continuous_flow(iso_files))
      stop("can only calculate raw deltas for dual inlet files, did you mean iso_calculate_peak_deltas?", call. = FALSE)
    else
      stop("can only calculate raw deltas for dual inlet files", call. = FALSE)
  }

  # get raw data
  raw_data <- iso_get_raw_data(iso_files, quiet = TRUE) %>%
    iso_calculate_deltas(deltas = deltas, bracket = bracket,
                         in_permil = in_permil, quiet = quiet) %>%
    { split(., .$file_id) }

  # update iso files
  delta_cols <- if (in_permil) paste0(deltas, ".permil") else deltas
  iso_files <- map(
    iso_files,
    ~{
      if (.x$file_info$file_id %in% names(raw_data)) {
        # make sure only to keep original cols and new delta columns
        original_cols <- names(.x$raw_data)
        .x$raw_data <- raw_data[[.x$file_info$file_id]][unique(c(original_cols, delta_cols))]
      }
      .x
    })

  return(iso_as_file_list(iso_files))
}

#' @rdname iso_calculate_deltas
#' @param df a data frame of raw dual inlet data, must have at minimum the columns 'file_id', 'type' (with values "standard" and "sample") and 'cycle' (integer column), as well as all of the required ratio columns with the correct \code{ratio_prefix}.
#' @param ratio_prefix the prefix of the ratio columns. Default is \code{"r"} which is what \code{\link{iso_calculate_ratios}} generates.
#' @export
iso_calculate_deltas.data.frame <- function(df, deltas, bracket = TRUE, in_permil = TRUE, ratio_prefix = "r", quiet = default(quiet)) {

  # check for data
  if (nrow(df) == 0) stop("no data provided", call. = FALSE)
  if(missing(deltas) || is.null(deltas)) stop("no deltas provided for delta calculations", call. = FALSE)
  if(!is.logical(bracket)) stop("bracket must be TRUE or FALSE - make sure to pass deltas as a vector, not separate arguments", call. = FALSE)

  # deltas
  delta_pattern <- "^d(\\d+/\\d+)$"
  if (!all(ok <- stringr::str_detect(deltas, delta_pattern))) {
    glue::glue("invalid delta(s): '{paste(deltas[!ok], collapse = \"', '\")}'. ",
               "Supported format example: c('d45/44', 'd46/44').") %>%
      stop(call. = FALSE)
  }

  # required ratios
  ratios <- str_c(ratio_prefix, stringr::str_match(deltas, delta_pattern)[,2])
  deltas <- if (in_permil) paste0(deltas, ".permil") else deltas

  # check for ratios
  if (!all(ok <- ratios %in% names(df))) {
    glue::glue("missing ratio column(s): '{paste(ratios[!ok], collapse = \"', '\")}'. ",
               "Do you need to 'iso_calculate_ratios()'?") %>%
      stop(call. = FALSE)
  }

  # check other columns
  grp_cols <- c("file_id", "type", "cycle")
  if (!all(ok <- grp_cols %in% names(df))) {
    glue::glue("missing key column(s): '{paste(grp_cols[!ok], collapse = \"', '\")}'.") %>%
      stop(call. = FALSE)
  }
  stopifnot(is.numeric(df$cycle))
  if ("block" %in% names(df)) {
    # has block
    grp_cols <- c(grp_cols, "block")
    stopifnot(is.numeric(df$block))
  }
  expected_type <- c("standard", "sample")
  found_type <- unique(as.character(df$type))
  unexpected <- setdiff(found_type, expected_type)
  missing <- setdiff(expected_type, found_type)
  if (length(unexpected) > 0 || length(missing) > 0) {
    glue::glue(
      "type column must have 'standard' and 'sample' values only.",
      if (length(missing) > 0) " Missing: {paste(missing, collapse = ', ')}." else "",
      if (length(unexpected) > 0) " Unexpected: {paste(unexpected, collapse = ', ')}." else "") %>%
      stop(call. = FALSE)
  }

  # information
  if (!quiet) {
    glue::glue(
      "Info: calculating delta(s) in {length(unique(df$file_id))} data file(s): ",
      "{paste(deltas, collapse = ', ')}") %>%
      message()
  }

  # get standards
  post_ratios <- setNames(ratios, paste("..post", ratios))
  pre_ratios <- setNames(ratios, paste("..pre", ratios))
  post_standard <- df %>%
    select(!!!grp_cols, !!!post_ratios) %>%
    filter(type == "standard") %>%
    mutate(type = "sample")
  df <- left_join(df, post_standard, by = grp_cols)

  # bracketing
  if (bracket) {
    pre_standard <- df %>%
      select(!!!grp_cols, !!!pre_ratios) %>%
      filter(type == "standard") %>%
      mutate(cycle = cycle + 1, type = "sample")
    df <- left_join(df, pre_standard, by = grp_cols)
  }

  # delta value quos
  generate_delta_quo <- function(ratio, post, pre = NULL) {
    if (bracket)
      # bracke
      dquo <- quo(2 * !!sym(ratio) / (!!sym(pre) + !!sym(post)) - 1)
    else
      # no bracket
      dquo <- quo(!!sym(ratio) / !!sym(post) - 1)
    if (in_permil) dquo <- quo((!!dquo) * 1e3)
    return(dquo)
  }
  dquos <- list(ratios, names(post_ratios), names(pre_ratios)) %>%
    pmap(generate_delta_quo) %>%
    setNames(deltas)

  # calculation
  df <- mutate(df, !!!dquos)

  # remove join columns
  return(select(df, -starts_with("..pre"), -starts_with("..post")))
}

