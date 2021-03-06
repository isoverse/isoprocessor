# unit conversion functions ----

#' Convert time units in continuous flow files
#'
#' This function can be used to convert the time units of a collection of iso_files to a new common time unit. The original time units are inferred from the naming of the time column. New time units are very flexible and can be anything that \code{\link[lubridate]{duration}} understands, i.e. "s", "seconds", "min", "minutes", "hours", "days", etc. are all valid units.
#'
#' @inheritParams iso_plot_raw_data
#' @param to what time units to convert to
#' @export
#' @return the passed in iso_file(s) with changed time units
iso_convert_time <- function(iso_files, to, quiet = default(quiet)) {

  # checks
  if(!iso_is_continuous_flow(iso_files)) stop("can only convert time in continuous flow iso_files", call. = FALSE)
  if(missing(to)) stop("no time unit to convert to specified", call. = FALSE)
  single_file <- iso_is_file(iso_files) # to make sure return is the same as supplied
  iso_files <- iso_as_file_list(iso_files)

  if (!quiet)
    sprintf("Info: converting time to '%s' for %d continuous flow data file(s)", to, length(iso_files)) %>% message()

  # make sure data is provided
  isoreader:::check_read_options(iso_files, "raw_data")

  # find time from units
  time_pattern <- "^time\\.(.*)$"
  time_from <- sapply(iso_files, function(iso_file) {
    time_column <- stringr::str_subset(names(iso_file$raw_data), time_pattern)
    if(length(time_column) == 0) return(NA_character_) # no time column (potentially data not read)
    if (length(time_column) != 1)
      stop("unclear which column is the time column in '", iso_file$file_info$file_id,
          "', found: ", str_c(time_column, collapse = ", "), call. = FALSE)
    stringr::str_match(time_column, time_pattern) %>% {.[2]}
  })

  # for each file convert the time units
  for (i in 1:length(iso_files)) {
    if (!is.na(time_from[i])) {
      old_time <- str_c("time.", time_from[i])
      new_time <- str_c("time.", to)
      iso_files[[i]]$raw_data[[old_time]] <- scale_time(iso_files[[i]]$raw_data[[old_time]], to = to, from = time_from[i])
      iso_files[[i]]$raw_data <- iso_files[[i]]$raw_data %>% rename(!!!setNames(old_time, new_time))
    }
  }

  # return single (if passed in as single)
  if (single_file) return (iso_files[[1]])
  return(iso_files)
}



#' Convert signal units in continuous flow and dual inlet files
#'
#' This function can be used to convert the intensity units of a collection of iso_files to a scaled unit (e.g. from mV to V, or nA to mA) or different unit (e.g. from mV to nA or vice-versa).
#' The original signal intensity units are inferred from the naming of the intensity columns.
#' The new units must be a voltage (V) or current (A) but can have any valid SI prefix.
#' Conversion from voltage to current and vice versa requires information about the resistors used in the op amp.
#' This information is automatically retrieved during file read for file formats that contain resistors values (e.g. dxf and did) and are read with the \code{read_method_info=TRUE} parameter (see \code{\link{iso_get_resistors_info}} for details on how to access resistor values in read files). If resistor values are not set in a file, it will not allow automatic conversion between voltage and current. Instead, the \code{R} and \code{R_units} parameters can be used to provide specific resistor values. However, if \code{R} is set, these values will be used for all passed in \code{iso_files}.
#'
#' @inheritParams iso_plot_raw_data
#' @param to what signal unit to convert to
#' @param R resistor value(s). If not specified, will use resitor values from individual iso_files. If specified, must be a named vector with Rm (m=mass) as names (e.g. \code{c(R45=0.3, R46=3)}), in units of \code{R_units}. If specified, will be used for ALL provided iso_files.
#' @param R_units determined what units resistor values (\code{R}) are in, if they are specified. Example \code{R_units = "GOhm"} designates that the resistor values provided in \code{R} parameter are in Giga-Ohm, i.e. 10^9 Ohm.
#' @export
#' @return the passed in iso_file(s) with changed signal units
iso_convert_signals <- function(iso_files, to, R, R_units = NA, quiet = default(quiet)) {

  # checks
  if(!iso_is_continuous_flow(iso_files) && !iso_is_dual_inlet(iso_files) &&!iso_is_scan(iso_files))
    stop("can only convert signals in continuous flow and dual inlet iso_files", call. = FALSE)
  if(missing(to))
    stop("no unit to convert to specified", call. = FALSE)
  if(!missing(R) && is.na(R_units))
    stop("resistor values (R) are specified but their units (R_units) are not", call. = FALSE)
  if(missing(R) && !is.na(R_units))
    stop("resistor units (R_units) are specified but resistor values (R) are not", call. = FALSE)
  if(!missing(R) && ( !is.vector(R, "numeric") || is.null(names(R)) || any(names(R) == "")))
    stop("specified resistance values have to be a named numeric vector - e.g., c(R45=0.3)", call. = FALSE)
  single_file <- iso_is_file(iso_files) # to make sure return is the same as supplied
  iso_files <- iso_as_file_list(iso_files)
  auto_R <- missing(R)

  if (!quiet) {
    sprintf("Info: converting signals to '%s' for %d data file(s) with %s",
            to, length(iso_files),
            if (auto_R) "automatic resistor values from individual iso_files (if needed for conversion)"
            else str_c("specific resistor value(s): ", str_c(str_c(names(R),"=",R, R_units), collapse = ", "))
            ) %>% message()
  }

  # make sure data is available
  isoreader:::check_read_options(iso_files, "raw_data")

  # apply signal conversion
  func <- "iso_convert_signals"
  signal_pattern <- sprintf("^[iv]C?(\\d+)\\.(\\w+)$")
  to_units <- get_unit_scaling(to, c("V", "A"))
  R_name <- R.Ohm <- NULL # global vars
  iso_files <- iso_files %>% lapply(function(iso_file) {

    # don't try to convert if raw data not present or otherwise empty
    if (!iso_file$read_options$raw_data || nrow(iso_file$raw_data) == 0) return(iso_file)

    # check whether background data exists that needs to be converted too
    convert_bgrd <- !is.null(iso_file$bgrd_data) && nrow(iso_file$bgrd_data) > 0

    # column names
    data_col_names <- stringr::str_subset(names(iso_file$raw_data), signal_pattern)
    if (convert_bgrd) {
      bgrd_col_names <- stringr::str_subset(names(iso_file$bgrd_data), signal_pattern)
      col_names <- c(data_col_names, bgrd_col_names)
    } else {
      col_names <- data_col_names
    }

    # safety check on data columns
    if (length(data_col_names) == 0) {
      return(isoreader:::register_warning(
        iso_file, func = func, details = "could not find any voltage or current data columns", warn = FALSE))
    }

    # see if resistors are required (i.e. any columns are v/i and 'to' is not the same)
    base_units <- col_names %>% stringr::str_match(signal_pattern) %>% {.[,3]} %>%
      lapply(get_unit_scaling, base_units = c("V", "A")) %>% sapply(`[[`, "base_unit")
    scaling_only <- all(base_units == to_units$base_unit)

    # resistors
    if (scaling_only) {
      # scaling only
      iso_file$raw_data <- scale_signals(iso_file$raw_data, data_col_names, to = to, quiet = TRUE)
      if (convert_bgrd)
        iso_file$bgrd_data <- scale_signals(iso_file$bgrd_data, bgrd_col_names, to = to, quiet = TRUE)
      return(iso_file)
    }

    if (!scaling_only && auto_R) {
      # find resistors from files
      if (!iso_file$read_options$method_info) {
        return(isoreader:::register_warning(
          iso_file, func = func,
          details = "cannot automatically determine resistor values, method info not available (read_method_info = FALSE)"))
      } else if (is.null(iso_file$method_info$resistors) || nrow(iso_file$method_info$resistors) == 0) {
        return(isoreader:::register_warning(
          iso_file, func = func,
          details = "cannot automatically determine resistor values, no resistor data available", warn = FALSE))
      } else if (!"mass" %in% names(iso_file$method_info$resistors)) {
        return(isoreader:::register_warning(
          iso_file, func = func,
          details = "cannot automatically determine resistor values, resistor data not linked to masses", warn = FALSE))
      } else {
        R <- iso_file$method_info$resistors %>%
          mutate(R_name = ifelse(!is.na(mass), str_c("R", mass), str_c("R", cup))) %>%
          select(R_name, R.Ohm) %>% tibble::deframe()
        R_units <- "Ohm"
      }
    }

    # convert signals
    iso_file$raw_data <-
      scale_signals(
        iso_file$raw_data,
        data_col_names,
        to = to,
        R = R,
        R_units = R_units,
        quiet = TRUE
      )

    # convert background
    if (convert_bgrd) {
      iso_file$bgrd_data <-
        scale_signals(
          iso_file$bgrd_data,
          bgrd_col_names,
          to = to,
          R = R,
          R_units = R_units,
          quiet = TRUE
        )
    }

    return(iso_file)
  }) %>% iso_as_file_list()

  # report problems from this particular function
  convert_signal_problems <- problems(iso_files) %>% filter(func == func)
  if (!quiet && nrow(convert_signal_problems) > 0) {
    sprintf("Warning: encountered %d problem(s) during signal conversion (%d total problems):",
            nrow(convert_signal_problems), isoreader:::n_problems(iso_files)) %>%
    message()
    print(convert_signal_problems)
    cat("\n")
  }

  # return single (if passed in as single)
  if (single_file) return (iso_files[[1]])
  return(iso_files)
}


# peak table unit conversion -----


#' Convert peak table column units
#'
#' This is a convenience function to convert the units of multiple peak table columns that are of type \code{\link{iso_double_with_units}} with the same units to a different SI prefix, e.g. from \code{mVs} to \code{Vs}. Uses \code{\link{iso_scale_double_with_units}} internally.
#'
#' @details At this time, this function cannot convert between different units yet (e.g. voltage and current), but this could be implemented using resistors values already available in the \code{iso_files} akin to the implementation in \link{iso_convert_signals} for raw data.
#'
#' @param ... S3 method placeholder parameters, see class specific functions for details on parameters
#' @export
iso_convert_peak_table_units <- function(...) {
  UseMethod("iso_convert_peak_table_units")
}

#' @export
iso_convert_peak_table_units.default <- function(...) {
  if(length(list(...)) == 0) stop("missing parameters", call. = FALSE)
  stop("this function is not defined for objects of type '",
       class(..1)[1], "'", call. = FALSE)
}

#' @export
iso_convert_peak_table_units.iso_file <- function(iso_files, ...) {
  iso_convert_peak_table_units(iso_as_file_list(iso_files), ...)[[1]]
}

#' @rdname iso_convert_peak_table_units
#' @param iso_files collection of continuous flow iso_file objects
#' @param ... which units to convert. Must be named character arguments - the values are the old units, the names are the new units. Each pair of old and new units must have the same base units and only differ in their SI prefix (e.g. \code{Vs = mVs, V = mV, nA = A, ...)}). For each unit to convert from, finds which columns amongst those found by \code{select} currently have those units.
#' @param select which columns to consider when converting units. Supports all \link[dplyr]{select} syntax. Columns not matching \code{select} will always keep their existing units. By default considers all columns.
#' @inheritParams iso_show_default_processor_parameters
#' @export
iso_convert_peak_table_units.iso_file_list <- function(iso_files, ..., select = everything(), quiet = default(quiet)) {

  # quo
  select_quo <- enquo(select)
  convert_quos <- rlang::enquos(...)
  if (length(convert_quos) == 0) return(iso_files)

  # info message
  if (!quiet) {
    convert <- purrr::map_chr(convert_quos, ~if (rlang::quo_is_symbol(.x) || rlang::quo_is_call(.x)) { rlang::as_label(.x) } else { rlang::eval_tidy(.x) })

    glue::glue(
      "Info: converting peak table column units where applicable from ",
      glue::glue_collapse(sprintf("'%s'->'%s'", convert, names(convert)), sep = ", ", last = " and "),
      " for columns matching '{rlang::as_label(select_quo)}'") %>%
      message()
  }

  # convert peaks
  iso_files <-
    map(iso_files,
      ~{
        if (!is.null(.x$peak_table)) {
          .x$peak_table <- iso_convert_peak_table_units(.x$peak_table, !!!convert_quos, select = !!select_quo, quiet = TRUE)
        }
        .x
      }
    )

  return(iso_as_file_list(iso_files))
}

#' @rdname iso_convert_peak_table_units
#' @param peak_table a peak table data frame
#' @export
iso_convert_peak_table_units.data.frame <- function(peak_table, ..., select = everything(), quiet = default(quiet)) {

  select_quo <- enquo(select)
  convert_quos <- rlang::enquos(...)
  if (length(convert_quos) == 0) return(peak_table)

  convert <- purrr::map_chr(convert_quos, ~if (rlang::quo_is_symbol(.x) || rlang::quo_is_call(.x)) { rlang::as_label(.x) } else { rlang::eval_tidy(.x) })
  if (!is.character(convert) || !rlang::is_named(convert))
    stop("all unit conversion parameters (...) must be named", call. = FALSE)

  # info message
  if (!quiet) {
    glue::glue(
      "Info: converting peak table column units where applicable from ",
      glue::glue_collapse(sprintf("'%s'->'%s'", convert, names(convert)), sep = ", ", last = " and "),
      " for columns matching '{rlang::as_label(select_quo)}'") %>%
      message()
  }

  if (rlang::as_label(select_quo) == "everything()")
    all_cols <- rlang::set_names(names(peak_table))
  else
    all_cols <- isoreader:::get_column_names(
      peak_table, select = select_quo, n_reqs = list(select = "*"),
      cols_must_exist = FALSE, warn = FALSE)$select

  all_units <- iso_get_units(peak_table[all_cols])
  all_cols <- all_cols[!is.na(all_units)]
  all_units <- all_units[!is.na(all_units)]
  if (length(all_cols) == 0) return(peak_table)

  # get quos for the mutate
  generate_quo <- function(col, to) {
    quo(iso_scale_double_with_units(!!rlang::sym(col), to_units = !!to))
  }
  conversions <- tibble(
    from_units = as.character(convert),
    to_units = names(convert),
    col = map(from_units, ~names(all_cols)[all_units == .x])
  ) %>% tidyr::unnest(col) %>%
    mutate(quos = map2(col, to_units, generate_quo))

  # run mutate
  return(dplyr::mutate(peak_table, !!!with(conversions, setNames(quos, col))))
}



# processing functions ----

# scale time units (uses lubridate)
# @param to unit to scale to
# @param from unit to scale from (only needs to be supplied if time is not already a duration)
scale_time <- function(time, to, from = NULL) {
  if (is(time, "Duration") && !is.null(from)) {
    warning("time is supplied as a duration so from will be ignored!", call. = FALSE, immediate. = TRUE)
  } else if (!is(time, "Duration")) {
    if (is.null(from)) stop("supplied times is not a duration object and therefore requires specifying from unit", call. = FALSE)
    time <- lubridate::duration(time, from)
  }
  time / lubridate::duration(1, to)
}

# get all supported SI prefixes
get_supported_si_prefixes <- function() {
  c(f = 1e-15, p = 1e-12, n = 1e-9,
    "\U00B5" = 1e-6, m = 1e-3, 1,
    k = 1e3, M = 1e6, G = 1e9, T = 1e12)
}

# find the base unit given a set of units and the available SI prefixes
# @param vector of units all with the same base unit
get_base_unit <- function(units, si_prefixes = get_supported_si_prefixes()) {
  chars <- units %>% strsplit("") %>% purrr::map(rev)
  shortest <- min(purrr::map_int(chars, length))
  shared <- 0L
  while(shared < shortest) {
    if (!all(chars %>% purrr::map_chr(shared + 1L) %>% {. == .[1]})) break
    shared <- shared + 1L
  }
  if (shared == 0)
    glue::glue("cannot determine shared base unit from these units: {paste(units, collapse = ', ')}") %>%
    stop(call. = FALSE)

  # determine base unit
  base_unit <- chars[[1]][1:shared] %>% rev() %>% paste0(collapse = "")

  # check for prefixes
  prefixes <- map_chr(chars, ~.x[-(1:shared)] %>% rev() %>% paste0(collapse = ""))
  ok <- prefixes %in% names(get_supported_si_prefixes())
  if (any(!ok))
    glue::glue(
      "encountered unsupported prefixes ",
      "'{paste(prefixes[!ok], collapse = \"', '\")}', for units ",
      "'{paste(units[!ok], collapse = \"', '\")}' ",
      "(base unit was determined to be '{base_unit}')") %>%
    stop(call. = FALSE)

  return(base_unit)
}

# get the scaling factor for a simple si unit prefix
# @param unit the unit to find the prefix for (e.g. mm, kg, ms, nA, kV)
# @param suffix the expected suffix (e.g. m, g, s, A, V), by default dtermined from the units themselves (only if they have a common base unit)
# @note does not currently process compound units (e.g. m/s) or powers (e.g. cm2 s-1)
get_si_prefix_scaling <- function(unit, suffix = get_base_unit(unit)) {
  #safety checks
  if(missing(unit)) stop("no unit supplied", call. = FALSE)
  if(length(unit) == 1 && missing(suffix)) stop("no unit suffix specified, cannot determine base unit from a single unit", call. = FALSE)

  # supported prefixes
  prefix <- get_supported_si_prefixes()

  # generate pattern
  prefix_pattern <- prefix %>% names() %>% str_c(collapse="|")
  pattern <- sprintf("^(%s)%s$", prefix_pattern, suffix)
  prefixes <- unit %>% stringr::str_match(pattern) %>% { .[,2] }
  if (any(is.na(prefixes))) {
    stop("Encountered unrecognized units: ",
         unit[is.na(prefixes)] %>% str_c(collapse = ", "),
         ". Supported are for this suffix: ",
         prefix %>% names() %>% str_c(suffix) %>% str_c(collapse = ", "),
         call. = FALSE)
  }

  # scaling
  prefix[sapply(prefixes, function(i) which(i == names(prefix)))] %>% unname()
}

# get unit scaling information (base unit and si prefix)
# @param unit unit to find scaling for
# @param base the valid base units(s) e.g. c("kg", "V")
get_unit_scaling <- function(unit, base_units) {
  if(missing(unit) || missing(base_units)) stop("missing parameters", call. = FALSE)
  if (length(unit) != 1)
    stop("can only find scaling for one unit at a time, received ", str_c(unit, collapse = ", "), call. = FALSE)
  base_unit <- stringr::str_extract(unit, sprintf("(%s)$", str_c(base_units, collapse = "|")))
  if (is.na(base_unit))
    stop("encountered invalid unit, expected '", str_c(base_units, collapse = "' or '"),
         "' with a valid SI prefix but received: ", unit, call. = FALSE)
  list(
    base_unit = base_unit,
    si_scaling = get_si_prefix_scaling(unit, base_unit)
  )
}

# get a conversion factor from one SI unit to another
get_unit_conversion_factor <- function(from_units, to_units) {
  purrr::map2_dbl(from_units, to_units, ~get_si_prefix_scaling(c(.x,.y)) %>% {.[1]/.[2]})
}

# scale SI
#' Scale a double with units
#'
#' Scale an isoverse \link[isoreader]{iso_double_with_units} to a different SI prefix. Will error if units are incompatible or si prefix is unknown.
#'
#' @param x an \link[isoreader]{iso_double_with_units} object
#' @param to_units what unit to scale to (must be in the same base unit as \code{x})
#' @param return \code{x} scaled to the new \code{to_units}
#' @export
iso_scale_double_with_units <- function(x, to_units) {
  if(!iso_is_double_with_units(x)) stop("can only scale double with units using this function", call. = FALSE)
  conv_factor <- get_unit_conversion_factor(from_units = iso_get_units(x), to_units = to_units)
  iso_double_with_units(x * conv_factor, units = to_units)
}

# Scale signal (voltage or current)
# @param data the data frame
# @param signal_cols the signal columns to convert, must have format [vi](\\d+)\\.(\\w+) to specifiy both mass and units
# @param to unit to convert to
# @param R resistor value(s), named vector with Rm (m=mass) as names (e.g. c(R45=0.3, R46=3)), in units of R_units
# @param R_units what units resistances are in
# @param V_pattern regular expression pattern how to recognize voltage columns and detect the masses they belong to (default is v followed by a number, e.g. v45)
# @param I_prefix prefix for the newly created current columns, the suffix is automatically the current units
# @note consider exporting this function
scale_signals <- function(data, signal_cols, to, R = c(), R_units = "GOhm", quiet = default(quiet)) {

  # safety checks
  if(missing(data) || !is.data.frame(data))  stop("data has to be supplied as a data frame to ", sys.call(0), call. = FALSE)
  if(missing(signal_cols) || is.null(signal_cols) || missing(to)) stop("signal_cols and to parameters required", call. = FALSE)
  if(!missing(R) && ( !is.vector(R, "numeric") || is.null(names(R)) || any(names(R) == "")))
    stop("resistance values have to be a named numeric vector - e.g., c(R45=0.3)", call. = FALSE)
  if (missing(R)) R <- rep(NA_real_, length(signal_cols))

  # signal columns
  v_prefix <- "v" # voltage columns prefix
  i_prefix <- "i" # current columns prefix
  signal_pattern <- sprintf("^[%s%s]C?(\\d+)\\.(\\w+)$", v_prefix, i_prefix)
  if (any(wrong <- !str_detect(signal_cols, signal_pattern))) {
    stop("some signal columns do not fit the expected pattern for signal colum names: ",
         str_c(signal_cols[wrong], collapse =", "), call. = FALSE)
  }
  cols <- names(data)
  cols_idx <- sapply(signal_cols, function(col) which(col == cols) %>% {.[1]})
  if(any(wrong <- is.na(cols_idx))) {
    stop("some signal columns do not exist in the provided data frame: ",
         str_c(signal_cols[wrong], collapse = ", "), call. = FALSE)
  }

  # get all the masses and units
  cols_masses <- signal_cols %>% stringr::str_match(signal_pattern) %>% {.[,2]}
  cols_Rs <- str_c("R", cols_masses)
  cols_units <- signal_cols %>% stringr::str_match(signal_pattern) %>% {.[,3]} %>%
    lapply(get_unit_scaling, base_units = c("V", "A"))
  cols_base_units <- sapply(cols_units, `[[`, "base_unit")
  to_units <- get_unit_scaling(to, c("V", "A"))
  R_units <- get_unit_scaling(R_units, "Ohm")

  # check that all columns that require V to I or vice versa have resistor values
  VtoI <- if (to_units$base_unit == "A") cols_base_units == "V" else rep(FALSE, length(cols_idx))
  ItoV <- if (to_units$base_unit == "V") cols_base_units == "A" else rep(FALSE, length(cols_idx))
  column_prefix <- if (to_units$base_unit == "A") i_prefix else if (to_units$base_unit == "V") v_prefix else stop("can't happen")
  if (length(missing <- setdiff(c(cols_Rs[VtoI], cols_Rs[ItoV]), names(R)))) {
    stop("not all resistors required for voltage/current conversion were provided, missing: ",
         str_c(missing, collapse = ", "), call. = FALSE)
  }

  # data scaling
  signal_scaling <-
    tibble(
      col_name = signal_cols,
      col_scale = sapply(cols_units, `[[`, "si_scaling"),
      col_idx = cols_idx,
      mass = cols_masses,
      VtoI = VtoI,
      ItoV = ItoV,
      scale = !VtoI & !ItoV,
      to_col_name = str_c(column_prefix, mass, ".", to),
      to_scale = to_units$si_scaling,
      R = R[cols_Rs],
      R_scale = R_units$si_scaling
    )

  # show scaling if in debug mode
  if (default(debug)) {
    message("DEBUG MSG: Using the following conversion table for signal scaling:")
    print(signal_scaling)
  }

  # convert
  for (i in 1:nrow(signal_scaling)) {
    # find value multiplier
    multiplier <- with(signal_scaling[i, ], {
      if (ItoV) col_scale*R*R_scale/to_scale
      else if (VtoI) col_scale/(R*R_scale * to_scale)
      else col_scale/to_scale
    })
    col_idx <- signal_scaling$col_idx[i]
    new_name <- signal_scaling$to_col_name[i]
    data[,col_idx] <- data[,col_idx] * multiplier
    names(data)[col_idx] <- new_name
  }

  return(data)
}

