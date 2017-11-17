#' @importFrom isoreader turn_info_messages_on
#' @export
isoreader::turn_info_messages_on

#' @importFrom isoreader turn_info_messages_off
#' @export
isoreader::turn_info_messages_off

# retrieve package settings, internal function, not exported
# first checks if the setting exists in the isoreader space, then in the isoprocessor
setting <- function(name) {
  value <- getOption(str_c("isoreader.", name))
  if (is.null(value))
    value <- getOption(str_c("isoprocessor.", name))
  if (is.null(value))
    stop("isoprocessor setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set package setting, internal function, not exported
set_setting <- function(name, value, overwrite = TRUE) {
  if (overwrite || !str_c("isoprocessor.", name) %in% names(options()))
    options(list(value) %>% setNames(str_c("isoprocessor.", name)))
  return(invisible(value))
}


# default columns =====

#' Set default parameters
#' Define the default parameters for calculations used in this package.
#' @param data can be used to include this function call within a pipeline
#' @param ... named entries for the default parameters (standard or non-standard evaluation supported).
#' @return invisibly returns the passed in \code{data} object
#' Names are equivalent to function parameter names and
#' @export
set_default_parameters <- function(data = NULL, ...) {
  def_cols <- quos(...)
  existing_def_cols <- setting("default_parameters")
  set_setting("default_parameters", modifyList(existing_def_cols, def_cols))
  return(invisible(data))
}

#' @details \code{reset_default_parameters} resets all default function parameters for this package.
#' @rdname set_default_parameters
#' @export
reset_default_parameters <- function(data = NULL) {
  set_setting("default_parameters", list(), overwrite = TRUE)
  return(invisible(data))
}

#' @details \code{get_default_parameters} returns all default function parameters for this package.
#' @rdname set_default_parameters
#' @export
get_default_parameters <- function() {
  setting("default_parameters")
}

#' @details \code{show_default_parameters} shows a table with the default function parameters for this package.
#' @rdname set_default_parameters
#' @export
show_default_parameters <- function(data = NULL) {
  message("Info: package default parameters")
  current <- get_default_parameters()
  if(length(current) == 0) {
    print(data_frame(parameter = character(0), value = character(0)))
  } else {
    data_frame(parameter = names(current), value = map_chr(current, quo_text)) %>%
      print()
  }
  return(invisible(data))
}

# retrieve a default value
default <- function(param) {
  param_quo <- enquo(param)
  param_name <- param_quo %>% quo_text()
  current <- get_default_parameters()
  if (param_name %in% names(current))
    return(current[[param_name]])
  else
    return(param_quo)
}

# resolve default cols in a list of quos
resolve_defaults <- function(quos) {
  resolve_default <- function(x) if (quo_is_lang(x) && lang_head(x) == sym("default")) eval_tidy(x) else x
  if (is_quosure(quos)) return(resolve_default(quos))
  else map(quos, resolve_default)
}
