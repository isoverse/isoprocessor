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
