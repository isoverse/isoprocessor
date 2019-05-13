# metadata assignment =====

#' Add metadata to data table
#'
#' This function was deprecated in favour of the more broadly applicable \code{\link[isoreader]{iso_add_file_info}}.
#'
#' @param ... deprecated
#' @export
iso_add_metadata <- function(...) {

  warning("the 'iso_add_metadata' function was deprecated in favor of the more broadly applicable 'iso_add_file_info' in the isoreader package. Please call 'iso_add_file_info' directly.", call. = FALSE, immediate. = TRUE)
  isoreader::iso_add_file_info(...)
}

