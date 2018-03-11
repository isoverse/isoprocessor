# @note: consider setting all available default process parameters here and then only allowing overwrite
# of those that do exist
initialize_options <- function() {
  # set default package options (always resets options to force deliberate change of settings)
  default_options <- list(
    isoprocessor.default_parameters = list()
  )
  options(default_options)
}

.onLoad <- function(libname, pkgname) {
  # initialize isoreader settings as well
  isoreader:::initialize_options()
  initialize_options()
  invisible()
}
