# @note: consider setting all available default process parameters here and then only allowing overwrite
# of those that do exist
initialize_options <- function() {
  # set default package options (always resets options to force deliberate change of settings)
  default_options <- list(
    isoprocessor.default_parameters = list()
  )
  options(default_options)

  # set default print function
  # @FIXME: this should probably go into isoreader since it is useful there already
  iso_set_default_process_parameters(print_func = identity)
}

.onLoad <- function(libname, pkgname) {
  # initialize isoreader settings as well
  isoreader:::initialize_options()
  initialize_options()
  invisible()
}
