initialize_options <- function() {
  # set default package options (always resets options to force deliberate change of settings)
  default_options <- list(
    isoprocessor.default_parameters = list()
  )
  options(default_options)

  # set default print function
  iso_set_default_process_parameters(print_func = identity)
}

.onLoad <- function(libname, pkgname) {
  # initialize isoreader settings as well
  isoreader:::initialize_options()
  initialize_options()
  invisible()
}
