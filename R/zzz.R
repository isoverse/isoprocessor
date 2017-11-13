.onLoad <- function(libname, pkgname) {
  # set default package options (always resets options to force deliberate change of settings)
  default_options <- list(
    isoprocessor.default_parameters = list()
  )
  options(default_options)
  invisible()
}
