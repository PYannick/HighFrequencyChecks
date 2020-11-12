.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath('logos',
                         system.file('logos',
                                     package = 'HighFrequencyChecks'))
}
