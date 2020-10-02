#' @name hfcApp
#' @rdname hfcApp
#' @title starts the graphical user interface developed with \emph{shiny}.
#'
#' @param maxRequestSize (numeric) number defining the maximum allowed filesize (in megabytes)
#' for uploaded files, defaults to 50MB
#' @param debug logical if \code{TRUE}, set shiny-debugging options
#' @return starts the interactive graphical user interface which may be used to perform the
#' anonymisation process.
#' @param theme select stylesheet for the interface. Supported choices are
#' \itemize{
#' \item 'yeti'
#' \item 'flatly'
#' }
#' @param ... arguments (e.g \code{host}) that are passed through \code{\link[shiny]{runApp}} when
#' starting the shiny application
#' @param shiny.server Setting this parameter to \code{TRUE} will return the app in the form of an
#' object rather than invoking it. This is useful for deploying \code{hfcApp} via \code{shiny-server}.
#' @export
#'
#' @examples
#' \dontrun{
#' hfcApp(theme="flatly")
#' }

hfcApp <- function(maxRequestSize=50, debug=FALSE, theme="yeti", ..., shiny.server = FALSE) {
  if(!shiny.server)
    runApp(hfcApp(maxRequestSize, debug, theme, ..., shiny.server = TRUE))

  if (!is.numeric(maxRequestSize)) {
    stop("argument 'maxRequestSize' must be numeric!\n")
  }
  if (maxRequestSize < 1) {
    maxRequestSize <- 10
  }
  appDir <- system.file("shiny", "hfcApp", package="HighFrequencyChecks")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `HighFrequencyChecks`.", call.=FALSE)
  }
  options(shiny.maxRequestSize=ceiling(maxRequestSize)*1024^2)
  options(shiny.fullstacktrace=debug)
  options(shiny.trace=debug)

  #.GlobalEnv$.startdir <- getwd()
  shinyOptions(.startdir = getwd())
  shinyOptions(.appDir = appDir)

  if (!theme %in% c("yeti","flatly")) {
    stop("Invalid value for argument 'theme'\n")
  }

  if (theme=="yeti") {
    shinyOptions(.guitheme = "bootswatch_yeti.css")
    shinyOptions(.guijsfile = NULL)
  }

  if (theme=="flatly") {
    shinyOptions(.guitheme = "bootswatch_flatly.css")
    shinyOptions(.guijsfile = NULL)
  }


  source_from_appdir <- function(filename){
    source(file.path(appDir, filename), local = parent.frame(), chdir = TRUE)$value
  }

  shinyOptions(hfcAppInvoked = TRUE)
  source_from_appdir("global.R")
  shinyOptions(hfcAppInvoked = NULL)

  shiny::shinyApp(
    ui = source_from_appdir("ui.R"),
    server = source_from_appdir("server.R"),
    options = list(launch.browser=TRUE, ...)
  )
}
