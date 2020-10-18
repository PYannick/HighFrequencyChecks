#' @name piechart
#' @rdname piechart
#' @title Create a pie chart with ggplot
#' @description This function allow to create a pie chart with ggplot.
#'
#' @param data dataset as a data.frame object
#' @param mapping aes paramters
#'
#' @return  a ggplot object
#'
#' @author https://ggplot2-book.org/programming.html
piechart <- function(data, mapping) {
  ggplot2::ggplot(data, mapping) +
    ggplot2::geom_bar(width = 1) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL)
}
