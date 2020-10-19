#' @name piechart
#' @rdname piechart
#' @title Create a pie chart with ggplot
#' @description This function allow to create a pie chart with ggplot.
#'
#' @param data dataset as a data.frame object
#' @param graphTitle aes paramters
#'
#' @return  a ggplot object
#'
# piechart <- function(data, mapping) {
#   ggplot2::ggplot(data, mapping) +
#     ggplot2::geom_bar(width = 1) +
#     ggplot2::coord_polar(theta = "y") +
#     ggplot2::xlab(NULL) +
#     ggplot2::ylab(NULL)
# }
piechart <- function(data, graphTitle){
  print(c(sum(data$check==FALSE), sum(data$check==TRUE)))
  t1 <- data.frame(categories=c("OK", "NOK"), Nb=c(sum(data$check==FALSE), sum(data$check==TRUE)))
  t1$fraction = t1$Nb / sum(t1$Nb)
  t1 = t1[order(t1$fraction), ]
  t1$ymax = cumsum(t1$fraction)
  t1$ymin = c(0, head(t1$ymax, n=-1))

  ggplot2::ggplot(t1, ggplot2::aes(fill=categories, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
    ggplot2::geom_rect(colour="grey30") +
    ggplot2::coord_polar(theta="y") +
    ggplot2::xlim(c(0, 4)) +
    ggplot2::annotate(geom = 'text', x = 0, y = 0,
                      label = paste0("Errors\n", round(t1$fraction[t1$categories == 'NOK']*100,0), "%"),
                      size = 10,
                      color = "red") +
    ggplot2::theme_void() +
    ggplot2::theme(panel.grid=ggplot2::element_blank()) +
    ggplot2::theme(axis.text=ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks=ggplot2::element_blank()) +
    ggplot2::labs(title=graphTitle)
}
