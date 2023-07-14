#' Test All Methods
#'
#' @param x List or column of numeric values.
#' @param y List or column of numeric values.
#' @param big.title Set equal to TRUE to have a bigger title.
#'
#' @export allmethods
#'
#' @returns A ggplot2 plot.
#' @examples
#' diamonds <- ggplot2::diamonds
#' allmethods(x = diamonds$carat, y = diamonds$depth)
#'
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @import patchwork
#' @global x y

allmethods = function(x=ggplot2::diamonds$price, y = ggplot2::diamonds$depth, big.title = FALSE){

  HDo.plot <- outlierplot(x = x, y = y, detection.method = "HDo", default.title = TRUE) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

  PCS.plot <- outlierplot(x = x, y = y, detection.method = "PCS", default.title = TRUE) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

  adjOut.plot <- outlierplot(x = x, y = y, detection.method = "adjOut", default.title = TRUE) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

  DDC.plot <- outlierplot(x = x, y = y, detection.method = "DDC", default.title = TRUE) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

  MCD.plot <- outlierplot(x = x, y = y, detection.method = "MCD", default.title = TRUE) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

  # (HDo.plot | PCS.plot | BAC.plot) /
  # (adjOut.plot | DDC.plot | MCD.plot)

  patchwork <- (HDo.plot | PCS.plot) / (adjOut.plot | DDC.plot | MCD.plot)

  if(big.title == FALSE){
    patchwork <- patchwork + plot_annotation(
      title = 'Outlier Detection Methods & Number Of Outliers Found',
      theme = theme(plot.title = element_text(size = 16))
    )
  } else {

    patchwork <- patchwork + plot_annotation(
      title = 'Outlier Detection Methods & Number Of Outliers Found',
      theme = theme(plot.title = element_text(size = 36))
      )

  }


  return(patchwork)

}
