##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- OUTLIERPLOT-----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create Outlier plot
#'
#' @param x List or column of numeric values.
#' @param y List or column of numeric values.
#' @param labels List or column of string labels.
#' @param alpha Transparency of scatterplot points.
#'
#' @export outlierplotold
#'
#' @returns A ggplot2 plot.
#' @examples
#' outlierplot(x = x, y = y, labels = labels)
#' outlierplot(x = x, y = y)
#' outlierplot(x = diamonds$carat, y = diamonds$depth)
#' outlierplot(x = diamonds$carat, y = diamonds$depth, labels = diamonds$cut)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_text
#' @importFrom stats lm
#' @importFrom stats predict
#' @global labels x y alpha

outlierplotold <- function(x, y, labels = NULL, alpha = NULL) {

  if(is.null(alpha)){

    if(length(x) > 2000){
      alpha <- .1
    } else if(length(x) > 1000){
      alpha <- .3
    } else if(length(x) > 500){
      alpha <- .5
    } else if(length(x) > 300){
      alpha <- .7
    } else if(length(x) > 100){
      alpha <- .8
    } else if(length(x) > 20){
      alpha <- .9
    } else{
      alpha <- 1
    }

  }


  #..........................With Labels...........................
  if (!is.null(labels)) {
    df <- data.frame("x" = x ,
                     "y" = y,
                     "labels" = labels)

    df.lm <- lm(y ~ x, data = df)

    xRange = data.frame(x = seq(min(x), max(x), 0.01))

    modelConfInt <- predict(df.lm,
                            level = 0.95,
                            interval = "prediction")

    insideInterval <-
      modelConfInt[, 'lwr'] < df[['y']] &
      df[['y']] < modelConfInt[, 'upr']

    df <- data.frame(
      "y" = df$y,
      "x" = df$x,
      "insideInterval" = insideInterval,
      "labels" = df$labels
    )

    outside.points <- subset(df, df$insideInterval == 0)

    inside.points <- subset(df, df$insideInterval == 1)

    p <- ggplot() +
      geom_point(data = inside.points, aes(x = x, y = y), alpha = alpha) +
      geom_point(
        data = outside.points,
        aes(x = x, y = y),
        shape = 1,
        color = "red",
        size = 3.5,
        alpha = alpha
      ) +
      geom_point(
        data = outside.points,
        aes(x = x, y = y),
        shape = 16,
        color = "red",
        size = 1.5,
        alpha = alpha
      ) +
      geom_text(
        data = outside.points,
        vjust = 0,
        nudge_y = 1,
        check_overlap = TRUE,
        aes(x = x, y = y, label = labels)
      )

  }
  #.........................Without Labels.........................
  else{
    df <- data.frame("x" = x , "y" = y)

    df.lm <- lm(y ~ x, data = df)

    xRange = data.frame(x = seq(min(x), max(x), 0.01))

    modelConfInt <- predict(df.lm,
                            level = 0.95,
                            interval = "prediction")

    insideInterval <-
      modelConfInt[, 'lwr'] < df[['y']] &
      df[['y']] < modelConfInt[, 'upr']

    df <- data.frame("y" = df$y,
                     "x" = df$x,
                     "insideInterval" = insideInterval)

    outside.points <- subset(df, df$insideInterval == 0)

    inside.points <- subset(df, df$insideInterval == 1)

    p <- ggplot() +
      geom_point(data = inside.points, aes(x = x, y = y), alpha = alpha) +
      geom_point(
        data = outside.points,
        aes(x = x, y = y),
        shape = 1,
        color = "red",
        size = 3.5,
        alpha = alpha
      ) +
      geom_point(
        data = outside.points,
        aes(x = x, y = y),
        shape = 16,
        color = "red",
        size = 1.5,
        alpha = alpha
      )

  }

  return(p)

}
