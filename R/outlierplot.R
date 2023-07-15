##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- OUTLIERPLOT VERSION 4-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create Outlier plot
#'
#' @param x List or column of numeric values.
#' @param y List or column of numeric values.
#' @param labels List or column of string labels.
#' @param alpha Transparency of scatterplot points.
#' @param withcolor Set to FALSE for black and white graph. Defalts to TRUE.
#' @param notheme Set to false to avoid setting theme.
#' @param detection.method Method of outlier detection ("HDo", "PCS", adjOut", "DDC", "MCD")
#' @param default.title Set to true to have title be name of outlier detection method and number of outliers found.
#'
#' @export outlierplot
#'
#' @returns A ggplot2 plot.
#' @examples
#' diamonds <- ggplot2::diamonds
#' outlierplot(x = diamonds$carat, y = diamonds$depth)
#' outlierplot(x = diamonds$carat, y = diamonds$depth, labels = diamonds$cut)
#' outlierplot(x = diamonds$carat, y = diamonds$depth, alpha = .5, withcolor = FALSE)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 scale_colour_gradientn
#' @importFrom ggplot2 scale_color_manual
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom dplyr mutate
#' @importFrom OutliersO3 O3prep
#' @importFrom FastPCS FastPCS
#' @importFrom cowplot theme_minimal_hgrid
#' @global labels x y alpha distance df.distance

outlierplot <-
  function(x,
           y,
           labels = NULL,
           alpha = NULL,
           withcolor = TRUE,
           notheme = FALSE,
           detection.method = "PCS",
           default.title = FALSE) {

    #........................Alpha Adjustment........................

    if (is.null(alpha)) {
      if (length(x) > 2000) {
        alpha <- .1
      } else if (length(x) > 1000) {
        alpha <- .3
      } else if (length(x) > 500) {
        alpha <- .5
      } else if (length(x) > 300) {
        alpha <- .7
      } else if (length(x) > 100) {
        alpha <- .8
      } else if (length(x) > 20) {
        alpha <- .9
      } else{
        alpha <- 1
      }

    }

    if (withcolor == FALSE & length(x) > 50) {
      alpha <- (alpha + .1) / 2
    }
    if (withcolor == FALSE & length(x) > 200) {
      alpha <- (alpha + .02) / 2
    }

    #...................Initial Dataframe Creation...................

    if (!is.null(labels)) {
      df <- data.frame("x" = x,
                       "y" = y,
                       "labels" = labels)

    } else {
      df <- data.frame("x" = x ,
                       "y" = y)

    }

    #..........2D Outlier Detection And Statistical Distance.........

    df.twocols <- df[,c("x","y")]

    pcs.results <- FastPCS(df.twocols)

    df$distance <- pcs.results$distance

    a0 <- O3prep(df.twocols, method=detection.method, tols=0.05, boxplotLimits=3)

    outlier.indexes <- a0$outList$outM[[3]]$outlierIndices

    df.outliers <- df[outlier.indexes,]

    df.regulars <- df[-(outlier.indexes),]

    #................Creating & Dividing Up Dataframes...............

    if(length(x) > 1000){
      labelpercent <- 1
    } else if(length(x) > 100){
      labelpercent <- 1
    } else if(length(x) > 50){
      labelpercent <- 2
    } else {
      labelpercent <- 3
    }

    to.label <-
      df.outliers[df.outliers$distance > quantile(df.outliers$distance, prob =
                                                    1 - labelpercent / 100), ]

    outside.top5 <-
      df.outliers[df.outliers$distance > quantile(df.outliers$distance, prob =
                                                    1 - 5 / 100), ]
    outside.5.thru.20 <-
      df.outliers[df.outliers$distance > quantile(df.outliers$distance, prob =
                                                    1 - 20 / 100) &
                    df.outliers$distance < quantile(df.outliers$distance, prob = 1 -
                                                      5 / 100), ]
    outside.20.thru.40 <-
      df.outliers[df.outliers$distance > quantile(df.outliers$distance, prob =
                                                    1 - 40 / 100) &
                    df.outliers$distance < quantile(df.outliers$distance, prob = 1 -
                                                      20 / 100), ]
    outside.40.thru.60 <-
      df.outliers[df.outliers$distance > quantile(df.outliers$distance, prob =
                                                    1 - 60 / 100) &
                    df.outliers$distance < quantile(df.outliers$distance, prob = 1 -
                                                      40 / 100), ]
    outside.60.thru.80 <-
      df.outliers[df.outliers$distance > quantile(df.outliers$distance, prob =
                                                    1 - 80 / 100) &
                    df.outliers$distance < quantile(df.outliers$distance, prob = 1 -
                                                      60 / 100), ]
    outside.80.thru.100 <-
      df.outliers[df.outliers$distance < quantile(df.outliers$distance, prob =
                                                    1 - 80 / 100), ]

    inside.top20 <-
      df.regulars[df.regulars$distance > quantile(df.regulars$distance, prob =
                                                    1 - 20 / 100), ]
    inside.20.thru.40 <-
      df.regulars[df.regulars$distance > quantile(df.regulars$distance, prob =
                                                    1 - 40 / 100) &
                    df.regulars$distance < quantile(df.regulars$distance, prob = 1 -
                                                      20 / 100), ]
    inside.40.thru.60 <-
      df.regulars[df.regulars$distance > quantile(df.regulars$distance, prob =
                                                    1 - 60 / 100) &
                    df.regulars$distance < quantile(df.regulars$distance, prob = 1 -
                                                      40 / 100), ]
    inside.60.thru.80 <-
      df.regulars[df.regulars$distance > quantile(df.regulars$distance, prob =
                                                    1 - 80 / 100) &
                    df.regulars$distance < quantile(df.regulars$distance, prob = 1 -
                                                      60 / 100), ]
    inside.80.thru.100 <-
      df.regulars[df.regulars$distance < quantile(df.regulars$distance, prob =
                                                    1 - 80 / 100), ]

    #..........................Creating Plot.........................

    if (withcolor == TRUE) {

      #......................Creating Plot: Color......................

      p <- ggplot(data = df, aes(color = distance)) +
        geom_point(
          data = outside.top5,
          aes(x = x, y = y),
          shape = 1,
          color = "#D12E60",
          size = 3.5 * 1.05,
          alpha = 1
        ) +
        geom_point(
          data = outside.top5,
          aes(x = x, y = y),
          shape = 16,
          color = "#D12E60",
          size = 1.5 * 1.05,
          alpha = 1
        ) +
        geom_point(
          data = outside.5.thru.20,
          aes(x = x, y = y),
          shape = 1,
          color = "#d14d2e",
          size = 3.5,
          alpha = (alpha + .9) / 2
        ) +
        geom_point(
          data = outside.5.thru.20,
          aes(x = x, y = y),
          shape = 16,
          color = "#d14d2e",
          size = 1.5,
          alpha = (alpha + .9) / 2
        ) +
        geom_point(
          data = outside.20.thru.40,
          aes(x = x, y = y),
          shape = 1,
          color = "#dc5089",
          size = 3.5,
          alpha = (alpha + .8) / 2
        ) +
        geom_point(
          data = outside.20.thru.40,
          aes(x = x, y = y),
          shape = 16,
          color = "#dc5089",
          size = 1.5,
          alpha = (alpha + .8) / 2
        ) +
        geom_point(
          data = outside.40.thru.60,
          aes(x = x, y = y),
          shape = 1,
          color = "#ac79cf",
          size = 3.5,
          alpha = (alpha + .7) / 2
        ) +
        geom_point(
          data = outside.40.thru.60,
          aes(x = x, y = y),
          shape = 16,
          color = "#ac79cf",
          size = 1.5,
          alpha = (alpha + .7) / 2
        ) +
        geom_point(
          data = outside.60.thru.80,
          aes(x = x, y = y),
          shape = 1,

          color = "#589de6",
          size = 3.5,
          alpha = (alpha + .6) / 2
        ) +
        geom_point(
          data = outside.60.thru.80,
          aes(x = x, y = y),
          shape = 16,
          color = "#589de6",
          size = 1.5,
          alpha = (alpha + .6) / 2
        ) +
        geom_point(
          data = outside.80.thru.100,
          aes(x = x, y = y),
          shape = 1,

          color = "#2eb2d1",
          size = 3.5,
          alpha = (alpha + .4) / 2
        ) +
        geom_point(
          data = outside.80.thru.100,
          aes(x = x, y = y),
          shape = 16,
          color = "#2eb2d1",
          size = 1.5,
          alpha = (alpha + .4) / 2
        ) +
        geom_point(
          data = inside.top20,
          aes(x = x, y = y),
          alpha = (alpha + .3) / 2,
          color = "#2eb2d1",
          size = 1.3
        ) +
        geom_point(
          data = inside.20.thru.40,
          aes(x = x, y = y),
          alpha = (alpha + .3) / 2,
          color = "#00a1d7",
          size = 1.1
        ) +
        geom_point(
          data = inside.40.thru.60,
          aes(x = x, y = y),
          alpha = (alpha + .3) / 2,
          color = "#008fdc",
          size = .9
        ) +
        geom_point(
          data = inside.60.thru.80,
          aes(x = x, y = y),
          alpha = (alpha + .3) / 2,
          color = "#0079db",
          size = .8
        ) +
        geom_point(
          data = inside.80.thru.100,
          aes(x = x, y = y),
          alpha = (alpha + .3) / 2,
          color = "#2e60d1",
          size = .7
        )

    } else {

      #..................Creating Plot: Black & White..................

      p <- ggplot() +
        geom_point(
          data = outside.top5,
          aes(x = x, y = y),
          shape = 1,
          size = 3.5 * 1.05,
          alpha = 1
        ) +
        geom_point(
          data = outside.top5,
          aes(x = x, y = y),
          shape = 16,
          size = 1.5 * 1.05,
          alpha = 1
        ) +
        geom_point(
          data = outside.5.thru.20,
          aes(x = x, y = y),
          shape = 1,
          size = 3.5,
          alpha = (alpha + .9) / 2
        ) +
        geom_point(
          data = outside.5.thru.20,
          aes(x = x, y = y),
          shape = 16,
          size = 1.5,
          alpha = (alpha + .9) / 2
        ) +
        geom_point(
          data = outside.20.thru.40,
          aes(x = x, y = y),
          shape = 1,
          size = 3.5,
          alpha = (alpha + .8) / 2
        ) +
        geom_point(
          data = outside.20.thru.40,
          aes(x = x, y = y),
          shape = 16,
          size = 1.5,
          alpha = (alpha + .8) / 2
        ) +
        geom_point(
          data = outside.40.thru.60,
          aes(x = x, y = y),
          shape = 1,
          size = 3.5,
          alpha = (alpha + .7) / 2
        ) +
        geom_point(
          data = outside.40.thru.60,
          aes(x = x, y = y),
          shape = 16,
          size = 1.5,
          alpha = (alpha + .7) / 2
        ) +
        geom_point(
          data = outside.60.thru.80,
          aes(x = x, y = y),
          shape = 1,
          size = 3.5,
          alpha = (alpha + .6) / 2
        ) +
        geom_point(
          data = outside.60.thru.80,
          aes(x = x, y = y),
          shape = 16,
          size = 1.5,
          alpha = (alpha + .6) / 2
        ) +
        geom_point(
          data = outside.80.thru.100,
          aes(x = x, y = y),
          shape = 1,
          size = 3.5,
          alpha = (alpha + .4) / 2
        ) +
        geom_point(
          data = outside.80.thru.100,
          aes(x = x, y = y),
          shape = 16,
          size = 1.5,
          alpha = (alpha + .4) / 2
        ) +
        geom_point(
          data = inside.top20,
          aes(x = x, y = y),
          alpha = (alpha + .3) / 2,
          size = 1.3
        ) +
        geom_point(
          data = inside.20.thru.40,
          aes(x = x, y = y),
          alpha = (alpha + .3) / 2,
          size = 1.1
        ) +
        geom_point(
          data = inside.40.thru.60,
          aes(x = x, y = y),
          alpha = (alpha + .3) / 2,
          size = .9
        ) +
        geom_point(
          data = inside.60.thru.80,
          aes(x = x, y = y),
          alpha = (alpha + .3) / 2,
          size = .8
        ) +
        geom_point(
          data = inside.80.thru.100,
          aes(x = x, y = y),
          alpha = (alpha + .3) / 2,
          size = .7
        )

    }

    #..........................Adding Labels.........................

    if (!is.null(labels)) {
      p <- p + geom_text(
        data = to.label,
        vjust = 0,
        nudge_y = 1.5,
        check_overlap = TRUE,
        aes(x = x, y = y, label = labels)

      )

    }

    #..........................Adding Theme..........................

    if(notheme == FALSE){
      p <- p + theme_minimal_hgrid(12)
    }

    #......................Adding Default Title......................

    if(default.title == TRUE){
      outlier.num <- nrow(df.outliers)
      to.title <- paste(detection.method, " (n=", as.character(outlier.num), ")", sep = "", collapse=NULL)
      p <- p + labs(title = to.title)
    }

    #.........................Returning Plot.........................

    return(p)

  }
