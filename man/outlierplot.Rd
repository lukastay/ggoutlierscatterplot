\name{outlierplot}
\alias{outlierplot}
\title{Scatterplot with outlier markings}
\description{
Creats ggplot scatterplot with outlier detection.
}
\examples{
diamonds <- ggplot2::diamonds
outlierplot(x = diamonds$carat, y = diamonds$depth)
outlierplot(x = diamonds$carat, y = diamonds$depth, alpha = .5)
outlierplot(x = diamonds$carat, y = diamonds$depth, labels = diamonds$cut)
outlierplot(x = diamonds$carat, y = diamonds$depth, alpha = .5, withcolor = FALSE)
}
