\name{outlierplot}
\alias{outlierplot}
\title{Scatterplot with outlier markings}
\description{
Creats ggplot scatterplot with outlier detection.
}
\examples{
y<-c(1.9,3.1,3.3,4.8,5.3,6.1,6.4,7.6,9.8,12.4,100,200,5.8)
labels<- c("a","b",'c',"d","e","f",'g','h',"i",'j','k','l','m')
x<-c(2,1,5,5,20,20,23,10,30,25,20,18,20)
outlierplot(x = x, y = y, labels = labels)
outlierplot(x = x, y = y, alpha = .9)
outlierplot(x = x, y = y)

diamonds <- ggplot2::diamonds
outlierplot(x = diamonds$carat, y = diamonds$depth)
outlierplot(x = diamonds$carat, y = diamonds$depth, alpha = .5)
outlierplot(x = diamonds$carat, y = diamonds$depth, labels = diamonds$cut)
}
