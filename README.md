# ggoutlierscatterplot
R Language: Builds scatterplot designed to pull attention to ourliers.

![outlier scatterplot example](https://github.com/lukastay/ggoutlierscatterplot/blob/main/plotexample.tiff?raw=true)

Just as there is no single formula for determining outliers in one dimension, there are multiple ways of categorizing outliers from points in two dimensional space. This code uses the 95% prediction interval of a linear regression model and categorizes anything outside that band as an outlier. A scatterplot is then drawn and outliers are represented with red targets, while other data is shown in black. The code automatically adjusts geom_point transparency to deal with plotting many observations (transparency can also be specified in the function). When labels are passed to the function, outliers will be labelled. 

Themes, additional geoms, and other ggplot functions can be added to the object returned by outlierplot.

ggoutlierscatterplot uses multiple tools to make outliers stand out:

1) Outliers are colored red
2) Outliers have a target shape, rather than a dot
3) Outliers are less transparent
4) Outliers are larger
5) When labels are passed, extreme outliers are labeled

To install, copy the following code into your R script or console:

```
library(devtools)
install_github("lukastay/ggoutlierscatterplot")
```

Plotting with ggoutlierplot is easy. Use the following syntax:

```
outlierplot(x = x, y = y)
```

To add labels for outliers, use the following syntax:

```
outlierplot(x = x, y = y, labels = labels)
```

If you would like to set the transparency, use alpha:

```
outlierplot(x = x, y = y, alpha = .5)
```

You can turn color off with the variable "withcolor":

```
outlierplot(x = diamonds$carat, y = diamonds$depth, alpha = .5, withcolor = FALSE)
```
