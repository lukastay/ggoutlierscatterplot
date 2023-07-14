# ggoutlierscatterplot
R Language: Builds scatterplot designed to pull attention to outliers.

![outlier scatterplot example](https://github.com/lukastay/ggoutlierscatterplot/blob/main/plotexample.tiff?raw=true)

Just as there is no single formula for determining outliers in one dimension, there are multiple ways of categorizing outliers from points in two dimensional space. This code uses FastPCS package at its default tolerance levels of 0.05 to detect outliers in two dimensions. 

To install, copy the following code into your R script or console:

```
library(devtools)
install_github("lukastay/ggoutlierscatterplot")
```

Plotting with ggoutlierplot is easy. Use the following syntax:

```
outlierplot(x = x, y = y)
```

FastPCS is a faster algorithm of Projection Congruent Subset (PCS). In their article "Finding multivariate outliers with FastPCS" by Kaveh Vakili and Eric Schmitt, the authors remark on the algorithm:

> "The main output of FastPCS is an outlyingness index measuring how much each observation departs from the pattern set by the majority of the data. The PCS outlyingness index is affine equivariant (meaning that the outlyingness ranking of the observations is not affected by a linear transformation of the data) and can be computed efficiently for moderate values of p and large values of n. To derive this index, FastPCS proceeds in two steps. First, it strives to select among many possible h-subsets of observations one devoid of outliers. Then, the outlyingness index is simply the distance of each observation to this subset. For easier outlier detection problems, we find that our approach produces results similar to state-of-the-art outlier detection algorithms. When considering more difficult cases however we find that the solution we propose leads to significantly better outcomes."
>
> -Finding multivariate outliers with FastPCS" by Kaveh Vakili and Eric Schmitt


Dynamic coloring is based off The PCS outlyingness index. The authors go on to say:

> "PCS looks for the h-subset of multivariate observations that is most congruent along many univariate projections. In this context, we measure the congruence of a given h-subset along a given projection by the size of its overlap with a second subset that is optimal (in a sense we make precise below) on that projection. The PCS criterion is based on the observation that a spatially cohesive h-subset will tend to be congruent with these optimal h-subsets, and a spatially disjoint one will not."
>
> -Finding multivariate outliers with FastPCS" by Kaveh Vakili and Eric Schmitt


Outliers are represented with red targets, while other data is shown in black. The code automatically adjusts geom_point transparency to deal with plotting many observations (transparency can also be specified in the function). When labels are passed to the function, outliers will be labelled. 

Themes, additional geoms, and other ggplot functions can be added to the object returned by outlierplot.

ggoutlierscatterplot uses multiple tools to make outliers stand out:

1) Outliers are colored red
2) Outliers have a target shape, rather than a dot
3) Outliers are less transparent
4) Outliers are larger
5) When labels are passed, extreme outliers are labeled

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
