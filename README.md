# ggoutlierscatterplot
R Language: Visualize multidimensional outlier detection algorithms on a scatterplot

![outlier scatterplot example](https://github.com/lukastay/ggoutlierscatterplot/blob/main/plotexample.tiff?raw=true)

Just as there is no single formula for determining outliers in one dimension, there are multiple ways of categorizing outliers from points in two dimensional space. This package visualizes different outlier detection algorithms on a scatterplot.

To install, copy the following code into your R script or console:

```
library(devtools)
install_github("lukastay/ggoutlierscatterplot")
```

Plotting with ggoutlierplot is easy. Use the following syntax:

```
outlierplot(x = x, y = y)
```

You can also use other methods of outlier detection. The allmethods function returns a graph comparing the five outlier detection methods:

```
allmethods(x = x, y = y)
```

![outlier scatterplot allmethods example](https://github.com/lukastay/ggoutlierscatterplot/blob/main/plotexample.allmethods.tiff?raw=true)

You can choose your outlier detection method using the detction.method argument. You can choose from: "HDo", "PCS", "BAC", "adjOut", "DDC", & "MCD".

```
outlierplot(x = x, y = y, detection.method = "HDo")
```

Outliers are represented with red targets, while other data is shown in black. The code automatically adjusts geom_point transparency to deal with plotting many observations (transparency can also be specified in the function). When labels are passed to the function, outliers will be labelled. Dynamic coloring is based off a mix of the PCS outlyingness index and the outlier detection method chosen. In the future, dynamic coloring will only be based on the chosen outlier detection algorithm.

Themes, additional geoms, and other ggplot functions can be added to the object returned by outlierplot.

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

Outlier detection is available from a wide-ranging algorithm choice set:
1) HDo - HDOutliers

> "It is unique for a) dealing with a mixture of categorical and continuous variables, b) dealing with big-p (many columns of data), c) dealing with big- n (many rows of data), d) dealing with outliers that mask other outliers, and e) dealing consistently with unidimensional and multidimensional datasets. Unlike ad hoc methods found in many machine learning papers, hdoutliers is based on a distributional model that allows outliers to be tagged with a probability. This critical feature reduces the likelihood of false discoveries."
>
> -"Visualizing Big Data Outliers Through Distributed Aggregation" by Leland Wilkinson

2) PCS - Projection Congruent Subset

Still, PCS is the default outlier detection method.  FastPCS is a faster algorithm of PCS. In their article "Finding multivariate outliers with FastPCS" by Kaveh Vakili and Eric Schmitt, the authors remark on the algorithm:

> "The main output of FastPCS is an outlyingness index measuring how much each observation departs from the pattern set by the majority of the data. The PCS outlyingness index is affine equivariant (meaning that the outlyingness ranking of the observations is not affected by a linear transformation of the data) and can be computed efficiently for moderate values of p and large values of n. To derive this index, FastPCS proceeds in two steps. First, it strives to select among many possible h-subsets of observations one devoid of outliers. Then, the outlyingness index is simply the distance of each observation to this subset. For easier outlier detection problems, we find that our approach produces results similar to state-of-the-art outlier detection algorithms. When considering more difficult cases however we find that the solution we propose leads to significantly better outcomes... PCS looks for the h-subset of multivariate observations that is most congruent along many univariate projections. In this context, we measure the congruence of a given h-subset along a given projection by the size of its overlap with a second subset that is optimal (in a sense we make precise below) on that projection. The PCS criterion is based on the observation that a spatially cohesive h-subset will tend to be congruent with these optimal h-subsets, and a spatially disjoint one will not."
>
> -"Finding multivariate outliers with FastPCS" by Kaveh Vakili and Eric Schmitt


3) BAC - BACON

> "BACON, short for ‘Blocked Adaptive Computationally-Efficient Outlier Nominators’, is a somewhat robust algorithm (set), with an implementation for regression or multivariate covariance estimation. BACON() applies the multivariate (covariance estimation) algorithm, using mvBACON(x) in any case, and when y is not NULL adds a regression iteration phase, using the auxiliary .lmBACON() function."
>
> -"BACON for Regression or Multivariate Covariance Estimation"

4) adjOut - Based on Donoho-Stahel outlyingness measure

> "The Stahel-Donoho estimators (t, V) of multivariate location and scatter are defined as a weighted mean and a weighted covariance matrix with weights of the form w(r), where w is a weight function and r is a measure of "outlyingness," obtained by considering all univariate projections of the data. It has a high breakdown point for all dimensions and order $\sqrt n$ consistency. The asymptotic bias of V for point mass contamination for suitable weight functions is compared with that of Rousseeuw's minimum volume ellipsoid (MVE) estimator. A simulation shows that for a suitable w, t and V exhibit high efficiency for both normal and Cauchy distributions and are better than their competitors for normal data with point-mass contamination. The performances of the estimators for detecting outliers are compared for both a real and a synthetic data set."
>
> -"The Behavior of the Stahel-Donoho Robust Multivariate Estimator" by Ricardo A. Maronna and Victor J. Yohai


4) DDC - Deviating Data Cells

> "A multivariate dataset consists of n cases in d dimensions, and is often stored in an n by d data matrix. It is well-known that real data may contain outliers. Depending on the situation, outliers may be (a) undesirable errors which can adversely affect the data analysis, or (b) valuable nuggets of unexpected information. In statistics and data analysis the word outlier usually refers to a row of the data matrix, and the methods to detect such outliers only work when at least half the rows are clean. But often many rows have a few contaminated cell values, which may not be visible by looking at each variable (column) separately. We propose the first method to detect deviating data cells in a multivariate sample which takes the correlations between the variables into account. It has no restriction on the number of clean rows, and can deal with high dimensions. Other advantages are that it provides estimates of the `expected' values of the outlying cells, while imputing missing values at the same time. We illustrate the method on several real data sets, where it uncovers more structure than found by purely columnwise methods or purely rowwise methods. The proposed method can help to diagnose why a certain row is outlying, e.g. in process control. It may also serve as an initial step for estimating multivariate location and scatter matrices."
>
> -Detecting deviating data cells" by Peter J. Rousseeuw and Wannes Van den Bossche

5) MCD - Minimum Covariance Determinant 

> "The minimum covariance determinant (MCD) method of Rousseeuw is a highly robust estimator of multivariate location and scatter. Its objective is to find h observations (out of n) whose covariance matrix has the lowest determinant. Until now, applications of the MCD were hampered by the computation time of existing algorithms, which were limited to a few hundred objects in a few dimensions. We discuss two important applications of larger size, one about a production process at Philips with n = 677 objects and p = 9 variables, and a dataset from astronomy with n = 137,256 objects and p = 27 variables. To deal with such problems we have developed a new algorithm for the MCD, called FAST-MCD. The basic ideas are an inequality involving order statistics and determinants, and techniques which we call “selective iteration” and “nested extensions.” For small datasets, FAST-MCD typically finds the exact MCD, whereas for larger datasets it gives more accurate results than existing algorithms and is faster by orders of magnitude. Moreover, FASTMCD is able to detect an exact fit—that is, a hyperplane containing h or more observations. The new algorithm makes the MCD method available as a routine tool for analyzing multivariate data. We also propose the distance-distance plot (D-D plot), which displays MCD-based robust distances versus Mahalanobis distances, and illustrate it with some examples."
>
> -"A Fast Algorithm for the Minimum Covariance Determinant Estimator" by Peter J. Rousseeuw  and Katrien Van Driessen

ggoutlierscatterplot uses multiple tools to make outliers stand out:

1) Outliers are colored red
2) Outliers have a target shape, rather than a dot
3) Outliers are less transparent
4) Outliers are larger
5) When labels are passed, extreme outliers are labeled
