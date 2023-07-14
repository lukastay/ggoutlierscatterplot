diamonds <- ggplot2::diamonds

p <- outlierplot(x = diamonds$price, y = diamonds$depth) +
  labs(title = "Scatterplot with Outliers", subtitle = "Using FastPCS Multideminsional Outlier Detection") +
  theme_minimal_hgrid(12)
p

tiff("plotexample.draft.tiff", units="in", width=6, height=4, res=500)
plot(p)
dev.off()
