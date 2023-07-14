p <- allmethods(big.title = TRUE)
p

tiff("plotexample.allmethods.draft.tiff", units="in", width=13, height=9, res=200)
plot(p)
dev.off()
