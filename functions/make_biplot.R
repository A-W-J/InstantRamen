make_biplot <- function(pca, data, ctl, trt, ctl_n, trt_n){
	data2 <- data[,-1]
	data2 <- as.matrix(data2)
	data2 <- t(data2)
	data2 <- data.frame(data2)
	data2$phenotype <- c(rep(ctl, ctl_n), rep(trt, trt_n))
	autoplot(pca, data2, color = "phenotype", loadings = TRUE)
}
