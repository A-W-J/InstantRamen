principle <- function(data){
	data2 <- data[-1]
	data2 <- as.matrix(data2)
	data2 <- t(data2)
	data_norm <- scale(data2)
	data_pca <- prcomp(data2)
	data_pca
}
