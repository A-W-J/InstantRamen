run_pca <- function(data){
	mat <- t(as.matrix(data[,-1]))
	#data_norm <- scale(mat)
	data_pca <- prcomp(mat)
	data_pca
}
