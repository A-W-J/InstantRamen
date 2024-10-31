make_biplot <- function(Data, Params){
	print(nrow(Data$raw_data))
	data <- Data$raw_data
	print(ncol(data))
	print("checkpoint 5.1.1")
	mat <- t(as.matrix(data[,-1]))
	print("checkpoint 5.1.2")
	data2 <- data.frame(mat)
	print(nrow(data2))
	print("checkpoint 5.1.3")
	data2$phenotype <- c(rep(Params$control_name, Params$control_number), rep(Params$treatment_name, Params$treatment_number))
	#write.csv(data2, "test.csv")
	print("checkpoint 5.1.4")
	plot <- autoplot(Data$PCA, data2, color = "phenotype", loadings = TRUE)
	plot
}
