plot_driver <- function(Data, Params){
	scree_plot <- make_scree_plot(Data$PCA)
	#print("checkpoint 5.1")
	biplot <- make_biplot(Data, Params)
	#print("checkpoint 5.2")
	barplot <- make_barplot(Data$pre_processed)
	#print("checkpoint 5.3")
	boxplot <- make_boxplot(Data$pre_processed)
	#print("checkpoint 5.4")
	bcv <- make_bcv_plot(Data$CDS)
	#print("checkpoint 5.5")
	meanvar <- make_meanvar_plot(Data$CDS)
	#print("checkpoint 5.6")
	histogram <- make_histogram(Data$processed_data)
	#print("checkpoint 5.7")
	smear <- make_smear_plot(Data, Params)
	#print("checkpoint 5.8")
	output <- list(scree_plot, biplot, barplot, boxplot, bcv, meanvar, histogram, smear)
	print(class(output[1]))
	output
}

