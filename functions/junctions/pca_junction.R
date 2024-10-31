pca_junction <- function(Plots, toggle){
	if(toggle == "screeplot"){
		plot <- Plots$plot_list[[1]]
		#print(class(plot))
	}
	else if(toggle == "biplot"){
		plot <- Plots$plot_list[[2]]
		#print(class(plot))
	}
	plot
}
