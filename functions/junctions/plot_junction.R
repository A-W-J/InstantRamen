plot_junction <- function(Plots, toggle){
	if(toggle == "bar"){
		plot <- Plots$plot_list[[3]]
	}
	else if(toggle == "box"){
		plot <- Plots$plot_list[[4]]
	}
	else if(toggle == "bcv"){
		plot <- Plots$plot_list[[5]]
	}
	else if(toggle == "mean_var"){
		plot <- Plots$plot_list[[6]]
	}
	else if(toggle == "histogram"){
		plot <- Plots$plot_list[[7]]
	}
	else if(toggle == "smear"){
		plot <- Plots$plot_list[[8]]
	}
	plot
}
