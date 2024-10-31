data_junction <- function(Data, toggle){
	if(toggle == "raw"){
		df <- Data$raw_data
	}
	else if(toggle == "processed"){
		df <- Data$processed_data
	}
}
