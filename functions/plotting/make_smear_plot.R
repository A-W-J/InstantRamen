make_smear_plot <- function(Data, Params){
  de <- rownames(Data$processed_data[Data$processed_data$FDR < 0.05,])
  p <- plotSmear(Data$CDS, 
                 de.tags = de,
                 pair = c(Params$control_name, Params$treatment_name))
}
