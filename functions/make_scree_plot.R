make_scree_plot <- function(data_pca){
	fviz_eig(data_pca, addlabels = TRUE)
}
