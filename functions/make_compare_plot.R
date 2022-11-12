make_compare_plot <- function(df){
  df <- melt(df, id.vars = 'genes', variable.name = 'LFC')
  plot <- ggplot(df, aes(fill = variable, y = value, x = genes))+
    geom_bar(position = 'stack', stat = 'identity')
  plot
}