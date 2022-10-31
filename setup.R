#loading packages
options(repos = BiocManager::repositories())
library(BiocManager)
library(shiny)
library(edgeR)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(shinyjs)
library(DT)

#loading functions
source('./functions/generate_CDS.R')
source('./functions/get_row_names.R')
source('./functions/make_barplot.R')
source('./functions/make_bcv_plot.R')
source('./functions/make_boxplot.R')
source('./functions/make_hist.R')
source('./functions/make_meanvar_plot.R')
source('./functions/make_results.R')
source('./functions/make_smear_plot.R')
source('./functions/plot_wrapper.R')
source('./functions/preprocess_data.R')
source('./functions/remove_low_counts.R')

#setting up variables
plots <- c("Bar", "Box", "BCV", "Mean-Var", "Histogram", "Smear")
