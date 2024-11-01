options(repos = BiocManager::repositories())
library(BiocManager)
library(shiny)
library(edgeR)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(shinyjs)
library(DT)
library(org.Hs.eg.db)
library(reshape)
library(factoextra)
library(ggfortify)
library(shinydashboard)


#source functions here
source('./functions/annotation/annotate_data.R')
source('./functions/annotation/convert_ids.R')

source('./functions/plotting/make_barplot.R')
source('./functions/plotting/make_bcv_plot.R')
source('./functions/plotting/make_biplot.R')
source('./functions/plotting/make_boxplot.R')
source('./functions/plotting/make_histogram.R')
source('./functions/plotting/make_meanvar_plot.R')
source('./functions/plotting/make_scree_plot.R')
source('./functions/plotting/make_smear_plot.R')
source('./functions/plotting/plot_driver.R')

source('./functions/processing/get_row_names.R')
source('./functions/processing/preprocess_data.R')
source('./functions/processing/process_data.R')
source('./functions/processing/remove_low_counts.R')

source('./functions/stats/generate_cds.R')
source('./functions/stats/run_pca.R')

source('./functions/junctions/data_junction.R')
source('./functions/junctions/pca_junction.R')
source('./functions/junctions/plot_junction.R')


#constants
plots <- c("bar", "box", "bcv", "mean_var", "histogram", "smear")

#ui
ui <- dashboardPage(skin = "black",
		    dashboardHeader(title = "Instant Ramen"),
		    dashboardSidebar(
				     fileInput("upload", "upload count data", accept = c(".txt", ".csv", ".tsv")),
				     textInput("user_settings", "parameters, seperated by a comma", value = "control,treatment,3,3"),
				     actionButton("run", "run"),
				     radioButtons("show_data_as", "Data type: ", choices = c("raw", "processed"), selected = "raw"),
				     radioButtons("show_pca_as", "Show PCA as: ", choices = c("screeplot", "biplot"), selected = "screeplot"),
				     selectInput("show_plot_as", "plot display", choices = plots)
				     ),
		    dashboardBody(
				  fluidRow(
					   column(width = 8,
						  box(width = NULL, DT::dataTableOutput("display"),
						      style = "height:400px; overflow-y: scroll; overflow-x: scroll;"),
						  ),
					   column(width = 4,
						  box(width = NULL, plotOutput("pca_display", height = 300, width = 250)),
						  box(width = NULL, plotOutput("plot_display", height = 300, width = 250))
						  
					   )
				  )
		    )
)

server <- function(input, output, session){
	Data <- reactiveValues()
	Params <- reactiveValues()
	Plots <- reactiveValues()

	#loading the data
	observeEvent(input$upload,{
			     raw_data <- reactive({
          			ext <- tools::file_ext(input$upload$name)
    				    switch(ext,
           				csv = vroom::vroom(input$upload$datapath, delim = ","),
           				tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           				txt = vroom::vroom(input$upload$datapath, delim = "\t"),
           				validate("invalid file; please upload a .csv, .tsv, or .txt file")
				      		)
			       })
			     Data$raw_data <- raw_data()
			     Data$pre_processed <- preprocess_data(Data$raw_data) #adding the preprocessing step here
			     #print("raw data row counts")
			     #print(nrow(Data$raw_data))
		    })
	#parsing out user input
	observeEvent(input$user_settings,{
			     settings <- unlist(strsplit(input$user_settings, split = ","))
			     Params$control_name = settings[1]
			     Params$treatment_name = settings[2]
			     Params$control_number = settings[3]
			     Params$treatment_number = settings[4]
		    })
	observeEvent(input$run,{
			     req(input$upload)
			     req(input$user_settings)
			     #print("checkpoint 1")
			     Data$CDS <- generate_cds(Data$pre_processed, Params)
			     #print("checkpoint 2")
			     Data$processed_data <- process_data(Data$CDS, Params)
			     #print("checkpoint 3")
			     Data$PCA <- run_pca(Data$raw_data)
			     #print("checkpoint 4")
			     Data$processed_data <- annotate_data(Data$processed_data)
			     #print("checkpoint 5")
			     Plots$plot_list <- plot_driver(Data, Params)
			     #print("checkpoint 6")
			     		    })
	
	observeEvent(input$show_pca_as,{
	  req(Plots$plot_list)
	  Plots$current_pca_plot <- pca_junction(Plots, toggle = input$show_pca_as)
	})
	
	observeEvent(input$show_plot_as,{
	  req(Plots$plot_list)
	  Plots$current_plot <- plot_junction(Plots, toggle = input$show_plot_as)
	})
	
	output$display <- DT::renderDataTable({
		req(input$upload)
	  df <- data_junction(Data, toggle = input$show_data_as)
	})
	
	output$pca_display <- renderPlot({
	  Plots$current_pca_plot
	})
	
	output$plot_display <- renderPlot({
	  Plots$current_plot
	})
}

shinyApp(ui, server)

