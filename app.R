options(repos = BiocManager::repositories())
library(BiocManager)
library(shiny)
library(edgeR)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(shinyjs)

get_row_names <- function(data){
  data = data[!duplicated(data[,1]),]
  data <- data.frame(data)
  rownames(data) = data[,1]
  data = data[,-1]
  data
}

remove_low_counts <- function(data){
  means <- rowMeans(data)
  filter <- means >= 10
  table(filter)
  data <- data[filter,]
  data
}

preprocess_data <- function(data){
  data <- get_row_names(data)
  data <- remove_low_counts(data)
  data
}

generate_CDS <- function(data, ctl, trt, ctl_n, trt_n){
  group <- c(rep(ctl, ctl_n), rep(trt, trt_n))
  data <- as.matrix(sapply(data, as.numeric))
  counts = data
  cds <- DGEList( counts , group = group )
  cds <- calcNormFactors(cds, method = "upperquartile")
  scale <- cds$samples$lib.size*cds$samples$norm_factors
  normCounts <- round(t(t(counts)/scale)*mean(scale))
  cds <- estimateCommonDisp(cds, verbose = TRUE)
  cds <- estimateTagwiseDisp(cds)
  cds
}

make_results <- function(data, cds, ctl, trt, ctl_n, trt_n){
  group <- c(rep(ctl, ctl_n), rep(trt, trt_n))
  design <- model.matrix(~group)
  fit <- glmFit(cds, design)
  lrt <- glmLRT(fit)
  top <- topTags(lrt, n = nrow(cds$counts))$table
  genes <- row.names(data)
  top <- cbind(genes = genes, top)
  top
}

make_barplot <- function(data){
  colors <- brewer.pal(9, "Set1")
  totCounts <- colSums(data)
  barplot(totCounts, las = 2, col = "blue")
}

make_boxplot <- function(data){
  colors <- brewer.pal(9, "Set1")
  boxplot(log2(data + 1), las = 2, col = "green")
}

make_bcv_plot <- function(cds){
  plotBCV(cds)
}

make_meanvar_plot <- function(cds){
  plotMeanVar(cds,
                      show.raw.vars = TRUE,
                      show.tagwise.vars = TRUE,
                      show.binned.common.disp.vars = FALSE,
                      show.ave.raw.vars = FALSE,
                      NBline = TRUE,
                      nbins = 100,
                      pch = 16,
                      xlab = "Mean Expression (log10 scale)",
                      ylab = "Variance (log10 scale",
                      main = "Mean-Variance Plot")
}

make_hist <- function(top){
  hist(top$PValue, breaks = 20)
}

make_smear_plot <- function(cds, ctl, trt, top){
  de <- rownames(top[top$FDR < 0.05,])
  p <- plotSmear(cds, 
                 de.tags = de,
                 pair = c(ctl, trt))
}

plot_wrapper <- function(data, ctl, trt, cds, top, toggle){
  if(toggle == "Bar"){
    make_barplot(data)
  }
  if(toggle == "Box"){
    make_boxplot(data)
  }
  if(toggle == "BCV"){
    make_bcv_plot(cds)
  }
  if(toggle == "Mean-Var"){
    make_meanvar_plot(cds)
  }
  if(toggle == "Histogram"){
    make_hist(top)
  }
  if(toggle == "Smear"){
    make_smear_plot(cds, ctl, trt, top)
  }
}

plots <- c("Bar", "Box", "BCV", "Mean-Var", "Histogram", "Smear")

ui <- fluidPage(
  useShinyjs(),
  fileInput("upload", "Upload count data", accept = c(".txt", ".csv", ".tsv")),
  numericInput("n", "Rows", value = 5, min = 1, step = 1),
  tableOutput("head"),
  textInput("ctl", "Control Group Name", value = "control"),
  textInput("trt", "Treatment Group Name", value = "treatment"),
  numericInput("ctl_num", "Number of control samples", value = 1),
  numericInput("trt_num", "Number of treatment samples", value = 1),
  actionButton("execute", "Process data"),
  dataTableOutput("out"),
  downloadButton("download", "download results"),
  selectInput("plotType", "Select a plot to display", choices = plots,
              selected = plots[1]),
  plotOutput("plotOut")
)

server <- function(input, output, session){
  main <- reactiveValues(userInput = NULL)
  observe({
    toggle(id = "execute", condition =! is.null(input$upload))
  })
  dummy <- reactive({
    data.frame()
  })
  observeEvent(dummy(),{
    main$output <- dummy()
  })
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           txt = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv, .txt, or .tsv file")
           )
  })
  observeEvent(data(),{
    main$userInput = data()
  })
  data1 <- reactive({
    req(input$upload)
    preprocess_data(main$userInput)
  })
  observeEvent(data1(),{
    main$data = data1()
  })
  output$head <- renderTable({
    head(data(), input$n)
  })
  r_ctl <- reactive({
    req(input$ctl)
    input$ctl
  })
  observeEvent(r_ctl(),{
    main$ctl_name = r_ctl()
  })
  r_trt <- reactive({
    req(input$trt)
    input$trt
  })
  observeEvent(r_trt(),{
    main$trt_name = r_trt()
  })
  r_ctl_n <- reactive({
    input$ctl_num
  })
  observeEvent(r_ctl_n(),{
    main$ctl_num = r_ctl_n()
  })
  r_trt_n <- reactive({
    input$trt_num
  })
  observeEvent(r_trt_n(),{
    main$trt_num = r_trt_n()
  })
  observeEvent(input$execute,{
    main$cds <- generate_CDS(data = main$data,
                             ctl = main$ctl_name,
                             trt = main $trt_name,
                             ctl_n = main$ctl_num,
                             trt_n = main$trt_num)
  })
  observeEvent(input$execute,{
    main$output <- make_results(data = main$data,
                                cds = main$cds,
                                ctl = main$ctl_name,
                                trt = main$trt_name,
                                ctl_n = main$ctl_num,
                                trt_n = main$trt_num)
  })
  output$out <- renderDataTable(
    main$output
  )
  output$download <- downloadHandler(
    filename = function(){
      "results.csv"
    },
    content = function(file){
      write.csv(main$output, file)
    }
  )
  output$plotOut <- renderPlot({
    req(input$execute)
    plot_wrapper(data = main$data,
                 ctl = main$ctl_name,
                 trt = main$trt_name,
                 cds = main$cds,
                 top = main$output,
                 toggle = input$plotType)
  })
}

shinyApp(ui, server)