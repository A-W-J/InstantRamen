source('setup.R')
#to do: figure out why memory usage skyrockets when the app is executed
#to do: fix defaults for treatment and control sample numbers
#to do: debug comparison tab- uploads are not currently working

ui <- fluidPage(title = "InstantRamen v1.5",
  useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "darkly"),
  tabsetPanel(
    tabPanel("Import Data",
      fluidRow(
        column(4,
               fileInput("upload", "upload count data",
                         accept = c(".txt", ".csv", ".tsv"))
               ),
        column(8,
               dataTableOutput("head"))
      )
    ),
    tabPanel("Get Results",
      fluidRow(
        column(4,
               textInput("ctl", "control group name", value = "control"),
               textInput("trt", "treatment group name", value = "treatment"),
               numericInput("ctl_num", 
                            "number of control samples",
                            value = 1),
               numericInput("trt_num",
                            "number of treatment samples",
                            value = 1),
               actionButton("execute", "process data"),
               actionButton("transform", "annotate data"),
               downloadButton("download", "download results")
               ),
        column(8,
               dataTableOutput("out")
               )
      )
    ),
    tabPanel("Visualize Results",
     fluidRow(
       column(4,
         selectInput("plotType",
                     "select a plot to display",
                     choices = plots,
                     selected = plots[1])
              ),
       column(8,
              plotOutput("plotOut")
              )
     )
    ),
    tabPanel("Compare Data",
      fluidRow(
        column(4,
               fileInput("upload1",
                         "upload the first dataset",
                         accept = c(".txt", ".csv", ".tsv")),
               fileInput("upload2",
                         "upload the second dataset",
                         accept = c(".txt", ".csv", ".tsv")),
               sliderInput("pval", "p-value cutoff", val = 0.05, min = 0, max = 1),
               sliderInput("geneCount", "number of genes", val = 10, min = 0, max = 100),
               actionButton("overlaps", "get overlapping genes"),
               downloadButton("download2", "download results")
        ),
        column(8,
               dataTableOutput("geneTableOut"),
               plotOutput("genePlotOut")
               )
      )
    )
  )
)

server <- function(input, output, session){
  #setting some things up
  #creating the primary reactive container
  main <- reactiveValues(userInput = NULL)
  #hiding some buttons while there is no data
  observe({
    toggle(id = "execute", condition =! is.null(main$userInput))
    toggle(id = "transform", condition =! is.null(main$userInput))
    toggle(id = "download", condition =! is.null(main$userInput))
  })
  #loading in the primary count data
  data <- reactive({
    #do not execute until the user actually uploads the file
    req(input$upload)
    #verify that we have the right data type
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           txt = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("invalid file; please upload a .csv, .tsv, or .txt file")
      )
  })
  #capture the user input and place it in the main container
  observeEvent(data(),{
    main$userInput <- data()
  })
  #show a preview of the uploaded data
  output$head <- renderDataTable({
    main$userInput
  })
  #when the user uploads the count data, process it and place it in the container
  data1 <- reactive({
    req(main$userInput)
    preprocess_data(main$userInput)
  })
  observeEvent(data1(),{
    main$data <- data1()
  })
  #accept a bunch of inputs from a series of displayed boxes
  #we have defaults here, so no need to check for user input
  r_ctl <- reactive({
    input$ctl
  })
  observeEvent(r_ctl(),{
    main$ctl_name = r_ctl()
  })
  r_trt <- reactive({
    input$trt
  })
  observeEvent(r_trt(),{
    main$trt_name = r_trt()
  })
  r_ctl_n <- reactive({
    input$ctl_num
  })
  observeEvent(r_ctl_n(),{
    main$ctl_num = input$ctl_num
  })
  r_trt_n <- reactive({
    main$trt_num = input$trt_num
  })
  observeEvent(r_trt_n(),{
    main$trt_num = r_trt_n()
  })
  #with this information, we can generate a CDS object
  #this is triggered when the 'execute' button is pushed
  observeEvent(input$execute,{
    main$CDS <- generate_CDS(data = main$data,
                             ctl = main$ctl_name,
                             trt = main$trt_name,
                             ctl_n = main$ctl_num,
                             trt_n = main$trt_num)
  })
  #then we can make the LFC table
  observeEvent(input$execute,{
    req(main$CDS)
    #print(main$CDS)
    #print(class(main$data))
    main$output <- make_results(
      #data = main$data, 
      cds = main$CDS,
      ctl = main$ctl_name,
      trt = main$trt_name,
      ctl_n = main$ctl_num,
      trt_n = main$trt_num
    )
  })
  #display the LFC table
  output$out <- renderDataTable(
    main$output, options = list(pageLength = 5)
  )
  #if the 'transform button' is clicked, apply the annotation function to
  #the LFC table
  observeEvent(input$transform,{
    main$output <- transform_data(data = main$output)
  })
  #download the LFC table
  output$download <- downloadHandler(
    filename = function(){
      "results.csv"
    },
    content = function(file){
      write.csv(main$output, file)
    }
  )
  #using information from the second tab, generate plots
  output$plotOut <- renderPlot({
    req(input$execute)
    plot_wrapper(data = main$data,
                  ctl = main$ctl,
                  trt = main$trt,
                  cds = main$CDS,
                  top = main$output,
                  toggle = input$plotType)
  })
  #setting up another reactive container
  sub <- reactiveValues(userInput1 = NULL)
  #loading in the first result dataframe
  df1 <- reactive({
    req(input$upload1)
    ext <- tools::file_ext(input$upload1$datapath, delim = ",")
    switch(ext,
      csv <- vroom::vroom(input$upload1$datapath, delim = ","),
      tsv <- vroom::vroom(input$upload1$datapath, delim = "\t"),
      txt <- vroom::vroom(input$upload1$datapath, delim = "\t")
    )
  })
  #loading in the second result dataframe
  df2 <- reactive({
    req(input$upload2)
    ext <- tools::file_ext(input$upload2$datapath, delim = ",")
    switch(ext,
      csv <- vroom::vroom(input$upload2$datapath, delim = ","),
      tsv <- vroom::vroom(input$upload2$datapath, delim = "\t"),
      txt <- vroom::vroom(input$upload2$datapath, delim = "\t")
    )
  })
  #set these to their respective reactive containers
  observeEvent(df1(),{
    sub$userInput <- df1()
  })
  observeEvent(df2(),{
    sub$userInput <- df2()
  })
  #set values corresponding to the slider bars
  observeEvent(input$pval,{
    sub$pval <- input$pval
  })
  observeEvent(input$genecount,{
    sub$genecount <- input$genecounts
  })
  #working on the primary comparison pipeline
  #this activates through the 'find overlaps' button
  observeEvent(input$overlaps,{
    #ensuring the dataframes have 'genes' as the column name containing the 
    #list of genes
    df1 <- fix_gene_col(sub$userInput1)
    df2 <- fix_gene_col(sub$userInput2)
    #finding the list of overlapping significant genes within the two datasets
    genelist <- find_overlaps(
      df1 = sub$userInput1,
      df2 = sub$userInput2,
      size = sub$geneCount,
      p_cutoff = sub$pval
    )
    #assembling these results into a dataframe
    gene_df <- build_frame(
      df1 = sub$userInput1,
      df2 = sub$userInput2,
      genes = geneslist
    )
    #plotting the results
    plot <- make_compare_plot(gene_df)
    #placing the dataframe and the plot within the reactive container
    sub$df <- gene_df
    sub$plot <- plot
  })
  #display the table
  output$geneTableOut <- renderDataTable({
    sub$df
  })
  #display the plot
  output$genePlotOut <- renderPlot({
    sub$plot
  })
  #download logic
  output$download2 <- downloadHandler(
    filename = function(){
      "results.csv"
    },
    content = function(file){
      write.csv(sub$df, file)
    }
  )
}

shinyApp(ui, server)