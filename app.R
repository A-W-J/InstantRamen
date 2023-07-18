source('setup.R')
#to do: figure out why memory usage skyrockets when the app is executed
#to do: fix defaults for treatment and control sample numbers


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
    tabPanel("Explore Results",
             navlistPanel(
               tabPanel("PCA","Put PCA Contents Here"),
               tabPanel("Pathway Analysis", "Put pathway contents here")
             ))
    #comp_ui("comp")
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
  #comp_server("comp")
}

shinyApp(ui, server)