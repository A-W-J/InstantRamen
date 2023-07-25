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
                            value = 3),
               numericInput("trt_num",
                            "number of treatment samples",
                            value = 3),
               actionButton("pca_button", "run PCA"),
               radioButtons("pca_display", "PCA Display", pca_plots),
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
               tabPanel("Volcano","Put volcano plot contents here"),
               tabPanel("Pathway Analysis", "Put pathway contents here")
             ))
    #comp_ui("comp")
  )
)

server <- function(input, output, session){
  #setting some things up
  #creating the primary reactive container
  main <- reactiveValues(user_input = NULL)
  #hiding some buttons while there is no data
  observe({
    toggle(id = "pca_button", condition =! is.null(main$user_input))
    toggle(id = "execute", condition =! is.null(main$user_input))
    toggle(id = "transform", condition =! is.null(main$user_input))
    toggle(id = "download", condition =! is.null(main$user_input))
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
    main$user_input <- data()
  })
  #show a preview of the uploaded data
  output$head <- renderDataTable({
    main$user_input
  })
  #when the user uploads the count data, process it and place it in the container
  #this is most likely causing a problem with the PCA analysis
  data1 <- reactive({
    req(main$user_input)
    preprocess_data(main$user_input)
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
  #this is where we will put the PCA information
  #with this information, we can generate a CDS object
  #this is triggered when the 'execute' button is pushed
  observeEvent(input$execute,{
    #moving the data processing here
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
  #similarly to the count processing, we can generate the PCA object
  observeEvent(input$pca_button,{
    main$pca <- principle(data = main$user_input)
    print(class(main$pca))
    #can we generate the plots here and put them in the 'main' object?
    main$scree <- make_scree_plot(main$pca)
    print(class(main$scree))
    print(main$ctl_num)
    print(main$trt_num)
    main$biplot <- make_biplot(pca = main$pca,
                               data = main$user_input,
                               ctl = main$ctl_name,
                               trt = main$trt_name,
                               ctl_n = main$ctl_num,
                               trt_n = main$trt_num)
    print(class(main$biplot))
  })
  #display the PCA plots

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