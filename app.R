source('setup.R')

ui <- fluidPage(title = "InstantRamen v1.0",
  useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "darkly"),
  tabsetPanel(
    tabPanel("Import Data",
      fluidRow(
        column(4,
          fileInput("upload", "Upload count data", accept = c(".txt", ".csv", ".tsv")),
          numericInput("n", "Rows", value = 5, min = 1, step = 1)
        ),
        column(8,
          tableOutput("head")
        )
      )
    ),
    tabPanel("Get Results",
      fluidRow(
        column(4,
          textInput("ctl", "Control Group Name", value = "control"),
          textInput("trt", "Treatment Group Name", value = "treatment"),
          numericInput("ctl_num", "Number of control samples", value = 1),
          numericInput("trt_num", "Number of treatment samples", value = 1),
          actionButton("execute", "Process data")
        ),
        column(6,
          dataTableOutput("out")
        ),
        column(2,
        downloadButton("download", "download results")
        )
      )
    ),
    tabPanel("Visualize Data",
      fluidRow(
        column(4,
          selectInput("plotType", "Select a plot to display", choices = plots,
                      selected = plots[1])
        ),
        column(8,
          plotOutput("plotOut")
        )
      )
    )
  )
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
    main$output, options = list(pageLength = 5)
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