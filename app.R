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
          fileInput("upload", "Upload count data", accept = c(".txt", ".csv", ".tsv")),
          #numericInput("n", "Rows", value = 5, min = 1, step = 1)
        ),
        column(8,
          dataTableOutput("head")
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
          actionButton("execute", "Process data"),
          actionButton("transform", "Annotate data"),
          downloadButton("download", "download results")
        ),
        column(8,
          dataTableOutput("out"),
        ),
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
    ),
    tabPanel("Compare Data",
      fluidRow(
        column(4,
          fileInput("upload1", "Upload the first dataset",
                    accept = c(".txt", ".csv", ".tsv")),
          fileInput("upload2", "Upload the second dataset",
                    accept = c(".txt", ".csv", ".tsv")),
          sliderInput("pval", "P-value Cutoff", val = 0.05, min = 0, max = 1),
          sliderInput("genecount", "Number of Genes", val = 10, min = 0, max = 100),
          actionButton("overlaps", "Get Overlapping Genes"),
          downloadButton("download2", "download results")
        ),
        column(8,
          dataTableOutput("geneTableOut"),
          plotOutput("GenePlotOut")
          )
      )
    )
  )
)

server <- function(input, output, session){
  main <- reactiveValues(userInput = NULL)
  observe({
    toggle(id = "execute", condition =! is.null(input$upload))
    toggle(id = "transform", condition =! is.null(input$upload))
    toggle(id = "download", condition =! is.null(input$upload))
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
    #col_numbers = check_col_number(main$userInput)
    #ctl_default = col_numbers[0]
    #trt_default = col_numbers[1]
    #updateNumericInput(inputId = ctl_num, val = ctl_default)
    #updateNumericInput(inputId = trt_nim, val = trt_default)
  })
  data1 <- reactive({
    req(input$upload)
    preprocess_data(main$userInput)
  })
  observeEvent(data1(),{
    main$data = data1()
  })
  output$head <- renderDataTable(
    main$data, options = list(pageLength = 5)
  )
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
  observeEvent(input$transform,{
    main$output <- transform_data(data = main$output)
  })
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
  sub <- reactiveValues(userInput1 = NULL)
  df1 <- reactive({
    req(input$upload1)
    ext <- tools::file_ext(input$upload1$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           txt = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv, .txt, or .tsv file")
    )
  })
  df2 <- reactive({
    req(input$upload2)
    ext <- tools::file_ext(input$upload2$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           txt = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv, .txt, or .tsv file")
    )
  })
  observeEvent(df1(),{
    sub$userInput1 <- df1()
  })
  observeEvent(df2(),{
    sub$userInput2 <- df2()
  })
  observeEvent(input$pval,{
    sub$pval <- input$pval
  })
  observeEvent(input$genecount,{
    sub$genecount <- input$genecount
  })
  observeEvent(input$overlaps,{
    genelist <- find_overlaps(
      df1 = sub$userInput1,
      df2 = sub$userInput2,
      size = sub$geneCount,
      pcutoff = sub$pval
    )
    gene_df <- build_frame(
      df1 = sub$userInput1,
      df2 = sub$userInput2,
      genes = genelist
    )
    plot <- make_compare_plot(gene_df)
    sub$df <- gene_df
    sub$plot <- plot
  })
  output$geneTableOut <- renderDataTable({
    sub$df
  })
  output$genePlotOut <- renderPlot({
    sub$plot
  })
}

shinyApp(ui, server)