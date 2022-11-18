ResultsUI <- function(id){
  tabPanel("Get Results",
    fluidRow(
      column(4,
           textInput(NS(id,"ctl"), "Control Group Name", value = "control"),
           textInput(NS(id,"trt"), "Treatment Group Name", value = "treatment"),
           numericInput(NS(id, "ctl_num"), "Number of control samples", value = 1),
           numericInput(NS(id, "trt_num"), "Number of treatment samples", value = 1),
           actionButton(NS(id, "execute"), "Process data"),
           actionButton(NS(id, "transform"), "Annotate data"),
           downloadButton(NS(id, "download"), "download results")
      ),
      column(8,
           dataTableOutput("out")
      )
    )
  )
}

ResultsServer <- function(id, data){
  moduleServer(id, function(input, output, session){
    main <- reactiveValues(userInput = NULL)
    observe({
      toggle(id = "execute", condition =! is.null(main$userInput))
      toggle(id = "transform", condition =! is.null(main$userInput))
      toggle(id = "download", condition =! is.null(main$userInput))
    })
    observeEvent(data(),{
      main$userInput <- data()
    })
    data1 <- reactive({
      req(main$userInput)
      preprocess_data(main$userInput)
    })
    observeEvent(data1(),{
      main$data = data1()
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
    reactive({
      req(main$userInput)
      list(main$data, main$cds, main$ctl_name, main$trt_name, main$output)
    })
  })
}