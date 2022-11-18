VisUI <- function(id, plots){
  tabPanel("Visualize Results",
    fluidRow(
      column(4,
           selectInput(NS(id, "plotType"), "Select a plot to display", choices = plots,
                       selected = plots[1])
      ),
      column(8,
           plotOutput(NS(id, "plotOut"))
      )
    )
  )
}

VisServer <- function(id, data){
  moduleServer(id, function(input, output, session){
    main <- reactiveValues(mega = NULL)
    observeEvent(data(),{
      main$mega <- data()
    })
    output$plotOut <- renderPlot({
      req(input$execute)
      plot_wrapper(data = main$mega[0],
                   ctl = main$mega[2],
                   trt = main$mega[3],
                   cds = main$mega[1],
                   top = main$mega[4],
                   toggle = input$plotType)
    })
  })
}