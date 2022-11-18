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
        ImportUI("import"),
        column(8,
          dataTableOutput("head")
        )
      )
    ),
    #ResultsUI("results"),
    #VisUI("vis", plots),
    #CompUI("comp")
    )
  )




server <- function(input, output, session){
  main <- reactiveValues(userInput = NULL)
  dummy <- reactive({
    data.frame()
  })
  observeEvent(dummy(),{
    main$output <- dummy()
  })
  data <- ImportServer("import")
  observeEvent(data(),{
    main$userInput = data()
    print(head(main$userInput))
  })
  output$head <- renderDataTable(
    data(), options = list(pageLength = 5)
  )
  #megaOutput <- ResultsServer("results", data = main$userInput)
  #VisServer('vis', data = megaOutput())
  #CompServer('comp')
}

shinyApp(ui, server)