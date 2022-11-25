comp_ui <- function(id){
  tabPanel("Compare Data",
           fluidRow(
             column(4,
                    fileInput(NS(id,"upload1"), "Upload the first dataset",
                              accept = c(".txt", ".csv", ".tsv")),
                    fileInput(NS(id,"upload2"), "Upload the second dataset",
                              accept = c(".txt", ".csv", ".tsv")),
                    sliderInput(NS(id,"pval"), "P-value Cutoff", val = 0.05, min = 0, max = 1),
                    sliderInput(NS(id,"genecount"), "Number of Genes", val = 10, min = 0, max = 100),
                    actionButton(NS(id,"overlaps"), "Get Overlapping Genes"),
                    downloadButton(NS(id,"download2"), "download results")
             ),
             column(8,
                    dataTableOutput(NS(id,"geneTableOut")),
                    plotOutput(NS(id,"GenePlotOut"))
             )
           )
  )
}

comp_server <- function(id){
  moduleServer(id, function(input, output, session){
    sub <- reactiveValues(userInput1 = NULL)
    df1 <- reactive({
      req(input$upload1)
      ext <- tools::file_ext(input$upload1$name)
      switch(ext,
             csv = vroom::vroom(input$upload1$datapath, delim = ","),
             tsv = vroom::vroom(input$upload1$datapath, delim = "\t"),
             txt = vroom::vroom(input$upload1$datapath, delim = "\t"),
             validate("Invalid file; Please upload a .csv, .txt, or .tsv file")
      )
    })
    df2 <- reactive({
      req(input$upload2)
      ext <- tools::file_ext(input$upload2$name)
      switch(ext,
             csv = vroom::vroom(input$upload2$datapath, delim = ","),
             tsv = vroom::vroom(input$upload2$datapath, delim = "\t"),
             txt = vroom::vroom(input$upload2$datapath, delim = "\t"),
             validate("Invalid file; Please upload a .csv, .txt, or .tsv file")
      )
    })
    observeEvent(df1(),{
      sub$userInput1 <- df1()
      print(head(sub$userInput1))
    })
    observeEvent(df2(),{
      sub$userInput2 <- df2()
      print(head(sub$userInput2))
    })
    observeEvent(input$pval,{
      sub$pval <- input$pval
    })
    observeEvent(input$genecount,{
      sub$genecount <- input$genecount
    })
    observeEvent(input$overlaps,{
      df1 <- fix_gene_col(sub$userInput1)
      df2 <- fix_gene_col(sub$userInput2)
      genelist <- find_overlaps(
        df1 = df1,
        df2 = df2,
        size = sub$geneCount,
        p_cutoff = sub$pval
      )
      print(length(genelist))
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
    output$download <- downloadHandler(
     filename = function(){
        "results.csv"
     },
     content = function(file){
        write.csv(main$output, file)
      }
    )
  })
}
