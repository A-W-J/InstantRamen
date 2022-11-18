ImportUI <- function(id){
    column(4,
         fileInput(NS(id,"upload"), "Upload count data", accept = c(".txt", ".csv", ".tsv"))
  )
}

ImportServer <- function(id){
  moduleServer(id, function(input, output, session){
    reactive({req(input$upload)
             ext <- tools::file_ext(input$upload$name)
             switch(ext,
                    csv = vroom::vroom(input$upload$datapath, delim = ","),
                    tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
                    txt = vroom::vroom(input$upload$datapath, delim = "\t"),
                    validate("Invalid file; Please upload a .csv, .txt, or .tsv file")
               )
    })
  })
}