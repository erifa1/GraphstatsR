#' MSPT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MSPT_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(title = "Settings:", width = 7, status = "warning", solidHeader = TRUE,
      fileInput(ns("file"), "Choose a file", accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
      numericInput(ns("p1"), "Value of p:", 0.513, min = 0, max = NA)
    ),
    box(title = "Preview:", width = 12, status = "warning", solidHeader = TRUE,
    tableOutput(ns("contents")),
    plotOutput(ns("plot1"))
    )
  )
}
    
#' MSPT Server Functions
#'
#' @noRd 
mod_MSPT_server <- function(id, session=session, r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r_values <- reactiveValues(res = NULL)



    output$contents <- renderTable({
      # head(rio::import(input$file$datapath))

      res <- r_values$res <- MSPT_fun(input$file$datapath, p = input$p1, outpath = "./data_test/dev_mspt/MSPT_out3/")
      head(res$Table)

    })


    output$plot1 <- renderPlot({

      res <- r_values$res
      res$figures[[1]]

    })

  })
}
    
## To be copied in the UI
# mod_MSPT_ui("MSPT_1")
    
## To be copied in the server
# mod_MSPT_server("MSPT_1")
