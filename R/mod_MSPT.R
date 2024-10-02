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
    fluidRow(
      box(title = "Settings:", width = 7, status = "warning", solidHeader = TRUE,
        fileInput(ns("file"), "Choose a file (csv, tsv, xlsx)", accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
        column(4,
          numericInput(ns("p1"), "Value of p:", 0.513, min = 0, max = NA)
        ),
        column(4,
          numericInput(ns("minCID"), "Min CID:", 0.02, min = 0, max = NA)
        ),
        column(4,
          numericInput(ns("maxBIAS"), "Max bias threshold:", 5, min = 0, max = 100)
        )

      ),
      box(title = "Preview:", width = 12, status = "warning", solidHeader = TRUE,
      downloadButton(outputId = ns("download_MSPT"), label = "Download archive"),
      tableOutput(ns("contents")),
      plotOutput(ns("plot1"))
      )
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
      req(input$file)
      res <- r_values$res <- MSPT_fun(input$file$datapath, p = input$p1, outpath = NULL, minCID = input$minCID, maxBias = input$maxBIAS)
      head(res$Table)

    })


    output$plot1 <- renderPlot({
      req(r_values$res)
      res <- r_values$res
      res$figures[[1]]

    })


    output$download_MSPT <- downloadHandler(
      filename = "MSPT.zip",
      content = function(file) {
        req(r_values$res)
        res <- r_values$res

        dir.create(glue::glue("{tmpdir}/MSPT_{systim}/"), recursive = TRUE)

        write.csv(res$Table, glue::glue("{tmpdir}/MSPT_{systim}/TP_results.csv"), sep=",", row.names = FALSE)
        saveWorkbook(res$workbook, glue::glue("{tmpdir}/MSPT_{systim}/TP_results.xlsx"), overwrite = TRUE)
        
        ml <- marrangeGrob(res$figures, nrow=2, ncol=1)
        ggsave(glue::glue("{tmpdir}/MSPT_{systim}/TP_figures.pdf"), ml , width = 11, height = 8, dpi = 200)

        zip(file, files = list.files(glue::glue("{tmpdir}/MSPT_{systim}/"), full.names = TRUE))
      }
    )


  })
}
    
## To be copied in the UI
# mod_MSPT_ui("MSPT_1")
    
## To be copied in the server
# mod_MSPT_server("MSPT_1")
