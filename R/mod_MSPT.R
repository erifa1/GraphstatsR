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
        fixedRow(
          column(4,
            numericInput(ns("p1"), "Value of p:", 0.513, min = 0, max = NA, step = 0.1)
          ),
          column(4,
            numericInput(ns("minCID"), "Min CID:", 0.02, min = 0, max = NA, step = 0.1)
          ),
          column(4,
            numericInput(ns("maxBIAS"), "Max bias threshold:", 5, min = 0, max = 100, step = 0.2)
          )
        ),
        fixedRow(
          column(4,
            selectInput(ns("feat1"), label = "Feature to preview:", choices = "")
          )
        ),
        
        downloadButton(outputId = ns("download_MSPT"), label = "Run complete analysis", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469")

      ),
      box(title = "Preview figure / table:", width = 12, status = "warning", solidHeader = TRUE,
      plotOutput(ns("plot1")),
      DTOutput(ns("preview_table"))
      ),
      box(title = "Global table :", width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      # tableOutput(ns("contents"))
      DTOutput(ns("global_table"))
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


    observe({
      req(input$file)

      input_data <- rio::import(input$file$datapath)

      updateSelectInput(session, "feat1",
                  choices = unique(input_data[,"metabolite"]),
                  selected = unique(input_data[,"metabolite"])[1])
    })

    preview_mspt <- reactive({
      req(input$file)
      print("MSPT function")
      res <- MSPT_fun(input$file$datapath, p = input$p1, outpath = NULL, minCID = input$minCID, maxBias = input$maxBIAS, feature = input$feat1)
      res
    })


    output$preview_table <- renderDT( {
      req(preview_mspt())
      print("Rendering table")
      res <- preview_mspt()
      res$Table %>% dplyr::mutate(across(where(is.numeric), \(x) round(x, digits = 4 ) ) )
    }, options = list(pageLength = 6, scrollX = TRUE, server=TRUE, autoWidth = FALSE), filter="top" )

    output$global_table <- renderDT( {
      req(r_values$res)
      print("Rendering table")
      res <- r_values$res
      res$Table %>% dplyr::mutate(across(where(is.numeric), \(x) round(x, digits = 4 ) ) )
    }, options = list(pageLength = 6, scrollX = TRUE, server=TRUE, autoWidth = FALSE), filter="top" )


    output$plot1 <- renderPlot({
      req(preview_mspt())
      print("Rendering plot")
      res <- preview_mspt()
      res$figures

    })


    output$download_MSPT <- downloadHandler(
      filename = "MSPT.zip",
      content = function(file) {

        res <- r_values$res <- MSPT_fun(input$file$datapath, p = input$p1, outpath = NULL, minCID = input$minCID, maxBias = input$maxBIAS)
        res

        showNotification("Creating archive, please wait.", duration = 10, type = "message")
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
