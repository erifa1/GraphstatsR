#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import rhdf5
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  r <- reactiveValues(
    tabs = reactiveValues()
  )
  
  observe({
    r$tabs$tabselected <- input$tabs
  })
  
  
  # List the first level callModules here
  # callModule(mod_Inputs_server, "Inputs_ui_1", session=session, r = r)
  mod_Inputs_server("Inputs_ui_1")
  mod_idmschoice_server("idmschoice_ui_1")
  
}
