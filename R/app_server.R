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
  mod_inputs_server("inputs_1", session=session, r=r)
  mod_acp_server("acp_1", session=session, r=r)
  mod_boxplots_server("boxplots_1", session=session, r=r)

  mod_inputs_isot_server("inputs_2", session=session, r=r)
  mod_plots_isot_server("plot-tab2", session=session, r=r)
  
}
