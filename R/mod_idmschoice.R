#' idmschoice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_idmschoice_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' idmschoice Server Functions
#'
#' @noRd 
mod_idmschoice_server <- function(id, r = r, session = session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_idmschoice_ui("idmschoice_ui_1")
    
## To be copied in the server
# mod_idmschoice_server("idmschoice_ui_1")
