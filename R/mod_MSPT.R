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
 
  )
}
    
#' MSPT Server Functions
#'
#' @noRd 
mod_MSPT_server <- function(id, session=session, r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_MSPT_ui("MSPT_1")
    
## To be copied in the server
# mod_MSPT_server("MSPT_1")
