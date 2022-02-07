#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyalert useShinyalert
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    # fluidPage(
    #   h1("graphstats")
    # )
    dashboardPage(skin = "red",
                  dashboardHeader(
                    title = "GraphStats",
                    tags$li(class="dropdown",tags$a(icon("gitlab"), headerText = "Source code",href="https://forgemia.inra.fr/etienne.rifa/graphstats", target="_blank")),
                    tags$li(class="dropdown",tags$a(icon("clinic-medical"), headerText = "Issues",href="https://forgemia.inra.fr/etienne.rifa/graphstats/-/issues", target="_blank"))#,
                    # tags$li(class="dropdown",tags$a(icon("twitter"), headerText = "Share", href="
# https://twitter.com", target="_blank"))
                  ),
                  
                  dashboardSidebar(
                    sidebarMenu(
                      id="tabs",
                      style = "position: fixed; overflow: visible",
                      menuItem("Easy Stats", tabName= 'easystats', icon=icon("diagnoses"))#,
                      # menuItem("IDMS choice", tabName= 'idmschoice', icon=icon("diagnoses"))
                      # menuItem("Community Composition", tabName = "tab_compo", icon = icon("chart-pie"))
                    )
                  ),
                  
                  dashboardBody(
                    
                    tabItems(
                      tabItem(tabName = 'easystats',
                              mod_Inputs_ui("Inputs_ui_1")
                      )#,
                      # tabItem(tabName = 'idmschoice',
                      #         mod_idmschoice_ui("idmschoice_ui_1")
                      # )
                    )
                  )
                  
    )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'graphstats'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyalert::useShinyalert()
  )
}

