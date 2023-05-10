#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyalert useShinyalert
#' @noRd

SK8img <- base64enc::dataURI(file=system.file(file.path('app/www', 'SK8.png'), package='graphstatsr'))

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
                    title = "GraphStatsR 1.8.1",

                    tags$li(class="dropdown",tags$a("Hosted by", img(src = SK8img,
                    title = "SK8", height = "20px"), headerText = "Source code",href="https://sk8.inrae.fr/", target="_blank")),

                    tags$li(class="dropdown",tags$a(icon("gitlab"), headerText = "Source code",href="https://forgemia.inra.fr/etienne.rifa/graphstats", target="_blank")),
                    tags$li(class="dropdown",tags$a(icon("clinic-medical"), headerText = "Issues",href="https://forgemia.inra.fr/etienne.rifa/graphstats/-/issues", target="_blank"))#,
                    # tags$li(class="dropdown",tags$a(icon("twitter"), headerText = "Share", href="
# https://twitter.com", target="_blank"))
                  ),
                  
                  dashboardSidebar(
                    sidebarMenu(
                      id="tabs",
                      menuItem("Easy Stats", tabName= 'easystats-tab', icon=icon("diagnoses"),
                          startExpanded = TRUE,
                          menuSubItem('Input data', tabName = 'inputs-tab'),
                          menuSubItem('ACP', tabName = 'acp-tab'),
                          menuSubItem('Boxplots', tabName = 'boxplot-tab')
                          )
                    )
                  ),
                  
                  dashboardBody(
                    tags$head(includeCSS(system.file(file.path('app/www', 'style.css'), package='graphstatsr'))),
                    tabItems(                               
                      tabItem(tabName = 'inputs-tab',
                              mod_inputs_ui("inputs_1")
                      ),
                      tabItem(tabName = 'acp-tab',
                              mod_acp_ui("acp_1")
                      ),
                      tabItem(tabName = 'boxplot-tab',
                              mod_boxplots_ui("boxplots_1")
                      )
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
      app_title = 'graphstatsr'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    # shinyalert::useShinyalert()
  )
}

