#' acp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_acp_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidPage(

       fluidRow(
         box(title = "PCA options:", width = 6, status = "warning", solidHeader = TRUE,
             radioButtons(
               ns("naomit_method"),
               label = "Missing values (drop lines or columns with NA) : ",
               inline = TRUE,
               choices = list(
                 "Samples based" = 0 ,
                 "Features based" = 1
               ), selected = 0
             ),
             actionButton(ns("go2"), "Run ACP", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469"),
             verbatimTextOutput(ns("naomitval"))
         ),
         box(title = "Plot Settings:", width = 6, status = "warning", solidHeader = TRUE,
             pickerInput(
              ns("fact3"),
              label = "Factor to color samples in PCA:",
              choices = "",
              multiple = TRUE
            ),
             fluidRow(
               column(3,
                      selectInput(ns("pc1"),
                                  label = "Component on X axis:",
                                  choices = "")), 
               column(3,
                      selectInput(ns("pc2"),
                                  label = "Component on Y axis:",
                                  choices = ""))
             ),
             materialSwitch(ns("ellipse1"), label = "Plot ellipses", value = TRUE, status = "primary"),
             actionButton(ns("go1"), "Plot ACP", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469")
         )
       ),
       fluidRow(box(width = 6, 
                    title = 'ACP plot individuals', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    plotlyOutput(ns("acpplot"), height = "500"),
                    downloadButton(outputId = ns("acpplot_download"), label = "Download html plot")
                    ),
                box(width = 6, 
                    title = 'ACP plot variables', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    plotOutput(ns("acpplotvar"), height = "500"),
                    downloadButton(outputId = ns("acpplotvar_download"), label = "Download plot")
                    )
                ),
       fluidRow(box(width = 12, 
                    title = 'Individuals Coordinates:', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    DT::dataTableOutput(ns("prevacp1")),
                    downloadButton(outputId = ns("acpind_download"), label = "Download table")
                    )
       ),
       fluidRow(box(width = 12, 
                    title = 'Variables Coordinates:', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    DT::dataTableOutput(ns("prevacp1var")),
                    downloadButton(outputId = ns("acpvar_download"), label = "Download table")
                    )
       )

      )
 
  )
}
    
#' acp Server Functions
#'
#' @noRd 
mod_acp_server <- function(id, r = r, session = session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r_values <- reactiveValues()

    observeEvent(r$tabs$tabselected, {
      if(r$tabs$tabselected=='acp-tab' && r$fdata() == "emptytable") { # && is.null(r$fdata) )
        print("alert")
        shinyalert::shinyalert(title = "Oops", text="Final table not available, check all steps.", type='error')          
      }
    })    

    ### ACP tab
  
    # Settings   
    observe({
      req(r$mt1()) #r$mt1())  # metadata1()
      metadata1 <- r$mt1() #r$mt1()
      if(!is.null(metadata1)){

        #ACP
        r_values$metadata_final <- r$mt1()
        updatePickerInput(session, "fact3",
                          choices = names(r_values$metadata_final),
                          selected = names(r_values$metadata_final)[2],
                          options = list(
                            `actions-box` = TRUE, 
                            size = 10,
                            `selected-text-format` = "count > 3"
                          )
                        )

        updateSelectInput(session, "pc1",
                          choices = colnames(acp1()$x)[1:10],
                          selected = colnames(acp1()$x)[1])
        updateSelectInput(session, "pc2",
                          choices = colnames(acp1()$x)[1:10],
                          selected = colnames(acp1()$x)[2])

      }

    })
    
    acp1 <- eventReactive(input$go2, {
      cat(file=stderr(), 'ACP1 ... ', "\n")


      req(r$ds1())  # r_values$metadata_final # r_values$features_final , r_values$mt1
      ds1 <- r_values$features_final <- r$ds1()
      print(prev(ds1))

      # print(head(normds1()))
      # print(str(normds1()))
      if(input$naomit_method == 0){
        Tfeat0 =r_values$features_final
        allNA_index = apply(Tfeat0,2,function(x){all(is.na(x))})
        Tfeat = Tfeat0[,!allNA_index]


        acp_input <- na.omit(Tfeat)
        r_values$snaomit <- setdiff(row.names(r_values$features_final),row.names(acp_input))
        r_values$snaomit_att <- "sample(s)"
        r_values$snaomit_ndim <- nrow(r_values$features_final)
      }
      
      if(input$naomit_method == 1){
        Tfeat0 =t(r_values$features_final)
        allNA_index = apply(Tfeat0,2,function(x){all(is.na(x))})
        Tfeat = Tfeat0[,!allNA_index]

        Tfeat_ok <- na.omit(Tfeat)
        acp_input <- t(Tfeat_ok)
        r_values$snaomit <- setdiff(row.names(Tfeat),row.names(Tfeat_ok))
        r_values$snaomit_att <- "feature(s)"
        r_values$snaomit_ndim <- ncol(r_values$features_final)
      }
      
      if(nrow(acp_input) == 0){
        print("Empty table")
        showNotification("Empty table for ACP ...", type="error", duration = 5)
        return()
      }
      
      # Simplify features names
      tt <- stringr::str_split(colnames(acp_input), "__")
      tt1 <- sapply(tt,"[[",1)
      if(length(unique(tt1) ) == length(tt1)){
        colnames(acp_input) = tt1
        print(head(acp_input))
        
        # Check SD
        sds = apply(acp_input, 2, sd, na.rm=TRUE)
        keepsds = which(sds > 0)
          cat(file=stderr(), 'Delete variables with sd = 0 ... ', "\n")
          print(which(sds==0))
          Facp_input <- acp_input[,keepsds]
          
        acp1 = stats::prcomp(Facp_input, scale. = TRUE)  #t(normds1()[,-1])
        r_values$acp1 <- acp1
        
        r_values$summary_acp <- summary(acp1)
        
        # print(colnames(r_values$acp1$x))
        acp1
        
      }else{print("NON UNIQUE FEATURES in table.")
        shinyalert(title = "Oops", text="Non unique features in table, consider filtering on metadata.", type='error')
        acp1 = NULL
      }
      
      acp1
    })
    
    # Print samples or features with missing values
    output$naomitval <- renderPrint({
      req(r_values$snaomit,r_values$snaomit_att)
      cat(file = stderr(), 'missing values', "\n")
        list1 <- glue_collapse(r_values$snaomit, ", ")
        glue::glue("Following {r_values$snaomit_att} were omitted for PCA ({length(r_values$snaomit)}/{r_values$snaomit_ndim}):\n{list1}")
     })
    
    # Generate ACP Table
    acptab <- eventReactive(input$go2, {
      req(acp1()$x, r$mt1())
      r_values$metadata_final <- r$mt1()
      cat(file=stderr(), 'ACP tab ... ', "\n")
      acptab= as.data.frame(acp1()$x) %>% tibble::rownames_to_column(var = "sample.id") %>% 
        dplyr::inner_join(x = r_values$metadata_final, by = "sample.id")
      acptab

    })
    
    output$prevacp1 <- DT::renderDataTable({
      cat(file=stderr(), 'ACP table', "\n")
      r_values$acptab1
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, server=TRUE))  # , rowCallback = DT::JS(rowCallback))
    
    output$acpind_download <- downloadHandler(
      filename = "acpind_table.csv",
      content = function(file) {
        req(r_values$acptab1)
        r_values$acptab1
        write.table(r_values$acptab1, file, sep="\t", row.names=FALSE)
      }
    )
    
    
    ## Table var
    acptabvar <- eventReactive(input$go2, {
      cat(file=stderr(), 'ACP tab var... ', "\n")
      acptabvar = factoextra::get_pca_var(acp1())$coord %>% as.data.frame() %>% tibble::rownames_to_column(var = "features") 
      acptabvar
    })
  
    output$prevacp1var <- DT::renderDataTable({
      cat(file=stderr(), 'ACP table variables', "\n")
      acptabvar()
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, server=TRUE))  # , rowCallback = DT::JS(rowCallback))
    
    output$acpvar_download <- downloadHandler(
      filename = "acpvar_table.csv",
      content = function(file) {
        req(acptabvar())
        write.table(acptabvar(), file, sep="\t", row.names=FALSE)
      }
    )
    
    # Acp PLOT
    acpplot <- eventReactive(input$go1, {
      req(input$fact3, acptab(), input$pc1, input$pc2)
    # acpplot <- reactive({
      cat(file=stderr(), 'ACP plot', "\n")
      showNotification("Processing visualization...", type="message", duration = 2)
      print(input$fact3)

      r_values$acptab1 <- acptab1 <- acptab()
      fact3ok <- input$fact3

      if(length(input$fact3) != 1){
          comb = glue::glue_collapse(input$fact3, sep = ', \"_\",')
          namevar <- glue::glue_collapse(input$fact3, sep = '_')
          fun = glue::glue('acptab1 <- acptab1 %>% dplyr::mutate({namevar} = paste0({comb}), .after= "sample.id")')
          eval(parse(text=fun))
          r_values$fact3ok <- fact3ok <- namevar
          r_values$acptab1 <- acptab1
        }

      
      pc1 = as.numeric(substring(input$pc1, 3, 10))
      pc2 = as.numeric(substring(input$pc2, 3, 10))

      p = ggplot(data = acptab1, aes_string(x = input$pc1, y =
                                        input$pc2, color = as.name(fact3ok), sampleID = "sample.id")) + 
        geom_point() + theme_bw() + 
        xlab(glue::glue("{input$pc1} ({round(r_values$summary_acp$importance[2,pc1]*100,1)}%)")) + ylab(glue::glue("{input$pc2} ({round(r_values$summary_acp$importance[2,pc2]*100,1)}%)"))
      
      if(input$ellipse1){
        p <- p + stat_ellipse(aes_string(x = input$pc1, y = input$pc2, color = as.name(fact3ok)), inherit.aes = FALSE) 
      }

      ggplotly(p, tooltip=c("x", "y", "sampleID"))
    })
      
    output$acpplot <- renderPlotly({
      req(acpplot())
      acpplot() %>% config(toImageButtonOptions = list(format = "svg"))
    })
    
    output$acpplot_download <- downloadHandler(
      filename = "ACP_plot.html",
      content = function(file) {
        req(acpplot())
        saveWidget(acpplot(), file= file)
      }
    )
    
    
    acpplotvar <- eventReactive(input$go1, {
      req(acp1(), input$pc1, input$pc2)
      pc1 = as.numeric(substring(input$pc1, 3, 10))
      pc2 = as.numeric(substring(input$pc2, 3, 10))
      print(c(pc1, pc2))
      plotvar  <- factoextra::fviz_pca_var(acp1(), repel = TRUE, axes = c(pc1, pc2))
      print(class(plotvar))
      plotvar
    })
    
    output$acpplotvar <- renderPlot({
      req(acpplotvar())
      acpplotvar()
    })
    
    output$acpplotvar_download <- downloadHandler(
      filename = "acp_plotvar.pdf",
      content = function(file) {
        req(acpplotvar())
        p <- acpplotvar()
        ggsave(file, p, units = "cm", width = 15, height = 15, dpi = 300)
      }
    )



 
  })
}
    
## To be copied in the UI
# mod_acp_ui("acp_1")
    
## To be copied in the server
# mod_acp_server("acp_1")
