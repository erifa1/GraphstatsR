#' Inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom tibble rownames_to_column 
#' @importFrom dplyr right_join
#' @importFrom plotly plotlyOutput
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom plotly config
#' @importFrom factoextra fviz_pca_var
#' @import ggplot2
#' @import DT

mod_Inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      
      tabsetPanel(
        tabPanel("Input tables",
                 fluidRow(
                   box(
                     title = "Input dataset", status = "warning", solidHeader = TRUE,
                     tags$div(
                       title = "A dataset with features in lines and samples in columns.",
                       fileInput(ns("dataset1"),
                                 label = "Dataset in tabulated format (.csv, .txt) : ",
                                 placeholder = "dataset.csv")
                     )
                   ),
                   box(
                     title = "Input metadata", status = "warning", solidHeader = TRUE,
                     tags$div(
                       title = "Metadata",
                       fileInput(ns("metadata1"),
                                 label = "Dataset in tabulated format (.csv, .txt) : ",
                                 placeholder = "metadata.csv")
                     )
                   )
                 ),
                fluidRow(
                  box(
                    title = 'Dataset preview', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    verbatimTextOutput(ns("prevds1"))
                  ),
                  box(
                    title = 'Metadata preview', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    verbatimTextOutput(ns("prevmt1"))
                  )
                ),
              fluidRow(
                box(
                  title = 'Normalization options', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  radioButtons(
                    ns("norm_method"),
                    label = "Normalization : ",
                    inline = TRUE,
                    choices = list(
                      "Raw" = 0 ,
                      "TSS (total-sum normalization)" = 1,
                      "CLR (center log-ration)" = 2
                    ), selected = 0
                  ),
                  shinyBS::bsButton(inputId = ns('norm'), label = "Normalize", block = F, style = 'danger', type='action')
                )
              ),
            fluidRow(
              box(width=12,
                title = 'Merged Table preview', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                DT::dataTableOutput(ns("mergedf_DT")),
                downloadButton(outputId = ns("mergedf_download"), label = "Download merged table")
              )
            )
        ),
        tabPanel("PCA",
                 fluidRow(
                   box(title = "Settings:", width = 6, status = "warning", solidHeader = TRUE,
                       # uiOutput(ns("factor1")),
                       selectInput(
                         ns("fact1"),
                         label = "Factor to color samples in PCA:",
                         choices = ""
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
                       actionButton(ns("go1"), "Plot ACP", icon = icon("play-circle"),
                                    style="color: #fff; background-color: #3b9ef5; border-color: #1a4469")
                   )
                 ),
                 fluidRow(box(width = 6, 
                              title = 'ACP plot individuals', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                              plotlyOutput(ns("acpplot"), height = "500")
                              ),
                          box(width = 6, 
                              title = 'ACP plot variables', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                              plotOutput(ns("acpplotvar"), height = "500")
                              )
                          ),
                 fluidRow(box(width = 12, 
                              title = 'ACP table', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                              DT::dataTableOutput(ns("prevacp1"))
                              )
                 )
        ),
        tabPanel("Boxplots",
                 fluidRow()
        )
      )
    )
  )
}
    
#' Inputs Server Functions
#'
#' @noRd 
mod_Inputs_server <- function(id, r = r, session = session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r_values <- reactiveValues(ds1=NULL, mt1=NULL)
    
    dataset1 <- reactive({
      cat(file=stderr(), 'dataset1 fun', "\n")
      if (!is.null(input$dataset1)){
        ds1 <- read.table(input$dataset1$datapath, sep = "\t", dec = ",", header = TRUE)
        row.names(ds1) <- ds1[,1]
        r_values$ds1 <- ds1
      }
      # else{
      #   print("no data")
      # }
      r_values$ds1
    })
    
    metadata1 <- reactive({
      cat(file=stderr(), 'metadata1 fun', "\n")
      if (!is.null(input$metadata1)){
        r_values$mt1 <- read.table(input$metadata1$datapath, sep = "\t", dec = ",", header = TRUE)
      }else{
        cat(file=stderr(), 'metadata1 is null', "\n")
        r_values$mt1 = NULL
      }
      r_values$mt1
    })
    
    output$prevds1 <- renderPrint({
      cat(file = stderr(), 'rendering ds1', "\n")
      cat('Running graphstats v0.0.0.9000\n')
      head(dataset1()[, 1:6])
      if (is.null(dataset1())) {
        print("no data")
      } else if (ncol(dataset1()) > 6) {
        head(dataset1()[, 1:6])
      } else{
        head(dataset1()[, 1:ncol(dataset1())])
      }
    })
    
    output$prevmt1 <- renderPrint({
      cat(file = stderr(), 'rendering mt1', "\n")
      if (is.null(metadata1())) {
        print("no data")
      } else if (ncol(metadata1()) > 6) {
        head(metadata1()[, 1:6])
      } else{
        head(metadata1()[, 1:ncol(metadata1())])
      }
      
    })
    
    output$normds1 <- renderPrint({
      cat(file = stderr(), 'rendering normds1', "\n")
      if (is.null(metadata1())) {
        print("no data")
      } else if (ncol(metadata1()) > 6) {
        head(metadata1()[, 1:6])
      } else{
        head(metadata1()[, 1:ncol(metadata1())])
      }
      
    })
    
    
    output$mergedf <- renderPrint({
      cat(file = stderr(), 'rendering mergedf', "\n")
      if (is.null(mergedf())) {
        print("no data")
      } else if (ncol(mergedf()) > 6) {
        head(mergedf()[, 1:6])
      } else{
        head(mergedf()[, 1:ncol(mergedf())])
      }
      
    })
    
    observeEvent(input$norm, {
      cat(file=stderr(), 'button normalize', "\n")
      normds1()
    },ignoreNULL = TRUE, ignoreInit = TRUE)
    
    normds1 <- reactive({
      req(r_values$ds1, input$norm_method)
      ds1 <- r_values$ds1
      
      if(input$norm_method == 0){
        normds1 <- ds1
      }
      
      if(input$norm_method == 1){
        normf = function(x){ x/sum(x) }
        # normds1 <- transform_sample_counts(ds1, normf)
        normds1 <- apply(ds1[,-1], 2, normf)
      }
      
      if(input$norm_method == 2){
        clr = function(x){log(x+1) - rowMeans(log(x+1), na.rm = TRUE)}
        normds1 <- clr(ds1[,-1])
      }

      r_values$normds1 <- normds1
      # print(r_values$phyobj_norm)
    })
    
    mergedf <- reactive({
      req(r_values$normds1, r_values$mt1)
      r_values$tabF = as.data.frame(t(normds1())) %>% 
        tibble::rownames_to_column(var = "sample.id") %>% 
        dplyr::right_join(x = metadata1(), by = "sample.id")
    })
    
    output$mergedf_download <- downloadHandler(
      filename = "merged_table.csv",
      content = function(file) {
        req(r_values$tabF)
        write.table(r_values$tabF, file, sep="\t", row.names=FALSE)
      }
    )
    
    
    rowCallback <- c(
      "function(row, data){",
      "  for(var i=0; i<data.length; i++){",
      "    if(data[i] === null){",
      "      $('td:eq('+i+')', row).html('NA')",
      "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
      "    }",
      "  }",
      "}"
    )
    
    output$mergedf_DT <- DT::renderDataTable({
      mergedf()
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, rowCallback = DT::JS(rowCallback)), server=TRUE)
    
    
    ### ACP tab
    
    # output$factor1 = renderUI({
    #   req(metadata1())
    #   selectInput(
    #     ns("fact1"),
    #     label = "Select factor to test: ",
    #     choices = names(metadata1())
    #   )
    # })
    
    observe({
      req(metadata1())
      updateSelectInput(session, "fact1",
                        choices = names(metadata1()),
                        selected = names(metadata1())[1])
      updateSelectInput(session, "pc1",
                        choices = colnames(acp1()$x)[1:10],
                        selected = colnames(acp1()$x)[1])
      updateSelectInput(session, "pc2",
                        choices = colnames(acp1()$x)[1:10],
                        selected = colnames(acp1()$x)[1])
    })
    
      
    acp1 <- reactive({
      req(r_values$normds1, r_values$mt1)
      # print(head(normds1()))
      # print(str(normds1()))
      acp1 = stats::prcomp(na.omit(t(normds1()[,-1])), scale. = TRUE)
      r_values$acp1 <- acp1
      
      print(colnames(r_values$acp1$x))
      acp1
    })
      
    acptab <- reactive({      
      acptab= as.data.frame(acp1()$x) %>% tibble::rownames_to_column(var = "sample.id") %>% 
        dplyr::inner_join(x = metadata1(), by = "sample.id")
      acptab

    })
    
    output$prevacp1 <- DT::renderDataTable({
      cat(file=stderr(), 'ACP table', "\n")
      acptab()
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, rowCallback = DT::JS(rowCallback)), server=TRUE) 
    
    acpplot <- eventReactive(input$go1, {
      req(input$fact1, acptab(), input$pc1, input$pc2)
    # acpplot <- reactive({
      cat(file=stderr(), 'ACP plot', "\n")
      print(input$fact1)
      p = ggplot(acptab(), aes_string(x = input$pc1, y =
                                        input$pc2, color = input$fact1, sampleID = "sample.id")) + 
        geom_point() + stat_ellipse(aes_string(x = input$pc1, y = input$pc2, color = input$fact1), inherit.aes = FALSE) + theme_bw()
      ggplotly(p, tooltip=c("x", "y", "sampleID"))
    })
      
    output$acpplot <- renderPlotly({
      req(acpplot())
      acpplot() %>% config(toImageButtonOptions = list(format = "svg"))
    })
    
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

 
  })
}
    
## To be copied in the UI
# mod_Inputs_ui("Inputs_ui_1")
    
## To be copied in the server
# mod_Inputs_server("Inputs_ui_1")
