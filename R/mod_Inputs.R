#' Inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
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
        r_values$ds1 <- read.table(input$dataset1$datapath, sep = "\t", dec = ",", header = TRUE)
      }
      else{
        print("no data")
      }
      r_values$ds1
    })
    
    metadata1 <- reactive({
      cat(file=stderr(), 'metadata1 fun', "\n")
      if (!is.null(input$metadata1)){
        r_values$mt1 <- read.table(input$metadata1$datapath, sep = "\t", dec = ",", header = TRUE)
      }
      else{
        cat(file=stderr(), 'metadata1 is null', "\n")
        r_values$mt1 = NULL
      }
      r_values$mt1
    })
    
    output$prevds1 <- renderPrint({
      cat(file=stderr(), 'rendering ds1', "\n")
      cat('Running graphstats v0.0.0.9000\n')
      head(dataset1()[,1:6])
    })
    
    output$prevmt1 <- renderPrint({
      cat(file=stderr(), 'rendering mt1', "\n")
      if(is.null(metadata1())){print("no data")
        }else if(ncol(metadata1())>6){
        head(metadata1()[,1:6])
      }else{
        head(metadata1()[,1:ncol(metadata1())])
      }
      
    })
    
    normalize <- reactive({
      req(r_values$phyobj_final, input$norm_method)
      FGdata <- r_values$phyobj_final
      
      if(input$norm_method == 0){
        FNGdata <- FGdata
      }
      
      if(input$norm_method == 1){
        normf = function(x){ x/sum(x) }
        FNGdata <- transform_sample_counts(FGdata, normf)
      }
      
      if(input$norm_method == 2){
        clr = function(x){log(x+1) - rowMeans(log(x+1))}
        otable <- otu_table(FGdata)
        otableCLR <- clr(otable)
        FNGdata <- FGdata; otu_table(FNGdata) <- otableCLR
      }
      
      #VST deseq2
      if(input$norm_method == 3){
        withProgress({
          otable <- FGdata@otu_table@.Data+1
          otableVST <- DESeq2::varianceStabilizingTransformation(otable, fitType='local')
          FNGdata <- FGdata; FNGdata@otu_table@.Data <- otableVST
        },message = "VST normalization, please wait...")
      }
      r_values$phyobj_norm <- FNGdata
      # print(r_values$phyobj_norm)
    })
 
  })
}
    
## To be copied in the UI
# mod_Inputs_ui("Inputs_ui_1")
    
## To be copied in the server
# mod_Inputs_server("Inputs_ui_1")
