#' Inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import tibble 
#' @import dplyr
#' @import tidyr
#' @importFrom gridExtra marrangeGrob
#' @importFrom plotly plotlyOutput
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom plotly config
#' @importFrom factoextra fviz_pca_var
#' @importFrom factoextra get_pca_var
#' @importFrom glue glue_collapse
#' @importFrom glue glue
#' @importFrom reshape2 melt
#' @importFrom shinyalert shinyalert
#' @importFrom ggrepel geom_text_repel
#' @import shinyWidgets
#' @import ggplot2
#' @import DT
#' @import datamods

mod_easystats_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      tabsetPanel(
        tabPanel("Input tables dev.",
              box(title = "Input features dataset", status = "warning", solidHeader = TRUE, width=12,
                 fluidRow(
                      column(
                        width = 12,
                        actionButton(ns("launch_modal"), "Features table input module", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469")#,
                        # tags$b("Imported data:"),
                        # verbatimTextOutput(outputId = ns("name")),
                        # verbatimTextOutput(outputId = ns("data"))
                      )
                    ),
                      tags$h3("Use filters to subset on features:"),

                        fluidRow(
                          column(
                            width = 3,
                            filter_data_ui(ns("filtering"), max_height = "500px")
                          ),
                          column(
                            width = 9,
                            progressBar(
                              id = ns("pbar"), value = 100,
                              total = 100, display_pct = TRUE
                            ),
                            DT::dataTableOutput(outputId = ns("table")),
                            # tags$b("Code dplyr:"),
                            # verbatimTextOutput(outputId = ns("code_dplyr")),
                            # tags$b("Expression:"),
                            # verbatimTextOutput(outputId = ns("code")),
                            # tags$b("Filtered data:"),
                            # verbatimTextOutput(outputId = ns("res_str"))
                            tags$b("Outliers:"),
                            verbatimTextOutput(outputId = ns("outliers"))
                          )
                        )
                      ),
                  box(title = "Input metadata dataset", status = "warning", solidHeader = TRUE, width=12,
                      actionButton(ns("launch_modal2"), "Metadata input module", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469"),
                      tags$h3("Use filters to subset on metadata, and click on rows you need to remove:"),
                      column(
                        width = 3,
                        filter_data_ui(ns("filtering2"), max_height = "500px")
                      ),
                      column(
                        width = 9,
                        progressBar(
                          id = ns("pbar2"), value = 100,
                          total = 100, display_pct = TRUE
                        ),
                        DT::dataTableOutput(outputId = ns("table2"))
                      ),                      
                      tags$b("Outlier(s) selected:"),
                      verbatimTextOutput(ns('x4'))
                    ),

                    box(title = "Normalization", status = "warning", solidHeader = TRUE, width = 3,
                        # verbatimTextOutput(ns('x4bis')),
                        selectInput(
                          ns("norm1fact1"),
                          label = "Numeric factor/covariable to weight features values with:",
                          choices = ""
                        ),
                        radioButtons(
                          ns("norm_method"),
                          label = "Normalization : ",
                          inline = TRUE,
                          choices = list(
                            "Raw" = 0 ,
                            "TSS (total-sum normalization)" = 1,
                            "CLR (center log-ration)" = 2
                          ), selected = "Raw"
                        ),
                        actionButton(ns("mergebutton"), "Merge features and metadata...", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469")
                      ),


                    box(title = "Final dataset", status = "primary", solidHeader = TRUE, width = 9,
                      DT::dataTableOutput(outputId = ns("mergetable_DT"))
                      )

                ),

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
                box(width=12,
                    title = 'Dataset filters', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                    DT::dataTableOutput(ns("metabo_datatable")),
                    shinyBS::bsButton(inputId = ns('update_features'), label = "Update features filter", block = F, style = 'danger', type='action')
                )
              ),
              fluidRow(
                box(
                  title = 'Normalization options', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  selectInput(
                    ns("norm1fact1"),
                    label = "Numeric factor/covariable to weight features values with:",
                    choices = ""
                  ),
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
                h4(icon("info-circle"), "Click on rows to filter some samples 'outliers'."),
                DT::dataTableOutput(ns("mergedf_DT")),
                shinyBS::bsButton(inputId = ns('update_samples'), label = "Update samples filter", block = F, style = 'danger', type='action'),
                downloadButton(outputId = ns("mergedf_download"), label = "Download merged table")
              )
            )
        ),
        tabPanel("PCA",
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
        ),
        tabPanel("Boxplots",
                 fluidRow(
                   box(title = "Plot Settings:", width = 7, status = "warning", solidHeader = TRUE,
                       pickerInput(
                         ns("fact3"),
                         label = "Factor to plot with in boxplot:",
                         choices = "",
                         multiple = TRUE
                       ),
                       selectInput(
                         ns("feat1"),
                         label = "Feature to plot in boxplot:",
                         choices = ""
                       ),
                       selectInput(
                         ns("nbPicPage"),
                         label = "Select number of plot per pdf page (max 4 per page):",
                         choices = c(1:4), selected = 1
                       ),
                       materialSwitch(ns("plotall"), label = "Plot all conditions (even NAs)", value = TRUE, status = "primary"),
                       actionButton(ns("go4"), "Just plot", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469"),
                       actionButton(ns("go3"), "Run plot/stats & tests", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469"),
                       downloadButton(outputId = ns("boxplots_download"), label = "Download all plots (long process)")
                   )
                 ),
                 # fluidRow(
                 #   box(title = "Boxplot:", width = 12, status = "warning", solidHeader = TRUE,
                 #       plotOutput(ns("boxplot_out"), height = "500")
                 #   )
                 # ),
                 fluidRow(
                   box(width = 12, 
                       title = 'Boxplot:', status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                       plotlyOutput(ns("boxplotly1"), height = "500")
                   )
                 ),
                 fluidRow(box(width = 12, 
                              title = 'Boxplot sumary stats:', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                              DT::dataTableOutput(ns("summaryBP")),
                              downloadButton(outputId = ns("summaryBP_download"), label = "Download table")
                 )),
                 fluidRow(box(width = 12, 
                              title = 'Pairwise Wilcox tests:', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                              DT::dataTableOutput(ns("wilcoxBP")),
                              downloadButton(outputId = ns("wilcoxBP_download"), label = "Download table")
                              
                 ))
        )
      )
    )
  )
}
    
#' Inputs Server Functions
#'
#' @noRd 
mod_easystats_server <- function(id, r = r, session = session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r_values <- reactiveValues(ds1=NULL, mt1=NULL)
    imported <- NULL


    # Input dataset dev 

    observeEvent(input$launch_modal, {
      import_modal(
        id = ns("myid"),
        from = c("file", "env", "copypaste", "googlesheets", "url"),
        title = "Import data to be used in application"
      )
    })

    imported <- import_server("myid", return_class = "data.frame")

    # output$name <- renderPrint({
    #   req(imported$name())
    #   imported$name()
    # })

    # output$data <- renderPrint({
    #   req(imported$data())
    #   as.tibble(imported$data())
    # })


    # Filters dev


      data <- reactive({
        imported$data()
        # get("iris")  #get(input$dataset)
      })

      # output$datainput <- renderPrint({
      #   # imported$data()[1:10,1:10]
      #   data()[1:10,]
      # })

      res_filter <- filter_data_server(
        id = "filtering",
        data = data,
        name = reactive("feature_table"),
        vars = reactive(NULL),
        widget_num = "slider",
        widget_date = "slider",
        label_na = "Missing"
      )

      observeEvent(res_filter$filtered(), {
        updateProgressBar(
          session = session, id = "pbar",
          value = nrow(res_filter$filtered()), total = nrow(data())
        )
      })

      output$table <- DT::renderDT({
        res_filter$filtered()
      }, options = list(pageLength = 6, scrollX = TRUE))


      output$code_dplyr <- renderPrint({
        res_filter$code()
      })
      output$code <- renderPrint({
        res_filter$expr()
      })

      output$res_str <- renderPrint({
        str(res_filter$filtered())
      })


    # Input metadata dev 

    observeEvent(input$launch_modal2, {
      import_modal(
        id = ns("myid2"),
        from = c("file", "env", "copypaste", "googlesheets", "url"),
        title = "Import data to be used in application"
      )
    })

    imported2 <- import_server("myid2", return_class = "data.frame")

    # output$name <- renderPrint({
    #   req(imported$name())
    #   imported$name()
    # })

    # output$data <- renderPrint({
    #   req(imported$data())
    #   as.tibble(imported$data())
    # })


    # Filters metadata dev


      data2 <- reactive({
        imported2$data()
        # get("iris")  #get(input$dataset)
      })

      # output$datainput <- renderPrint({
      #   # imported$data()[1:10,1:10]
      #   data()[1:10,]
      # })

      res_filter2 <- filter_data_server(
        id = "filtering2",
        data = data2,
        name = reactive("metadata_table"),
        vars = reactive(NULL),
        widget_num = "slider",
        widget_date = "slider",
        label_na = "Missing"
      )

      observeEvent(res_filter2$filtered(), {
        updateProgressBar(
          session = session, id = "pbar2",
          value = nrow(res_filter2$filtered()), total = nrow(data2())
        )
      })


        # Function for table filters
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

      output$table2 <- DT::renderDT({
        print(class(res_filter2$filtered()))
        print(str(res_filter2$filtered()))
        res_filter2$filtered()
      }, 
      options = list(
        pageLength = 6, scrollX = TRUE, server=TRUE, autoWidth = FALSE)#, , rowCallback = DT::JS(rowCallback)
      # extensions = "Select", selection = "multiple"
      )

      output$x4bis <- output$x4 <- renderPrint({
        s = input$table2_rows_selected
        if (length(s)) {
          cat('These rows were selected:\n')
          cat(s, sep = ', ')
        }else{
          cat("None")
        }
      })

      outliers <- reactive({
        r_values$outliers <- input[["table2_rows_selected"]]
        print("reactive outliers")
        print(r_values$outliers)
        r_values$outliers
      })

      observe({
        print(input[["table2_rows_selected"]])
      })

      # output$outliers <- renderPrint({
      #   outliers()
      # })

      observe({
        req(res_filter2$filtered()) #metadata
        metadata1 <- res_filter2$filtered()
        #Norm1
        class1 <- sapply(metadata1, class)
        r_values$norm1fact = names(metadata1)[class1 %in% "integer" | class1 %in% "numeric"]
        updateSelectInput(session, "norm1fact1",
                          choices = c("Raw", r_values$norm1fact),
                          selected = names(r_values$metadata_final)[1])
      })


      mergetable <- eventReactive(input$mergebutton, {
        metadata1 <- res_filter2$filtered()
        row.names(metadata1) <- metadata1[,"sample.id"]
        feat1 <- res_filter$filtered()

        print("Outliers:")
        outliers1 <- input[["table2_rows_selected"]]
        samplenames_out <- metadata1[input[["table2_rows_selected"]], "sample.id"]
        print(outliers1)
        print(samplenames_out)

        mt1 <- metadata1 %>% filter(!row_number() %in% outliers1)
        print(mt1$sample.id)
        ds0 <- feat1 %>% select(-samplenames_out)
        print(colnames(ds0))



        row.names(ds0) <- glue::glue("{ds0[,1]}__{ds0[,2]}__{ds0[,3]}")


        cat(file=stderr(), 'PONDERATION', "\n")
        
        class1 <- sapply(ds0, class)
        ds1 <- ds0[,class1 == "numeric" | class1 == "integer"]
        # print(colnames(ds1))
        r_values$wgt1 <- input$norm1fact1
        # print(prev(ds1))
        
        if(input$norm1fact1 == "Raw"){
          pondds1 <- ds1
        }else{
          fp1 = metadata1[colnames(ds1),input$norm1fact1]  # force same order between table
          fp1[fp1 == 0] <- NA
          pondds1 <- t(apply(ds1, 1, function(x){x/fp1}))
        }
        
        print(prev(pondds1))
        # r_values$pondds1 <- pondds1
        
        
        cat(file=stderr(), 'NORMALIZATION', "\n")
        ds1 <- pondds1
        # print(head(ds1))
        norm_names = c("Raw", "TSS", "CLR")
        r_values$norm1 <- norm_names[as.numeric(input$norm_method)+1]
        print(r_values$norm1)

        if(input$norm_method == 0){
          normds1 <- ds1
        }
        
        if(input$norm_method == 1){
          normf = function(x){ x/sum(x, na.rm = TRUE) }
          # normds1 <- transform_sample_counts(ds1, normf)
          normds1 <- apply(ds1, 2, normf)
        }
        
        if(input$norm_method == 2){
          clr = function(x){log(x+1) - rowMeans(log(x+1), na.rm = TRUE)}
          normds1 <- clr(ds1)
        }
        # save(list = ls(all.names = TRUE), file = "debug.rdata", envir = environment()); print("SAVE0")

        print("Final data")

        
        r_values$subsetds_final <- Fdataset <- as.data.frame(t(normds1)) %>% 
        tibble::rownames_to_column(var = "sample.id") %>% 
        dplyr::right_join(x = mt1, by = "sample.id")  # %>% mutate_if(is.character,as.factor)

        # melt final dataset for boxplot
        r_values$subsetds_final_melt <- reshape2::melt(Fdataset, id.vars = 1:ncol(mt1), measure.vars = (ncol(mt1)+1):ncol(Fdataset), variable.name = "features")
        

        #for PCA
        r_values$metadata_final <- droplevels(Fdataset[,1:ncol(mt1)])
        print(prev(r_values$metadata_final))
        r_values$features_final <- Fdataset[,(ncol(mt1)+1):ncol(Fdataset)]
        print(prev(r_values$features_final))

        Fdataset

      })


      output$mergetable_DT <- DT::renderDataTable({
        mergetable()
      }, 
      options = list(
        pageLength = 6, scrollX = TRUE,server=TRUE, autoWidth = TRUE)#, #, rowCallback = DT::JS(rowCallback), 
      #extensions = "Select", selection = "multiple"
      )



      # output$code_dplyr <- renderPrint({
      #   res_filter2$code()
      # })
      # output$code <- renderPrint({
      #   res_filter2$expr()
      # })

      # output$res_str <- renderPrint({
      #   str(res_filter2$filtered())
      # })



    # Merge DEV


      


    
    # Input Dataset
    dataset1 <- reactive({
      cat(file=stderr(), 'dataset1 fun', "\n")

      if (!is.null(input$dataset1)){
        # options(encoding = "UTF-8")
        # options(digits = 4, scipen = -2)
        ds1 <- read.csv(input$dataset1$datapath, sep = "\t", dec = ".", header = TRUE, stringsAsFactors = TRUE)
        ds1 <- replace_mu(ds1)
        row.names(ds1) <- glue::glue("{ds1[,1]}__{ds1[,2]}__{ds1[,3]}")
        r_values$ds1 <- ds1
        # print(unique(ds1[,3]))
      }
      # else{
      #   print("no data")
      # }
      r_values$ds1
    })
    
    # Input Metadata
    metadata1 <- reactive({
      cat(file=stderr(), 'metadata1 fun', "\n")
      if (!is.null(input$metadata1)){
        mt0 <- read.csv(input$metadata1$datapath, sep = "\t", dec = ".", header = TRUE, stringsAsFactors = TRUE)
        r_values$mt1 <- replace_mu(mt0)
      }else{
        cat(file=stderr(), 'metadata1 is null', "\n")
        r_values$mt1 = NULL
      }
      r_values$mt1
    })
    
    # Preview
    output$prevds1 <- renderPrint({
      cat(file = stderr(), 'rendering ds1', "\n")
      cat('Running graphstatsr v1.3.2\n')
      cat(glue::glue("Features table with {nrow(dataset1())} rows and {ncol(dataset1())} columns.\n\n"))
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
      cat(glue::glue("\nMetadata table with {nrow(metadata1())} rows and {ncol(metadata1())} columns.\n\n"))
      
      if (is.null(metadata1())) {
        print("no data")
      } else if (ncol(metadata1()) > 6) {
        head(metadata1()[, 1:6])
      } else{
        head(metadata1()[, 1:ncol(metadata1())])
      }


    })
    
    # Datatable with reactive filters & button
    output$metabo_datatable <- DT::renderDataTable({
      req(dataset1())
      cat(file=stderr(), 'Dataset datatable', "\n")
      dataset1() %>% mutate(across(where(is.numeric), round, 3))
    }, filter="top",
    options = list(pageLength = 5, scrollX = TRUE, rowCallback = DT::JS(rowCallback), 
    columnDefs = list(list(targets = 3, width = '200px'), list(targets = 2, width = '200px'), list(targets = 1, width = '100px')), autoWidth = TRUE, server=TRUE))
    
    subset_metabo <- reactive({
      req(dataset1())
      cat(file=stderr(), 'subset features...', "\n")
      cat(file=stderr(), 'number of features before',nrow(dataset1()), "\n")
      Fdataset <- dataset1()[input$metabo_datatable_rows_all,]
      cat(file=stderr(), 'number of features after',nrow(Fdataset), "\n")
      r_values$subsetds1 <- Fdataset
      
    })
    
    observeEvent(input$update_features, {
      cat(file=stderr(), 'button update_features', "\n")
      subset_metabo()
      print(nrow(subset_metabo()))
    },
    ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    # Normalization & button
    normds1 <- eventReactive(input$norm,{

      cat(file=stderr(), 'PONDERATION', "\n")
      req(r_values$subsetds1, input$norm1fact1, metadata1(), input$norm_method)
      
      ds0 <- r_values$subsetds1
      class1 <- sapply(ds0, class)
      ds1 <- ds0[,class1 == "numeric" | class1 == "integer"]
      r_values$wgt1 <- input$norm1fact1
      print(prev(ds1))
      
      if(input$norm1fact1 == "Raw"){
        pondds1 <- ds1
      }else{
        fp1 = metadata1()[[input$norm1fact1]]
        fp1[fp1 == 0] <- NA
        pondds1 <- t(apply(ds1, 1, function(x){x/fp1}))
      }
      
      print(prev(pondds1))
      # r_values$pondds1 <- pondds1
      
      
      cat(file=stderr(), 'NORMALIZATION', "\n")
      ds1 <- pondds1
      # print(head(ds1))
      norm_names = c("Raw", "TSS", "CLR")
      r_values$norm1 <- norm_names[as.numeric(input$norm_method)+1]
      print(r_values$norm1)

      if(input$norm_method == 0){
        normds1 <- ds1
      }
      
      if(input$norm_method == 1){
        normf = function(x){ x/sum(x, na.rm = TRUE) }
        # normds1 <- transform_sample_counts(ds1, normf)
        normds1 <- apply(ds1, 2, normf)
      }
      
      if(input$norm_method == 2){
        clr = function(x){log(x+1) - rowMeans(log(x+1), na.rm = TRUE)}
        normds1 <- clr(ds1)
      }
      
      print(prev(normds1))
      
      r_values$normds1 <- normds1

    })
    
    observeEvent(input$norm, {
      cat(file=stderr(), 'button normalize', "\n")
      # pondds1()
      normds1()
    },ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    
    # Merged table 
    mergedf <- reactive({
      req(r_values$normds1, r_values$mt1)
      r_values$tabF = as.data.frame(t(normds1())) %>% 
        tibble::rownames_to_column(var = "sample.id") %>% 
        dplyr::right_join(x = metadata1(), by = "sample.id") %>% mutate_if(is.character,as.factor)
    })
    
    output$mergedf_download <- downloadHandler(
      filename = "merged_table.csv",
      content = function(file) {
        req(r_values$subsetds_final)
        write.table(r_values$subsetds_final, file, sep="\t", row.names=FALSE)
      }
    )
    
  # Function for table filters
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
    
    
    # # function for selecting row
    # callback <- c(
    #   "var id = $(table.table().node()).closest('.datatables').attr('id');",
    #   "table.on('click', 'tbody', function(){",
    #   "  setTimeout(function(){",
    #   "    var indexes = table.rows({selected:true}).indexes();",
    #   "    var indices = Array(indexes.length);",
    #   "    for(var i = 0; i < indices.length; ++i){",
    #   "      indices[i] = indexes[i];",
    #   "    }",
    #   "    Shiny.setInputValue(id + '_rows_selected', indices);",
    #   "  }, 0);",
    #   "});"
    # )
    
    
    # Merged datatable for filtering.
    output$mergedf_DT <- DT::renderDataTable({
      mergedf() %>% mutate(across(where(is.numeric), round, 3))
    }, filter="top",
    options = list(
      columnDefs = list(list(targets = 1, width = '150px'), list(targets = 2, width = '150px'),
                        list(targets = 3, width = '150px'),list(targets = 4, width = '150px'),
                        list(targets = 5, width = '150px')),
      pageLength = 5, scrollX = TRUE, rowCallback = DT::JS(rowCallback), server=TRUE, autoWidth = TRUE),
    extensions = "Select", selection = "multiple"#, callback = JS(callback)
    )
    
    
    outliers <- reactive({
      r_values$outliers <- input[["mergedf_DT_rows_selected"]]
      print("reactive outliers")
      print(r_values$outliers)
      r_values$outliers
    })

    subset_merged <- reactive({
      
      req(mergedf())
      cat(file=stderr(), 'subset samples ... ', "\n")
      cat(file=stderr(), 'number of samples before',nrow(mergedf()), "\n")
      Fdataset <- mergedf()[input$mergedf_DT_rows_all,]
      cat(file=stderr(), 'number of samples after',nrow(Fdataset), "\n")
      row.names(Fdataset) <- Fdataset[,1] # sample.id

      if( !is.null(outliers()) ){
        print("OUTLIERS SELECTED")
        print(nrow(Fdataset))
        Fdataset <- Fdataset[-outliers(), ]
        print(nrow(Fdataset))
        showNotification(glue::glue("{length(outliers())} outlier(s) filtered..."), type="message", duration = 5)
      }

      r_values$subsetds_final <- Fdataset
      
      # melt final dataset for boxplot
      r_values$subsetds_final_melt <- reshape2::melt(Fdataset, id.vars = 1:ncol(metadata1()), measure.vars = (ncol(metadata1())+1):ncol(Fdataset), variable.name = "features")
      
      #for PCA
      r_values$metadata_final <- droplevels(Fdataset[,1:ncol(metadata1())])
      # print(head(r_values$metadata_final))
      r_values$features_final <- Fdataset[,(ncol(metadata1())+1):ncol(Fdataset)]
      # print(head(r_values$features_final))
    })
    
    observeEvent(input$update_samples, {
      cat(file=stderr(), 'button update_samples', "\n")
        subset_merged()
      
      # print(nrow(subset_merged()))
      # print(str(subset_merged()))
      print(str(r_values$metadata_final))
      showNotification("Dataset ready !", type="message", duration = 5)
    },
    ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    ### ACP tab
  
    # Settings   
    observe({
      req(res_filter2$filtered())  # metadata1()
      
      #Norm1
      class1 <- sapply(res_filter2$filtered(), class)
      r_values$norm1fact = names(res_filter2$filtered())[class1 %in% "integer" | class1 %in% "numeric"]
      updateSelectInput(session, "norm1fact1",
                        choices = c("Raw", r_values$norm1fact),
                        selected = names(r_values$metadata_final)[1])
      #ACP
      updateSelectInput(session, "fact1",
                        choices = names(r_values$metadata_final),
                        selected = names(r_values$metadata_final)[1])
      updateSelectInput(session, "pc1",
                        choices = colnames(acp1()$x)[1:10],
                        selected = colnames(acp1()$x)[1])
      updateSelectInput(session, "pc2",
                        choices = colnames(acp1()$x)[1:10],
                        selected = colnames(acp1()$x)[2])
    })
    
    ### ACP 
    observeEvent({input$go1
                 input$go2}, {
      if(!isTruthy(r_values$features_final)){
        cat(file=stderr(), 'ACP1 no table... ', "\n")
        shinyalert(title = "Oops", text="Final table not available, check all steps.", type='error')
      }
                   
    })
    
    acp1 <- eventReactive(input$go2, {
      cat(file=stderr(), 'ACP1 ... ', "\n")
      req(r_values$features_final)  # r_values$metadata_final # r_values$features_final , r_values$mt1
      
      # print(head(normds1()))
      # print(str(normds1()))
      if(input$naomit_method == 0){
      acp_input <- na.omit(r_values$features_final)
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
      req(acp1()$x)
      cat(file=stderr(), 'ACP tab ... ', "\n")
      acptab= as.data.frame(acp1()$x) %>% tibble::rownames_to_column(var = "sample.id") %>% 
        dplyr::inner_join(x = r_values$metadata_final, by = "sample.id")
      acptab

    })
    
    output$prevacp1 <- DT::renderDataTable({
      cat(file=stderr(), 'ACP table', "\n")
      acptab()
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, rowCallback = DT::JS(rowCallback)), server=TRUE) 
    
    output$acpind_download <- downloadHandler(
      filename = "acpind_table.csv",
      content = function(file) {
        req(acptab())
        write.table(acptab(), file, sep="\t", row.names=FALSE)
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
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, rowCallback = DT::JS(rowCallback)), server=TRUE) 
    
    output$acpvar_download <- downloadHandler(
      filename = "acpvar_table.csv",
      content = function(file) {
        req(acptabvar())
        write.table(acptabvar(), file, sep="\t", row.names=FALSE)
      }
    )
    
    # Acp PLOT
    acpplot <- eventReactive(input$go1, {
      req(input$fact1, acptab(), input$pc1, input$pc2)
    # acpplot <- reactive({
      cat(file=stderr(), 'ACP plot', "\n")
      showNotification("Processing visualization...", type="message", duration = 2)
      print(input$fact1)
      
      pc1 = as.numeric(substring(input$pc1, 3, 10))
      pc2 = as.numeric(substring(input$pc2, 3, 10))
      
      p = ggplot(acptab(), aes_string(x = input$pc1, y =
                                        input$pc2, color = input$fact1, sampleID = "sample.id")) + 
        geom_point() + stat_ellipse(aes_string(x = input$pc1, y = input$pc2, color = input$fact1), inherit.aes = FALSE) + theme_bw() + 
        xlab(glue::glue("{input$pc1} ({round(r_values$summary_acp$importance[2,pc1]*100,1)}%)")) + ylab(glue::glue("{input$pc2} ({round(r_values$summary_acp$importance[2,pc2]*100,1)}%)"))
      
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
    
    ###BOXPLOT
    
    observeEvent(input$go3, {
      if(!isTruthy(r_values$features_final)){
        cat(file=stderr(), 'Boxplot no table... ', "\n")
        shinyalert(title = "Oops", text="Final table not available, check all steps.", type='error')
      }
    })
    
    # Settings   
    observe({
      req(metadata1(), r_values$subsetds_final_melt)
      updateSelectInput(session, "feat1",
                        choices = unique(r_values$subsetds_final_melt[,"features"]),
                        selected = unique(r_values$subsetds_final_melt[,"features"])[1])
      updateSelectInput(session, "fact2",
                        choices = names(r_values$metadata_final),
                        selected = names(r_values$metadata_final)[2])
      updatePickerInput(session, "fact3",
                        choices = names(r_values$metadata_final),
                        selected = names(r_values$metadata_final)[2],
                        options = list(
                          `actions-box` = TRUE, 
                          size = 10,
                          `selected-text-format` = "count > 3"
                        )
      )
    })

    
    
    boxplot1 <- eventReactive(c(input$go3, input$go4), {
      cat(file=stderr(), 'BOXPLOT', "\n")
      req(r_values$subsetds_final_melt, input$fact3)
      r_values$tabF_melt2 <- tabF_melt2 <- tabF_melt <- r_values$subsetds_final_melt
      if(length(input$fact3) == 1){r_values$fact3ok <- fact3ok <- input$fact3
        }else{
          comb = glue::glue_collapse(input$fact3, sep = ', \"_\",')
          fun = glue::glue('tabF_melt2 <- tabF_melt %>% dplyr::mutate(newfact = paste0({comb}), .after= "sample.id")')
          eval(parse(text=fun))
          r_values$fact3ok <- fact3ok <- "newfact"
          r_values$tabF_melt2 <- tabF_melt2
        }
      print(head(r_values$tabF_melt2))
      print(r_values$fact3ok)
      
      ytitle <- glue::glue("{as.character(dataset1()[input$feat1,3])}")
      if(r_values$wgt1 != "Raw"){
        ytitle <- glue::glue("{ytitle}, weight: {r_values$wgt1}")
      }
      if(r_values$norm1 != "Raw"){
        ytitle <- glue::glue("{ytitle}, norm.: {r_values$norm1}")
      }
      
      fun <-  glue::glue('tabfeat = tabF_melt2[tabF_melt2$features == input$feat1,] %>% 
        group_by({fact3ok}) %>% 
        mutate(outlier=ifelse(is_outlier(value), as.character(sample.id), NA))')
      eval(parse(text=fun))

      if(!input$plotall){
        tabfeat <- tabfeat %>% filter(!is.na(value))
      }

     fun <-  glue::glue('p <- ggplot(tabfeat, aes(x = {fact3ok}, y = value)) + 
        geom_boxplot(fill = "#99AFE3") + theme_bw() + xlab("Condition") + ylab(ytitle) + ggtitle(input$feat1) +
        theme(legend.position = "None", axis.text.x = element_text(angle = 45, hjust=1))')
      eval(parse(text=fun))
      ggly <- ggplotly(p)
      
      # # Hoverinfo BUG
      # tabfeat$sample.id <- as.character(tabfeat$sample.id)
      # hoverinfo <- with(tabfeat, paste0("sample: ", sample.id, "</br></br>", 
      #                                 "value: ", value))
      # ggly$x$data[[1]]$text <- hoverinfo
      # ggly$x$data[[1]]$hoverinfo <- c("text", "boxes")
      
      
      cat(file=stderr(), 'BOXPLOT done', "\n")
      
      outlist = list()
      outlist$p <- p
      outlist$tabF_melt2 <- tabF_melt2
      outlist$fact3ok <- fact3ok
      outlist$ggly <- ggly

      outlist
    })
    
    # output$boxplot_out <- renderPlot({
    #   req(boxplot1())
    #   bp1 <- boxplot1()
    #   
    #   bp1$p
    # })
    
    output$boxplotly1 <- renderPlotly({
      req(boxplot1())
      bp1 <- boxplot1()
      ggplotly(bp1$ggly)
    })
    
    # Export all figures
    
    pdfall <- reactive({
      cat(file=stderr(), 'ALL BOXPLOT', "\n")
      req(r_values$tabF_melt2, r_values$fact3ok)

        fact3ok <- r_values$fact3ok
        tabF_melt2 <- r_values$tabF_melt2
        tabF_melt2$sample.id <- as.character(tabF_melt2$sample.id)
        listP <- list()
        FEAT = levels(tabF_melt2$features)
        print(head(FEAT))

        for(i in 1:length(FEAT)){
          
          tt <- stringr::str_split(FEAT[i], "__")
          print(tt)
          ytitle <- sapply(tt,"[[",2)
          print(ytitle)
          if(r_values$wgt1 != "Raw"){
            ytitle <- glue::glue("{ytitle}, weight: {r_values$wgt1}")
          }
          if(r_values$norm1 != "Raw"){
            ytitle <- glue::glue("{ytitle}, norm.: {r_values$norm1}")
          }
          
          fun <-  glue::glue('tabfeat = tabF_melt2[tabF_melt2$features == FEAT[i],] %>% 
                  group_by({fact3ok}) %>% 
                  mutate(outlier=ifelse(is_outlier(value), sample.id, NA))')
          eval(parse(text=fun))

           if(!input$plotall){
              tabfeat <- tabfeat %>% filter(!is.na(value))
            }

          if(nrow(tabfeat) == 0){print("no data"); next}
          
          fun <-  glue::glue('listP[[FEAT[i]]] <- ggplot(tabfeat, aes(x = {fact3ok}, y = value)) + 
        geom_boxplot(fill = "#99AFE3") + theme_bw() + xlab("Condition") + ylab(ytitle) + ggtitle(FEAT[i]) +
        theme(legend.position = "None", axis.text.x = element_text(angle = 45, hjust=1)) + 
        ggrepel::geom_text_repel(aes(label = outlier), na.rm = TRUE, show.legend = F, 
                             direction = "both",
                             nudge_x = 0.1,
                             size= 3
                             )')
          eval(parse(text=fun))
          
          print(length(listP))
        }
      
      print(length(listP))
      
      listP
    })
    
    output$boxplots_download <- downloadHandler(
      filename = "figures.pdf",
      content = function(file) {
        print('DOWNLOAD ALL')
        req(pdfall())
        p <- pdfall()
        print('pdf output')
        
        withProgress({
          if(as.numeric(input$nbPicPage) < 4){
            ml <- marrangeGrob(p, nrow= 1, ncol=as.numeric(input$nbPicPage))
          }else{
            ml <- marrangeGrob(p, nrow=2, ncol=2)
          }
          
          ggsave(file, ml, units = "cm", width = 20, height = 15, dpi = 300)
        }, message = "Prepare pdf file... please wait.")

        
      }
    )
    
    
    
    summaryBP <- eventReactive(input$go3, {
      cat(file=stderr(), 'BOXPLOT summary', "\n")
      req(boxplot1())
      
      
      
      q = c(.25, .5, .75)
      boxstat <- data.frame()
      #calculate quantiles by grouping variable
      Amelt <- boxplot1()$tabF_melt2
      print(head(Amelt))
      for(i in unique(Amelt$features)){
        boxstat1 <- Amelt[Amelt$features == i,] %>%
          filter(!is.na(value)) %>%
          group_by(.dots = boxplot1()$fact3ok) %>%
          summarize(min = min(value),
                    quant25 = quantile(value, probs = q[1]),
                    median = quantile(value, probs = q[2]),
                    quant75 = quantile(value, probs = q[3]),
                    max = max(value),
                    mean = mean(value),
                    sd = sd(value)) %>% 
          add_column(Features = i, .after = 0) %>% mutate_if(is.character,as.factor)
        
        boxstat <- rbind(boxstat, boxstat1)
      }
      cat(file=stderr(), 'BOXPLOT summary done', "\n")
      print(head(boxstat))
      
      as.data.frame(boxstat)
    })
    
    output$summaryBP <- DT::renderDataTable({
      cat(file=stderr(), 'SummaryBP DT', "\n")
      summaryBP()
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, rowCallback = DT::JS(rowCallback)), server=TRUE) 
    
    output$summaryBP_download <- downloadHandler(
      filename = "summary-boxplot_table.csv",
      content = function(file) {
        req(summaryBP())
        write.table(summaryBP(), file, sep="\t", row.names=FALSE)
      }
    )
 
    #wilcoxBP
    wilcoxBP <- eventReactive(input$go3, {
      cat(file=stderr(), 'wilcoxBP table', "\n")
      req(boxplot1())
      
      Amelt <- boxplot1()$tabF_melt2
      
      pval_table <- data.frame()
      for(feat1 in unique(Amelt$features)){
        Ftabtest = Amelt[Amelt$features == feat1,] %>%
          filter(!is.na(value)) 
        if(nrow(Ftabtest)==0){next}
        if(length(which(table(Ftabtest[Ftabtest$features == feat1,boxplot1()$fact3ok]) >= 3)) < 2){next} # si moins de 2 groupes avec au moins 3 repetitions next.
        print(feat1)
        print(table(Ftabtest[Ftabtest$features == feat1,boxplot1()$fact3ok]))
        wcoxtab = pairwise.wilcox.test(Ftabtest[Ftabtest$features == feat1,"value"], as.factor(Ftabtest[,boxplot1()$fact3ok]),
                                       p.adjust.method = "none")
        
        ftable1 <- as.data.frame(wcoxtab$p.value) %>%
          rownames_to_column() %>% pivot_longer(!rowname, names_to = "condition", values_to = "pvalue") %>%
          na.omit() %>% add_column(Features = feat1, .after = 0)
        
        pval_table <- rbind.data.frame(pval_table, ftable1)
      }
      colnames(pval_table) = c("Features", "Condition1", "Condition2", "pvalue")
      
      Fpvaltable <- pval_table %>% mutate(adjusted_pval = p.adjust(pvalue, method = "fdr")) %>% mutate_if(is.character,as.factor) 
      print(dim(Fpvaltable))
      cat(file=stderr(), 'wilcoxBP table done', "\n")
      
      Fpvaltable
    })
    
    output$wilcoxBP <- DT::renderDataTable({
      cat(file=stderr(), 'wilcoxBP DT', "\n")
      wilcoxBP()
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, rowCallback = DT::JS(rowCallback)), server=TRUE) 
    
    output$wilcoxBP_download <- downloadHandler(
      filename = "wilcoxtests_table.csv",
      content = function(file) {
        req(wilcoxBP())
        write.table(wilcoxBP(), file, sep="\t", row.names=FALSE)
      }
    )
      
  })
}
    
## To be copied in the UI
# mod_easystats_ui("Inputs_ui_1")
    
## To be copied in the server
# mod_easystats_server("Inputs_ui_1")
