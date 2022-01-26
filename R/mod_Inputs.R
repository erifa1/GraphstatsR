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
#' @import shinyWidgets
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
                 ),
                 fluidRow(box(width = 12, 
                              title = 'Variables Coordinates:', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                              DT::dataTableOutput(ns("prevacp1var"))
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
                       actionButton(ns("go3"), "Run plot/stats & tests", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469")
                       ,
                       actionButton(ns("go4"), "Update Plot", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469")
                   )
                 ),
                 fluidRow(
                   box(title = "Plot Settings:", width = 12, status = "warning", solidHeader = TRUE,
                       plotOutput(ns("boxplot1"), height = "500")
                   )
                 ),
                 fluidRow(box(width = 12, 
                              title = 'Boxplot sumary stats:', status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                              DT::dataTableOutput(ns("summaryBP"))
                 ))
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
    
    # Input Dataset
    dataset1 <- reactive({
      cat(file=stderr(), 'dataset1 fun', "\n")
      if (!is.null(input$dataset1)){
        ds1 <- read.table(input$dataset1$datapath, sep = "\t", dec = ".", header = TRUE, stringsAsFactors = TRUE)
        row.names(ds1) <- glue::glue("{ds1[,1]}__{ds1[,2]}__{ds1[,3]}")
        r_values$ds1 <- ds1
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
        r_values$mt1 <- read.table(input$metadata1$datapath, sep = "\t", dec = ".", header = TRUE, stringsAsFactors = TRUE)
      }else{
        cat(file=stderr(), 'metadata1 is null', "\n")
        r_values$mt1 = NULL
      }
      r_values$mt1
    })
    
    # Preview
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
    
    # Datatable with reactive filters & button
    output$metabo_datatable <- DT::renderDataTable({
      req(dataset1())
      cat(file=stderr(), 'Dataset datatable', "\n")
      dataset1()
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, rowCallback = DT::JS(rowCallback)), server=TRUE) 
    
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
    
    
    
    # output$mergedf <- renderPrint({
    #   cat(file = stderr(), 'rendering mergedf', "\n")
    #   if (is.null(mergedf())) {
    #     print("no data")
    #   } else if (ncol(mergedf()) > 6) {
    #     head(mergedf()[, 1:6])
    #   } else{
    #     head(mergedf()[, 1:ncol(mergedf())])
    #   }
    #   
    # })
    
    # Norm 1  norm1fact1
    pondds1 <- eventReactive(input$norm,{
      cat(file=stderr(), 'PONDERATION', "\n")
      req(r_values$subsetds1, input$norm1fact1, metadata1())
      ds0 <- r_values$subsetds1
      class1 <- sapply(ds0, class)
      ds1 <- ds0[,class1 == "numeric"]
      r_values$wgt1 <- input$norm1fact1
      # print(head(ds1))
      
      if(input$norm1fact1 == "Raw"){
        pondds1 <- ds1
      }else{
        pondds1 <- t(apply(ds1, 1, function(x){x/metadata1()[[input$norm1fact1]]}))
      }
      
      # print(head(pondds1))
      r_values$pondds1 <- pondds1
    })
    
    
    
    # Normalization & button
    normds1 <- eventReactive(input$norm,{
      cat(file=stderr(), 'NORMALIZATION', "\n")
      req(r_values$pondds1, input$norm_method)
      # ds0 <- r_values$subsetds1
      # class1 <- sapply(ds0, class)
      # ds1 <- ds0[,class1 == "numeric"]
      
      ds1 <- r_values$pondds1
      # print(head(ds1))
      norm_names = c("Raw", "TSS", "CLR")
      r_values$norm1 <- norm_names[as.numeric(input$norm_method)+1]
      
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
      
      # print(head(normds1))
      r_values$normds1 <- normds1
    })
    
    observeEvent(input$norm, {
      cat(file=stderr(), 'button normalize', "\n")
      pondds1()
      normds1()
    },ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    
    # Merged table 
    mergedf <- reactive({
      req(r_values$normds1, r_values$mt1)
      r_values$tabF = as.data.frame(t(normds1())) %>% 
        tibble::rownames_to_column(var = "sample.id") %>% 
        dplyr::right_join(x = metadata1(), by = "sample.id")
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
    
    # Merged datatable for filtering.
    output$mergedf_DT <- DT::renderDataTable({
      mergedf()
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, rowCallback = DT::JS(rowCallback)), server=TRUE)
    
    subset_merged <- reactive({
      
      req(mergedf())
      cat(file=stderr(), 'subset samples ... ', "\n")
      cat(file=stderr(), 'number of samples before',nrow(mergedf()), "\n")
      Fdataset <- mergedf()[input$mergedf_DT_rows_all,]
      cat(file=stderr(), 'number of samples after',nrow(Fdataset), "\n")
      row.names(Fdataset) <- Fdataset[,1] # sample.id
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
      req(metadata1())
      
      #Norm1
      class1 <- sapply(metadata1(), class)
      r_values$norm1fact = names(metadata1())[class1 %in% "integer" | class1 %in% "numeric"]
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
      }
      
      if(input$naomit_method == 1){
        Tfeat =t(r_values$features_final)
        Tfeat_ok <- na.omit(Tfeat)
        acp_input <- t(Tfeat_ok)
        r_values$snaomit <- setdiff(row.names(Tfeat),row.names(Tfeat_ok))
        r_values$snaomit_att <- "feature(s)"
      }
      
      # Simplify features names
      tt <- stringr::str_split(colnames(acp_input), "__")
      tt1 <- sapply(tt,"[[",1)
      if(length(unique(tt1) ) == length(tt1)){
        colnames(acp_input) = tt1
        print(head(acp_input))
        acp1 = stats::prcomp(acp_input, scale. = TRUE)  #t(normds1()[,-1])
        r_values$acp1 <- acp1
        
        r_values$summary_acp <- summary(acp1)
        
        print(colnames(r_values$acp1$x))
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
      
      glue::glue("Following {r_values$snaomit_att} were omitted for PCA:\n{list1}")
      
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
    
    boxplot1 <- eventReactive({input$go3
      input$go4}, {
      cat(file=stderr(), 'BOXPLOT', "\n")
      req(r_values$subsetds_final_melt)
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
      
      p <- ggplot(tabF_melt2[tabF_melt2$features == input$feat1,], aes_string(x = fact3ok, y = "value", fill = fact3ok)) + 
        geom_boxplot() + theme_bw() + xlab("Condition") + ylab(ytitle) + ggtitle(input$feat1) + theme(legend.position = "None")
      cat(file=stderr(), 'BOXPLOT done', "\n")
      p
      
    })
    
    output$boxplot1 <- renderPlot({
      req(boxplot1())
      boxplot1()
    })
    
    summaryBP <- eventReactive(input$go3, {
      cat(file=stderr(), 'BOXPLOT summary', "\n")
      q = c(.25, .5, .75)
      boxstat <- data.frame()
      #calculate quantiles by grouping variable
      Amelt <- r_values$tabF_melt2
      print(head(Amelt))
      
      for(i in unique(Amelt$features)){
        boxstat1 <- na.omit(Amelt[Amelt$features == i,]) %>%
          group_by(.dots = r_values$fact3ok) %>% #TimePoint_Condition_Traitement
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
    
 
  })
}
    
## To be copied in the UI
# mod_Inputs_ui("Inputs_ui_1")
    
## To be copied in the server
# mod_Inputs_server("Inputs_ui_1")
