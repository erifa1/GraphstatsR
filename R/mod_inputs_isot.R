#' inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList

mod_inputs_isot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(

      box(title = "Input features dataset from isocor", status = "warning", solidHeader = TRUE, width=12,
         fluidRow(
              column(
                width = 12,
                actionButton(ns("launch_modal"), "Features table input module", 
                  icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469")#,
                # downloadButton(ns("dl_ds_test"), "Data test")
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
                    DT::dataTableOutput(outputId = ns("table"))
                  )
                )
              ),
          box(title = "Input metadata dataset", status = "warning", solidHeader = TRUE, width=12,
                actionButton(ns("launch_modal2"), "Metadata input module", icon = icon("play-circle"), 
                  style="color: #fff; background-color: #3b9ef5; border-color: #1a4469"),
                # downloadButton(ns("dl_mt_test"), "MetaData test"),
                # uiOutput(ns("DLTemp")),
                # downloadButton(outputId = ns("metadatTemplate_download"), label = "Download metadata template"),
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

            box(title = "Final dataset", status = "primary", solidHeader = TRUE, width = 12,
              actionButton(ns("mergebutton"), "Merge features and metadata...", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469"),
              DT::dataTableOutput(outputId = ns("mergetable_DT")),
              downloadButton(outputId = ns("mergedf_download"), label = "Download merged table")
            )

    )
  )
}
    
#' inputs Server Functions
#'
#' @noRd 
mod_inputs_isot_server <- function(id, r = r, session = session){
  moduleServer( id, function(input, output, session){

    ns <- session$ns
    r_values <- reactiveValues(merged = NULL, imported = NULL, imported2 = NULL, 
      subsetds_final = "emptytable", metadata_final = NULL, 
      features_final = NULL, subsetds_final_melt = "emptytable")
    imported <- NULL


    # Input dataset dev 

    observeEvent(input$launch_modal, {
      print("inputMODAL1")
      r_values$subsetds_final <- "emptytable" # for shinyalert acp / boxplot
      r_values$subsetds_final_melt <- "emptytable"
      r_values$merged <- NULL

      import_modal(
        id = ns("myid"),
        from = c("file","copypaste", "googlesheets", "url"), #
        title = "Import data to be used in application",
        file_extensions = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")
      )
    })

    imported <- import_server("myid", return_class = "data.frame")

    output$myid <- renderPrint({
      req(input$myid)
      input$myid
    })


    # Filters 

      data <- reactive({
        r_values$imported <- imported$data()
        if(is.null(imported$data())){
          dat <- imported$data()
        }else{
          dat <- imported$data() %>% mutate_if(bit64::is.integer64,as.numeric)
        }

        dat
      })

      res_filter <- filter_data_server(
        id = "filtering",
        data = data  ,
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

      output$dl_ds_test <- downloadHandler(
        filename = glue::glue("datatest.csv"),
        content = function(file){
        print("DATATEST")

        dstest <- read.csv(system.file("dataset", "features_quanti_data.csv", package="graphstatsr"), sep = ",")
        write.csv(dstest, file, row.names=FALSE)
      },
        contentType = "application/tar"
      )


      output$dl_mt_test <- downloadHandler(
        filename = glue::glue("metadata_test.csv"),
        content = function(file){
        print("METADATATEST")

        mttest <- read.csv(system.file("dataset", "metadata_file.csv", package="graphstatsr"), sep = "\t")
        write.csv(mttest, file, row.names=FALSE)
      },
        contentType = "application/tar"
      )



      output$table <- DT::renderDT({
        print("renderDS")
            res_filter$filtered()

      }, options = list(pageLength = 6, scrollX = TRUE))


      # output$code_dplyr <- renderPrint({
      #   res_filter$code()
      # })
      # output$code <- renderPrint({
      #   res_filter$expr()
      # })

      # output$res_str <- renderPrint({
      #   str(res_filter$filtered())
      # })

    output$metadatTemplate_download <- downloadHandler(
      filename = "metadata_template.csv",
      content = function(file) {
        req(data())
        A <- data() #r_values$imported

        if(!is.null(A)){
          print("there is a DATASET")
          DF <- data.frame(row.names = names(A)[4:ncol(A)])
          DF$sample.id <- names(A)[4:ncol(A)]
          DF$factor_example <- glue::glue("group_{rep(LETTERS[1:3], each = 2, length.out=nrow(DF))}")
          write.csv(DF, file , row.names=FALSE)
        }else{
          print("no dataset")
          return(NULL)
        }

      }
    )

    output$DLTemp <- renderUI({
      # req(input$launch_modal)
      req(data())
        downloadButton(outputId = ns("metadatTemplate_download"), label = "Download metadata template")
    })


    # Input metadata dev 

    observeEvent(input$launch_modal2, {
      print("inputMODAL2")
      r_values$merged <- NULL

      import_modal(
        id = ns("myid2"),
        from = c("file", "copypaste", "googlesheets", "url"),
        title = "Import data to be used in application",
        file_extensions = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")
      )
    })

    imported2 <- import_server("myid2", return_class = "data.frame")


    # Filters metadata dev


      data2 <- reactive({
        r_values$imported2 <- imported2$data()
        imported2$data()
      })

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
        # print(class(res_filter2$filtered()))
        # print(str(res_filter2$filtered()))
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
        ds1 <- res_filter$filtered()
        #Norm1
        class1 <- sapply(metadata1, class)
        r_values$norm1fact = names(metadata1)[class1 %in% "integer" | class1 %in% "numeric"]
        r_values$mergefact = ds1 %>% select(where(is.character)) %>% names()

        updateSelectInput(session, "mergefact",
                          choices = c("Raw", r_values$mergefact),
                          selected = c("Raw", r_values$mergefact)[1])


        updateSelectInput(session, "norm1fact1",
                          choices = c("Raw", r_values$norm1fact),
                          selected = c("Raw", r_values$norm1fact)[1]) #names(r_values$metadata_final)[1]
      })



      mergetable <- eventReactive(input$mergebutton, {
        print("merge")
        if(is.null(r_values$imported) | is.null(r_values$imported2)){
          showNotification("Please use modules for input files...", type="message", duration = 5)
        }

        metadata1 <- res_filter2$filtered()

        if(length(unique(metadata1$sample)) != length(metadata1$sample)){
            print("non unique sample id")
            shinyalert(title = "Oops", text=glue::glue("Each sample ID needs to be unique."), type='error')
            return(data.frame())
        }

        row.names(metadata1) <- metadata1[,"sample"]
        feat1 <- res_filter$filtered()

        print("Outliers:")
        outliers1 <- input[["table2_rows_selected"]]
        samplenames_out <- metadata1[input[["table2_rows_selected"]], "sample"]
        print(outliers1)
        # print(samplenames_out)

        mt1 <- metadata1 %>% filter(!row_number() %in% outliers1)
        names(mt1) <- gsub(" ","_",names(mt1))
        r_values$mt1 <- mt1
        # print(mt1$sample)
        
        ds0 <- feat1 %>% filter(!sample %in% samplenames_out) #select(-samplenames_out)

          Calcul <- ds0 %>% mutate(Miso = as.factor(glue::glue("M{stringr::str_pad(ds0$isotopologue, 2, pad = '0')}"))) %>%
          mutate(Area_Iso = corrected_area * isotopologue) %>% group_by(sample, metabolite) %>% 
          mutate(mean_area_persample = mean(corrected_area)) %>% 
          # ungroup() %>% group_by(metabolite) %>% 
          mutate(maxIso = max(isotopologue)) %>%
          data.frame() #%>% head()

          Fdataset <- Calcul %>% 
          dplyr::left_join(x = mt1, by = "sample")  
          r_values$subsetds_final <- Fdataset

          showNotification("Dataset ready !", type="message", duration = 5)
          Fdataset

      })


      output$histo_plotly <- renderPlotly({
        req(mergetable())
        # req(input$go3)
        tab_plot <- mergetable() %>% filter(metabolite == "AMP")

        xform <- list()
        p1 <- plotly::plot_ly(tab_plot, x = ~sample, y = ~corrected_area, type = 'bar', 
                name = ~Miso, color = ~Miso, height = 700) %>% 
        plotly::layout(title="Raw area", yaxis = list(title = 'Raw area'), 
        barmode = 'stack', xaxis = xform)

        p1
      })



      output$mergetable_DT <- DT::renderDataTable({
        # req(mergetable())
        if(is.null(r_values$merged)){validate('\t\t\t\t\t\t\t\t\t\tValidate each step.')}
        # print("rendermergeDT")

        mergetable()
      }, 
      options = list(
        pageLength = 6, scrollX = TRUE,server=TRUE, autoWidth = TRUE)#, #, rowCallback = DT::JS(rowCallback), 
      #extensions = "Select", selection = "multiple"
      )

      output$mergedf_download <- downloadHandler(
        filename = "merged_table.csv",
        content = function(file) {
          req(r_values$subsetds_final)
          write.csv(r_values$subsetds_final, file, sep=",", row.names=FALSE)
        }
      )

      observe({
        r_values$merged <- mergetable()
      })


      r$merged2 <- reactive({
        req(mergetable())
        mergetable()
      })

      r$mt1_isoT <- reactive({
        req(r_values$mt1)
        r_values$mt1
      })

  })
}
    
## To be copied in the UI
# mod_inputs_ui("inputs_1")
    
## To be copied in the server
# mod_inputs_server("inputs_1")
