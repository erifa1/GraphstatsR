#' boxplots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom sortable rank_list bucket_list add_rank_list sortable_options
#' @importFrom ggstatsplot ggbetweenstats
#' @import PMCMRplus



labels <- list(
  "one",
  "two",
  "three",
  htmltools::tags$div(
    htmltools::em("Complex"), " html tag without a name"
  ),
  "five" = htmltools::tags$div(
    htmltools::em("Complex"), " html tag with name: 'five'"
  )
)

mod_boxplots_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(

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

            dropMenu(
              actionButton("go0", "More parameters..."),
              {fluidRow(
                
                column(
                  h3("General settings"),
                  textInput(ns("custom_ytitle"), "Custom y title", "None"),
                  materialSwitch(ns("ggplotstats1"), label = "Display ggstatsplot", value = TRUE, status = "primary"),
                  materialSwitch(ns("plotall"), label = "Plot all conditions (even NAs)", value = TRUE, status = "primary"),
                  materialSwitch(ns("grey_mode"), label = "Colored boxplot", value = TRUE, status = "primary"),
                  width = 6
                  ),
                column(
                  h3("PDF and PNGs output settings"),
                  selectInput( ns("nbPicPage"), label = "Select number of plot per pdf page (max 4 per page):", choices = c(1:4), selected = 1),
                  materialSwitch(ns("ggstatOUT"), label = "PDF with ggstat plots", value = FALSE, status = "primary"),
                  materialSwitch(ns("verticaldisplay"), label = "Vertical display in pdf or not (2 per page)", value = TRUE, status = "primary"),
                  sliderInput(ns("sizexlab"), label = "X labels size", min = 0, max = 1, value = 0.8, step = 0.05),
                  materialSwitch(ns("outlier_labs"), label = "Inform outlier in pdf output", value = TRUE, status = "primary"),
                  materialSwitch(ns("pngs_out"), label = "Output png for each feature (long process)", value = FALSE, status = "primary"),
                  textInput(ns("outpath"), "Output path for pngs", ""),
                  width = 6
                  )

                )

              },
                  theme = "light-border",
                  placement = "right"
              ),
            actionButton(ns("go3"), "Run plot/stats & tests", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469"),
            actionButton(ns("go4"), "Update plot only", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469"),
            downloadButton(outputId = ns("boxplots_download"), label = "Download pdf and pngs (long process)")
        ),
        box(title = "Reorder boxplots:", width = 5, status = "warning", solidHeader = TRUE, collapsible = TRUE,
            uiOutput(ns("sortable"))#,
            # verbatimTextOutput(ns("results_sort"))
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
      fluidRow(
        box(width = 12, 
            title = 'Boxplot with stats:', status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
            plotOutput(ns("ggplotstatsOUT1"), height = "500")
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
}
    
#' boxplots Server Functions
#'
#' @noRd 
mod_boxplots_server <- function(id, r = r, session = session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r_values <- reactiveValues(ggly = NULL)

    ###BOXPLOT
    
    observeEvent(r$tabs$tabselected, {
      if(r$tabs$tabselected=='boxplot-tab' && r$fdata_melt() == "emptytable"){ #r_values$features_final
        cat(file=stderr(), 'Boxplot no table... ', "\n")
        shinyalert(title = "Oops", text="Final table not available, check all steps.", type='error')
      }
    })
    
    # Settings   
    observe({
      # req(metadata1(), r_values$subsetds_final_melt)
      req(r$mt1(), r$fdata_melt())
      r_values$subsetds_final_melt <- r$fdata_melt()
      r_values$metadata_final <- r$mt1()
      if(is.data.frame(r$fdata_melt())){
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
        updateTextInput(
          session = session,
          "outpath",
          label = "Output path for pngs",
          value = getwd(),
          placeholder = NULL
        )

      }
    })

    
    boxtab <- eventReactive(c(input$go4, input$go3), {  #
      cat(file=stderr(), 'BOXTAB', "\n")
      req(r_values$subsetds_final_melt, input$fact3, r$ds1())
      r_values$tabF_melt2 <- tabF_melt2 <- tabF_melt <- r_values$subsetds_final_melt
      if(length(input$fact3) == 1){r_values$fact3ok <- fact3ok <- input$fact3
          fun = glue::glue('tabF_melt2 <- tabF_melt %>% dplyr::mutate(newfact = {input$fact3}, .after= "sample.id")')
          eval(parse(text=fun))
        }else{
          comb = glue::glue_collapse(input$fact3, sep = ', \"_\",')
          fun = glue::glue('tabF_melt2 <- tabF_melt %>% dplyr::mutate(newfact = paste0({comb}), .after= "sample.id")')
          eval(parse(text=fun))
          r_values$fact3ok <- fact3ok <- "newfact"
          r_values$tabF_melt2 <- tabF_melt2
        }
      # print(head(r_values$tabF_melt2))
      # print(r_values$fact3ok)
      
      
      fun <-  glue::glue('tabfeat = tabF_melt2[tabF_melt2$features == input$feat1,] %>% 
        group_by({fact3ok}) %>% 
        mutate(outlier=ifelse(is_outlier(value), as.character(sample.id), NA))')
      eval(parse(text=fun))

      if(!input$plotall){
        tabfeat <- tabfeat %>% filter(!is.na(value))
      }

      tabfeat
    })



    output$sortable <- renderUI({
      tabF_melt2 <- tabF_melt <- r_values$subsetds_final_melt

      if(length(input$fact3) == 1){r_values$fact3ok <- fact3ok <- input$fact3
          fun = glue::glue('tabF_melt2 <- tabF_melt %>% dplyr::mutate(newfact = {input$fact3}, .after= "sample.id")')
          eval(parse(text=fun))
        }else{
          comb = glue::glue_collapse(input$fact3, sep = ', \"_\",')
          fun = glue::glue('tabF_melt2 <- tabF_melt %>% dplyr::mutate(newfact = paste0({comb}), .after= "sample.id")')
          eval(parse(text=fun))
          fact3ok <- "newfact"
          tabF_melt2
        }

      print("SORTABLE UI")
      # print(str(tabF_melt2))
      # print(names(tabF_melt2))
      bucket_list("Drag condition names to change order (multiple selection allowed)",
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list("Plotted conditions",
          unique(tabF_melt2$newfact), ns("sorted1"),
          options = sortable_options(multiDrag = TRUE)
        ),
        add_rank_list("Stashed conditions",
          NULL, ns("stashed1"),
          options = sortable_options(multiDrag = TRUE)
        )
      )
    })


    output$results_sort <- renderPrint({
      input$sorted1 # This matches the input_id of the rank list
    })



    boxplot1 <- eventReactive(c(input$go4, input$go3), {  #
      outlist = list()
      showNotification("Processing ...", type="message", duration = 5)

      cat(file=stderr(), 'BOXPLOT', "\n")
      tabfeat0 <- boxtab()

      if(input$custom_ytitle == "None"){
        ytitle <- stringr::str_split(input$feat1, "__",simplify = TRUE)[2]
        if(r$wgt1() != "Raw"){
          ytitle <- glue::glue("{ytitle}, weight: {r$wgt1()}")
        }
        if(r$norm1() != "Raw"){
          ytitle <- glue::glue("{ytitle}, norm.: {r$norm1()}")
        }

      }else{
        ytitle <- input$custom_ytitle
      }
      fun <- glue::glue("
          tabfeat <- tabfeat0 %>%
            dplyr::filter({r_values$fact3ok} %in% input$sorted1) %>%
            droplevels() %>%
            mutate({r_values$fact3ok} = factor({r_values$fact3ok}, levels = input$sorted1))
        ")
      eval(parse(text=fun))

      # tabfeat[[r_values$fact3ok]] <- factor(tabfeat[[r_values$fact3ok]], levels = input$sorted1)
      
      cat(file=stderr(), 'Factor', "\n")
      print(tabfeat[[r_values$fact3ok]])
     fun <-  glue::glue('p <- ggplot(tabfeat, aes(x = {r_values$fact3ok}, y = value, fill = {r_values$fact3ok})) + 
        theme_bw() + xlab("Condition") + ylab(ytitle) + ggtitle(input$feat1) +
        theme(legend.position = "None", axis.text.x = element_text(angle = 45, hjust=1)) + 
        labs(fill="")')
      eval(parse(text=fun))

      if(!input$grey_mode){
        p <- p + 
            geom_boxplot(fill = "grey")

        r_values$ggly <- ggly <- ggplotly(p)
        # # Hoverinfo works only in grey mode 
        tabfeat$sample.id <- as.character(tabfeat$sample.id)
        hoverinfo <- with(tabfeat, paste0("sample: ", sample.id, "</br></br>", 
                                        "value: ", value))
        ggly$x$data[[1]]$text <- hoverinfo
        ggly$x$data[[1]]$hoverinfo <- c("text", "boxes")
        ######
      }else{
        p <- p + 
            geom_boxplot()            
      r_values$ggly <- ggly <- ggplotly(p)
      }

      if(input$ggplotstats1 & max(table(tabfeat[[r_values$fact3ok]])) != 1){
        fun <-  glue::glue('
          ggstats <- ggbetweenstats(tabfeat, {r_values$fact3ok}, value, type = "nonparametric", 
              p.adjust.method = "fdr", pairwise.display = "significant", xlab = "", ylab = ytitle,
              outlier.tagging = TRUE, outlier.label = "sample.id", results.subtitle = FALSE, title = input$feat1)
              ')
        eval(parse(text=fun))
        r_values$ggstats <- ggstats
        outlist$ggstats <- ggstats
      }


      
      
      cat(file=stderr(), 'BOXPLOT done', "\n")
      

      outlist$p <- p
      outlist$tabF_melt2 <- r_values$tabF_melt2
      outlist$fact3ok <- r_values$fact3ok 
      outlist$ggly <- ggly

      outlist
    })
    

    
    output$boxplotly1 <- renderPlotly({
      # req(boxplot1())
      req(input$go3)
      if(!is.null(r_values$ggly)){
        bp1 <- boxplot1()
        ggplotly(bp1$ggly)
      }
    })

    output$ggplotstatsOUT1 <- renderPlot({
      # req(boxplot1())
      req(input$go3)
      if(!is.null(r_values$ggstats)){
        bp1 <- boxplot1()
        bp1$ggstats
      }
    }, res = 100)
    
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

        withProgress({

          for(i in 1:length(FEAT)){
            incProgress(1/length(FEAT))
            tt <- stringr::str_split(FEAT[i], "__")
            if(input$custom_ytitle == "None"){
              print(tt)
              ytitle <- sapply(tt,"[[",2)
              print(ytitle)
              if(r$wgt1() != "Raw"){
                ytitle <- glue::glue("{ytitle}, weight: {r$wgt1()}")
              }
              if(r$norm1() != "Raw"){
                ytitle <- glue::glue("{ytitle}, norm.: {r$norm1()}")
              }

            }else{
              ytitle <- input$custom_ytitle
            }
            
            fun <-  glue::glue('tabfeat0 = tabF_melt2[tabF_melt2$features == FEAT[i],] %>% 
                    group_by({fact3ok}) %>% 
                    mutate(outlier=ifelse(is_outlier(value), sample.id, NA))')
            eval(parse(text=fun))

            fun <- glue::glue("
                tabfeat <- tabfeat0 %>%
                  dplyr::filter({r_values$fact3ok} %in% input$sorted1) %>%
                  droplevels() %>%
                  mutate({r_values$fact3ok} = factor({r_values$fact3ok}, levels = input$sorted1))
              ")
            eval(parse(text=fun))

             if(!input$plotall){
                tabfeat <- tabfeat %>% filter(!is.na(value))
              }

            if(nrow(tabfeat) == 0){print("no data"); next}
            
            fun <-  glue::glue('listP[[FEAT[i]]] <- ggplot(tabfeat, aes(x = {fact3ok}, y = value, fill = {fact3ok})) + 
          geom_boxplot(fill = "#99AFE3") + theme_bw() + xlab("Condition") + ylab(ytitle) + ggtitle(FEAT[i]) +
          theme(legend.position = "None", axis.text.x = element_text(size=rel(input$sizexlab), angle = 45, hjust=1))  + 
          labs(fill="")')
            eval(parse(text=fun))

            if(input$outlier_labs){
              listP[[FEAT[i]]] <- listP[[FEAT[i]]] + 
                                ggrepel::geom_text_repel(aes(label = outlier), na.rm = TRUE, show.legend = F, 
                                direction = "both",
                                nudge_x = 0.1,
                                size= 3
                               )
            }

            if(!input$grey_mode){
              listP[[FEAT[i]]] <- listP[[FEAT[i]]] + 
                                geom_boxplot(fill = "grey")
            }else{
              listP[[FEAT[i]]] <- listP[[FEAT[i]]] + 
                                geom_boxplot()            
            }

            if(input$pngs_out){
              if(!dir.exists(input$outpath)){
                dir.create(input$outpath, recursive = TRUE)
              }
              ggsave(glue::glue("{input$outpath}/boxplot_{sapply(tt,'[[',1)}.png"), listP[[FEAT[i]]])
            }

            print(length(listP))
          }

        }, value = 0 ,message = "Processing boxplots ... please wait.")
      print(length(listP))
      
      listP
    })
    

    pdfall_ggstat <- reactive({
      cat(file=stderr(), 'ALL BOXPLOT', "\n")
      req(r_values$tabF_melt2, r_values$fact3ok)

        fact3ok <- r_values$fact3ok
        tabF_melt2 <- r_values$tabF_melt2
        tabF_melt2$sample.id <- as.character(tabF_melt2$sample.id)
        listP <- list()
        FEAT = levels(tabF_melt2$features)
        print(head(FEAT))

        withProgress({

          for(i in 1:length(FEAT)){ #i = 1
            incProgress(1/length(FEAT))
            tt <- stringr::str_split(FEAT[i], "__")
            if(input$custom_ytitle == "None"){
              print(tt)
              ytitle <- sapply(tt,"[[",2)
              print(ytitle)
              if(r$wgt1() != "Raw"){
                ytitle <- glue::glue("{ytitle}, weight: {r$wgt1()}")
              }
              if(r$norm1() != "Raw"){
                ytitle <- glue::glue("{ytitle}, norm.: {r$norm1()}")
              }

            }else{
              ytitle <- input$custom_ytitle
            }
            
            fun <-  glue::glue('tabfeat0 = tabF_melt2[tabF_melt2$features == FEAT[i],] %>% 
                    group_by({fact3ok}) %>% 
                    mutate(outlier=ifelse(is_outlier(value), sample.id, NA))')
            eval(parse(text=fun))

            fun <- glue::glue("
                tabfeat <- tabfeat0 %>%
                  dplyr::filter({r_values$fact3ok} %in% input$sorted1) %>%
                  droplevels() %>%
                  mutate({r_values$fact3ok} = factor({r_values$fact3ok}, levels = input$sorted1))
              ")
            eval(parse(text=fun))

             if(!input$plotall){
                tabfeat <- tabfeat %>% filter(!is.na(value))
              }

            if(nrow(tabfeat) == 0){print("no data"); next}
            
              fun <-  glue::glue('
          listP[[FEAT[i]]] <- ggbetweenstats(tabfeat, {r_values$fact3ok}, value, type = "nonparametric", 
              p.adjust.method = "fdr", pairwise.display = "significant", xlab = "", ylab = ytitle,
              outlier.tagging = TRUE, outlier.label = "sample.id", title = FEAT[i], results.subtitle = FALSE)')


            eval(parse(text=fun))

            if(input$pngs_out){
              if(!dir.exists(input$outpath)){
                dir.create(input$outpath, recursive = TRUE)
              }
              ggsave(glue::glue("{input$outpath}/boxplot_{sapply(tt,'[[',1)}.png"), listP[[FEAT[i]]], width = 20, height = 15, units = "cm")
            }

            print(length(listP))
          }

        }, value = 0 ,message = "Processing boxplots ... please wait.")
      print(length(listP))
      
      listP
    })


    output$boxplots_download <- downloadHandler(
      filename = "figures.pdf",
      content = function(file) {
        print('DOWNLOAD ALL')
        if(input$ggstatOUT){
          print("ggstat")
          req(pdfall_ggstat())
          p <- pdfall_ggstat()
          
          withProgress({
            ml <- marrangeGrob(p, nrow=1, ncol=1)
            ggsave(file, ml, units = "cm", width = 20, height = 15, dpi = 300)
          }, message = "Prepare pdf file... please wait.")

        }else{
          print("ggplot")
          req(pdfall())
          p <- pdfall()
            withProgress({
              ml <- marrangeGrob(p, nrow=1, ncol=1)

                if(as.numeric(input$nbPicPage) == 4){
                  ml <- marrangeGrob(p, nrow=2, ncol=2)
                 }else if(as.numeric(input$nbPicPage) == 3){
                  ml <- marrangeGrob(p, nrow= 1, ncol=as.numeric(input$nbPicPage))
                  }else if(as.numeric(input$nbPicPage) == 2){
                    if(input$verticaldisplay){
                      ml <- marrangeGrob(p, nrow= as.numeric(input$nbPicPage), ncol= 1)
                    }else{
                      ml <- marrangeGrob(p, nrow= 1, ncol=as.numeric(input$nbPicPage))
                    }
                  }

              # ggsave(file, ml, units = "cm", width = 20, height = 15, dpi = 100)
              ggsave(file, ml , width = 11, height = 8, dpi = 100)
            }, message = "Prepare pdf file... please wait.")
        }
        print('pdf output')
        

        
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
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, server=TRUE)) # , rowCallback = DT::JS(rowCallback)) 
    
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
      req(boxplot1(), boxtab())
      Amelt <- boxplot1()$tabF_melt2
      if(max(table(boxtab()[, boxplot1()$fact3ok])) == 1){
        return(NULL)
      }

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
      if(is.null(wilcoxBP())){
        validate('\t\t\t\t\t\t\t\t\t\tNo tests possible.')
      }else{
        wilcoxBP()
      }
    }, filter="top",options = list(pageLength = 5, scrollX = TRUE, server=TRUE)) # , rowCallback = DT::JS(rowCallback)) 
    
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
# mod_boxplots_ui("boxplots_1")
    
## To be copied in the server
# mod_boxplots_server("boxplots_1")
