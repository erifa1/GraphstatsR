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
#' @importFrom car Boxplot
#' @importFrom waiter useWaiter waiter_show waiter_hide spin_fading_circles
#' @import PMCMRplus

tmpdir <- tempdir()
systim <- as.numeric(Sys.time())


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
      useWaiter(),

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
            pickerInput(
              ns("outtype"),
              label = "Select specific type of features to download :",
              choices = "",
              multiple = TRUE
            ),
            dropMenu(
              actionButton("go0", "More parameters..."),
              {fluidRow(
                
                column(
                  h3("General settings"),
                  materialSwitch(ns("ggplotstats1"), label = "Display ggstatsplot", value = TRUE, status = "primary"),
                  materialSwitch(ns("plotall"), label = "Plot all conditions (even NAs)", value = TRUE, status = "primary"),
                  materialSwitch(ns("grey_mode"), label = "Colored boxplot", value = TRUE, status = "primary"),
                  h3("Y axis settings"),
                  textInput(ns("custom_ytitle"), "Custom y title", "None"),
                  materialSwitch(ns("ySci"), label = "Yaxis scientific numbers:", value = TRUE, status = "primary"),
                  numericInput(ns("ymin"), "Y min:", 0),
                  numericInput(ns("ymax"), "Y max:", NA),
                  numericInput(ns("ysteps"), "Y steps:", NA),
                  width = 6
                  ),
                column(
                  h3("PDF and Images output settings"),
                  selectInput( ns("nbPicPage"), label = "Select number of plot per pdf page (max 4 per page):", choices = c(1:4), selected = 1),
                  materialSwitch(ns("ggstatOUT"), label = "Output PDF/Images with ggstat plots", value = FALSE, status = "primary"),
                  materialSwitch(ns("verticaldisplay"), label = "Vertical display in pdf or not (2 per page)", value = TRUE, status = "primary"),
                  sliderInput(ns("sizexlab"), label = "X labels size", min = 0, max = 1, value = 0.8, step = 0.05),
                  materialSwitch(ns("outlier_labs"), label = "Inform outlier in pdf output", value = TRUE, status = "primary"),
                  selectInput( ns("ImgFormat"), label = "Image format :", choices = c("jpeg", "eps", "ps", "pdf", "tiff", "png", "bmp", "svg"), selected = 1),
                  # materialSwitch(ns("pngs_out"), label = "Output png for each feature (long process)", value = FALSE, status = "primary"),
                  # textInput(ns("outpath"), "Output path for pngs", ""),
                  width = 6
                  )

                )

              },
                  theme = "light-border",
                  placement = "right"
              ),
            actionButton(ns("go3"), "Run plot/stats & tests", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469"),
            actionButton(ns("go4"), "Update plot only", icon = icon("play-circle"), style="color: #fff; background-color: #3b9ef5; border-color: #1a4469"),
            uiOutput(ns("DLbuttons"))
        ),
        box(title = "Reorder boxplots:", width = 5, status = "warning", solidHeader = TRUE, collapsible = FALSE,
          style='height:400px;overflow-y: scroll;',
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
            title = 'Boxplot with stats:', status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
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
      if(r$tabs$tabselected=='boxplot-tab' & is.character(r$fdata_melt())) {
        cat(file=stderr(), 'Boxplot no table... ', "\n")
        shinyalert(title = "Oops", text="Final table not available, check all steps.", type='error')
      }
    })
    
    # Settings   
    observe({
      # req(metadata1(), r_values$subsetds_final_melt)
      req(r$mt1(), r$fdata_melt(), r$ds0())
      r_values$subsetds_final_melt <- r$fdata_melt()
      r_values$metadata_final <- r$mt1()


      ds0 <- r$ds0()
      print(names(ds0))
      type1 <- unique(ds0[,2])  # colonne type
      print("OBSERVE")
      print(type1)
      updatePickerInput(session, "outtype",
                        choices = type1,
                        selected = type1)

      if(is.data.frame(r$fdata_melt())){
        updateSelectInput(session, "feat1",
                          choices = unique(r_values$subsetds_final_melt[,"features"]),
                          selected = unique(r_values$subsetds_final_melt[,"features"])[1])

        updatePickerInput(session, "fact3",
                          choices = names(r_values$metadata_final),
                          selected = names(r_values$metadata_final)[2],
                          options = list(
                            `actions-box` = TRUE, 
                            size = 10,
                            `selected-text-format` = "count > 3"
                          )
        )

        updateNumericInput(session, "ymax", label = glue::glue("Ymax: (max value in dataset: {format(max(r_values$subsetds_final_melt$value, na.rm = TRUE), scientific = TRUE, digits = 2)})"), 
          value = NA )

      }
    })

    
    boxtab <- reactive({  
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
      
      # waiter_show(
      #   html = tagList(spin_fading_circles(), h4("Processing, please wait...")) 
      # )

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


      # Y custom 
      p <- p + coord_cartesian(ylim = c(input$ymin, input$ymax)) + 
        scale_y_continuous(labels = function(x) format(x, scientific = input$ySci))
      
      if(!is.na(input$ysteps) & !is.na(input$ymin) & !is.na(input$ymax) ){
        print("ycustom1")
        p <- p + scale_y_continuous(breaks = seq(input$ymin, input$ymax, input$ysteps), 
          labels = function(x) format(x, scientific = input$ySci))
      }

      if( is.na(input$ymin) | is.na(input$ymax) & !is.na(input$ysteps) ){
        print("ycustom2")
        p <- p + coord_cartesian(ylim = c(0, max(tabfeat$value, na.rm = TRUE))) + 
          scale_y_continuous(breaks = seq(0, max(tabfeat$value, na.rm = TRUE), input$ysteps), 
          labels = function(x) format(x, scientific = input$ySci))
      }


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
              outlier.tagging = TRUE, outlier.label = "sample.id", results.subtitle = FALSE, title = input$feat1) + 
              coord_cartesian(ylim = c(0, NA))
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

      # waiter_hide()

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
      LL <- list()
      req(r_values$tabF_melt2, r_values$fact3ok)
        fact3ok <- r_values$fact3ok
        tabF_melt2 <- r_values$tabF_melt2
        
        tabF_melt2 <- tidyr::separate(tabF_melt2, features, c("feature","type","unit"), "__", remove= FALSE) %>% 
                      mutate_if(is.character,as.factor) %>%
                      filter(type %in% input$outtype) %>%
                      droplevels()   

        tabF_melt2$sample.id <- as.character(tabF_melt2$sample.id)
        listP <- list()
        FEAT = levels(tabF_melt2$features)

        if(length(FEAT) > 200){
            showNotification("More than 200 features to plot...", type="warning", duration = 5)
        }


        print(head(FEAT))

        withProgress({

          print("GENERATING BOXPLOTS")
          r_values$systim <- as.numeric(Sys.time())
          dir.create(paste(tmpdir, "/figures_", r_values$systim, "/", sep = ""), recursive = TRUE)
          print(paste(tmpdir, "/figures_", r_values$systim, "/", sep = ""))

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

          # Y custom 
          listP[[FEAT[i]]] <- listP[[FEAT[i]]] + coord_cartesian(ylim = c(input$ymin, input$ymax)) + 
            scale_y_continuous(labels = function(x) format(x, scientific = input$ySci))
          
          if(!is.na(input$ysteps) & !is.na(input$ymin) & !is.na(input$ymax) ){
            print("ycustom1")
            listP[[FEAT[i]]] <- listP[[FEAT[i]]] + scale_y_continuous(breaks = seq(input$ymin, input$ymax, input$ysteps), 
              labels = function(x) format(x, scientific = input$ySci))
          }

          if( is.na(input$ymin) | is.na(input$ymax) & !is.na(input$ysteps) ){
            print("ycustom2")
            listP[[FEAT[i]]] <- listP[[FEAT[i]]] + coord_cartesian(ylim = c(0, max(tabfeat$value, na.rm = TRUE))) + 
              scale_y_continuous(breaks = seq(0, max(tabfeat$value, na.rm = TRUE), input$ysteps), 
              labels = function(x) format(x, scientific = input$ySci))
          }

          

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

            #   print("WRITE PLOTS")
            # if(input$pngs_out){
            #   ggsave(glue::glue("{tmpdir}/figures/boxplot_{sapply(tt,'[[',1)}.png"), listP[[FEAT[i]]], width = 20, height = 15, units = "cm")
            #   tar(glue::glue("{tmpdir}/figures_{systim}.tar"), files = glue::glue("{tmpdir}/figures") )
            # }

            print(length(listP))
          }

        }, value = 0 ,message = "Processing boxplots ... please wait.")
      print(length(listP))
      
      listP
    })
    

    pdfall_ggstat <- reactive({
      cat(file=stderr(), 'ALL BOXPLOT ggstat', "\n")
      req(r_values$tabF_melt2, r_values$fact3ok)

        fact3ok <- r_values$fact3ok
        tabF_melt2 <- r_values$tabF_melt2
        tabF_melt2 <- tidyr::separate(tabF_melt2, features, c("feature","type","unit"), "__", remove= FALSE) %>% 
                      mutate_if(is.character,as.factor) %>%
                      filter(type %in% input$outtype) %>%
                      droplevels()

        tabF_melt2$sample.id <- as.character(tabF_melt2$sample.id)
        listP <- list()
        r_values$FEAT <- FEAT <- levels(tabF_melt2$features)

        if(length(FEAT) > 200){
            showNotification("More than 200 features to plot ...", type="warning", duration = 5)
        }

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
              outlier.tagging = TRUE, outlier.label = "sample.id", title = FEAT[i], results.subtitle = FALSE) + 
              coord_cartesian(ylim = c(0, NA))')

            eval(parse(text=fun))

            # print("WRITE PLOTS")
            # dir.create(paste(tmpdir, "/figures_ggstat/", sep = ""), recursive = TRUE)
            # print(paste(tmpdir, "/figures_ggstat/", sep = ""))

            # if(input$pngs_out){
            #   ggsave(glue::glue("{tmpdir}/figures_ggstat/boxplot_{sapply(tt,'[[',1)}.png"), listP[[FEAT[i]]], width = 20, height = 15, units = "cm")
            #   tar(glue::glue("{tmpdir}/figures_ggstat.tar"), files = glue::glue("{tmpdir}/figures_ggstat") )
            # }

            print(length(listP))
          }

        }, value = 0 ,message = "Processing boxplots ... please wait.")
      print(length(listP))
      
      listP
    })


    output$downloadTAR <- downloadHandler(
      filename <- glue::glue("{tmpdir}/figures_pngs_ggplot.tar"), 

      content <- function(file) {
        print("WRITE PLOTS")
        systim <- as.numeric(Sys.time())
        print(glue::glue("{tmpdir}/figures_pngs/"))

        if(length(input$outtype)>1){
          for (i in input$outtype){
            dir.create(glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}/"), recursive = TRUE)
          }
        }else{
          dir.create(glue::glue("{tmpdir}/figures_jpgs_{systim}/"), recursive = TRUE)
        }

        if(input$ggstatOUT){
          req(pdfall_ggstat())
          listP <- pdfall_ggstat()
          print(names(listP))
        }else{
          req(pdfall())
          listP <- pdfall()
        }
        FEAT = names(listP)


        withProgress({
          for(i in 1:length(FEAT)){
            incProgress(1/length(FEAT))
            # tt <- stringr::str_split(FEAT[i], "__")
            met1 <- stringr::str_split_1(FEAT[i], "__")[1] %>% stringr::str_replace("/", "_")
            typ1 <- stringr::str_split_1(FEAT[i], "__")[2] %>% stringr::str_replace("/", "_")

            if(length(input$outtype)>1){
              path1 <- glue::glue("{tmpdir}/figures_jpgs_{systim}/{typ1}/")
            }else{
              path1 <- glue::glue("{tmpdir}/figures_jpgs_{systim}")
            }

            # ggsave(glue::glue("{tmpdir}/figures_ggplot/figures_{systim}/{sapply(tt,'[[',2)}_boxplot_{sapply(tt,'[[',1)}.{input$ImgFormat}"), listP[[FEAT[i]]], width = 20, height = 15, units = "cm", device = input$ImgFormat)
            ggsave(glue::glue("{path1}/{met1}.{input$ImgFormat}"), listP[[FEAT[i]]], width = 20, height = 15, units = "cm", device = input$ImgFormat)
          }

        }, value = 0, message = "Generating Images...")

        # tar(filename, files = glue::glue("{tmpdir}/figures_ggplot/figures_{systim}") )

        if(length(input$outtype) > 1){

          for (i in input$outtype){
            tar(glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}.tar"), files = glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}"))
            tar(glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}.tar"), files = glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}"))
            tar(glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}.tar"), files = glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}"))
          }

          print("TAR2")
          # browser()
          files <- dir(glue::glue("{tmpdir}/figures_jpgs_{systim}/"))
          outfiles <- files[stringr::str_detect(files, ".tar")]

          print(outfiles)

          tar(filename, files = glue::glue("{tmpdir}/figures_jpgs_{systim}/{outfiles}"))  #glue::glue("{tmpdir}/figures_jpgs_{systim}/")

        }else{
          tar(filename, files = glue::glue("{tmpdir}/figures_jpgs_{systim}/"))
        }


        file.copy(filename, file)
      },
      contentType = "application/tar"
    )



    output$boxplots_download <- downloadHandler(
      filename = glue::glue("{input$outtype}_figures_{systim}.pdf"),
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


    output$pdf_rbase <- downloadHandler(
      filename = glue::glue("{input$outtype}_figuresRbase_{systim}.pdf"),
      content = function(file) {
        req(r_values$tabF_melt2,r_values$fact3ok)
        tabF_melt2 <- r_values$tabF_melt2 %>% tidyr::separate(features, sep = "__", into = c("feat","type","unit"), remove = FALSE) %>% 
          filter(type %in% input$outtype)
        # print(unique(tabF_melt2[r_values$fact3ok]))
        # print(input$sorted1)
          if(!any(unique(tabF_melt2[r_values$fact3ok]) %in% input$sorted1)){
            # validate("Run plot/stats & tests again.")
            print("Run plot/stats & tests again.")
          }

        pdf(file)
        for(i in 1:length(unique(as.character(tabF_melt2$features)))){

          if(input$nbPicPage == 4){
            if((i %% 4) == 1) {par(mfrow= c(2,2), mar=c(4,4,2,0.5))}
          }else if(input$nbPicPage == 3){
            if((i %% 3) == 1) {
              if(input$verticaldisplay){
                par(mfrow= c(1,3), mar=c(4,4,2,0.5))
              }else{
                par(mfrow= c(3,1), mar=c(4,4,2,0.5))
              }
            }
          }else if(input$nbPicPage == 2){
            if((i %% 2) == 1) {
              if(input$verticaldisplay){
                par(mfrow= c(1,2), mar=c(4,4,2,0.5))
              }else{
                par(mfrow= c(2,1), mar=c(4,4,2,0.5))
              }
            }
          }

          feat1 <- unique(as.character(tabF_melt2$features))[i]
          if(input$custom_ytitle != "None"){
            YLAB <- input$custom_ytitle
          }else{
            YLAB <- stringr::str_split_1(feat1, "__")[3]
          }



            fun1 <- glue::glue('
                tab1 <- tabF_melt2 %>% dplyr::filter(features == feat1) %>% 
                tidyr::separate(features, c("feature","type","unit"), "__", remove= FALSE) %>%
                dplyr::filter({r_values$fact3ok} %in% input$sorted1) %>%
                droplevels() %>%
                mutate({r_values$fact3ok} = factor({r_values$fact3ok}, levels = input$sorted1))
              ')
            eval(parse(text=fun1))
            row.names(tab1) <- tab1$sample.id

            if(all(is.na(tab1$value))){
              plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
              text(x = 0.5, y = 0.5, glue::glue("{feat1}\nNo data"), cex = 1.6, col = "black")
              next
            }


            # Y custom 

            #validate steps 
            if(!is.na(input$ymin) & is.na(input$ysteps) & is.na(input$ymax) ){
              # print("ycustom01")
              YLIM <- c(0, max(tab1$value, na.rm = TRUE))
              STEPS <- NULL
            }

            if(!is.na(input$ymin) & is.na(input$ysteps) & !is.na(input$ymax) ){
              # print("ycustom02")
              YLIM <- c(input$ymin, input$ymax)
              STEPS <- NULL
            }
            
            if(!is.na(input$ysteps) & !is.na(input$ymin) & !is.na(input$ymax) ){
              # print("ycustom1")
              print(input$ymin); print(input$ymax); print(input$ysteps)
              YLIM <- c(input$ymin, input$ymax)
              STEPS <- format(seq(input$ymin, input$ymax, input$ysteps), scientific = input$ySci)
            }

            if( is.na(input$ymin) | is.na(input$ymax) & !is.na(input$ysteps) ){
              # print("ycustom2")
              YLIM <- c(0, max(tab1$value, na.rm = TRUE))
              STEPS <- format(seq(0, max(tab1$value, na.rm = TRUE), input$steps), scientific = input$ySci)
            }

            fact3 <- r_values$fact3ok

            if(input$outlier_labs){
              car::Boxplot(as.formula(glue::glue("value~{r_values$fact3ok}")), data = tab1, main = feat1, 
                  cex.main = 0.6, boxwex=.3, col = gg_color_hue(nrow(unique(tab1[r_values$fact3ok]))),
                  cex.lab = 0.9, cex.axis = 0.5, las = 2, xlab = "", ylab = YLAB, 
                  ylim=YLIM, axes = FALSE)
              axis(1, at= 1:length(levels(tab1[,r_values$fact3ok])), labels = levels(tab1[,r_values$fact3ok]), las = 2, cex.axis = 0.6)
              if(!is.null(STEPS)){
                axis(2, at = STEPS, cex.axis = 0.6, labels=format(as.numeric(STEPS), scientific=TRUE)); graphics::box()
              }else{
                axis(2); graphics::box()
              }

            }else{
              boxplot(as.formula(glue::glue("value~{r_values$fact3ok}")), data = tab1, main = feat1, 
                  cex.main = 0.6, boxwex=.3, col = gg_color_hue(nrow(unique(tab1[r_values$fact3ok]))),
                  cex.lab = 0.9, cex.axis = 0.5, las = 2, xlab = "", ylab = "area", 
                  ylim=YLIM, axes = FALSE)
              axis(1, at= 1:length(levels(tab1[,r_values$fact3ok])), labels = levels(tab1[,r_values$fact3ok]), las = 2, cex.axis = 0.6)
              if(!is.null(STEPS)){
                axis(2, at = STEPS, cex.axis = 0.6, labels=format(as.numeric(STEPS), scientific=TRUE)); graphics::box()
              }else{
                axis(2); graphics::box()
              }
            }

              grid()
        }
        dev.off()
      })

    output$downloadTAR_rbase <- downloadHandler(
      filename <- glue::glue("{tmpdir}/figures_jpgs_rbase.tar"), 

      content <- function(file) {
        print("WRITE PLOTS")
        systim <- as.numeric(Sys.time())
        print(glue::glue("{tmpdir}/figures_jpgs_{systim}"))

        if(length(input$outtype)>1){
          for (i in input$outtype){
            dir.create(glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}/"), recursive = TRUE)
          }
        }else{
          dir.create(glue::glue("{tmpdir}/figures_jpgs_{systim}/"), recursive = TRUE)
        }

        req(r_values$tabF_melt2,r_values$fact3ok)
        tabF_melt1 <- r_values$tabF_melt2 
        tabF_melt2 <- r_values$tabF_melt2 %>%
                      separate(features, sep = "__", into = c("feat","type","unit"), remove =FALSE) %>%
                      filter(type %in% input$outtype)

        print(input$ymin); print(input$ymax); print(input$steps)

        for(i in 1:length(levels(tabF_melt2$features))){

          if(input$nbPicPage == 4){
            if((i %% 4) == 1) {par(mfrow= c(2,2), mar=c(4,4,2,0.5))}
          }else if(input$nbPicPage == 3){
            if((i %% 3) == 1) {
              if(input$verticaldisplay){
                par(mfrow= c(1,3), mar=c(4,4,2,0.5))
              }else{
                par(mfrow= c(3,1), mar=c(4,4,2,0.5))
              }
            }
          }else if(input$nbPicPage == 2){
            if((i %% 2) == 1) {
              if(input$verticaldisplay){
                par(mfrow= c(1,2), mar=c(4,4,2,0.5))
              }else{
                par(mfrow= c(2,1), mar=c(4,4,2,0.5))
              }
            }
          }

          feat1 <- levels(tabF_melt2$features)[i]
          if(input$custom_ytitle != "None"){
            YLAB <- input$custom_ytitle
          }else{
            YLAB <- stringr::str_split_1(feat1, "__")[3]
          }

            fun1 <- glue::glue('
                tab1 <- tabF_melt2 %>% dplyr::filter(features == feat1) %>% 
                tidyr::separate(features, c("feature","type","unit"), "__", remove= FALSE) %>%
                dplyr::filter({r_values$fact3ok} %in% input$sorted1) %>%
                droplevels() %>%
                mutate({r_values$fact3ok} = factor({r_values$fact3ok}, levels = input$sorted1))
              ')
            eval(parse(text=fun1))
            row.names(tab1) <- tab1$sample.id

            if(all(is.na(tab1$value))){
              plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
              text(x = 0.5, y = 0.5, glue::glue("{feat1}\nNo data"), cex = 1.6, col = "black")
              next
            }

            met1 <- stringr::str_split_1(feat1, "__")[1] %>% stringr::str_replace("/", "_")
            typ1 <- stringr::str_split_1(feat1, "__")[2] %>% stringr::str_replace("/", "_")

            if(length(input$outtype)>1){
              path1 <- glue::glue("{tmpdir}/figures_jpgs_{systim}/{typ1}/")
            }else{
              path1 <- glue::glue("{tmpdir}/figures_jpgs_{systim}")
            }


            if(input$ImgFormat == "jpeg"){
              jpeg(glue::glue("{path1}/{met1}.jpeg"), width = 1422, height = 800, quality = 100, res = 150)
            }else if(input$ImgFormat == "png"){
              png(glue::glue("{path1}/{met1}.png"), width = 1422, height = 800, res = 150)
            }else if(input$ImgFormat == "tiff"){
              tiff(glue::glue("{path1}/{met1}.tiff"), width = 1422, height = 800, res = 150) 
            }else if(input$ImgFormat == "bmp"){
              bmp(glue::glue("{path1}/{met1}.bmp"), width = 1422, height = 800, res = 150)
            }else{
              jpeg(glue::glue("{path1}/{met1}.jpeg"), width = 1422, height = 800, quality = 100, res = 150)              
            }


            # Y custom 

            #validate steps 
            if(!is.na(input$ymin) & is.na(input$ysteps) & is.na(input$ymax) ){
              # print("ycustom01")
              YLIM <- c(0, max(tab1$value, na.rm = TRUE))
              STEPS <- NULL
            }

            if(!is.na(input$ymin) & is.na(input$ysteps) & !is.na(input$ymax) ){
              # print("ycustom02")
              YLIM <- c(input$ymin, input$ymax)
              STEPS <- NULL
            }
            
            if(!is.na(input$ysteps) & !is.na(input$ymin) & !is.na(input$ymax) ){
              # print("ycustom1")
              print(input$ymin); print(input$ymax); print(input$ysteps)
              YLIM <- c(input$ymin, input$ymax)
              STEPS <- format(seq(input$ymin, input$ymax, input$ysteps), scientific = input$ySci)
            }

            if( is.na(input$ymin) | is.na(input$ymax) & !is.na(input$ysteps) ){
              # print("ycustom2")
              YLIM <- c(0, max(tab1$value, na.rm = TRUE))
              STEPS <- format(seq(0, max(tab1$value, na.rm = TRUE), input$steps), scientific = input$ySci)
            }

            # HERE
            # if(!is.na(input$steps) & length(STEPS)> 100){
            #   print("Too much steps on Y axis.")
            #   print(length(STEPS))
            #   validate("Too much steps on Y axis.")
            #   return() 
            # }

            fact3 <- r_values$fact3ok

            if(input$outlier_labs){
              car::Boxplot(as.formula(glue::glue("value~{r_values$fact3ok}")), data = tab1, main = feat1, 
                  cex.main = 0.6, boxwex=.3, col = gg_color_hue(nrow(unique(tab1[r_values$fact3ok]))),
                  cex.lab = 0.9, cex.axis = 0.5, las = 2, xlab = "", ylab = YLAB, 
                  ylim=YLIM, axes = FALSE)
              axis(1, at= 1:length(levels(tab1[,r_values$fact3ok])), labels = levels(tab1[,r_values$fact3ok]), las = 2, cex.axis = 0.6)
              if(!is.null(STEPS)){
                axis(2, at = STEPS, cex.axis = 0.6, labels=format(STEPS,scientific=input$ySci)); graphics::box()
              }else{
                axis(2); graphics::box()
              }
              #c(0,max(tab1$value, na.rm = TRUE))

            }else{
              boxplot(as.formula(glue::glue("value~{r_values$fact3ok}")), data = tab1, main = feat1, 
                  cex.main = 0.6, boxwex=.3, col = gg_color_hue(nrow(unique(tab1[r_values$fact3ok]))),
                  cex.lab = 0.9, cex.axis = 0.5, las = 2, xlab = "", ylab = "area", 
                  ylim=YLIM, axes = FALSE)
              axis(1, at= 1:length(levels(tab1[,r_values$fact3ok])), labels = levels(tab1[,r_values$fact3ok]), las = 2, cex.axis = 0.6)
              if(!is.null(STEPS)){
                axis(2, at = STEPS, cex.axis = 0.6, labels=format(STEPS,scientific=input$ySci)); graphics::box()
              }else{
                axis(2); graphics::box()
              }
            }

              grid()
            dev.off()
        }


        if(length(input$outtype) > 1){

          for (i in input$outtype){
            tar(glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}.tar"), files = glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}"))
            tar(glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}.tar"), files = glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}"))
            tar(glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}.tar"), files = glue::glue("{tmpdir}/figures_jpgs_{systim}/{i}"))
          }

          print("TAR2")
          # browser()
          files <- dir(glue::glue("{tmpdir}/figures_jpgs_{systim}/"))
          outfiles <- files[stringr::str_detect(files, ".tar")]

          print(outfiles)

          tar(filename, files = glue::glue("{tmpdir}/figures_jpgs_{systim}/{outfiles}"))  #glue::glue("{tmpdir}/figures_jpgs_{systim}/")

        }else{
          tar(filename, files = glue::glue("{tmpdir}/figures_jpgs_{systim}/"))
        }



        file.copy(filename, file)
      },
      contentType = "application/tar"
    )





      output$DLbuttons <- renderUI({
        req(input$go3)
        tagList(
          column(width = 6,
            downloadButton(outputId = ns("boxplots_download"), label = "Download PDF (long process)"),
            downloadButton(outputId = ns("downloadTAR"), label = "Download Images (long process)")
            ),
          column(width = 6,
            downloadButton(outputId = ns("pdf_rbase"), label = "Download PDF rbase (faster)"),
            downloadButton(outputId = ns("downloadTAR_rbase"), label = "Download JPEG rbase (faster)")
            )
        )
      })
    
    
    
    summaryBP <- eventReactive(input$go3, {
      cat(file=stderr(), 'BOXPLOT summary', "\n")
      req(boxplot1())
      Amelt <- boxplot1()$tabF_melt2
      fact3ok <- boxplot1()$fact3ok
      print(head(Amelt))

      if( length(unique(Amelt$sample.id)) == length(unique(pull(Amelt, fact3ok))) ){
        print("No statistic, no test...")
        return()
      }

      waiter_show(
        html =  tagList(spin_fading_circles(), h4("Calculating statistics 1/2..."))
      )
      
      q = c(.25, .5, .75)
      boxstat <- data.frame()
      #calculate quantiles by grouping variable
      suppressWarnings({

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

      })

      cat(file=stderr(), 'BOXPLOT summary done', "\n")
      print(head(boxstat))
      
      waiter_hide()
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
      fact3ok <- boxplot1()$fact3ok

      if( length(unique(Amelt$sample.id)) == length(unique(pull(Amelt, fact3ok))) ){
        print("No statistic, no test...")
        return()
      }

      waiter_show(
        html = tagList(spin_fading_circles(), h4("Calculating statistics 2/2..."))
      )

      if(max(table(boxtab()[, boxplot1()$fact3ok])) == 1){
        return(NULL)
      }

      pval_table <- data.frame()
      for(feat1 in unique(Amelt$features)){
        Ftabtest = Amelt[Amelt$features == feat1,] %>%
          filter(!is.na(value)) 
        if(nrow(Ftabtest)==0){next}
        if(length(which(table(Ftabtest[Ftabtest$features == feat1,boxplot1()$fact3ok]) >= 3)) < 2){next} # si moins de 2 groupes avec au moins 3 repetitions next.
        # print(feat1)
        # print(table(Ftabtest[Ftabtest$features == feat1,boxplot1()$fact3ok]))
        
        suppressWarnings({
        wcoxtab = pairwise.wilcox.test(Ftabtest[Ftabtest$features == feat1,"value"], as.factor(Ftabtest[,boxplot1()$fact3ok]),
                                       p.adjust.method = "none")
        })

        ftable1 <- as.data.frame(wcoxtab$p.value) %>%
          rownames_to_column() %>% pivot_longer(!rowname, names_to = "condition", values_to = "pvalue") %>%
          na.omit() %>% add_column(Features = feat1, .after = 0)
        
        pval_table <- rbind.data.frame(pval_table, ftable1)
      }

      if(nrow(pval_table) == 0){
        print("No results")
        waiter_hide()
        return()
      }


      colnames(pval_table) = c("Features", "Condition1", "Condition2", "pvalue")
      
      Fpvaltable <- pval_table %>% mutate(adjusted_pval = p.adjust(pvalue, method = "fdr")) %>% mutate_if(is.character,as.factor) 
      print(dim(Fpvaltable))
      cat(file=stderr(), 'wilcoxBP table done', "\n")
      
      waiter_hide()

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
