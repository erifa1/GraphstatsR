#' plots_isot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom RColorBrewer brewer.pal
#' 

tmpdir <- tempdir()
systim <- as.numeric(Sys.time())

mod_plots_isot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(

        box(title = "Plot Settings:", width = 5, status = "warning", solidHeader = TRUE,
            selectInput(
              ns("feat2"),
              label = "Feature to preview:",
              choices = ""
            ),
            selectInput(
              ns("group1"),
              label = "Variable used to calculate means:",
              choices = ""
            )
          ),
        box(title = "Reorder boxplots:", width = 7, status = "warning", solidHeader = TRUE, collapsible = FALSE,
          style='height:400px;overflow-y: scroll;',
            uiOutput(ns("sortable1"))#,
            # verbatimTextOutput(ns("results_sort"))
        )
      ),
    fluidRow(
      box(width = 12, height = "700",
          title = 'CID barplot preview:', status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
          materialSwitch(ns("relativOUT"), label = "Absolute or relative plot", value = TRUE, status = "primary"),
          materialSwitch(ns("dodge1"), label = "Dodge histogram", value = FALSE, status = "primary"),
          downloadButton(outputId = ns("hist_download"), label = "Download PDF (long process)"),
          downloadButton(outputId = ns("hist_downloadTAR"), label = "Download PNGs (long process)"),
          plotlyOutput(ns("histo_plotly"))
        ),

      box(width = 12,
        title = 'EnrC13 / TotalArea preview:', status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        downloadButton(outputId = ns("bars_download"), label = "Download PDF (long process)"),
        plotOutput(ns("histo_Aire_enrC13"), height = "800px")
        ),

      box(width = 12,
        title = 'EnrC13 / TotalArea preview per specific group or sample :', status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        selectInput(
          ns("level1"),
          label = "Select group for preview:",
          choices = ""
        ),
        downloadButton(outputId = ns("bars_spec_download"), label = "Download PDF (long process)"),
        plotOutput(ns("histo_Aire_enrC13_allFeat_1group"), height = "800px")
      )
    
      )
  )
}
    
#' plots_isot Server Functions
#'
#' @noRd 
mod_plots_isot_server <- function(id, r = r, session = session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r_values <- reactiveValues()

  observe({

        req(r$merged2())
        dsF <- r_values$merged <- r$merged2()
        mtF <- r_values$mt1 <- r$mt1_isoT()

          updateSelectInput(session, "feat2",
                            choices = unique(dsF$metabolite),
                            selected = unique(dsF$metabolite)[1])

          updateSelectInput(session, "group1",
                            choices = colnames(mtF),
                            selected = colnames(mtF)[1])
  })
  
      observe({
        tt <- r$MeanSD_Area_EnrC13_per_compound
              updateSelectInput(session, "level1",
                                choices = unique(tt[,input$group1]))
      })


    output$sortable1 <- renderUI({
      tabF_melt2 <- tabF_melt <- r$merged2()

      # if(length(input$group1) == 1){
          r_values$group1ok <- group1ok <- input$group1
          fun = glue::glue('tabF_melt2 <- tabF_melt %>% dplyr::mutate(newfact = {input$group1}, .after= "sample")')
          eval(parse(text=fun))
        
        # }else{  # concat factors
        #   comb = glue::glue_collapse(input$group1, sep = ', \"_\",')
        #   fun = glue::glue('tabF_melt2 <- tabF_melt %>% dplyr::mutate(newfact = paste0({comb}), .after= "sample")')
        #   eval(parse(text=fun))
        #   fact3ok <- "newfact"
        #   tabF_melt2
        # }

      print("SORTABLE UI")
      # print(str(tabF_melt2))
      # print(names(tabF_melt2))
      bucket_list("Drag condition names to change order (multiple selection allowed)",
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list("Plotted conditions",
          unique(tabF_melt2$newfact), ns("sorted2"),
          options = sortable_options(multiDrag = TRUE)
        ),
        add_rank_list("Stashed conditions",
          NULL, ns("stashed2"),
          options = sortable_options(multiDrag = TRUE)
        )
      )
    })


      output$histo_plotly <- renderPlotly({
        req(r$merged2())
        mtab <- r$merged2()
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(20)
        xform <- list()
          fun <- glue::glue("
          mtab <- mtab %>%
          dplyr::filter({input$group1} %in% input$sorted2) %>%
          droplevels() %>%
          mutate({input$group1} = factor({input$group1}, levels = input$sorted2))
          ")
          eval(parse(text=fun))

        if(input$group1 != "sample"){
          print("GROUPING")
          cols2group <- c("metabolite", "Miso", input$group1)

          tab_plot4 <- mtab %>% group_by(across(all_of(cols2group))) %>% 
            summarise(meanGroup = mean(isotopologue_fraction), sdGroup = sd(isotopologue_fraction),
              meanGroupAbs = mean(corrected_area), sdGroupAbs = sd(corrected_area), .groups = "keep") %>% 
            arrange(as.character(Miso)) %>%
            arrange(across(c("metabolite",input$group1))) %>%
            group_by(across(c("metabolite",input$group1))) %>%
            mutate(SDPos = cumsum(meanGroup), SDPosAbs = cumsum(meanGroupAbs)) %>%
            as.data.frame()

          tab_plot4[which(tab_plot4$sdGroup == 0), "sdGroup"] <- NA
          tab_plot5 <- r_values$tab_plot4 <- tab_plot4

          tab_plot5$Miso = factor(tab_plot5$Miso, levels = sort(levels(tab_plot5$Miso)) )
        
          r_values$tab_plot5 <- tab_plot5

          print("PLOTS")
          if(input$dodge1){
            tab_plot <- tab_plot5 %>% filter(metabolite == input$feat2)

          if(input$relativOUT){ # newfact / as.formula(glue::glue("~{input$group1}"))
            p1 <- plotly::plot_ly(tab_plot, x = as.formula(glue::glue("~{input$group1}")), y = ~meanGroup, type = 'bar', 
                  name = ~Miso, color = ~Miso, height = 500, colors = mycolors[1:length(levels(tab_plot$Miso))]) %>% 
                  plotly::layout(title=glue::glue("Isotopologue Fraction {input$feat2}"), yaxis = list(title = 'Isotopologue fraction'), 
                  barmode = "group", xaxis = xform, barnorm = "fraction")
          }else{        
            p1 <- plotly::plot_ly(tab_plot, x = as.formula(glue::glue("~{input$group1}")), y = ~meanGroupAbs, type = 'bar', 
                  name = ~Miso, color = ~Miso, height = 500, colors = mycolors[1:length(levels(tab_plot$Miso))]) %>% 
                  plotly::layout(title=glue::glue("Corrected Area {input$feat2}"), yaxis = list(title = 'Isotopologue fraction'), 
                  barmode = "group", xaxis = xform, barnorm = "")
          }

          }else{
            tab_plot <- tab_plot4 %>% filter(metabolite == input$feat2)
          if(input$relativOUT){
            p1 <- plotly::plot_ly(tab_plot, x = as.formula(glue::glue("~{input$group1}")), y = ~meanGroup, type = 'bar', 
                  name = ~Miso, color = ~Miso, height = 500, colors = mycolors[1:length(levels(tab_plot$Miso))]) %>% 
                  plotly::layout(title=glue::glue("Isotopologue Fraction {input$feat2}"), yaxis = list(title = 'Isotopologue fraction'), 
                  barmode = "stack", xaxis = xform, barnorm = "fraction")
          }else{        
            p1 <- plotly::plot_ly(tab_plot, x = as.formula(glue::glue("~{input$group1}")), y = ~meanGroupAbs, type = 'bar', 
                  name = ~Miso, color = ~Miso, height = 500, colors = mycolors[1:length(levels(tab_plot$Miso))]) %>% 
                  plotly::layout(title=glue::glue('Raw area {input$feat2}'), yaxis = list(title = 'Raw area'), 
                  barmode = "stack", xaxis = xform, barnorm = "")
          }

          }

        }else{
          tab_plot <- mtab %>% filter(metabolite == input$feat2)

          if(input$dodge1){
            BARMOD <- "group"
          }else{BARMOD <- "stack"}

          xform <- list()
          if(input$relativOUT){
            p1 <- plotly::plot_ly(tab_plot, x = ~sample, y = ~corrected_area, type = 'bar', 
                  name = ~Miso, color = ~Miso, height = 500, colors = mycolors[1:length(levels(tab_plot$Miso))]) %>% 
                  plotly::layout(title=glue::glue("Isotopologue Fraction {input$feat2}"), yaxis = list(title = 'Isotopologue fraction'), 
                  barmode = BARMOD, xaxis = xform, barnorm = "fraction")
          }else{        
            p1 <- plotly::plot_ly(tab_plot, x = ~sample, y = ~corrected_area, type = 'bar', 
                  name = ~Miso, color = ~Miso, height = 500, colors = mycolors[1:length(levels(tab_plot$Miso))]) %>% 
                  plotly::layout(title=glue::glue('Raw area {input$feat2}'), yaxis = list(title = 'Raw area'), 
                  barmode = BARMOD, xaxis = xform)
          }
        }
        p1
      })

    output$histo_Aire_enrC13 <- renderPlot({
      req(r$merged2())
      mtab <- r$merged2()

      fun <- glue::glue("
      mtab <- mtab %>%
      dplyr::filter({input$group1} %in% input$sorted2) %>%
      droplevels() %>%
      mutate({input$group1} = factor({input$group1}, levels = input$sorted2))
      ")
      eval(parse(text=fun))

      mycolors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(20)
      xform <- list()

      CalculPerMerabolite <- mtab %>% group_by(sample) %>% group_by(metabolite, .add = TRUE) %>% 
      mutate(TotArea = sum(corrected_area), CID = 100 * corrected_area / sum(corrected_area), 
      EnrC13 = 100 * sum(Area_Iso)/(max(isotopologue) * sum(corrected_area)))

      cols2group <- c(input$group1)
      r$MeanSD_Area_EnrC13_per_compound <- MeanSD_Area_EnrC13_per_compound <- CalculPerMerabolite %>% group_by(across(all_of(cols2group)), .add = TRUE) %>%
          summarise(MeanTotalArea = mean(TotArea), SDTotalArea = sd(TotArea), 
          MeanEnrC13 = mean(EnrC13), SDEnrC13 = sd(EnrC13))

      cols2group <- c("metabolite", input$group1)
      r$MeanSD_Area_EnrC13_per_compound_groups <- MeanSD_Area_EnrC13_per_compound_groups <- MeanSD_Area_EnrC13_per_compound %>% ungroup() %>% 
          group_by(across(all_of(cols2group))) %>%
          summarise(MeanGroupArea = mean(MeanTotalArea, na.rm = TRUE), SDTotalArea = sd(MeanTotalArea, na.rm = TRUE), 
          MeanGroupEnrC13 = mean(MeanEnrC13, na.rm = TRUE), SDEnrC13 = sd(MeanEnrC13, na.rm = TRUE))

      if(input$group1 == "sample"){

        tabhisto <- MeanSD_Area_EnrC13_per_compound %>% filter(metabolite == input$feat2)

        p3_bar <- p3_bar1 <- ggplot(tabhisto, aes(x = sample, y = MeanEnrC13)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("EnrC13") +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("EnrC13 {input$feat2} all samples") )

        p4_bar <- p4_bar1 <- ggplot(tabhisto, aes(x = sample, y = MeanTotalArea)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("TotalArea") +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("TotalArea {input$feat2} all samples") )

      }else{

        tabhisto2 <- MeanSD_Area_EnrC13_per_compound_groups %>% filter(metabolite == input$feat2)

        p3_bar <- p3_bar_group <- ggplot(tabhisto2, aes(x = get(input$group1), y = MeanGroupEnrC13)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("Mean EnrC13") + xlab(input$group1) +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("EnrC13 {input$feat2} all groups")) +
                geom_errorbar(aes(ymin=MeanGroupEnrC13-SDEnrC13, ymax=MeanGroupEnrC13+SDEnrC13), width=.2,
                             position=position_dodge(.9)) 

        p4_bar <- p4_bar_group <- ggplot(tabhisto2, aes(x = get(input$group1), y = MeanGroupArea)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("Mean TotalArea") + xlab(input$group1) +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("TotalArea {input$feat2} all groups")) +
                geom_errorbar(aes(ymin=MeanGroupArea-SDTotalArea, ymax=MeanGroupArea+SDTotalArea), width=.2,
                             position=position_dodge(.9)) 

      }

      gridExtra::grid.arrange(p3_bar, p4_bar, nrow = 2)
    })

    output$histo_Aire_enrC13_allFeat_1group <- renderPlot({

      # pour chaque condition  metabolite en x
      MeanSD_Area_EnrC13_per_compound <- r$MeanSD_Area_EnrC13_per_compound
      tabhisto3 <- MeanSD_Area_EnrC13_per_compound %>% filter(!!as.symbol(input$group1) == input$level1)  %>% ungroup() %>% 
          group_by(metabolite) %>% 
          summarise(MeanEnrC13Group = mean(MeanEnrC13, na.rm = TRUE), MeanTotAreaGroup = mean(MeanTotalArea, na.rm = TRUE),
            sdEnrC13Group = sd(MeanEnrC13, na.rm = TRUE), sdTotAreaGroup = sd(MeanTotalArea, na.rm = TRUE))

      p3_bar_all_feats_1group <- ggplot(tabhisto3, aes(x = metabolite, y = MeanEnrC13Group)) +
            geom_bar(stat="identity", color="black", fill = "#b6bced",
                     position=position_dodge()) + 
              theme_bw() + ylab("EnrC13") +
            theme(legend.position = "None", 
              axis.text.x = element_text(
              angle = 45, hjust=1)) +
            ggtitle(glue::glue("EnrC13 {input$group1} == {input$level1} all metabolites")) +
              geom_errorbar(aes(ymin=MeanEnrC13Group-sdEnrC13Group, ymax=MeanEnrC13Group+sdEnrC13Group), width=.2,
                           position=position_dodge(.9)) 

      p4_bar_all_feats_1group <- ggplot(tabhisto3, aes(x = metabolite, y = MeanTotAreaGroup)) +
            geom_bar(stat="identity", color="black", fill = "#b6bced",
                     position=position_dodge()) + 
              theme_bw() + ylab("Total Area") +
            theme(legend.position = "None", 
              axis.text.x = element_text(
              angle = 45, hjust=1)) +
            ggtitle(glue::glue("Total Area {input$group1} == {input$level1} all metabolites")) +
              geom_errorbar(aes(ymin=MeanTotAreaGroup-sdTotAreaGroup, ymax=MeanTotAreaGroup+sdTotAreaGroup), width=.2,
                           position=position_dodge(.9)) 

      
      gridExtra::grid.arrange(p3_bar_all_feats_1group, p4_bar_all_feats_1group, nrow = 2)

    })

        




    pdfall_isoplot <- reactive({
      cat(file=stderr(), 'All Barplots ...', "\n")
      req(r$merged2())
      mtab <- r$merged2()

      fun <- glue::glue("
      mtab <- mtab %>%
      dplyr::filter({input$group1} %in% input$sorted2) %>%
      droplevels() %>%
      mutate({input$group1} = factor({input$group1}, levels = input$sorted2)) %>%
      as.data.frame()
      ")
      eval(parse(text=fun))

      LL <- list()
      mycolors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(20)

      col1 = mycolors[1:length(levels(mtab$Miso))]
      withProgress({

        mtab$Miso <- factor(mtab$Miso, rev(levels(mtab$Miso)))

        if(input$group1 == "sample"){
          for(i in unique(mtab$metabolite)){
            # incProgress(1/length(i))
            print(i)
            tab_plot <- as.data.frame(mtab) %>% filter(metabolite == i)

            if(input$dodge1){

              if(input$relativOUT){
                LL[[i]] <- ggplot(tab_plot, aes(fill=Miso, y=isotopologue_fraction, x=sample)) + 
                    geom_bar(position="dodge", stat="identity") + scale_fill_manual(values = rev(col1[1:length(levels(droplevels(tab_plot$Miso)))])) +
                    theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} CID")) +
                    xlab("") + ylab("CID") +
                    theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1))
              }else{
                LL[[i]] <- ggplot(tab_plot, aes(fill=Miso, y=corrected_area, x=sample)) + 
                    geom_bar(position="dodge", stat="identity") + scale_fill_manual(values = rev(col1[1:length(levels(droplevels(tab_plot$Miso)))])) +
                    theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} Area")) +
                    xlab("") + ylab("Area") +
                    theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1))
              }


            }else{

              if(input$relativOUT){
                LL[[i]] <- ggplot(tab_plot, aes(fill=Miso, y=corrected_area, x=sample)) + 
                    geom_bar(position="fill", stat="identity") + scale_fill_manual(values = rev(col1[1:length(levels(droplevels(tab_plot$Miso)))])) +
                    theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} CID")) +
                    xlab("") + ylab("CID") +
                    theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1))
              }else{
                LL[[i]] <- ggplot(tab_plot, aes(fill=Miso, y=corrected_area, x=sample)) + 
                    geom_bar(position="stack", stat="identity") + scale_fill_manual(values = rev(col1[1:length(levels(droplevels(tab_plot$Miso)))])) +
                    theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} Area")) +
                    xlab("") + ylab("Area") +
                    theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust=1))
              }
            }

          }

        }else{
          print("GROUP BY")

        # cols2group <- c("metabolite", "Miso", input$group1)
        # tab_plot4 <- mtab %>% group_by(across(all_of(cols2group))) %>% 
        #     summarise(meanGroup = mean(isotopologue_fraction), sdGroup = sd(isotopologue_fraction), .groups = "keep") %>% 
        #     arrange(as.character(Miso)) %>%
        #     arrange(across(c("metabolite",input$group1))) %>%
        #     group_by(across(c("metabolite",input$group1))) %>%
        #     mutate(SDPos = cumsum(meanGroup)) %>%
        #     as.data.frame()

        #   tab_plot4[which(tab_plot4$sdGroup == 0), "sdGroup"] <- NA
          
        #   tab_plot5 <- tab_plot4
        #   tab_plot5$Miso = factor(tab_plot5$Miso, levels = sort(levels(tab_plot5$Miso)) )

          

          if(input$dodge1){
            tab_plot5 <- r_values$tab_plot5
            for(i in unique(mtab$metabolite)){

              if(input$relativOUT){
                LL[[i]] <- ggplot(as.data.frame(tab_plot5) %>% filter(metabolite == i), aes(fill=Miso, y=meanGroup, x=get(input$group1))) + 
                  geom_bar(position="dodge", stat="identity") + scale_fill_manual(values = col1) +
                  theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} mean CID by '{input$group1}' factor")) + xlab("") + ylab("Mean Isotopologue fraction") +
                  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
                  geom_errorbar(aes(ymin = meanGroup-sdGroup, ymax = meanGroup+sdGroup), width = 0.3, position = position_dodge(0.9))

              }else{
                LL[[i]] <- ggplot(as.data.frame(tab_plot5) %>% filter(metabolite == i), aes(fill=Miso, y=meanGroupAbs, x=get(input$group1))) + 
                  geom_bar(position="dodge", stat="identity") + scale_fill_manual(values = col1) +
                  theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} mean Area by '{input$group1}' factor")) + xlab("") + ylab("Mean corrected area") +
                  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
                  geom_errorbar(aes(ymin = meanGroupAbs-sdGroupAbs, ymax = meanGroupAbs+sdGroupAbs), width = 0.3, position = position_dodge(0.9))

              }
            }

          }else{
            tab_plot4 <- r_values$tab_plot4
            tab_plot4$Miso = factor(tab_plot4$Miso, levels = rev(levels(tab_plot4$Miso)) )

            col2 <- rev(col1)
            names(col2) <- levels(tab_plot4$Miso)
        
            for(i in unique(mtab$metabolite)){
              print(i)
              if(input$relativOUT){
                LL[[i]] <- ggplot(as.data.frame(tab_plot4) %>% filter(metabolite == i), aes(fill=Miso, y=meanGroup, x=get(input$group1))) + 
                    geom_bar(position="stack", stat="identity") + scale_fill_manual(values = col2) +
                    theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} mean CID by '{input$group1}' factor")) +
                    xlab("") + ylab("Mean Isotopologue fraction") +
                    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
                    geom_linerange(aes(ymin = SDPos-sdGroup, ymax = SDPos+sdGroup), width = 0.1, position = position_jitter(0.1))            

                }else{
                LL[[i]] <- ggplot(as.data.frame(tab_plot4) %>% filter(metabolite == i), aes(fill=Miso, y=meanGroupAbs, x=get(input$group1))) + 
                    geom_bar(position="stack", stat="identity") + scale_fill_manual(values = col2) +
                    theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} mean Area by '{input$group1}' factor")) +
                    xlab("") + ylab("Mean corrected area") +
                    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
                    geom_linerange(aes(ymin = SDPosAbs-sdGroupAbs, ymax = SDPosAbs+sdGroupAbs), width = 0.1, position = position_jitter(0.1)) 

                  }

            }

          }


        }

      }, value = 0 ,message = glue::glue("Processing barplots ... please wait."))

      LL

    })



    output$hist_download <- downloadHandler(
      filename = glue::glue("isoplot_figures_{systim}.pdf"),
      content = function(file) {
        print('DOWNLOAD ALL')
          print("isoplot")
          req(pdfall_isoplot())
          p <- pdfall_isoplot()
            withProgress({
              ml <- marrangeGrob(p, nrow=2, ncol=1)

                # if(as.numeric(input$nbPicPage) == 4){
                #   ml <- marrangeGrob(p, nrow=2, ncol=2)
                #  }else if(as.numeric(input$nbPicPage) == 3){
                #   ml <- marrangeGrob(p, nrow= 1, ncol=as.numeric(input$nbPicPage))
                #   }else if(as.numeric(input$nbPicPage) == 2){
                #     if(input$verticaldisplay){
                #       ml <- marrangeGrob(p, nrow= as.numeric(input$nbPicPage), ncol= 1)
                #     }else{
                #       ml <- marrangeGrob(p, nrow= 1, ncol=as.numeric(input$nbPicPage))
                #     }
                #   }

              # ggsave(file, ml, units = "cm", width = 20, height = 15, dpi = 100)
              ggsave(file, ml , width = 11, height = 8, dpi = 100)
            }, message = "Prepare pdf file... please wait.")
        print('pdf output')
        

        
      }
    )

    output$hist_downloadTAR <- downloadHandler(
      filename <- glue::glue("{tmpdir}/figures_pngs.tar"), 

      content <- function(file) {
        print("WRITE PLOTS")
        print(glue::glue("{tmpdir}/figures_{systim}/"))
        dir.create(glue::glue("{tmpdir}/figures_{systim}/"), recursive = TRUE)


        req(pdfall_isoplot())
        listP <- pdfall_isoplot()

        FEAT = names(listP)

        withProgress({
          for(i in 1:length(FEAT)){
            incProgress(1/length(FEAT))
            ggsave(glue::glue("{tmpdir}/figures_{systim}/HistPlot_{FEAT[i]}.png"), listP[[FEAT[i]]], width = 30, height = 15, units = "cm")
          }

        }, value = 0, message = "Generating PNGs...")

        tar(glue::glue("{tmpdir}/figures_pngs.tar"), files = glue::glue("{tmpdir}/figures_{systim}") )


        file.copy(filename, file)
      },
      contentType = "application/tar"
    )



    pdfall_EnrC13_Area <- reactive({
      cat(file=stderr(), 'All Barplots EnrC13 Area ...', "\n")
      req(r$merged2())
      mtab <- r$merged2()
      LL <- list()

      withProgress({

      if(input$group1 == "sample"){
        mtab <- MeanSD_Area_EnrC13_per_compound <- r$MeanSD_Area_EnrC13_per_compound
        print(head(mtab))
        
        for(i in sort(unique(mtab$metabolite))){
          print("per sample")
          print(i)
        tabhisto <- MeanSD_Area_EnrC13_per_compound %>% filter(metabolite == i)

        LL[[glue::glue("{i}_enrC13")]] <- p3_bar <- ggplot(tabhisto, aes(x = sample, y = MeanEnrC13)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("EnrC13") +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("EnrC13 {i} all samples") )

        LL[[glue::glue("{i}_area")]] <- p4_bar <- ggplot(tabhisto, aes(x = sample, y = MeanTotalArea)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("TotalArea") +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("TotalArea {i} all samples") )

        }

      }else{
        mtab <- MeanSD_Area_EnrC13_per_compound_groups <- r$MeanSD_Area_EnrC13_per_compound_groups
        print(head(mtab))
        
        for(i in sort(unique(mtab$metabolite))){
          print("per group")
          print(i)
          tabhisto2 <- MeanSD_Area_EnrC13_per_compound_groups %>% filter(metabolite == i)

          LL[[glue::glue("{i}_enrC13")]] <- p3_bar_group <- ggplot(tabhisto2, aes(x = get(input$group1), y = MeanGroupEnrC13)) +
                geom_bar(stat="identity", color="black", fill = "#b6bced",
                         position=position_dodge()) + 
                  theme_bw() + ylab("Mean EnrC13") + xlab("")  + 
                theme(legend.position = "None", 
                  axis.text.x = element_text(
                  angle = 45, hjust=1)) +
                ggtitle(glue::glue("EnrC13 {i} all groups")) +
                  geom_errorbar(aes(ymin=MeanGroupEnrC13-SDEnrC13, ymax=MeanGroupEnrC13+SDEnrC13), width=.2,
                               position=position_dodge(.9)) 

          LL[[glue::glue("{i}_area")]] <- p4_bar_group <- ggplot(tabhisto2, aes(x = get(input$group1), y = MeanGroupArea)) +
                geom_bar(stat="identity", color="black", fill = "#b6bced",
                         position=position_dodge()) + 
                  theme_bw() + ylab("Mean TotalArea") + xlab("")  +
                theme(legend.position = "None", 
                  axis.text.x = element_text(
                  angle = 45, hjust=1)) +
                ggtitle(glue::glue("TotalArea {i} all groups")) +
                  geom_errorbar(aes(ymin=MeanGroupArea-SDTotalArea, ymax=MeanGroupArea+SDTotalArea), width=.2,
                               position=position_dodge(.9)) 

        }


      }
    }, value = 0 ,message = glue::glue("Processing barplots ... please wait."))


  LL
  })


  output$bars_download <- downloadHandler(
    filename = glue::glue("isoplot_figures_bars_{systim}.pdf"),
    content = function(file) {
      print('DOWNLOAD ALL')
        print("bars")
        req(pdfall_EnrC13_Area())
        p <- pdfall_EnrC13_Area()
          withProgress({
            ml <- marrangeGrob(p, nrow=2, ncol=1)

            ggsave(file, ml , width = 11, height = 8, dpi = 100)
          }, message = "Prepare pdf file... please wait.")
      print('pdf output')
      

      
    }
  )

      pdfall_EnrC13_Area_spec <- reactive({
      cat(file=stderr(), 'All Barplots EnrC13 Area ...', "\n")
      req(r$merged2())
      LL <- list()


      # pour chaque condition  metabolite en x
      mtab <- MeanSD_Area_EnrC13_per_compound <- r$MeanSD_Area_EnrC13_per_compound
      # for i in all groups from chosen factor
      withProgress({
      for(i in levels(as.data.frame(mtab)[,input$group1])) {
        print(input$group1)
        print(i)
        tabhisto3 <- MeanSD_Area_EnrC13_per_compound %>% filter(!!as.symbol(input$group1) == i)  %>% ungroup() %>% 
            group_by(metabolite) %>% 
            summarise(MeanEnrC13Group = mean(MeanEnrC13, na.rm = TRUE), MeanTotAreaGroup = mean(MeanTotalArea, na.rm = TRUE),
              sdEnrC13Group = sd(MeanEnrC13, na.rm = TRUE), sdTotAreaGroup = sd(MeanTotalArea, na.rm = TRUE))

        LL[[glue::glue("{i}_enrC13")]] <- p3_bar_all_feats_1group <- ggplot(tabhisto3, aes(x = metabolite, y = MeanEnrC13Group)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("EnrC13") +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("EnrC13 {input$group1} == {i} all metabolites")) +
                geom_errorbar(aes(ymin=MeanEnrC13Group-sdEnrC13Group, ymax=MeanEnrC13Group+sdEnrC13Group), width=.2,
                             position=position_dodge(.9)) 

        LL[[glue::glue("{i}_area")]] <- p4_bar_all_feats_1group <- ggplot(tabhisto3, aes(x = metabolite, y = MeanTotAreaGroup)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("Total Area") +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("Total Area {input$group1} == {i} all metabolites")) +
                geom_errorbar(aes(ymin=MeanTotAreaGroup-sdTotAreaGroup, ymax=MeanTotAreaGroup+sdTotAreaGroup), width=.2,
                             position=position_dodge(.9))

      }
      }, message = "Processing barplots ... please wait.")

      LL



    })


  output$bars_spec_download <- downloadHandler(
    filename = glue::glue("isoplot_figures_bars_spec_{systim}.pdf"),
    content = function(file) {
      print('DOWNLOAD ALL')
        print("bars")
        req(pdfall_EnrC13_Area_spec())
        p <- pdfall_EnrC13_Area_spec()
          withProgress({
            ml <- marrangeGrob(p, nrow=2, ncol=1)

            ggsave(file, ml , width = 11, height = 8, dpi = 100)
          }, message = "Prepare pdf file... please wait.")
      print('pdf output')
        
    }
  )




  })
}
    
## To be copied in the UI
# mod_plots_isot_ui("plots_isot_1")
    
## To be copied in the server
# mod_plots_isot_server("plots_isot_1")
