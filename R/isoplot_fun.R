#' @title IsoPlot function
#' @name IsoPlot_fun
#' @description A function to plot isotopic data from metabolomic experiments
#' 
#' @param feat_table Features table from IsoCor (csv, tsv)
#' @param metadata_table Metadata table (csv, tsv)
#' @param groups Grouping factor
#' @param relativeCID Stacked barplot of CID with relative value (TRUE) or absolute value (FALSE)
#' @param dodgeCID Stacked barplot of CID with dodge position (TRUE) or stack position (FALSE)
#' @param outpath Output path
#' @param pdf Output PDF (TRUE) or not (FALSE)
#' @param img Output JPG (TRUE) or not (FALSE)
#' @return 
#' A list containing the following elements:
#' 
#' \itemize{
#' 
#' \item \strong{Isoplot} A list of ggplot objects containing the barplots of the experimental isotopologue fraction of each metabolite with error bars
#' \item \strong{EnrC13_TotArea} A list of ggplot objects containing the barplots of the mean EnrC13 and Total Area of each metabolite (one figure per metabolite)
#' \item \strong{EnrC13_TotArea_allMet} A list of ggplot objects containing the barplots of the mean EnrC13 and Total Area of all metabolites (one figure per sample or group of samples)
#'}
#'
#' @examples 
#' \dontrun{
#' feat <- glue::glue(system.file(package = "graphstatsr"), "/dataset/isoplot_quantification_table.csv")
#' metadata <- glue::glue(system.file(package = "graphstatsr"), "/dataset/isoplot_metadata.csv")
#' res <- IsoPlot_fun(feat_table = feat, metadata_table = metadata, groups = "sample", 
#' 	relativeCID = TRUE, outpath = "./isoplot_results")
#' 
#' }
#' 
#' @export


IsoPlot_fun <- function(feat_table = NULL, metadata_table = NULL, groups = "sample", relativeCID = TRUE, dodgeCID = FALSE, outpath = "./isoplot_results", pdf = TRUE, img = TRUE){

mycolors <- c("#A6CEE3","#001287","#579CC7","#70ff6b","#40A635","#89CB6C","#0c6e09","#EB494A","#F99392","#ad1203","#FDA746","#f5f54e","#FE8205","#cc8b2f","#8a0da6","#BFA5CF","#8861AC","#E7E099","#DEB969","#B15928")

if(is.null(feat_table) || is.null(metadata_table)){
	stop("The features table / metadata table is not defined")
}

ds0 <- data.table::fread(feat_table)

mt0 <- data.table::fread(metadata_table)

if(is.null(groups)){
	stop("The grouping factor is not defined")
}
if (!groups %in% colnames(mt0)) {
	stop(glue::glue("The grouping factor '{groups}' is not a column in the metadata table : {paste(colnames(mt0), collapse = ', ')}"))
}

# Ensure the levels of the groups column in mt0 are in the original order provided in the metadata table
mt0[[groups]] <- factor(mt0[[groups]], levels = unique(mt0[[groups]]) )

	# Preprocessing
	Calcul <- ds0 %>% mutate(Miso = as.factor(glue::glue("M{stringr::str_pad(ds0$isotopologue, 2, pad = '0')}"))) %>%
	mutate(Area_Iso = corrected_area * isotopologue) %>% group_by(sample, metabolite) %>% 
	mutate(mean_area_persample = mean(corrected_area)) %>% 
	# ungroup() %>% group_by(metabolite) %>% 
	mutate(maxIso = max(isotopologue)) %>%
	data.frame() #%>% head()

	# Merge with metadata
	mtab <- Fdataset <- Calcul %>% 
	dplyr::left_join(x = mt0, by = "sample")

	CalculPerMerabolite <- mtab %>% group_by(sample) %>% group_by(metabolite, .add = TRUE) %>% 
      mutate(TotArea = sum(corrected_area), CID = 100 * corrected_area / sum(corrected_area), 
      EnrC13 = 100 * sum(Area_Iso)/(max(isotopologue) * sum(corrected_area)))

	MeanSD_Area_EnrC13_per_compound <- CalculPerMerabolite %>% group_by(across(all_of(groups)), .add = TRUE) %>%
          summarise(MeanTotalArea = mean(TotArea), SDTotalArea = sd(TotArea), 
          MeanEnrC13 = mean(EnrC13), SDEnrC13 = sd(EnrC13))

	MeanSD_Area_EnrC13_per_compound_groups <- MeanSD_Area_EnrC13_per_compound %>% ungroup() %>% 
          group_by(across(all_of(c("metabolite", groups)))) %>%
          summarise(MeanGroupArea = mean(MeanTotalArea, na.rm = TRUE), SDTotalArea = sd(MeanTotalArea, na.rm = TRUE), MeanGroupEnrC13 = mean(MeanEnrC13, na.rm = TRUE), SDEnrC13 = sd(MeanEnrC13, na.rm = TRUE))


	cat(file=stderr(), 'All Barplots ...', "\n")

	LL <- list(); LL2 <- list() ; LL3 <- list()

	col1 = mycolors[1:length(levels(mtab$Miso))]

	mtab$Miso <- factor(mtab$Miso, rev(levels(mtab$Miso)))

	if(groups == "sample"){
		cat(file=stderr(), glue::glue('	All samples ...'), "\n")
		for(i in unique(mtab$metabolite)){
		# incProgress(1/length(i))
		# print(i)
		tab_plot <- as.data.frame(mtab) %>% filter(metabolite == i)

		if(dodgeCID){

			if(relativeCID){
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

			if(relativeCID){
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

	# Mean EnrC13 / Total Area
	cat(file=stderr(), glue::glue('	EnrC13 / TotArea ...'), "\n")
        for(i in sort(unique(mtab$metabolite))){
        tabhisto <- MeanSD_Area_EnrC13_per_compound %>% filter(metabolite == i)

        LL2[[glue::glue("{i}_enrC13")]] <- p3_bar <- ggplot(tabhisto, aes(x = sample, y = MeanEnrC13)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("EnrC13") +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("EnrC13 {i} all samples") )

        LL2[[glue::glue("{i}_area")]] <- p4_bar <- ggplot(tabhisto, aes(x = sample, y = MeanTotalArea)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("TotalArea") +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("TotalArea {i} all samples") )

        }

	}else{
		cat(file=stderr(), glue::glue('	Grouping based on "{groups}" ...'), "\n")
		cols2group <- c("metabolite", "Miso", groups)
		tab_plot4 <- mtab %>% group_by(across(all_of(cols2group))) %>% 
            summarise(meanGroup = mean(isotopologue_fraction), sdGroup = sd(isotopologue_fraction),
              meanGroupAbs = mean(corrected_area), sdGroupAbs = sd(corrected_area), .groups = "keep") %>% 
            arrange(as.character(Miso)) %>%
            arrange(across(c("metabolite",groups))) %>%
            group_by(across(c("metabolite",groups))) %>%
            mutate(SDPos = cumsum(meanGroup), SDPosAbs = cumsum(meanGroupAbs)) %>%
            as.data.frame()
		
		tab_plot4[which(tab_plot4$sdGroup == 0), "sdGroup"] <- NA
		tab_plot4$Miso = factor(tab_plot4$Miso, levels = sort(levels(tab_plot4$Miso)) )
		
		if(dodgeCID){
		for(i in unique(mtab$metabolite)){

			if(relativeCID){
			LL[[i]] <- ggplot(as.data.frame(tab_plot4) %>% filter(metabolite == i), aes(fill=Miso, y=meanGroup, x=get(groups))) + 
				geom_bar(position="dodge", stat="identity") + scale_fill_manual(values = col1) +
				theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} mean CID by '{groups}' factor")) + xlab("") + ylab("Mean Isotopologue fraction") +
				theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
				geom_errorbar(aes(ymin = meanGroup-sdGroup, ymax = meanGroup+sdGroup), width = 0.3, position = position_dodge(0.9))

			}else{
			LL[[i]] <- ggplot(as.data.frame(tab_plot4) %>% filter(metabolite == i), aes(fill=Miso, y=meanGroupAbs, x=get(groups))) + 
				geom_bar(position="dodge", stat="identity") + scale_fill_manual(values = col1) +
				theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} mean Area by '{groups}' factor")) + xlab("") + ylab("Mean corrected area") +
				theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
				geom_errorbar(aes(ymin = meanGroupAbs-sdGroupAbs, ymax = meanGroupAbs+sdGroupAbs), width = 0.3, position = position_dodge(0.9))

			}
		}

		}else{
			tab_plot4$Miso = factor(tab_plot4$Miso, levels = rev(levels(tab_plot4$Miso)) )

			col2 <- rev(col1)
			names(col2) <- levels(tab_plot4$Miso)

			for(i in unique(mtab$metabolite)){
				# print(i)
				if(relativeCID){
				LL[[i]] <- ggplot(as.data.frame(tab_plot4) %>% filter(metabolite == i), aes(fill=Miso, y=meanGroup, x=get(groups))) + 
					geom_bar(position="stack", stat="identity") + scale_fill_manual(values = col2) +
					theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} mean CID by '{groups}' factor")) +
					xlab("") + ylab("Mean Isotopologue fraction") +
					theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
					geom_linerange(aes(ymin = SDPos-sdGroup, ymax = SDPos+sdGroup), width = 0.1, position = position_jitter(0.1))            

				}else{
				LL[[i]] <- ggplot(as.data.frame(tab_plot4) %>% filter(metabolite == i), aes(fill=Miso, y=meanGroupAbs, x=get(groups))) + 
					geom_bar(position="stack", stat="identity") + scale_fill_manual(values = col2) +
					theme_bw() + labs(fill='') + ggtitle(glue::glue("{i} mean Area by '{groups}' factor")) +
					xlab("") + ylab("Mean corrected area") +
					theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
					geom_linerange(aes(ymin = SDPosAbs-sdGroupAbs, ymax = SDPosAbs+sdGroupAbs), width = 0.1, position = position_jitter(0.1)) 

					}

			}

		}


	#Mean EnrC13 + Total Area groups
	# save(list = ls(all.names = TRUE), file = "~/Bureau/tmp/debug.rdata", envir = environment()); print("SAVE0")
        for(i in sort(unique(mtab$metabolite))){
        #   print("per group")
        #   print(i)
          tabhisto2 <- MeanSD_Area_EnrC13_per_compound_groups %>% filter(metabolite == i)

          LL2[[glue::glue("{i}_enrC13")]] <- p3_bar_group <- ggplot(tabhisto2, aes(x = get(groups), y = MeanGroupEnrC13)) + geom_bar(stat="identity", color="black", fill = "#b6bced", position=position_dodge()) +
                theme_bw() + ylab("Mean EnrC13") + xlab("")  + 
                theme(legend.position = "None", axis.text.x = element_text(angle = 45, hjust=1)) +
                ggtitle(glue::glue("EnrC13 {i} all groups")) +
                geom_errorbar(aes(ymin=MeanGroupEnrC13-SDEnrC13, ymax=MeanGroupEnrC13+SDEnrC13), width=.2,position=position_dodge(.9)) 

          LL2[[glue::glue("{i}_area")]] <- p4_bar_group <- ggplot(tabhisto2, aes(x = get(groups), y = MeanGroupArea)) + geom_bar(stat="identity", color="black", fill = "#b6bced", position=position_dodge()) + 
                theme_bw() + ylab("Mean TotalArea") + xlab("")  +
                theme(legend.position = "None", axis.text.x = element_text(angle = 45, hjust=1)) +
                ggtitle(glue::glue("TotalArea {i} all groups")) +
                geom_errorbar(aes(ymin=MeanGroupArea-SDTotalArea, ymax=MeanGroupArea+SDTotalArea), width=.2,position=position_dodge(.9)) 

        }


	}

	# All metabolites EnrC13 / Total Area

      for(i in unique(as.data.frame(MeanSD_Area_EnrC13_per_compound)[,groups])) {
        tabhisto3 <- MeanSD_Area_EnrC13_per_compound %>% filter(!!as.symbol(groups) == i)  %>% ungroup() %>% 
            group_by(metabolite) %>% 
            summarise(MeanEnrC13Group = mean(MeanEnrC13, na.rm = TRUE), MeanTotAreaGroup = mean(MeanTotalArea, na.rm = TRUE),
              sdEnrC13Group = sd(MeanEnrC13, na.rm = TRUE), sdTotAreaGroup = sd(MeanTotalArea, na.rm = TRUE))

        LL3[[glue::glue("{i}_enrC13")]] <- p3_bar_all_feats_1group <- ggplot(tabhisto3, aes(x = metabolite, y = MeanEnrC13Group)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("EnrC13") +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("EnrC13 {groups} == {i} all metabolites")) +
                geom_errorbar(aes(ymin=MeanEnrC13Group-sdEnrC13Group, ymax=MeanEnrC13Group+sdEnrC13Group), width=.2,
                             position=position_dodge(.9)) 

        LL3[[glue::glue("{i}_area")]] <- p4_bar_all_feats_1group <- ggplot(tabhisto3, aes(x = metabolite, y = MeanTotAreaGroup)) +
              geom_bar(stat="identity", color="black", fill = "#b6bced",
                       position=position_dodge()) + 
                theme_bw() + ylab("Total Area") +
              theme(legend.position = "None", 
                axis.text.x = element_text(
                angle = 45, hjust=1)) +
              ggtitle(glue::glue("Total Area {groups} == {i} all metabolites")) +
                geom_errorbar(aes(ymin=MeanTotAreaGroup-sdTotAreaGroup, ymax=MeanTotAreaGroup+sdTotAreaGroup), width=.2,
                             position=position_dodge(.9))

      }



	if(pdf){
		cat(file=stderr(), '	Output PDF ...', "\n")
		dir.create(outpath, showWarnings = FALSE, recursive = TRUE)
		ml <- marrangeGrob(LL, nrow=2, ncol=1)
		ggsave(glue::glue("{outpath}/01_CID_plot.pdf"), ml , width = 11, height = 8, dpi = 100)

		ml <- marrangeGrob(LL2, nrow=2, ncol=1)
		ggsave(glue::glue("{outpath}/02_EnrC13_TotArea_plot.pdf"), ml , width = 11, height = 8, dpi = 100)

		ml <- marrangeGrob(LL3, nrow=2, ncol=1)
		ggsave(glue::glue("{outpath}/03_EnrC13_TotArea_plot_allMet.pdf"), ml , width = 11, height = 8, dpi = 100)
	}

	if(img){
		cat(file=stderr(), '	Output JPG ...', "\n")
		cat(file=stderr(), '		CID ...', "\n")
		dir.create(glue::glue("{outpath}/01_isoplot"), showWarnings = FALSE, recursive = TRUE)
		for (i in names(LL)) {
			ggsave(glue::glue("{outpath}/01_isoplot/{i}_CID_plot.jpg"), LL[[i]], width = 11, height = 8, dpi = 100)
		}

		cat(file=stderr(), '		EnrC13/TotArea ...', "\n")
		dir.create(glue::glue("{outpath}/02_EnrC13_TotArea"), showWarnings = FALSE, recursive = TRUE)
		for (i in names(LL2)) {
			ggsave(glue::glue("{outpath}/02_EnrC13_TotArea/{i}_plot.jpg"), LL2[[i]], width = 11, height = 8, dpi = 100)
		}

		cat(file=stderr(), '		EnrC13/TotArea all Met ...', "\n")
		dir.create(glue::glue("{outpath}/03_EnrC13_TotArea_plot"), showWarnings = FALSE, recursive = TRUE)
		for (i in names(LL3)) {
			ggsave(glue::glue("{outpath}/03_EnrC13_TotArea_plot/{i}_plot.jpg"), LL3[[i]], width = 11, height = 8, dpi = 100)
		}


	}

	cat(file=stderr(), '	Done ...', "\n")
	return(list(IsoPlot = LL, EnrC13_TotArea = LL2, EnrC13_TotArea_allMet = LL3))

}

