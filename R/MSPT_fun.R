#' @title MSPT function
#' @name MSPT_fun
#' @description This function calculates the isotopologue fraction of each metabolite and compares it with the theoretical isotopologue fraction. It generates a table with the results and barplots of the experimental and theoretical isotopologue fraction of each metabolite with error bars.
#' 
#' @param path Path to the input file (TSV format)
#' @param p Natural abundance of 13C (default = 0.513)
#' @param outpath Path to the output directory (default = "./MSPT_out/"), if NULL, no output is generated
#' @param minCID Minimum value of CID to be considered (default = 0.02)
#' @param maxBias Maximum value of bias to be considered (default = 5)
#' @param plotLowCID Logical, if TRUE, plot the isotopologues with CID < minCID (default = FALSE)
#' @return 
#' A list containing the following elements:
#' 
#' \itemize{
#' 
#' \item \strong{Table} A data frame containing the results of the calculations (sample, metabolite, isotopologue, area, Total_Area, Ratio, Theoretical_Ratios, Mean_Ratios, Mean_Ratios_SD, Thresholds, Bias (%), Mean Bias (%), Mean Bias SD (%))
#' \item \strong{figures} A list of ggplot objects containing the barplots of the experimental and theoretical isotopologue fraction of each metabolite with error bars
#'}
#'
#' @examples 
#' \dontrun{
#' file <- glue::glue(system.file(package = "graphstatsr"), "/dataset/MSPT_test.tsv")
#' res <- MSPT_fun(path = file, outpath = NULL)
#' str(res, max.level = 2)
#' }
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @import openxlsx
#' @export




MSPT_fun <- function(path, p=0.513, outpath = "./MSPT_out/", minCID = 0.02, maxBias = 5){
  LL <- list()

  input_data <- rio::import(path)
  # Calculate CID and theoretical isotopologue fraction
  th_data <- input_data %>% mutate(Miso = as.factor(glue::glue("M{stringr::str_pad(input_data$isotopologue, 2, pad = '0')}"))) %>%
    group_by(sample, metabolite) %>%
    mutate(CID = corrected_area/sum(corrected_area), mean_area_persample = mean(area),
           nCarbon = max(isotopologue)) %>%
    ungroup() %>%
    mutate(theoretical_isotopologue_fraction = theoretical_abundances(nCarbon, isotopologue, p),
           .after = isotopologue_fraction) %>%
    data.frame()
  # Calculate bias and cancel value when CID < 0.02
  A3 <- th_data %>%
    mutate(bias = abs(CID - theoretical_isotopologue_fraction) * 100, .after = theoretical_isotopologue_fraction)  %>%
    mutate(bias = ifelse(CID < minCID, NA, bias)) %>% group_by(sample, metabolite) %>%
    mutate(meanBias = mean(bias, na.rm = TRUE), sdBias = sd(bias, na.rm = TRUE), .after = bias)
  meanA3 <- A3 %>% group_by(metabolite, Miso) %>%
    summarise(MeanBias = mean(bias, na.rm = TRUE), SDBias = sd(bias, na.rm = TRUE),
              MeanCID = mean(CID, na.rm = TRUE), SDCID = sd(CID, na.rm = TRUE),
              MeanThCID = mean(theoretical_isotopologue_fraction, na.rm = TRUE),
              SDThCID = sd(theoretical_isotopologue_fraction, na.rm = TRUE))
  #  Here filter (or not) cid < 0.02 for ploting / postponed dev 
  pivot_meanA3 <- meanA3 %>% tidyr::pivot_longer(cols = c(MeanCID, MeanThCID),
                                                 names_to = "Type", values_to = "Value") %>%
    mutate(SDCID = ifelse(Type == "MeanThCID", NA, SDCID))
  # Output results table
  OutA3 <- A3 %>% select( sample, metabolite, Miso, area,  CID, theoretical_isotopologue_fraction, bias, meanBias, sdBias) %>%
    mutate(Total_Area = sum(area), .after = area) %>%
    group_by(metabolite, Miso) %>%
    mutate(Mean_Ratios = mean(CID, na.rm = TRUE), Mean_Ratios_SD = sd(CID, na.rm = TRUE), .after = theoretical_isotopologue_fraction) %>%
    mutate(Theshold = ifelse(CID > minCID, 1, 0), .after = Mean_Ratios_SD) #%>%
  # as.data.frame() %>% head()
  names(OutA3) <- c("sample",	"metabolite",	"isotopologue",	"area",	"Total_Area",	"Ratio",	"Theoretical_Ratios",	"Mean_Ratios",	"Mean_Ratios_SD",	"Thresholds",	"Bias (%)",	"Mean Bias (%)",	"Mean Bias SD (%)")
  wb <- createWorkbook()
  addWorksheet(wb, "sheet1")
  writeData(wb, sheet = "sheet1", x = OutA3)
  conditionalFormatting(wb, "sheet1",
                        cols = 12,
                        rows = 2:nrow(OutA3),
                        rule = glue::glue(">={maxBias}"),
                        style = createStyle(fontColour = "#C01B17", bgFill = "#FFC7CE")
  )
  conditionalFormatting(wb, "sheet1",
                        cols = 12,
                        rows = 2:(nrow(OutA3)+1),
                        rule = glue::glue("<{maxBias}"),
                        style = createStyle(fontColour = "#006432", bgFill = "#C6EFCE")
  )
  style_bold <- createStyle(textDecoration = "bold")
  addStyle(wb, sheet = "sheet1", style = style_bold, rows = 1, cols = 1:ncol(OutA3), gridExpand = TRUE)
  
  # Barplot of experimental and theoretical isotopologue fraction of each metabolite with error bars

  # pdf("rplot.pdf")
  for(metabo in unique(OutA3$metabolite)){
    p2_bar <- pivot_meanA3 %>% filter(metabolite == metabo) %>%
      ggplot(aes(x = Miso, y = Value, fill = Type)) +
      geom_bar(stat="identity",
               position=position_dodge()) +
      theme_bw() +
      geom_errorbar(aes(ymin=Value-SDCID, ymax=Value+SDCID), width=.2,
                    position=position_dodge(.9)) +
      ggrepel::geom_text_repel(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), vjust = -0.25) +
      ggtitle(metabo) + ylab("Recorded Area") + xlab("") + theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "") + scale_fill_manual(values = c("#1f77b4", "#ff7f0e"),
                                          labels = c("Experimental", "Theory"))
    print(p2_bar)
    LL$figures[[metabo]] <- p2_bar
  }

  # OUTPUT
  if(!is.null(outpath) && outpath != ""){
    dir.create(outpath, showWarnings = FALSE, recursive = TRUE)

    ml <- marrangeGrob(LL$figures, nrow=2, ncol=1)
    ggsave(glue::glue("{outpath}/TP_figures.pdf"), ml , width = 11, height = 8, dpi = 200)

    write.csv(OutA3, glue::glue("{outpath}/TP_results.csv"), row.names = FALSE)

    saveWorkbook(wb, glue::glue("{outpath}/TP_results.xlsx"), overwrite = TRUE)
  }

  LL$Table <- OutA3
  LL$workbook <- wb

  return(LL)

}




#' @title Theoretical abundance calculation function
#' @name theoretical_abundances
#' @description Calcuate the theoretical abundance of isotopologues
#' 
#' @param n Number of carbons in the molecule
#' @param k Number of the isotopologue
#' @param p Abundance of labeled precursor
#' 
#' @return Theoretical abundance of isotopologues
#' @keywords internal
#' #' @examples 
#' res <- theoretical_abundances(10, 0:10, 0.513); res
#' plot(res)

theoretical_abundances <- function(n, k, p){
  res = choose(n, k) * (p ^ k) * ((1 - p) ^ (n - k))
  return(res)
}

