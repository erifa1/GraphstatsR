#' gg_color_hue
#' @param n a numeric
#' @keywords internal
#' 
#' 

smart_label <- function(x, digits = 3) {
  ifelse(
    abs(x) >= 1e4 | abs(x) < 1e-3,
    formatC(x, format = "e", digits = digits),
    formatC(x, format = "f", digits = digits, drop0trailing = TRUE)
  )
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#' replace_mu
#' @param x a dataframe
#' @keywords internal
replace_mu <- function(x){
  for(i in 1:ncol(x)){
    if(is.factor(x[,i])){
      x[,i] <- as.factor(gsub("microg", "\u00b5g", x[,i]))
      x[,i] <- as.factor(gsub("microM", "\u00b5M", x[,i]))
      x[,i] <- as.factor(gsub("\xb5", "\u00b5", x[,i]))
    }
  }
  return(x)
}

#' preview 10 lines 10columns max
#' @param x a dataframe
#' @keywords internal
prev <- function(x){
  if(nrow(x)>10){nr = 10}else{nr = nrow(x)}
  if(ncol(x)>10){nc = 10}else{nc = ncol(x)}
  x[1:nr,1:nc]
}

#' Detect outlier from numeric vector
#' @param x a vector
#' @keywords internal
is_outlier <- function(x) {
vec0 = x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE) | x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE)
vec0[is.na(vec0)] <- FALSE
return(vec0)
}


# Raised by devtools::check()
globalVariables(c("Area_Iso", "EnrC13", "IQR", "MeanEnrC13", "MeanEnrC13Group", "MeanGroupArea", "MeanGroupEnrC13", "MeanTotAreaGroup", "MeanTotalArea", "Miso", "SDEnrC13", "SDPos", "SDPosAbs", "SDTotalArea", "TotArea", "as.formula", "axis", "bmp", "boxplot", "corrected_area", "dev.off", "features", "ggstats", "grid", "hcl", "head", "isotopologue", "isotopologue_fraction", "jpeg", "meanGroup", "meanGroupAbs", "metabolite", "na.omit", "outlier", "p.adjust", "pairwise.wilcox.test", "par", "pdf", "png", "pvalue", "quantile", "read.csv", "rowname", "sd", "sdEnrC13Group", "sdGroup", "sdGroupAbs", "sdTotAreaGroup", "str", "tar", "text", "tiff", "type", "value", "write.csv", "write.table"))