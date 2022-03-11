#' replace_mu
#' @param x a dataframe
#' 

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
#' 
prev <- function(x){
  if(nrow(x)>10){nr = 10}else{nr = nrow(x)}
  if(ncol(x)>10){nc = 10}else{nc = ncol(x)}
  x[1:nr,1:nc]
}

#' Detect outlier from numeric vector
#' @param x a vector
#' 
is_outlier <- function(x) {
vec0 = x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE) | x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE)
vec0[is.na(vec0)] <- FALSE
return(vec0)
}