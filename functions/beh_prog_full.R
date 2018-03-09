#' Function created for reshaeping anc subsetting original data frame storing 
#' data of behavioral performance during dual n-back task across 4 scanning sessions, 
#' Funtion returns data frane which is convenient for further analyses and visualization. 
#'
#' @param x The data frame with all data in wide format (data.frame)
#' @param modality The modality of interest - "ns" (spatial n-back) /"na" (audo n-back) / 
#' "both" (mean audio and spatial n-back(character)
#' @param measure The measure of interest - "pRT" (penalized reaction time) / "dp" (d-prime) / 
#' "acc" (accuracy)  (character)
#' @param n The n-back level of interest - "1-back" / "2-back" / "delta" (2-back - 1-back) /  (character)
#' @return A data.frame in long format restricted to variables of interest
#
beh_prog_full <- function(x, modality, measure, n){
  if(modality != "both"){
    beh_prog <- single_mod_beh_prog(x, modality, measure, n)
  }
  
  else {
    na <- single_mod_beh_prog(x, "na", measure, n)
    ns <- single_mod_beh_prog(x, "ns", measure, n)
    
    means <- rowMeans(cbind(na[, 6], ns[, 6]))
    na[, 6] <- means
    beh_prog <- na
    beh_prog$modality <- "both"
    return(beh_prog)
    
  }
}
