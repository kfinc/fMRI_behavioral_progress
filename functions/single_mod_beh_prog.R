#' Function created for reshaeping anc subsetting original data frame storing 
#' data of behavioral performance during dual n-back task across 4 scanning sessions, 
#' Funtion returns data frane which is convenient for further analyses and visualization. 
#'
#' @param x The data frame with all data in wide format (data.frame)
#' @param modality The modality of interest - "ns" (spatial n-back) /"na" (audo n-back) (character)
#' @param measure The measure of interest - "pRT" (penalized reaction time) / "dp" (d-prime) / 
#' "acc" (accuracy)  (character)
#' @param n The n-back level of interest - "1-back" / "2-back" / "delta" (2-back - 1-back) /  (character)
#' @return A data.frame in long format restricted to variables of interest
#

single_mod_beh_prog <- function(x, modality, measure, n){
  
  selected_const <- grepl("ID|group|row|case", names(x))
  selected_condition <- grepl(modality, names(x))
  selected_measure <- grepl(measure, names(x))
  selected <- (selected_condition & selected_measure == TRUE) | selected_const
  beh_sel <- x[, selected]
  
  # Reshaping data into long format
  
  nb1_long <- reshape(beh_sel, timevar = "session", varying = list(names(beh_sel)[grep("1b", names(beh_sel))]),
                      idvar = "ID", direction = "long")
  nb2_long <- reshape(beh_sel, timevar = "session", varying = list(names(beh_sel)[grep("2b", names(beh_sel))]),
                      idvar = "ID", direction = "long")
  nb1_long$nback <- "1-back"
  nb2_long$nback <- "2-back"
  
  nb_long <- rbindlist(list(nb1_long, nb2_long))
  nb_long <- subset(nb_long, select = -c(2:5))
  
  
  if (n == "delta"){
    delta <- nb2_long[, 10] - nb1_long [, 10] 
    nb_long <- subset(nb1_long, select = -c(2:5))
    nb_long[, 6] <- delta
    names(nb_long)[6] <- paste0("delta_", measure)
    nb_long <- data.table(nb_long)
    nb_long$session <- as.factor(nb_long$session)
    nb_long$nback <- "delta"
    nb_long$modality <- modality
    
    return(nb_long)
  }
  else {
    names(nb_long)[6] <- measure 
    nb_long$session <- as.factor(nb_long$session)
    nb_long$modality <- modality
    
    return(nb_long[nb_long$nback == n])
  }
  
}

