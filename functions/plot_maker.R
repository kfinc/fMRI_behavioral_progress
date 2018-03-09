#' Makes plots representing group x session interaction.
#' Data: behavioral performance during dual n-back task across 4 scanning session.
#' 
#' @param df A data.frame in long format
#' @return Plot (group x session)
#


plot_maker <- function(df){
  
  if(length(grep("dp", names(df), value = TRUE)) == 1){
    measure <- "d-prime"
  }
  
  if(length(grep("pRT", names(df), value = TRUE)) == 1){
    measure <- "pRT"
  }
  
  if( length(grep("acc", names(df), value = TRUE)) == 1){
    measure <- "accuracy"
  }
  
  
  if("ns" %in% df$modality == TRUE){
    task_name <- "spatial"
  }
  
  if("na" %in% df$modality == TRUE){
    task_name <- "audio"
  }
  
  if("both" %in% df$modality == TRUE){
    task_name <- "spatial & audio"
  }
  
  if("delta" %in% df$nback == TRUE){
    y <- substitute(paste(Delta, " ", measure, " (2-back - 1-back)"))
  }
  
  if("1-back" %in% df$nback == TRUE){
    y <- paste0(measure, " (1-back)")
  }
  
  if("2-back" %in% df$nback == TRUE){
    y <- paste0(measure, " (2-back)")
  }
  
  g <- ggplot(aes_string(y = names(df[, 6]) , x = "session", fill = "group"), data = df)
  g <- g + geom_boxplot()
  g <- g + ggtitle(paste0("Training progress - ", task_name, " n-back"))
  g <- g + xlab("Session")
  g <- g + guides(fill = guide_legend(title = "Groups"))
  g <- g + ylab(y)
  
  return(g)
  
}