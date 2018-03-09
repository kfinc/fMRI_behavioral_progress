#'Performs robust repeated analysis of variance test for dataframe in long format (group x session).
#'Data: behavioral performance during dual n-back task across 4 scanning session.
#' 
#' 
#'
#' @param df A data.frame in long format
#' @return Statistical test value and p-value
#

stats_robust <- function(df){
  s <- bwtrim(unlist(df[, 6]) ~ group * session, id = case, data = df)
  return(s)
}  
