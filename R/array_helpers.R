


#' Convert data cube to data frame 
#' 
#' NB: REDUNDANCY WITH CUBE_TO_DF, SO THIS REMAINS
#' 
#' @importFrom plyr adply
#' @importFrom lubridate ymd days
#' @importFrom dplyr rename mutate 
#' @importFrom magrittr %>%
#' 
dcube_to_df <- function(dcube, yr = "2006") {
  
  yr_start <- paste0(yr, "-01-01")
  num_days <- dim(dcube)[3]
  yr_days <-  as.character( ymd(yr_start) + days(0:(num_days - 1)) )
  dimnames(dcube)[3] <- list(Date = yr_days)
  
  # return results
  res <- adply(dcube, 3, slice_to_df)
  if("X1" %in% colnames(res)) {
    res <- res %>% rename_("Date" = "X1")
  }
  res <- res %>% mutate(Date = as.character(Date))
  
  return(res)
}


#' 
#' 
#' Note: what was originally "Rj_vals" was renamed to "Values"
#' 
#' @param df dataframe
#' 
df_to_slice <- function(df) {
  
  ydim <- 112; xdim <- 148
  inds <- matrix( 1:(xdim * ydim), nrow=ydim, byrow=T) %>% apply(2, rev)
  
  df <- df %>% select(SiteInd, Values) %>% arrange(SiteInd)
  stopifnot( all.equal(df$SiteInd, 1:(xdim * ydim)) )
  
  day_slice <- df$Values[as.vector( inds )] %>% 
    matrix(nrow = ydim)
  
  day_slice
}

#' 
#' 
#' 
#' @param cmaq_slice
#' @param day_date
#' 
slice_to_df <- function(cmaq_slice, day_date = NULL) {
  
  # generate matrix of indices, with positions of indices corresponding to 
  # positions of the values
  xdim <- 148; ydim <- 112
  inds <- matrix( 1:(xdim * ydim), nrow=ydim, byrow=T) %>% apply(2, rev)
  
  # convert to indexed dataframe
  res <- data.frame(
    SiteInd = as.vector( inds ), 
    Values = as.vector( cmaq_slice )
  ) 
  res <- res %>% arrange(SiteInd)
  
  # return results
  res
}
