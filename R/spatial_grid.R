

# global definition
# but how to handle this, since code only all run when building/checking?
lcc_proj4 <- paste("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97", 
                   "+x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")

#' Transform a dataframe to an SPDF
#' 
#' @param df dataframe containing values. Assumes a "Date" column is present, 
#'  and that it is of character class 
#' @param proj projection for data
#' 
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom sp `coordinates<-` `proj4string<-`  
make_spdf <- function(df, proj = lcc_proj4) {
  
  df <- df %>% mutate(Date = as.POSIXct(Date))
  coordinates(df) <- ~ Xm + Ym
  proj4string(df) <- CRS(proj)
  
  return(df)
}

#' Perform linear interpolation of missing values
#' 
#' @param df
#' 
#' @importFrom lubridate yday ymd year
#' @importFrom signal interp1
#' @importFrom dplyr mutate arrange left_join arrange rename
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' 
#' @return dataframe with predictions for missing values linearly interpolated 
#'  from present values
gen_full_spt_dom <- function(df) {
  
  num_cells <- 112 * 148
  if(!is_sorted(df$Date)) df <- df %>% arrange(Date) # not checked...
  
  # obtain year of data
  data_year <- year(ymd(df$Date)) %>% unique
  assert_that(length(data_year) == 1)
  
  # generate all dates in that year 
  start_date <- sprintf("%s-01-01", data_year)
  end_date <- sprintf("%s-12-31", data_year)
  year_days <- seq(ymd(start_date), ymd(end_date), by="day") %>% as.character
  
  # generate full spatial-temporal domain, and join it with present data
  full_grid <- expand.grid(SiteInd = 1:num_cells, Date = year_days, KEEP.OUT.ATTRS = FALSE)
  res <- full_grid %>% left_join(make_grid()) %>% left_join(df) %>% 
    rename(Rj_vals = var1.pred) %>% arrange(SiteInd, Date)
  
  res
}

#' temporally interpolate Rj values
#' 
#' @importFrom lubridate yday ymd
#' @importFrom signal interp1
#' 
#' @param df a long-form dataframe with full spatio-temporal domain
interpol <- function(df) {
  not_na <- which(!is.na(df$Rj_vals))
  x_preds <- yday(ymd(df$Date)[not_na])
  y_preds <- df$Rj_vals[not_na]
  
  x_new <- yday(ymd(df$Date))
  
  df$Rj_vals <- interp1(x_preds, y_preds, x_new, "linear")
  df
}

