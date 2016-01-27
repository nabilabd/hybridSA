

#' Extract csim values for all sites for entire year
#' 
#' This is to be used on the object containing only csim values, for all sites 
#' throughout the year, not on the raw CMAQ data that also includes object  
#' sensitivity values.
#' 
#' @param site_coords a list of site coordinates (i, j) from csn_site_index. 
#'  The (i,j) are indexed from the lower-left corner of the matrix. Note, 
#'  though, that we must first convert this to standard terminology for matrices
#'  by transforming these indices to (113 - j, i)
#' @param year_csim_list list of csim values for each day. Each day's data is 
#'  a list of 41 elements, one for each species, and each species corresponds to 
#'  a 112x148 grid of csim values across the grid.
#' 
#' @importFrom plyr ldply llply
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>% set_names 
sites_get_csim <- function(site_coords, year_csim_list) {
  
  # given coordinates, then for each day (which is a list of 41 arrays), extract 
  # from each array the csim values (for that species on that day at that site)
  extract_site_csim <- function(xcoord, ycoord) {
    # TODO: ADD ASSERTIONS TO CHECK INDICES IN PROPER RANGES
    # rename the result of ldply 
    year_csim_list %>% 
      llply(., function(day) ldply(day, function(m) m[xcoord, ycoord]))
  }
  
  # for each site coordinates, call extract c_sim, return results by site
  site_coords %>% 
    llply(., function(loc) extract_site_csim(113 - loc$j, loc$i)) %>% 
    llply(function(m) ldply(m, .id = "Date")) %>% 
    ldply(.id="SiteID") %>% 
    mutate(SiteID = as.character(SiteID), Date = as.character(Date)) %>% 
    rename(Species=.id, c_sim=V1) 
}



# TODO: REMOVE REDUNDANCY OF HAVING SITES GET SENS AND SITES GET CSIM
# IDEA: CONSOLIDATE BY MAKING sites_get_csim have third index in 
# extract_site_csim, add "ind" parameter indicating which third indices to take

#' 
#' 
#' @param site_coords
#' @param intv_sens_list
#' @importFrom plyr ldply llply
#' @importFrom magrittr set_names %>% 
sites_get_sens <- function(site_coords, intv_sens_list, yr) {
  
  if(is.character(yr)) yr <- as.numeric(yr)
  # inds <- ifelse(yr < 2009, 2:21, 2:17)
  
  # given coordinates, then for each day (which is a list of 41 arrays), extract 
  # from each array the csim values (for that species on that day at that site)
  # set_names(c("Species", sources) can be moved to the loop thru intv_agg files
  extract_site_sens <- function(xcoord, ycoord) {
    # TODO: ADD ASSERTIONS TO CHECK INDICES IN PROPER RANGES
    intv_sens_list %>% 
      llply(., function(day) ldply(day, function(m) m[xcoord, ycoord, 2:21])) # FIX THIS
  }
  
  # for each site coordinates, call extract c_sim, return results by site
  site_coords %>% 
    llply(., function(loc) extract_site_sens(113 - loc$j, loc$i)) %>% 
    set_names(., names(site_coords))
}



#' Convert a dataframe to a list of sites of a list of dates
#' 
#' 
to_sitedatelist <- function(df) {
  df %>% 
    dlply(.(SiteID)) %>% 
    llply(function(dlist) dlply(dlist, .(Date)))
}

#' Convert from a list of sites of a list of dates, to a dataframe
#' 
#' 
from_sitedatelist <- function(df) {
  df %>% 
    llply(function(alist) ldply(alist, .id="Date")) %>% 
    ldply(.id="SiteID")
}


#' Plot a time series for teh source
#' 
#' Given site data for single chem source, generates pdf of time series at each
#' site
#' 
#' @param site_data data frame for observations from a single site
#' @return pdf file for each source, containing time series of Rj values by site
#' 
source_tseries <- function(site_data) {
  
  chem_source <- unique(site_data$Source)
  
  ggsave(filename = str_c("tseries/", chem_source, "_tseries.pdf"), 
         device = "pdf", 
         site_data %>% d_ply(.(SiteID), function(my_data) { 
           site <- unique(my_data$SiteID)
           ggplot(my_data, aes(Date, Rj_vals)) + geom_point() + ylim(0, 10) + 
             ggtitle(str_c("Rj values of ", chem_source, " at site ", site))
         }, .print=TRUE), 
         width=8, height=4)
}

#' Adds montior frequency
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom lubridate year ymd
#' 
#' 
add_monitor_freq <- function(df) {
  df %>% 
    mutate(Year = year(ymd(Date))) %>% 
    mutate(sixths_ind = ifelse(Year <= 2008, Year %% 6, -1 + Year %% 6)) %>% 
    mutate(sixths_ind = ifelse(sixths_ind < 0, sixths_ind + 6, sixths_ind)) %>% 
    mutate(thirds_ind = (sixths_ind + 3) %% 6)
}
