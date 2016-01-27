

#' Aggregate the Concentration Data for the year
#' 
#' 
#' 
#' @param year_data_path file path to the "Other data" folder for the year
#' @param yr_bef the year before, as a string
#' 
#' @importFrom stringr str_sub
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @importFrom lubridate days ymd
#' @importFrom plyr llply ldply dlply
#' @importFrom dplyr inner_join rename mutate select
#' 
#' @export
aggregate_concentrations <- function(year_data_path, yr_bef) {
  
  orig_loc <- setwd(year_data_path)
  on.exit( setwd(orig_loc) )
  
  # TODO: FIX THIS PART WITH THE FILE PATHS
  if( ! any(c("year_csim_vals.rds", "year_aqs.rds") %in% dir()) ) {
    stop("Simulated or observed concentrations are missing. Please check to \ 
         make sure the file path is correct.")
  }
  
  message("Loading csim values...")
  csim_year <- readRDS("year_csim_vals.rds") # all the year's simulated concentrations
  assert_that( 
    all.equal( sort(as.numeric(csim_year %>% names %>% str_sub(4, -1))), 1:364 ) 
  )
  
  # Obtain csim values by site, then format (~ 1.5 minutes)
  message("Extracting simulated values from monitoring sites....")
  sitevals_csim_yr <- csn_site_index %>% select(SiteID, j, i) %>% 
    dlply(., .(SiteID)) %>% sites_get_csim(., csim_year)
  
  sitevals_csim_yr <- sitevals_csim_yr %>% 
    llply(function(day_list) ldply(day_list, .id="Date")) %>% 
    ldply(.id="SiteID") %>% 
    rename(Species = .id, c_sim = V1) %>% 
    mutate(Date = str_sub(as.character(Date), 4, -1)) %>% 
    mutate( Date = days(as.numeric(Date)) + ymd(paste0(yr_bef, "-12-31")) ) %>% 
    mutate(Date = as.character(Date))
  
  message("Combining concentration data for the whole year...")
  aqs_year <- readRDS("year_aqs.rds")
  concen_year <- sitevals_csim_yr %>% inner_join(aqs_year) 
  
  # cleanup
  rm(csim_year, sitevals_csim_yr)
  
  # return results
  concen_year
}


#' Generate Sensitivity Matrices from CMAQ output
#' 
#' @param agg_intv_folder folder containing aggregated 10-day interval rds files
#' @param yr_bef the year before that for which sensitivity 
#'  matrices are to be collected, as a string
#' @param .sources sources of pollution
#' 
#' @details For start_date, if the SA matrices are for 2006, the day before 
#'  2006 is Dec 31, 2005, so the string would be set as "2005-12-31".
#' 
#' @importFrom stringr str_sub
#' @importFrom plyr dlply
#' @importFrom dplyr select
#' @importFrom magrittr %>% set_colnames multiply_by add
gen_sens_mats <- function(agg_intv_folder, yr_bef, .sources = sources) {
  
  out <- vector("list", nrow(csn_site_index)) # KEEP THIS LINE
  names(out) <- csn_site_index$SiteID %>% as.character
  site_inds <- csn_site_index %>% select(SiteID, i, j) %>% dlply(.(SiteID)) 
  if(is.character(yr_bef)) yr_bef <- as.numeric(yr_bef) # include a "try" ??
  # if(as.numeric(yr_bef) == 2006) .sources <- names07 # COME BACK HERE
  
  # Each agg intv file is a list of ten days (or fewer), each day a list of 
  # 41 elements, one for each species. Then for each species for that day, is 
  # the 112x148x21 grid.
  
  
  # FIX AGG_FILES HERE TO AGG_PATH
  for(filename in dir(agg_intv_folder, full.names = TRUE)) {
    
    # store results of sites_get_sens to a temp object
    agg_holder <- readRDS(filename)
    temp_sens <- sites_get_sens(site_inds, agg_holder, yr = yr_bef + 1)
    
    # for each site
    for(i in seq_along(names(temp_sens))) {
      # convert element names from 'DayX' to a date
      names(temp_sens[[i]]) <- temp_sens[[i]] %>% names %>% 
        str_sub(., 4, -1) %>% as.numeric %>% 
        add( as.Date(paste0(yr_bef, "-12-31")) )
      
      # temp_sens[[i]] <- Map(set_colnames, temp_sens[[i]], c("Species", sources))
      Map(function(x) set_colnames(x, c("Species", .sources)), temp_sens[[i]])
    }
    
    temp_sens <- temp_sens[names(out)] # so corresponding terms match
    for(k in seq_along(out)) out[[k]] <- c(out[[k]], temp_sens[[k]])
    rm(agg_holder, temp_sens)
  }
  
  return(out)
}

# CHANGED: gen_sens_mats, sites_get_sens


#' WHAT DO I DO?
#' 
#' @importFrom magrittr %>%
#' 
format_sens_mats <- function(df, yr, .sources = sources) {
  if(as.numeric(yr) == 2007) .sources <- names07
  
  df %>% 
    llply(function(dlist) ldply(dlist, .id="Date")) %>% 
    ldply(.id="SiteID") %>% 
    set_colnames(c("SiteID", "Date", "Species", .sources)) %>% 
    mutate(SiteID = as.character(SiteID), Date = as.character(Date)) %>% 
    to_sitedatelist()
}

