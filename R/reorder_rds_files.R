


# functions for re-arranging the .mat species files

# # can be used for scripting if following lines un-commented
# hdir <- "/Volumes//My Passport for Mac/gpfs:pace1:/new_2006/"
# mat_fold <- "new_mat_files_2006/"
# rds_fold <- "new_rds_files_2006/"
# 
# load("data/sources_06.rda")
# load("data/species.rda")

#################################
### 
#################################


#' return all the days in a given interval
#' 
#' @param grid_mat a matrix of values corresponding to the grid over the 
#'  US
#' @param intv_num the indices of the time interval to extract the data for
#' 
get_intv <- function(grid_mat, intv_num) {
  # TODO: split into two functions, one that gets interval, another that subsets
  # the 3d array
  
  day_dim <- 3 # dimension with days
  
  ending_day <- ifelse(intv_num < 37, 10*intv_num, dim(grid_mat)[day_dim])
  intv <- (10* (intv_num-1) + 1):ending_day
  grid_mat[, , intv]
}


#' Returns numbers of days in given interval
#' 
#' @param intv_num the number of the interval, and integer between 1 and 37, 
#'  inclusive
#' 
intv_range <- function(intv_num) {
   
   if( !(intv_num %in% c(1:37))  ) stop("Interval should be in 1-37")
   
   ending_day <- ifelse(intv_num < 37, 10*intv_num, 364)
   intv <- (10* (intv_num-1) + 1):ending_day
   
   intv
 }


#' Generate rds files by interval
#' 
#' @param path folder containing rds files
#' @param drop_last oftentimes additional modifiers are added to the species 
#'  name. \code{drop_last} allows one to specify how many final characters in 
#'  the file names should be removed to only have the species names
#'  
#' @importFrom abind abind
#' @importFrom stringr str_c str_sub
#' @importFrom magrittr %>%
#' @importFrom plyr alply
#' 
#' @export
gen_interval_rds <- function(path, drop_last = 5, FROM = 1, TO = 41) {
  # how do I do what I do as a function?
  
  if(!is.character(path)) 
    stop("File path should be a character string.", call. = FALSE)
  
  orig_loc <- setwd(path)
  on.exit(setwd(orig_loc))
  
  max_intvs <- 37
  spec_files <- dir() %>% str_sub(1, -drop_last)
  
  if(FROM == 1 && TO == 41) {
    stopifnot( length(dir()) == length(species)  ) # species IS ASSUMED HERE
    stopifnot( spec_files == species)
  }
  
  # works for a given interval; now generalize
  
  for (rds_file in dir()[FROM:TO] ) {
    spec_file <- readRDS(rds_file)
    
    for (n in 1:max_intvs) {
      message(str_c("Starting file ", n))
      
      # get first ten days, by source
      intv_results <- lapply(spec_file, function(x) get_intv(x, n))
      
      # combine data by adding "source" as a fourth dim of the array
      fourd_results <- abind::abind(intv_results, along = 4) # here's a problem
      
      # re-arrange data to be divided by day, not by source now
      by_day <- plyr::alply(fourd_results, 3)
      names(by_day) <- paste0("Day", intv_range(n))
      
      # save results; add: if(!file.exists(FILE PATH))
      if(!file.exists("../Interval_Files/")) dir.create("../Interval_Files/")
      
      target_path <- str_c("../Interval_Files/Interval", n, "/")
      if(!file.exists(target_path)) dir.create(target_path)
      saveRDS(by_day, str_c(target_path, "Intv", n, "_", rds_file))
    }
    
  }
}



###### Combine lists of different species, same interval 

# testing
# x <- list(a1=2, b1=c(1,2), c1=1:3)
# y <- list(a1=5, b1=c(2,5), c1=2:4)
# Map(c, x, y)

# for file in intv_files, load one rds file, keep loading another and merging

#' 
#' 
#' For each interval, takes all the by-species rds files and aggregates them. 
#' Stores all such aggregated-interval rds files in a folder.
#' 
#' @param path file path of the folder containing interval folders
#' @importFrom magrittr %>%
#' @importFrom stringr str_c str_sub str_length
combine_species_files <- function(path, drop_last = 5) {
  
  if(length(dir(path)) != 37) stop("Path should only have 37 intv folders.")
  
  max_intvs <- 37
  orig_loc <- setwd(path)
  on.exit(setwd(orig_loc))
  
  intv_files <- paste0("Interval", 1:max_intvs, "/")
  
  for(fold_name in intv_files) {
    
    # go down one folder
    targ_path <- fold_name
    rds_data <- dir(targ_path)
    if(length(rds_data) != length(species)) stop("Some species files missing.")
    
    # extract interval number and species names
    intv_num <- str_sub(fold_name, 9, -2) # includes the / as well
    species_names <- str_sub(rds_data, 5 + str_length(intv_num) + 1, -drop_last)
    # print(species_names)
    if( !all(species_names == species) ) stop("naming error with species files") 
    
    # start with base case, to combine subsequent data with
    base_spec <- readRDS(str_c(targ_path, rds_data[1]))
    all_specs <- lapply(base_spec, list)
    
    for(m in 2:length(rds_data)) {
      second_spec <- readRDS(str_c(targ_path, rds_data[m])) %>% lapply(list)
      all_specs <- mapply(c, all_specs, second_spec, SIMPLIFY = FALSE)
    }
    
    # name appropriately!
    # naming the elements as the species here is valid since the ordering 
    # matches the file directory listing, which is alphabetical
    for(i in 1:length(all_specs)) names(all_specs[[i]]) <- species_names
    
    # save results in a folder
    agg_fold <- "../Agg_intv_files/"
    if(!file.exists(agg_fold)) dir.create(agg_fold)
    saveRDS(all_specs, str_c(agg_fold, "/Intv_", intv_num, "_agg.rds"))
    
  }
}




#################################  
# Extract csim values for the year
#################################  

#' Extract simulated concentration values 
#' 
#' @param list_of_days CMAQ output by day. 
#' 
#' @details Intervals usually consisting of 
#'  ten days, where for each day, there is a 112x148x21 matrix of values. 
#'  The third dimension has on component the c_sim values, and the other 
#'  twenty are sensitivities to the sources.
#' 
#' @importFrom plyr llply
#' @importFrom magrittr %>%
#' 
extract_csim <- function(list_of_days) {
  list_of_days %>% llply(function(daylist) llply(daylist, function(x) x[, , 1]))
}

# USES EXTRACT_CSIM, LOOPS THROUGH INTV_AGG FILES, COMBINES ALL CSIM DATA
# TODO: COMBINE THIS LOOPING WTIH EXTRACTING SENS VALUES

#' Combine csim values from all species, to one file
#' 
#' 
#' @param agg_path file path of aggregate interval files
#' @importFrom magrittr %>%
#' @importFrom stringr str_c str_sub
#' 
#' @export
aggregate_csim <- function(agg_path) {
  
  num_intvs <- 37
  orig_loc <- setwd(agg_path)
  on.exit(setwd(orig_loc))
  
  if( length(dir()) !=  num_intvs) stop("Incomplete set of intervals")
  
  # base case; start with first file, combine results with other files
  csim_holder <- readRDS(dir()[1]) %>% extract_csim
  
  # add each other interval's csim values
  for(intv_file in dir()[-1]) {
    # load an agg.rds file, extract the c_sim values, then 
    # load next file. Goal is to have one file with all c_sim values
    # TO FIX FOR NEXT TIME: days numbered apprpriately in agg files. DONE
    
    temp_agg <- readRDS(intv_file)
    temp_csim <- temp_agg %>% extract_csim
    csim_holder <- c(csim_holder, temp_csim)
  }
  
  all_days <- csim_holder %>% names %>% str_sub(4, -1) %>% as.numeric
  if( !all.equal(sort(all_days), c(1:364)) ) stop("days aren't all aligned")
  
  my_order <- all_days %>% order
  csim_holder <- csim_holder[my_order]
  
  # all simulated concentrations for 2006
  if( !file.exists("../Other_data/")) dir.create("../Other_data/")
  saveRDS(csim_holder, str_c("../Other_data/year_csim_vals.rds"))
  rm(csim_holder, temp_agg, temp_csim)
}




#' Function to get 
#' 
#' testing for a function to get intervals of ten days
#' 
#' AM I NEEDED ?? I'm NOT USED IN THIS FILE...
#' 
ten_days_intvs <- function(days_in_year) {
  
  max_intvs <- 37
  day_mat <- matrix(0, nrow=max_intvs, ncol=10)
  
  for(n in 1:max_intvs) {
    num_days <- length(days)
    
    begin <- (10*(n-1) + 1)
    assert_that(begin < num_days)
    ending <- min(10*n, num_days)
    
    intv <- begin:ending
    day_mat[n] <- intv
    print(days[intv])
  }
}


