#############################################################
## This script is to generate the AQS data for 2005-12
############################################################

# -----------------------------
# Step 1) download the data
# -----------------------------

years <- 2005:2012
spec_url <- "http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_SPEC_"
pm_url   <- "http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_88101_"

speciation_paths <- paste0(spec_url, years, ".zip")
mass_pm_paths    <- paste0(pm_url, years, ".zip")
pmdata_paths <- c(speciation_paths, mass_pm_paths)

# ~ 36 sec
for (fpath in pmdata_paths) {

  tmp <- tempfile(fileext = ".zip")
  download.file(fpath, tmp)

  unzip(tmp, exdir = "data-raw/", junkpaths = T)
}



# -----------------------------
# Step 2) Extract relevant data
# -----------------------------

suppressPackageStartupMessages({
  library(readr)
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(magrittr)
})


csn_site_index2 <- load("data/csn_site_index2.rda")
source("R/utils.R")

#' @param fpath file path of the observation data
read_pm <- function(fpath) {

  raw_data <- read_csv(fpath)
  raw_data %>%
    rm_space_names %>%
    unite(StateCountySite, StateCode:SiteNum, sep="-") %>%
    semi_join(csn_site_index2)
}


raw_files <- dir("data-raw/", full.names = TRUE)
csv_files <- Filter(function(x) str_sub(basename(x), -4, -1) == ".csv", raw_files)
agg_obs <- csv_files %>% ldply(read_pm) %>% tbl_df # ~6.8 million observations

# uncomment to store the observations
# save(agg_obs, "data/agg_obs.rdata")


# -----------------------------
# Step 3) Filter
# -----------------------------

all_param_codenames <- agg_obs %>%
  select(ParameterName, ParameterCode, MethodName, MethodCode, POC) %>%
  unique

# for reference
save(all_param_codenames, "data/all_param_codenames.rda")

# extract observations relevant to the analysis
relev_obs <- agg_obs %>% inner_join(used_paramcodes)

# split up to perform corrections separately, before re-combining
pm_data <- relev_obs %>% filter(Species == "PM25")
ocec_data <- relev_obs %>% filter(Species %in% c("OC25", "EC25"))
rest_data <- relev_obs %>% anti_join( bind_rows(pm_data, ocec_data) )

# -----------------------------
# Step 4) Perform Corrections
# -----------------------------







