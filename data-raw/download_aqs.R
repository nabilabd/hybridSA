#############################################################
## This script is to generate the AQS data for 2005-12
############################################################

# -----------------------------
# Step 1) download the data
# -----------------------------

years <- 2005:2014
spec_url <- "http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_SPEC_"
pm_url   <- "http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_88101_"

speciation_paths <- paste0(spec_url, years, ".zip")
mass_pm_paths    <- paste0(pm_url, years, ".zip")
pmdata_paths <- c(speciation_paths, mass_pm_paths)

# ~ 36 sec
for (fpath in pmdata_paths) {

  tmp <- tempfile(fileext = ".zip")
  dir.create(tmp)
  download.file(fpath, tmp)

  unzip(tmp, exdir = "data-raw/", junkpaths = T)
}

rm(years, spec_url, pm_url, speciation_paths, mass_pm_paths, pmdata_paths)

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


# csn_siteind <- read_rds("~/csn_siteind.rds")
# use_data(csn_siteind)
load("data/csn_siteind.rda")
source("R/utils.R")

#' @param fpath file path of the observation data
read_pm <- function(fpath) {

  raw_data <- read_csv(fpath)
  raw_data %>%
    unspace_names %>%
    unite(StateCountySite, StateCode:SiteNum, sep="-") %>%
    semi_join(csn_siteind)
}


# raw_files <- dir("data-raw/", full.names = TRUE)
spec_paths <- str_c("/Volumes/My Passport for Mac/Daily Speciation/", c("csv_files", "pm_csv_files"))
raw_files <- dir(spec_paths, full.names = TRUE)
csv_files <- Filter(function(x) str_sub(basename(x), -4, -1) == ".csv", raw_files)
agg_obs <- csv_files %>% str_subset("2013|2014") %>% ldply(read_pm) %>% tbl_df # ~6.8 million observations

# # uncomment to store the observations
# save(agg_obs, "data/agg_obs.rdata")
agg_obs %>% write_rds("data/later_aqs_13_14.rds")

rm(csv_files, raw_files, spec_paths)

# -----------------------------
# Step 3) Filter
# -----------------------------

all_param_codenames <- agg_obs %>%
  select(ParameterName, ParameterCode, MethodName, MethodCode, POC) %>%
  unique

# for reference
save(all_param_codenames, "data/all_param_codenames.rda")

# extract observations relevant to the analysis
relev_obs <- agg_obs %>%
  inner_join(used_paramcodes) %>%
  rename(lat = Latitude, long = Longitude, Date = DateLocal, avg_conc = ArithmeticMean) %>%
  rename(ParamCode = ParameterCode, ParamName = ParameterName) %>%
  select(StateCountySite:long, Date, MethodCode:MethodName, Species, avg_conc) %>%
  select(StateCountySite, Date, Species, avg_conc, lat:long, everything()) %>%
  mutate(Year = year(ymd(Date)))

# split up to perform corrections separately, before re-combining
pm_data <- relev_obs %>% filter(Species == "PM25")
ocec_data <- relev_obs %>% filter(Species %in% c("OC25", "EC25"))
rest_data <- relev_obs %>% anti_join( bind_rows(pm_data, ocec_data) )

# -----------------------------
# Step 4) Perform Corrections
# -----------------------------

# commented code below demonstrates some of the rationale behind the computations

#### a) for OC/EC data

source("data-raw/corrections.R")

# see OCEC analysis

use_data(ocec_data) # stores the OC/EC values from 2005-2012 for an analysis


#### b) average over the duplicates

# # ~62,000 site-dates with multiple measurements
# pm_data %>%
#   group_by(StateCountySite, Date) %>%
#   summarize(len = length(avg_conc)) %>%
#   filter(len > 1)
#
# # not sure why; some are due to be in POC or MethodCode, many are not
# pm_data %>%
#   group_by(StateCountySite, Date) %>%
#   mutate(len = length(avg_conc)) %>%
#   filter(len == 3) %>% ungroup %>%
#   select(-MethodName)

# # so remove the duplicates by averaging
# pm_nodups <- pm_data %>%
#   select(Date, StateCountySite, Species, avg_conc, Year) %>%
#   group_by(Date, StateCountySite) %>%
#   summarize(Conc_obs = mean(avg_conc)) %>%
#   mutate(Species = "PM25", Year = year(ymd(Date))) %>%
#   select(StateCountySite, Date, Conc_obs, everything()) %>%
#   ungroup

# # the other species have the same issue of duplicate values
# rest_data %>%
#   group_by(StateCountySite, Date, Species) %>%
#   summarize(len = length(avg_conc)) %>%
#   filter(len > 1)

# so, remove dups for all species other than EC/OC
rest_data2 <- bind_rows(rest_data, pm_data) # dups can be removed from both categories at once

# ~ 7 min
rest_nodups <- rest_data2 %>%
  select(Date, StateCountySite, Species, avg_conc, Year) %>%
  group_by(Date, StateCountySite, Species) %>%
  summarize(Conc_obs = mean(avg_conc)) %>%
  mutate(Year = year(ymd(Date))) %>%
  select(StateCountySite, Date, Conc_obs, everything()) %>%
  ungroup

corrected_ocec_all <- readRDS("../hsa_data/corrected_ocec_all.rds") # WHERE IS THIS COMPUTED ???

# for 2013-2014
corrected_ocec_all <- fin_later_ocec %>% ungroup %>%
  select(-lat, -long) %>% rename(Conc_obs = mean_conc) %>%
  mutate(Year = year(Date)) %>%
  select(StateCountySite:Date, Conc_obs, Species, Year)

# ~ 30 sec
complete_aqs <- rest_nodups %>%
  bind_rows(corrected_ocec_all) %>%  # see why "corrected_aqs_ocec" doesn't work here now
  mutate(Date = as.character(Date)) %>%
  from_statecountysite %>%
  arrange(SiteID, Date, Species)
dest <- "data-raw/complete_aqs_1314.rds"
complete_aqs %>% write_rds(dest)


# ------------------------------------------
# Step 5) Include uncertainties
# ------------------------------------------

load("data/aqs06.rda")
metals <- c("Ga", "Hg", "La", "Mo")

# generate linear models for uncertainty by concentration
make_model <- function(df) lm(sig_c_obs ~ Conc_obs, data = df)
unc_models <- aqs06 %>% dlply(.(Species), make_model)

# check that model coefficients are non-negative
unc_models %>% ldply(coefficients)
err_mods <- .Last.value %>% filter(Conc_obs < 0 | `(Intercept)` < 0)


# Adds column that contains uncertainties of the concentration measurements
add_uncertainty <- function(df, model) {
  df2 <- df %>% mutate(sig_c_obs = predict(model, newdata = df))
  df2
}

# prepare observations for extrapolation from linear model
aqs_by_species <- complete_aqs %>% dlply(.(Species))
stopifnot( all(names(aqs_by_species) == names(unc_models)))

unc_models_nonmets <- unc_models[-which(names(unc_models) %in% metals)] # for 2013-2014
stopifnot( all(names(aqs_by_species) == names(unc_models_nonmets)))

# Generate uncertainties
myres <- Map(add_uncertainty, aqs_by_species, unc_models_nonmets) # unc_models_nonmets for 2013-2014
myres_df <- myres %>% ldply(.id = "Species") %>% tbl_df


# re-calculate regressino extrapolation for species with negative model coefficients
new_oc <- myres_df %>% filter(Species == "OC25") %>%
  mutate(sig_c_obs = 0 + err_mods[1, 3] * Conc_obs )
myres_df[myres_df$Species == "OC25", ] <- new_oc
rm(new_oc)

# # for Vanadium, only two days affected, and Rj's are day-by-day, so no correction
# myres_df %>% filter(Species == "V", sig_c_obs < 0)

# note that there are negative Conc_obs values for: OC25, PM25, Pb. But, even
#   in agg_obs, there are negative concentrations for ArithmeticMean ...
myres_df %>% saveRDS("data/all_aqs_05_12.rds")
# myres_df %>% write_rds("data/all_aqs_13_14.rds")

# in 2013-2014, there are negative Conc_obs values for: PM25, Pb.
# specifically for 2007, but can be generalized to other years:
aqs07 <- myres_df %>% filter(Year == 2007) %>% arrange(SiteID, Date, Species)
aqs07 %>%
  filter(Conc_obs >= 0, sig_c_obs >= 0) %>%
  readr::write_rds("/Volumes/My Passport for Mac/gpfs:pace1:/data_2007/Other_data/year_aqs.rds")




