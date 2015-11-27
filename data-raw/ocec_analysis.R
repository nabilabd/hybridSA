# OCEC analysis

library(stringr)
library(magrittr)
library(readr)
library(dplyr)


########################################################
## Part 1) obtain data
########################################################

agg_obs <- readRDS("~/hsa_data/agg_observations_05_12.rds")

data("all_param_codenames")
uniq_params <- all_param_codenames %>% extract(, 1:2) %>% unique

# absolutely all EC/OC observations from complete aqs data
# paramaters used are: filter(uniq_params, str_detect(ParameterName, "EC|OC"))
ocec_obs <- agg_obs %>%
  semi_join(
    filter(uniq_params, str_detect(ParameterName, "EC|OC"))
    ) %>%
  rename(lat = Latitude, long = Longitude)
# ocec_obs %>% write_rds("data/ocec_obs.rda")

rm(uniq_params, agg_obs)

########################################################
## Part 2) analyze temporal trends of parameter codes
########################################################

# ocec_obs <- read_rds("data/ocec_obs.rda")

# combine
ocec_obs <- ocec_obs %>%
  mutate(Species = str_sub(ParameterName, 1, 2), DateLocal = ymd(DateLocal))

# group observations by month and year made
ocec_obs <- ocec_obs %>%
  mutate( Year = year(DateLocal), Month = month(DateLocal), Day = 1) %>%
  unite(Date, Year, Month, Day, sep = "-")

# table used for plot above
table_ocec <- ocec_obs %>%
  select(ParameterCode, ParameterName, Species) %>% unique %>%
  mutate(Type = ifelse(str_detect(ParameterName, "TOT"), "TOT",
                       ifelse(str_detect(ParameterName, "TOR"), "TOR", "LC")
  )) %>% as.data.frame


# monthly counts by species and type
counts_ocec <- ocec_obs %>%
  mutate(Type = ifelse(str_detect(ParameterName, "TOT"), "TOT",
                       ifelse(str_detect(ParameterName, "TOR"), "TOR", "LC")
                       )) %>%
  group_by(Date, Type, Species) %>%
  summarize(num_vals = length(ArithmeticMean)) %>%
  ungroup %>%
  mutate(Date = ymd(Date))



# TOT measurements clearly decreasing
counts_ocec %>%
  ggplot(aes(Date, num_vals, color = Type)) + geom_line() + # geom_point() +
  facet_grid(Species ~ .) + ggtitle("EC/OC Measurement Types") +
  ylab("Number of values")




# Changes in number of measurements, over time
library(ggplot2)
ocec_data %>% group_by(Year, ParamCode, Species) %>% summarize(counts = n()) %>%
  ggplot(aes(Year, counts, color = factor(ParamCode))) +
  geom_point() + geom_line() + ylab("Number of Observations") +
  ggtitle("EC/OC Measurements")



#####

# actually combining the EC/OC data

ocec_data



