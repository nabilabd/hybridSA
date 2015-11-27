
library(pryr)
library(beepr)
library(ggplot2)
library(devtools)
library(lubridate)

# can see how unadjusted TOR meaasurements grew over time
ocec_data %>%
  group_by(Year, ParamCode) %>%
  summarize(num_vals = length(avg_conc)) %>%
  filter(ParamCode %in% c(88370, 88380)) %>%
  left_join(used_paramcodes) %>% as.data.frame %>%
  arrange(Year, Species) %>%
  left_join(
    unique(
      select(
        rename(all_param_codenames, ParamCode = ParameterCode),
        ParameterName:ParamCode)
    )
  )

# .Last.value
aa %>%
  ggplot(aes(Year, num_vals, color = Species)) + geom_point() + geom_line()


agg_obs <- readRDS("~/hsa_data/agg_observations_05_12.rds")



######################

## corrections to OC/EC data

## CONTINUED FROM STEP 3 OF download_aqs.R

library(lubridate)

boundary_yr <- 2009 # year separating "later" vs. "early" oc/ec

later_codes <- c(88370, 88380) # for 2010 and beyond, EC and OC
later_ocec <- ocec_data %>%
  filter(year(Date) >= boundary_yr, ParamCode %in% later_codes)

# measurements by paramCode
later_ocec %>%
  group_by(ParamCode, Species, Year = year(Date)) %>%
  summarize(num_vals = length(avg_conc))

later_ocec %>%
  group_by(StateCountySite, Date, Species, lat, long) %>%
  mutate(len = length(avg_conc)) %>%
  arrange(StateCountySite, Date) %>% select(-MethodName) %>%
  filter(len > 1) # then check len > 2, 3. So no site has more than 3 of these measurements.

# FINAL FOR LATER OCEC
# takes mean concentration over the different POC values, when multiple instruments used
# number of remaining row agrees with what is expected: nrow(later_ocec) - 1882 - 660
fin_later_ocec <- later_ocec %>%
  group_by(StateCountySite, Date, Species, lat, long) %>%
  summarize(mean_conc = mean(avg_conc))


# double check for 2009; the later codes start dominating slightly

early_ocec <- ocec_data %>% filter(Year < boundary_yr)

# to see trends in paramcode measurements over the years
early_ocec %>%
  group_by(ParamCode, Species, Year = year(Date)) %>%
  summarize(num_vals = length(avg_conc)) %>% ungroup %>% as.data.frame %>%
  ggplot(aes(Year, num_vals, color = factor(ParamCode), group = ParamCode)) +
  geom_line() + geom_point() + facet_grid(. ~ Species)

# so:
# final "early" EC/OC dataset. (simply removing the two "later" codes from above)
early_ocec <- ocec_data %>% filter(Year < boundary_yr, ! (ParamCode %in% later_codes))

# can see the distribution here
early_ocec %>%
  group_by(ParamCode, Species, Year = year(Date)) %>%
  summarize(num_vals = length(avg_conc)) %>% ungroup %>% as.data.frame %>%
  ggplot(aes(Year, num_vals, color = factor(ParamCode), group = ParamCode)) +
  geom_line() + geom_point() + facet_grid(. ~ Species)


#### split early ocec into "oc_res", "ec_res"

## remove duplicated values
## (except where multiple values corrected to form single val)

# first, take average concentration over the different POC's used

# see:
# early_ocec %>% arrange(StateCountySite, Date, Species)  %>%
#   select(-MethodName) %>% group_by(StateCountySite, Date, Species, ParamCode) %>%
#   mutate(num_vals = length(avg_conc)) %>% filter(ParamCode == 88329, num_vals > 1)

# METHOD CODE goes away here, but it's needed for the
early_ocec_nodups <- early_ocec %>%
  select(-MethodName) %>%
  group_by(StateCountySite, Date, Species, ParamCode, lat, long, MethodCode, Year) %>%
  summarize(mean_conc = mean(avg_conc))

# to see that this worked:
# early_ocec_nodups %>% arrange(StateCountySite, Date, Species) %>%
#   group_by(StateCountySite, Date, Species, ParamCode) %>%
#   mutate(num_vals = length(mean_conc)) %>% filter(num_vals > 1)


## so at this stage, for each: site, date, paramcode, there is only one value.

# all OC concentrations, with no duplicate measurements
oc_res <- early_ocec_nodups %>%
  filter(Species == "OC25") %>%
  group_by(Date, StateCountySite) %>%
  mutate(num_rows = length(ParamCode)) %>%
  ungroup %>%
  ddply(.(Date, StateCountySite), oc_site_filt) %>%
  arrange(Date, StateCountySite) %>% tbl_df

# test that only one value per siteday:
# oc_res %>% group_by(StateCountySite, Date) %>% mutate(len = length(mean_conc)) %>% filter(len > 1) # WORKS!

# ~ 2 min
ec_res <- early_ocec_nodups %>%
  ungroup %>%
  filter(Species == "EC25") %>%
  group_by(Date, StateCountySite) %>%
  mutate(orig_num_rows = length(ParamCode)) %>%
  ungroup %>%
  ddply(.(Date, StateCountySite), ec_site_filt) %>%
  arrange(Date, StateCountySite, ParamCode) %>% tbl_df

# check uniqueness:
# aa <- ec_res %>% group_by(Date, StateCountySite, ParamCode) %>%
#     mutate(numvals = length(ParamCode)) %>% filter(numvals > 1)

# note that of the four paramcodes present in ec_res, only one (88307) doesn't
# always have unique values for each siteday. Now need a means of selecting
# based on the methodcode. For now, will just take the minimum as a proxy


## compute corrections

# For OC corrections, need the revised EC values

# for OC25, corrections only on 88305. Corresponding EC values would have
# paramcode of 88307:
# "problem EC" - a misnomer
prob_ec <- ec_res %>% filter(ParamCode == 88307) %>% select(-orig_num_rows)

# need corresponding EC measurements, for the OC corrections
prob_oc_res <- oc_res %>% filter(ParamCode == 88305) # OC measurements to be corrected
prob_ocec <- prob_oc_res %>%
  spread(Species, mean_conc) %>%
  left_join(
    spread( prob_ec, Species, mean_conc),
    by = c("Date", "StateCountySite", "lat", "long", "Year")
  ) %>%
  filter(!is.na(EC25))

# generate oc IMPROVE corrected concentrations
oc_imp <- prob_ocec %>%
  rename(SamplerCode = MethodCode.x) %>%
  gen_oc_imp %>% tbl_df %>%
  select(StateCountySite:Date, imp_conc = oc_imp, lat:long, Year)

# combine all OC concentrations
# NB: need to have completed the corrected OC data before this stage
# Note: what to do about naming conc column for oc_imp? don't want to join with mean_conc
# DESC: this works by taking the OC concentrations which haven't been corrected,
#   and joining (i.e., row-binding) with those that have been
fin_oc_res <- oc_res %>%
  anti_join(prob_oc_res) %>%
  select(StateCountySite:Date, imp_conc = mean_conc, lat:long, Year) %>%
  bind_rows(oc_imp) %>% # this comes from prob_ocec from prob_oc_res from oc_res
  filter(!is.na(imp_conc)) %>%
  select(-Year) %>% mutate(Species = "OC25")

# no missing values, and unique measurements per sitedate
fin_oc_res %>% is.na %>% sum # ZERO!
fin_oc_res %>% group_by(StateCountySite, Date, Species) %>%
  mutate(len = length(imp_conc)) %>% filter(len > 1) %>% nrow

# only ~500 rows with ParamCode 88321, so we just get rid of them

# performs the corrections for EC, whether that's by addition or multiplication,
# then combines both
ec_res2 <- ec_res2 %>% filter(ParamCode != 88321)

# EC corrections by adding
adding_ec_res <- ec_res2 %>%
  filter(ParamCode != 88307) %>% # paramcode that is not corrected by adding
  group_by(Date, StateCountySite, long, lat, Year) %>%
  summarize(ec_imp = sum(avg_conc)) %>%
  select(StateCountySite, Date, ec_imp, lat, long)

# no missing values, unique vals per sitedate
adding_ec_res %>% is.na %>% sum
adding_ec_res %>% group_by(StateCountySite, Date) %>%
  mutate(len = length(ec_imp)) %>% filter(len > 1)


# EC corrections by mutliplication factor
# the paramcode has multiple POC measurements for each sitedate, so we first
# average over them all to get unique vals by sitedate
mult_ec_res <- ec_res2 %>%
  filter(ParamCode == 88307) %>%
  group_by(Date, StateCountySite, long, lat, Year) %>%
  summarize(ec_conc = mean(avg_conc)) %>%
  mutate(ec_imp = 1.3 * ec_conc) %>%
  select(-ec_conc) %>% ungroup %>%
  select(StateCountySite, Date, ec_imp, lat, long) %>%
  arrange(StateCountySite, Date)

fin_ec_res <- adding_ec_res %>%
  bind_rows(mult_ec_res) %>%
  mutate(Species = "EC25") %>%
  rename(imp_conc = ec_imp) %>%
  arrange(StateCountySite, Date)


# need to work on this
fin_ec_res %>% is.na %>% sum # ZERO!!!
fin_ec_res %>% group_by(StateCountySite, Date, Species) %>%
  mutate(len = length(imp_conc)) %>% filter(len > 1) %>% nrow

# complete corrected OC/EC data for earlier years
fin_early_ocec <- fin_ec_res %>% bind_rows(fin_oc_res)


####

# AQS OC/EC

# corrections for OC/EC performed for earlier years
# here, later OC/EC concentrations combined with earlier concentrations
corrected_aqs_ocec <- fin_later_ocec %>% ungroup %>%
  select(StateCountySite:Date, imp_conc = mean_conc, Species) %>%
  bind_rows(
    select(fin_early_ocec, -lat, -long)
    ) %>%
  rename(Conc_obs = imp_conc) %>% mutate(Year = year(ymd(Date))) %>%
  arrange(StateCountySite, Date, Species)

# no missing values, and unique vals by sitedate
corrected_aqs_ocec %>% is.na %>% sum # ZERO !!!
corrected_aqs_ocec %>% group_by(StateCountySite, Date, Species) %>%
  mutate(len = length(Conc_obs)) %>% filter(len > 1) %>% nrow

corrected_aqs_ocec %>% saveRDS("../hsa_data/corrected_ocec_all.rds")

