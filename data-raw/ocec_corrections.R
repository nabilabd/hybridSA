
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

early_ocec_nodups <- early_ocec %>%
  group_by(StateCountySite, Date, Species, ParamCode, lat, long, Year) %>%
  summarize(mean_conc = mean(avg_conc))

# to see that this worked:
# early_ocec_nodups %>% arrange(StateCountySite, Date, Species) %>%
#   group_by(StateCountySite, Date, Species, ParamCode) %>%
#   mutate(num_vals = length(mean_conc)) %>% filter(num_vals > 1)


## so at this stage, for each: site, date, paramcode, there is only one value.


oc_res <- early_ocec_nodups %>%
  filter(Species == "OC25") %>%
  group_by(Date, StateCountySite) %>%
  mutate(num_rows = length(ParamCode)) %>%
  ungroup %>%
  ddply(.(Date, StateCountySite), oc_site_filt) %>%
  arrange(Date, StateCountySite) %>% tbl_df

# test that only one value per siteday:
# oc_res %>% group_by(StateCountySite, Date) %>% mutate(len = length(mean_conc)) %>% filter(len > 1) # WORKS!


ec_res <- early_ocec_nodups %>%
  filter(Species == "EC25") %>%
  group_by(Date, StateCountySite) %>%
  mutate(orig_num_rows = length(ParamCode)) %>%
  ungroup %>%
  ddply(.(Date, StateCountySite), ec_site_filt) %>%
  arrange(Date, StateCountySite, ParamCode) %>% tbl_df

# NEW STUFF

# "ones" are: the concentrations for which the duplicates are not removed. so,
# they are  temporarily (i.e., by a hack) removed
ones <- ec_res %>% filter(ParamCode == 88307) %>%
  group_by(Date, StateCountySite) %>% mutate(len = length(ParamCode)) %>%
  filter(len > 1) %>% select(-MethodName)

ec_no_dups <- ones %>%
  group_by(StateCountySite, Date, Species, lat, long) %>%
  summarize(ec_imp = mean(avg_conc)) %>%  # averages over POC
  select(StateCountySite:Species, avg_conc = ec_imp, everything())

# combine the (manually) dup-eliminated concentrations, with the
# automatically dup-elim'ed concentrations
ec_res2 <- ec_res %>%
  anti_join(ec_no_dups) %>%
  bind_rows(ec_no_dups)


## compute corrections

# For OC corrections, need the revised EC values

# for OC25, corrections only on 88305. Corresponding EC values would have
# paramcode of 88307:
# "problem EC" - a misnomer
prob_ec <- ec_res2 %>% filter(ParamCode == 88307) %>% select(-num_rows)

# need corresponding EC measurements, for the OC corrections
prob_oc_res <- oc_res %>% filter(ParamCode == 88305)
prob_ocec <- prob_oc_res %>%
  spread(Species, avg_conc) %>%
  left_join(
    spread( filter(prob_ec, ParamCode == 88307),  Species, avg_conc),
    by = c("Date", "StateCountySite", "lat", "long", "Year")
  ) %>%
  filter(!is.na(EC25))

# generate oc IMPROVE corrected concentrations
oc_imp <- prob_ocec %>%
  rename(SamplerCode = MethodCode.x) %>%
  gen_oc_imp %>%
  select(StateCountySite:Date, imp_conc = oc_imp, lat:long, Year)

# combine all OC concentrations
# COMPLETE CORRECTED EARLY OC DATA
fin_oc_res <- oc_res %>% anti_join(prob_oc_res) %>%
  select(StateCountySite:Date, imp_conc = avg_conc, lat:long, Year) %>%
  bind_rows(oc_imp) %>%
  filter(!is.na(imp_conc)) %>%
  select(-Year) %>% mutate(Species = "OC25")

fin_oc_res %>% is.na %>% sum # ZERO!

# only ~500 rows with ParamCode 88321, so we just get rid of them

# performs the corrections for EC, whether that's by addition or multiplication,
# then combines both
ec_res2 <- ec_res2 %>% filter(ParamCode != 88321)
adding_ec_res <- ec_res2 %>%
  filter(ParamCode != 88307) %>% # code for not adding
  group_by(Date, StateCountySite, long, lat, Year) %>%
  summarize(ec_imp = sum(avg_conc)) %>%
  select(StateCountySite, Date, ec_imp, lat, long)

mult_ec_res <- ec_res2 %>%
  filter(ParamCode == 88307) %>%
  group_by(Date, StateCountySite, long, lat, Year) %>%
  mutate(ec_imp = 1.3 * avg_conc) %>%
  select(-avg_conc) %>% ungroup %>%
  select(StateCountySite, Date, ec_imp, lat, long)

fin_ec_res <- adding_ec_res %>%
  bind_rows(mult_ec_res) %>%
  mutate(Species = "EC25") %>%
  rename(imp_conc = ec_imp)

fin_ec_res %>% is.na %>% sum # ZERO!!!


fin_ocec_res <- fin_ec_res %>% bind_rows(fin_oc_res)

####

# AQS OC/EC

corrected_aqs_ocec <- later_ocec %>%
  select(StateCountySite:Date, imp_conc = avg_conc, Species) %>%
  bind_rows(
    select(fin_ocec_res, -lat, -long)
    ) %>%
  rename(Conc_obs = imp_conc) %>% mutate(Year = year(ymd(Date)))

corrected_aqs_ocec %>% is.na %>% sum # ZERO !!!
corrected_aqs_ocec %>% saveRDS("../hsa_data/corrected_ocec_all.rds")

