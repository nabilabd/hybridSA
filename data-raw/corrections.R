
#' Filter CSN data to obtain single OC measurement per site-day
#'
#' CSN measurement data can have the
#'
#' @param ocdf dataframe wtih oc measurements
oc_site_filt <- function(ocdf) {

  # filtering only needed if there are multiple measurements
  if(nrow(ocdf) == 1) return(ocdf)

  # define values
  oc_codes <- c(88320, 88305)
  ocdf <- ocdf %>% filter(ParamCode %in% oc_codes) # in case other params used

  if(oc_codes[1] %in% ocdf$ParamCode) {
    res <- filter(ocdf, ParamCode == oc_codes[1])
  } else {
    res <- filter(ocdf, ParamCode == oc_codes[2])
  }

  # return results
  return(res)
}

#' Remove Duplicated Measurements from Monitoring Sites
#'
#' The original data has duplicated measurements by different instruments for
#' some sites. For those sites, measurements for a single instrument needs to
#' be chosen.
#'
#' @details non-88307 param codes need to be removed prior, for locations with
#'  two measurements
#'
#' @param ecdf
ec_site_filt <- function(ecdf) {

  # filtering only needed if there are "excess" measurements
  # if there are one or three rows, then can uniquely determined corrected concentration
  if( nrow(ecdf) %in% c(1, 3) ) return(ecdf)

  # define values
  ec_codes <- c(88329:88331, 88307)
  ecdf <- ecdf %>% filter(ParamCode %in% ec_codes) # remove other codes
  # select(StateCountySite, Date, Species, avg_conc, lat:long, ParamCode, Year)

  if( all(ec_codes[1:3] %in% ecdf$ParamCode) ) {
    res <- filter(ecdf, ParamCode %in% ec_codes[1:3])
  } else if(ec_codes[4] %in% ecdf$ParamCode) {
    res <- filter(ecdf, ParamCode %in% ec_codes[4])
  } else
    return(ecdf)

  # after filtering, should have either 1 or 3, or else there might be twice
  # the measurements.
  if(nrow(res) %% 2 == 0) {
    poc_vals <- unique(res$POC) # just takes one of them, since doesn't sort
    res <- filter(res, POC == poc_vals[1]) # THIS IS ARBITRARY
  }

  # keeping first row, doesn't change # of NA's in oc_res2
  # if(nrow(res) > 1) { #  && unique(res$ParamCode) == 88307) {
  #   res <- res[1, ] # try results w/o this condition
  #   # res <- res %>%
  #   #   group_by(StateCountySite, Date, long, lat, Year) %>%
  #   #   summarize(avg_conc = mean(avg_conc))
  # }

  # return results
  return(res)
}


#' Generate IMPROVE OC concentrations
#'
#'
#' @param df dataframe with CSN data for OC (paramCode 88305), and EC
#'  (paramCode 88307). This dataframe is assumed the result of left-joining
#'  the EC dataframe to the OC dataframe
#'
#'  @importFrom dplyr mutate left_join rename
#'  @importFrom magrittr `%>%`
#'
#' @references pg 4 of Carbon Spatial temporal Patterns pdf, by Schichtel et al.
#'
gen_oc_imp <- function(df) {

  # define correction factors

  df2 <- df %>%
    mutate(mon = month(ymd(Date))) %>%
    left_join(addit, by = c("mon" = "month")) %>%
    left_join(multip, by = c("SamplerCode.x" = "SamplerCode")) %>%
    rename(A = addit_corr, M = SA)

  # compute then return results
  df3 <- df2 %>% mutate( oc_imp = ( (OC25 - .3 * EC25) - A) / M )
  df3
}


#' Generate IMPROVE EC concentrations
#'
#'
#' @param df dataframe with a single non-IMPROVE EC concentration per site-day
#'
#' @importFrom dplyr filter mutate
#' @importFrom magrittr `%>%`
gen_ec_imp <- function(df) {

  # define correction factors
  imp_codes <- c(809, 814, 841)

  # split based on measurement type
  ec_res <- df %>% filter(MethodCode %in% imp_codes)

  # correct the non-IMPROVE measurements
  not_ec_res <- df %>% filter( !(MethodCode %in% imp_codes) )
  not_ec_res <- not_ec_res %>% mutate(avg_conc = 1.3 * avg_conc)

  # combine then return results
  df2 <- rbind(ec_res, not_ec_res)
  df2
}

