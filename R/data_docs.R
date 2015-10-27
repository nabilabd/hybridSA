#' Geographic data on 188 CSN sites
#'
#' A dataset containing information on the geographic location of Chemical
#' Speciation Network (CSN) sites. More detailed descriptions of the
#' fields/codes contained in it can be found via the url below (see "source").
#'
#' @docType data
#' @name csn_site_index2
#' @format A data frame with 188 rows and 9 columns
#' \describe{
#'   \item{SiteID}{Site ID of the monitoring station. The first }
#'   \item{LON}{Longitude of the site}
#'   \item{LAT}{Latitude of the site}
#'   \item{Xm}{horizontal coordinate of the site in Lambert Conformal Conical (LCC) projetion, in meters}
#'   \item{Ym}{vertical coordinate of the site in LCC projetion, in meters}
#'   \item{StateCountySite}{the state code, county code, and site number of the
#'   monitoring site, separated by hyphens. This makes it easier to merge wtih AQS
#'   data available online.}
#'   \item{i,j}{respectively, the horizontal and vertical grid cell coordinates of the
#'   monitoring sites}
#'   \item{Network}{monitoring network the site is in}
#' }
#' @source \url{http://www.epa.gov/ttn/airs/airsaqs/manuals/codedescs.htm}
NULL

#' Uncertainties for the optimization
#'
#' A named vector of length 41, with the \eqn{\sigma_{SR_i^CTM}} values for
#' each chemical species for 2006. These are taken to be constant throughout
#' the year. Note that these values need to be squared before being used in
#' the hybrid equation.
#'
#' @docType data
#' @name sig_ctm06
#' @source \url{http://www.diamondse.info/}
#' @references \url{http://atmos-chem-phys.net/14/5415/2014/}
NULL

#' Uncertainties of ln(Rj) values
#'
#' An (un)named vector of length 20, with the \eqn{\sigma_{ln R_i}} values for
#' each source for 2006. These are taken to be constant throughout
#' the year. Note that these values need to be squared before being used in
#' the hybrid equation.
#'
#' @docType data
#' @name sig_lnr06
#' @source \url{http://www.diamondse.info/}
#' @references \url{http://atmos-chem-phys.net/14/5415/2014/}
NULL

#' Parameter Codes Used in Selecting Observations
#'
#' A dataset containing the parameter codes of the chemical species for which
#' the observational data was obtained.
#'
#' @docType data
#' @name all_param_codenames
#' @format A data frame with 46 rows and 2 columns
NULL

#' All Parameter and Method Codes in AQS data
#'
#' A dataset containing the unique combinations of Parameter and Method Codes
#' contained the in AQS observation data.
#'
#' @docType data
#' @name all_param_codenames
#' @format A data frame with 1348 rows and 5 columns
NULL

