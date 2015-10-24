#' Evaluate Hybrid SA Objective function
#'
#' Describe this.
#'
#' @param Rj vector of R_j values. Has length equal to the number of sources.
#' @param error difference in observations and simulated concentrations, given
#'  by c_obs - c_init
#' @param asens 2d matrix of average sensitivities, with rows corresponding to
#'  species and columns corresponding to sources. The sensitivies are in the
#'  CMAQ output, along with simulated concentrations
#' @param w2 vector of values eqtn{sigma^{-2}_{ln(R_j)}}.
#' @param weight the reciprocal of eqtn{sigma^2_{c_i^obs} + sigma^2_{SR_i^CTM}}; a
#'  constant in the first summation term. sigma^2_{c_i^obs} is the uncertainty
#'  in the observed concentration of species "i". eqtn{sigma^2_{SR_i^CTM}} is ...
#' @param gama value of the Gamma weight for the second summation in the Hybrid
#'  equation
#'
#'  @details Gamma is a parameter specified by the user, and not necessarily
#'    bound to any observed or simulated data. The simulated concentrations
#'    are needed to obtain the error, which is given by c_obs - c_sim, the
#'    difference between the observed concentrations and the simulated
#'    concentrations.
#'
#'
#'  @references \url{http://atmos-chem-phys.net/14/5415/2014/}
#'  @export
#'
hybridsa <- function (Rj, c_obs, csim, asens, sig_c_obs, sig_ctm, sig_lnr) {
  # The hybrid function contains three summations, a first, a second, and a
  # summation within the first summation.

  # directly calculate:
  weight <- 1 / ( sig_c_obs^2 + (c_obs * sig_ctm)^2 )
  asens <- asens %>% as.matrix
  error = c_obs - csim

  # initialize constants, and placeholders for sums
  num_species <- nrow(asens); num_sources <- ncol(asens)
  summ1 <- summ2 <- 0

  for (i in 1:num_species) {
    # performs vectorized mutliplication, then sums the elements of the result
    inner_sum <- sum(asens[i, ] * (Rj - 1))
    summ1 <- summ1 + weight[i]*(error[i]-inner_sum)^2
  }

  # instantiate last variable (remove it from parameters?)
  gama <- summ1 / 20

  # calculate second summation
  for (j in 1:num_sources) summ2 <- summ2 + gama*(log(Rj[j]) / sig_lnr[j])^2

  func_val <- summ1 + summ2
  func_val
}



#' Get optimization values given asens and
#'
#' @details this is a function in beta
#'
#'
#' @param conc_by_day
#' @param sens_by_day
#' @return vector of length 20, either of NA's if input has any missing values,
#'  or numeric vector otherwise
#' @export
get_optim <- function(conc_by_day, sens_by_day, .sources = sources) {

  # TODO: DEFINE "SOURCES"
  # return NA values for the optimization if there are any missing values
  # in the SA sensitivity matrix or observed values
  cols <- c("sig_c_obs", "Conc_obs", "c_sim")
  leng <- length(.sources)

  if(anyNA(conc_by_day[, cols]) | anyNA(sens_by_day[, .sources])) {
    return(rep(NA, leng))
  }

  # define parameters
  num_sources <- length(.sources)
  lb1 <- rep(0.1, num_sources); ub1 <- rep(10.0, num_sources) # Suni used these
  optim_init <- rep(1, num_sources) # initial values; vec of 20 1's

  # otherwise, if no missing values, then perform the optimization
  # if
  opt_vals <- try({nloptr::lbfgs(x0=optim_init, fn=hybridsa, lower=lb1, upper=ub1,
                                 control=list(maxeval=200, xtol_rel=1e-6),
                                 c_obs = conc_by_day$Conc_obs,
                                 csim = conc_by_day$c_sim,
                                 asens=sens_by_day[, .sources],
                                 sig_c_obs = conc_by_day$sig_c_obs,
                                 sig_lnr = sig_lnr06,
                                 sig_ctm = sig_ctm06)}, silent = TRUE)

  if( !is.error(opt_vals) ) return(opt_vals$par)

  # second attempt
  opt_vals <- try({nloptr::slsqp(x0=optim_init, fn=hybridsa, lower=lb1, upper=ub1,
                                 control=list(maxeval=200, xtol_rel=1e-6),
                                 c_obs = conc_by_day$Conc_obs,
                                 csim = conc_by_day$c_sim,
                                 asens=sens_by_day[, .sources],
                                 sig_c_obs = conc_by_day$sig_c_obs,
                                 sig_lnr = sig_lnr06,
                                 sig_ctm = sig_ctm06)}, silent = TRUE)

  if(is.atomic(opt_vals)) return( rep(NA, length(.sources)) )
  else return(opt_vals$par)
}





