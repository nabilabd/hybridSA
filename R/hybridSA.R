load("data/sig_ctm06.rda"); load("data/sig_lnr06.rda")

#' Evaluate Hybrid SA Objective function
#'
#' Describe this.
#'
#' @param Rj vector of R_j values. Has length equal to the number of sources.
#' @param asens 2d matrix of average (daily) sensitivities, with rows corresponding to
#'  species and columns corresponding to sources. The sensitivies are in the
#'  CMAQ output, along with simulated concentrations
#' @param c_obs observed concentration
#' @param csim simulated concentration
#' @param sig_c_obs uncertainties in observed concentrations
#' @param sig_ctm uncertainty in ....
#' @param sig_lnr uncertainty in Rj value (???)
#'
#' @return a dataframe containing Rj values for the sources
#'  @details Gamma is a parameter specified by the user, and not necessarily
#'    bound to any observed or simulated data. The simulated concentrations
#'    are needed to obtain the error, which is given by c_obs - c_sim, the
#'    difference between the observed concentrations and the simulated
#'    concentrations.
#'
#'
#'  @references \url{http://atmos-chem-phys.net/14/5415/2014/}
#'
#'
#'
#'  @export
#'
hybridsa <- function (Rj, c_obs, csim, asens, sig_c_obs, sig_ctm, sig_lnr) {
  # The hybrid function contains three summations, a first, a second, and a
  # summation within the first summation.

  # directly calculate:
  weight <- 1 / ( sig_c_obs^2 + (c_obs * sig_ctm)^2 )
  asens <- as.matrix(asens)
  error <- c_obs - csim

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
#' @param conc_by_day concentrations of chemical species for a given day
#' @param sens_by_day matrix of sensitivities, for a given day
#' @param .sources sources of PM2.5
#'
#' @return vector of length 20, either of NA's if input has any missing values,
#'  or numeric vector otherwise
#'
#' @importFrom nloptr lbfgs slsqp
#' @importFrom assertthat is.error
#'
#' @export
get_optim <- function(conc_by_day, sens_by_day, .sources) {

  # return NA values for the optimization if there are any missing values
  # in the SA sensitivity matrix or observed values
  cols <- c("sig_c_obs", "Conc_obs", "c_sim")
  num_sources <- length(.sources)
  res <- data.frame(Source = .sources, Rj_vals = rep(NA, num_sources)) # default

  if(anyNA(conc_by_day[, cols]) | anyNA(sens_by_day[, .sources])) {
    return(res)
  }

  # define parameters
  lb <- rep(0.1, num_sources); ub <- rep(10.0, num_sources) # Suni used these
  optim_init <- rep(1, num_sources) # initial values; vec of 1's

  # otherwise, if no missing values, then perform the optimization
  opt_vals <- try({nloptr::lbfgs(x0=optim_init, fn=hybridsa, lower=lb, upper=ub,
                                 control=list(maxeval=200, xtol_rel=1e-6),
                                 c_obs = conc_by_day$Conc_obs,
                                 csim = conc_by_day$c_sim,
                                 asens=sens_by_day[, .sources],
                                 sig_c_obs = conc_by_day$sig_c_obs,
                                 sig_lnr = sig_lnr06,
                                 sig_ctm = sig_ctm06)}, silent = TRUE)

  # if no error, then store the values, and return the results
  if( !assertthat::is.error(opt_vals) ) {
    res$Rj_vals = opt_vals$par
    return(res)
  }

  # second attempt
  opt_vals <- try({nloptr::slsqp(x0=optim_init, fn=hybridsa, lower=lb, upper=ub,
                                 control=list(maxeval=200, xtol_rel=1e-6),
                                 c_obs = conc_by_day$Conc_obs,
                                 csim = conc_by_day$c_sim,
                                 asens=sens_by_day[, .sources],
                                 sig_c_obs = conc_by_day$sig_c_obs,
                                 sig_lnr = sig_lnr06,
                                 sig_ctm = sig_ctm06)}, silent = TRUE)

  # if no error, then store the values. Otherwise, the NA's are returned
  if(!is.atomic(opt_vals)) res$Rj_vals <- opt_vals$par
  return(res)
}


