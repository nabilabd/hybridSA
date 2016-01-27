


#' Form spatial domain of Rj value interpolation
#'
#' Results with points having coordinates both for Lambert Conformal Conical
#' projection and Latitude/Longitude.
#'
#' Adds indices to grid locations, if complete spatial grid is 
#' generated.
#'
#' @importFrom rdgal spTransform
#' @importFrom sp SpatialPoints
#' @importFrom magrittr set_colnames %>%
#' @export
make_grid <- function(xstep = 1, ystep = 1) {
  
  # define boundaries, in meters
  gwidth <- 36000
  x_range <- c(-2718000, 2574000)
  y_range <- c(-2070000, 1926000)
  
  xpoints <- seq(x_range[1], x_range[2], xstep * gwidth)
  ypoints <- seq(y_range[1], y_range[2], ystep * gwidth)
  
  # form grid in lambert projection, then remove all extra attributes 
  lam_us_grid <- expand.grid(xpoints, ypoints) %>% 
    set_colnames(c("Xm", "Ym")) %>% { attr(., "out.attrs") <- NULL; . }
  
  # add long/lat coords
  us_grid <- lam_us_grid %>% as.data.frame %>% 
    SpatialPoints(., proj4string =CRS(lcc_proj4 )) %>% 
    spTransform(., CRS("+init=epsg:4326")) %>% as.data.frame %>% 
    set_colnames(c("LON", "LAT")) %>% cbind(lam_us_grid)
  
  if(xstep == 1 & ystep == 1) {
    us_grid <- cbind(SiteInd = 1:nrow(lam_us_grid), us_grid)
  }
  
  return(us_grid)
}


#' Generate Spatio-Temporal Grid for Predictions
#' 
#' TODO: IMPROVE READABILITY
#' 
#' @param year year to interpolate for
#' @param sname name of source
#' @importFrom plyr ldply
#' @importFrom magrittr %>%
pred_grid <- function(year, sname, ...) {
  loc_grid <- make_grid(...)
  
  src_grid <- loc_grid %>% 
    mutate(SiteInd = 1:nrow(.), Source = sname, Rj_vals = NA) %>% {
      res <- .
      seq(ymd("2006-01-01"), ymd("2006-12-31"), by = "day") %>%   
        as.character %>% 
        ldply(function(x) mutate(res, Date = as.POSIXct(x))) 
    }
  
  src_grid
}

#' Generates matrix of unique spatial locations
#' AM I STILL USEFUL??
#' @importFrom magrittr %>%
#' 
pt_mat <- function(df) {
  df %>% select(LON, LAT) %>% unique %>% as.matrix
}



