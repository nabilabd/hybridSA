#' Extract data from an excel worksheet
#'
#' @param wb_path character vector, path of the xl workbook to load data from
#' @param sheetname character vector, name of the sheet in the workbook to load
#'  data from
#'
#' @importFrom XLConnect loadWorkbook readWorksheet
getxl_sheet <- function(wb_path, sheetname) {
  testbook <- loadWorkbook(wb_path);
  readWorksheet(testbook, sheet = sheetname)
}

#' Read in AQS data from folder of xlsx files
#'
#' Given a directory of AQS excel workbooks, reads the data from all the
#' sheets (named "UNC"), which contain uncertainty values, and the data from the
#' sheets (named "CONC") containing observed concentrations, and combines the
#' values all into a single dataframe.
#'
#' Many assumptions are made concerning the naming of the files. For more
#' details, see the vignette describing the AQS data files
#'
#' @param fol_path file path (relative or complete) of the folder containing
#'  the AQS files
#' @param year character vector of year the data corresponds to
#'
#' @importFrom tidyr gather_
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @importFrom assertthat assert_that
#' @importFrom plyr ldply mutate arrange join
#' @importFrom stringr str_c str_length str_sub
#' @return a dataframe of the aggregated data
#'
#' @export
aqs_merge <- function(fol_path, yr) {
  # takes path of folder, checks that folder non-empty, checks all files
  # for same year

  loc_files <- dir(fol_path) # ; yr <- str_sub(fol_path, -9, -6)
  num_cols <- 43 # SiteID column, Date, and 41 species
  assert_that(all(str_length(loc_files) > 0)) # ensure all filenames non-empty

  # combine all. TODO: fix the check on filenames
  aqs_files <- dir(fol_path, pattern = str_c(yr,"\\.xlsx"), full.names = TRUE)
  aqs_files <- aqs_files[str_sub(basename(aqs_files), 1, 3)=="AQS"] # FIX HACK
  names(aqs_files) <- str_sub(basename(aqs_files), 8, -11) # extract site id

  # read in data from "UNC" and "CONC" sheets, delete empty columns
  all_unc  <- ldply(aqs_files, getxl_sheet, "UNC", .id="SiteID") %>% compact
  all_conc <- ldply(aqs_files, getxl_sheet, "CONC", .id="SiteID") %>% compact

  # check the two dataframes have same number of columns
  assert_that(length(all_unc) == num_cols)
  assert_that(length(all_conc) == num_cols)

  # return merged results; "gather"-ing alphabetizes species names
  # checked: results matched with aqs_agg from before
  plyr::join(
    gather_(all_conc, "Species", "Conc_obs", species),
    gather_(all_unc, "Species", "sig_c_obs", species)
  ) %>%
    mutate(Date=as.character(ymd(Date)), SiteID=as.character(SiteID)) %>%
    mutate(Species = as.character(Species)) %>%
    arrange(., SiteID, Date, Species)
}


