
rm_space_names <- function(df) {
  colnames(df) <- str_replace_all(colnames(df), " ", "")
  df
}

#' Is the vector sorted?
is_sorted <- function(vec) all(vec == sort(vec))

