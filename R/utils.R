
allNA <- function(x) all(is.na(x))


compact <- function(x) x[!vapply(x, allNA, logical(1))]


rm_space_names <- function(df) {
  colnames(df) <- str_replace_all(colnames(df), " ", "")
  df
}

# Is the vector sorted?
is_sorted <- function(vec) all(vec == sort(vec))

