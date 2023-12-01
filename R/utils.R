
# let's try working without stringr!

chr_replace <- function(x, pattern, replacement, ...) {
  gsub(pattern = pattern, x = x, replacement = replacement, ...)
}

chr_replace_many <- function(x, plan, ...) {
  for (pattern in names(plan)) {
    x <- chr_replace(x, pattern, plan[pattern])
  }
  x
}

chr_reverse <- function(x) {
  x |>
    strsplit("") |>
    lapply(rev) |>
    lapply(paste0, collapse = "") |>
    unlist()
}
