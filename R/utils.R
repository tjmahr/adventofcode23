
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


chr_extract <- function(x, pattern, ...) {
  matches <- regexpr(pattern, x)
  r <- regmatches(x, matches)
  x[which(matches > 0)] <- r
  x[which(matches < 0)] <- NA_character_
  x
}


vec_replace_na <- function(xs, replacement) {
  xs[is.na(xs)] <- replacement
  xs
}

list_map2 <- function(xs, ys, f, ...) {
  Map(f, xs, ys, ...)
}

list_filter <- function(xs, f) {
  Filter(f, xs)
}

list_invoke <- function(args, what, ...) {
  do.call(what, args, ...)
}
