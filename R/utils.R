

chr_match_matrix <- function(x, pattern) {
  m <- regexec(pattern, x)
  r <- regmatches(x, m)
  lapply(r, matrix, nrow = 1)
}

chr_replace <- function(x, pattern, replacement, ...) {
  gsub(pattern = pattern, x = x, replacement = replacement, ...)
}

chr_replace_many <- function(x, plan, ...) {
  for (pattern in names(plan)) {
    x <- chr_replace(x, pattern, plan[pattern], ...)
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
  matches <- regexpr(pattern, x, ...)
  r <- regmatches(x, matches)
  x[which(matches > 0)] <- r
  x[which(matches < 0)] <- NA_character_
  x
}

df_add_column <- function(df, values, name) {
  # length(NULL) is 0, nrow(NULL) is NULL
  if (length(df) && nrow(df)) {
    df[[name]] <- values
  }
  df
}

df_add_rowid <- function(df, name) {
  df_add_column(df, seq_len(nrow(df)), name)
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

vec_element <- function(xs, i) {
  xs[[i]]
}

vec_index <- function(xs, i) {
  xs[i]
}

vec_index_into <- function(i, xs) {
  xs[i]
}


mod1 <- function(xs, m) {
  r <- xs %% m
  ifelse(r == 0, m, r)
}


vec_tail <- function(xs, n = 6) {
  n <- min(c(length(xs), n))
  if (n < 0) n <- length(xs) + n
  xs[seq(to = length(xs), length.out = n)]
}

vec_last <- function(xs) {
  vec_tail(xs, 1)
}

vec_head <- function(xs, n = 6) {
  n <- min(c(length(xs), n))
  if (n < 0) n <- length(xs) + n
  xs[seq(from = 1, length.out = n)]
}

vec_but_tail <- function(xs, n) {
  vec_head(xs, -n)
}



vec_wrapped_index_into <- function(i, xs) {
  i |>
    mod1(length(xs)) |>
    vec_index_into(xs)
}


vec_replace_na <- function(xs, replacement) {
  xs[is.na(xs)] <- replacement
  xs
}

vec_remove_na <- function(xs) {
  xs[!is.na(xs)]
}

vec_set_names <- function(xs, ns) {
  names(xs) <- ns
  xs
}

mat_set_rownames <- function(x, value) {
  rownames(x) <- value
  x
}

mat_set_colnames <- function(x, value) {
  colnames(x) <- value
  x
}
