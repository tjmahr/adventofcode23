#' Day 13: Point of Incidence
#'
#' [Point of Incidence](https://adventofcode.com/2023/day/13)
#'
#' @name day13
#' @rdname day13
#' @details
#'
#' **Part One**
#'
#' With your help, the hot springs team locates an appropriate spring which
#' launches you neatly and precisely up to the edge of *Lava Island*.
#'
#' There\'s just one problem: you don\'t see any *lava*.
#'
#' You *do* see a lot of ash and igneous rock; there are even what look
#' like gray mountains scattered around. After a while, you make your way
#' to a nearby cluster of mountains only to discover that the valley
#' between them is completely full of large *mirrors*. Most of the mirrors
#' seem to be aligned in a consistent way; perhaps you should head in that
#' direction?
#'
#' As you move through the valley of mirrors, you find that several of them
#' have fallen from the large metal frames keeping them in place. The
#' mirrors are extremely flat and shiny, and many of the fallen mirrors
#' have lodged into the ash at strange angles. Because the terrain is all
#' one color, it\'s hard to tell where it\'s safe to walk or where you\'re
#' about to run into a mirror.
#'
#' You note down the patterns of ash (`.`) and rocks (`#`) that you see as
#' you walk (your puzzle input); perhaps by carefully analyzing these
#' patterns, you can figure out where the mirrors are!
#'
#' For example:
#'
#'     #.##..##.
#'     ..#.##.#.
#'     ##......#
#'     ##......#
#'     ..#.##.#.
#'     ..##..##.
#'     #.#.##.#.
#'
#'     #...##..#
#'     #....#..#
#'     ..##..###
#'     #####.##.
#'     #####.##.
#'     ..##..###
#'     #....#..#
#'
#' To find the reflection in each pattern, you need to find a perfect
#' reflection across either a horizontal line between two rows or across a
#' vertical line between two columns.
#'
#' In the first pattern, the reflection is across a vertical line between
#' two columns; arrows on each of the two columns point at the line between
#' the columns:
#'
#'     123456789
#'         ><
#'     #.##..##.
#'     ..#.##.#.
#'     ##......#
#'     ##......#
#'     ..#.##.#.
#'     ..##..##.
#'     #.#.##.#.
#'         ><
#'     123456789
#'
#' In this pattern, the line of reflection is the vertical line between
#' columns 5 and 6. Because the vertical line is not perfectly in the
#' middle of the pattern, part of the pattern (column 1) has nowhere to
#' reflect onto and can be ignored; every other column has a reflected
#' column within the pattern and must match exactly: column 2 matches
#' column 9, column 3 matches 8, 4 matches 7, and 5 matches 6.
#'
#' The second pattern reflects across a horizontal line instead:
#'
#'     1 #...##..# 1
#'     2 #....#..# 2
#'     3 ..##..### 3
#'     4v#####.##.v4
#'     5^#####.##.^5
#'     6 ..##..### 6
#'     7 #....#..# 7
#'
#' This pattern reflects across the horizontal line between rows 4 and 5.
#' Row 1 would reflect with a hypothetical row 8, but since that\'s not in
#' the pattern, row 1 doesn\'t need to match anything. The remaining rows
#' match: row 2 matches row 7, row 3 matches row 6, and row 4 matches row
#' 5.
#'
#' To *summarize* your pattern notes, add up *the number of columns* to the
#' left of each vertical line of reflection; to that, also add *100
#' multiplied by the number of rows* above each horizontal line of
#' reflection. In the above example, the first pattern\'s vertical line has
#' `5` columns to its left and the second pattern\'s horizontal line has
#' `4` rows above it, a total of *`405`*.
#'
#' Find the line of reflection in each of the patterns in your notes. *What
#' number do you get after summarizing all of your notes?*
#'
#' **Part Two**
#'
#' You resume walking through the valley of mirrors and - *SMACK!* - run
#' directly into one. Hopefully [nobody]{title="Sorry, Nobody saw that."}
#' was watching, because that must have been pretty embarrassing.
#'
#' Upon closer inspection, you discover that every mirror has exactly one
#' *smudge*: exactly one `.` or `#` should be the opposite type.
#'
#' In each pattern, you\'ll need to locate and fix the smudge that causes a
#' *different reflection line* to be valid. (The old reflection line won\'t
#' necessarily continue being valid after the smudge is fixed.)
#'
#' Here\'s the above example again:
#'
#'     #.##..##.
#'     ..#.##.#.
#'     ##......#
#'     ##......#
#'     ..#.##.#.
#'     ..##..##.
#'     #.#.##.#.
#'
#'     #...##..#
#'     #....#..#
#'     ..##..###
#'     #####.##.
#'     #####.##.
#'     ..##..###
#'     #....#..#
#'
#' The first pattern\'s smudge is in the top-left corner. If the top-left
#' `#` were instead `.`, it would have a different, horizontal line of
#' reflection:
#'
#'     1 ..##..##. 1
#'     2 ..#.##.#. 2
#'     3v##......#v3
#'     4^##......#^4
#'     5 ..#.##.#. 5
#'     6 ..##..##. 6
#'     7 #.#.##.#. 7
#'
#' With the smudge in the top-left corner repaired, a new horizontal line
#' of reflection between rows 3 and 4 now exists. Row 7 has no
#' corresponding reflected row and can be ignored, but every other row
#' matches exactly: row 1 matches row 6, row 2 matches row 5, and row 3
#' matches row 4.
#'
#' In the second pattern, the smudge can be fixed by changing the fifth
#' symbol on row 2 from `.` to `#`:
#'
#'     1v#...##..#v1
#'     2^#...##..#^2
#'     3 ..##..### 3
#'     4 #####.##. 4
#'     5 #####.##. 5
#'     6 ..##..### 6
#'     7 #....#..# 7
#'
#' Now, the pattern has a different horizontal line of reflection between
#' rows 1 and 2.
#'
#' Summarize your notes as before, but instead use the new different
#' reflection lines. In this example, the first pattern\'s new horizontal
#' line has 3 rows above it and the second pattern\'s new horizontal line
#' has 1 row above it, summarizing to the value *`400`*.
#'
#' In each pattern, fix the smudge and find the different line of
#' reflection. *What number do you get after summarizing the new reflection
#' line in each pattern in your notes?*
#'
#' @param x some data
#' @return For Part One, `f13a_summarize_mirrors(x)` returns the summary of the
#'   fold axes. For Part Two, `f13b_summarize_adjusted_mirrors(x)` returns the
#'   summary of newly found fold axes.
#' @export
#' @examples
#' f13a_summarize_mirrors(example_data_13())
#' f13b_summarize_adjusted_mirrors(example_data_13())
f13a_summarize_mirrors <- function(x) {
  vec_which_or_zero <- function(xs) {
    w <- which(xs)
    if (length(w)) w else 0
  }

  data <- f13_helper(x)
  data$folds |>
    lapply(function(l) {
      l[[1]] <- vec_which_or_zero(l[[1]])
      l[[2]] <- vec_which_or_zero(l[[2]]) * 100
      l
    }) |>
    unlist() |>
    sum()
}


#' @rdname day13
#' @export
f13b_summarize_adjusted_mirrors <- function(x) {
  vec_value_or_zero <- function(xs) {
    if (length(xs)) xs else 0
  }

  # The solution wants the sums from the newly uncovered lines.
  # We recorded this information when looking for the smudges.

  data <- f13_helper(x)
  data$smudges |>
    lapply(function(l) {
      l[[1]] <- vec_value_or_zero(l[[1]])
      l[[2]] <- vec_value_or_zero(l[[2]]) * 100
      l[[3]] <- NULL
      l
    }) |>
    unlist() |>
    sum()
}


f13_helper <- function(x) {
  # (4, 8) -> (1,2,3,4) (8,7,6,5)
  # (4, 6) -> (3,4) (6,5)
  get_fold_indices <- function(i, length) {
    left <- seq_len(i)
    right <- left + vec_last(left)
    right <- right[right <= length]
    left <- vec_tail(left, length(right))
    list(left = left, right = rev(right))
  }

  # TRUE for any folds with identical halves
  check_folds_in_vector <- function(xs) {
    seq_len(length(xs) - 1) |>
      lapply(get_fold_indices, length(xs)) |>
      lapply(function(l) xs[l$left] == xs[l$right]) |>
      lapply(all) |>
      unlist()
  }

  check_folds_in_matrix <- function(mat) {
    col_mirrors <- mat |>
      apply(1, check_folds_in_vector) |>
      apply(1, all)
    row_mirrors <- mat |>
      apply(2, check_folds_in_vector) |>
      apply(1, all)
    list(col_mirrors = col_mirrors, row_mirrors = row_mirrors)
  }

  locate_smudge <- function(mat) {
    # Create a matrix for each half of the reflection.
    # Look for a pair matrices with only one TRUE in mat1 != mat2.

    results <- list(col_mirrors = numeric(), row_mirrors = numeric())

    row_smudge <- FALSE
    for (i in seq_len(nrow(mat) - 1)) {
      l <- get_fold_indices(i, nrow(mat))
      mask <- mat[l$left, , drop = FALSE] != mat[l$right, , drop = FALSE]
      # This becomes a new fold axis when smudge is flipped
      if (sum(mask) == 1) {
        row_smudge <- TRUE
        smudge_location <- which(mask, arr.ind = TRUE)
        results$row_mirrors <- i
        break
      }
    }

    if (!row_smudge) {
      col_smudge <- FALSE
      for (i in seq_len(ncol(mat) - 1)) {
        l <- get_fold_indices(i, ncol(mat))
        mask <- mat[, l$left, drop = FALSE] != mat[, l$right, drop = FALSE]
        if (sum(mask) == 1) {
          col_smudge <- TRUE
          smudge_location <- which(mask, arr.ind = TRUE)
          results$col_mirrors <- i
          break
        }
      }
    }
    results$smudge_location <- smudge_location
    results
  }

  runs <- cumsum(x == "") + 1
  to_drop <- vec_which_value(x, "")
  l <- x[-to_drop] |> split(runs[-to_drop])
  l <- l |> lapply(strsplit, "") |> lapply(list_invoke, rbind)

  data <- list()
  data$grids <- l
  data$folds <- data$grids |> lapply(check_folds_in_matrix)

  data$smudges <- data$grids |> lapply(locate_smudge)
  data
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day13
#' @export
example_data_13 <- function(example = 1) {
  l <- list(
    a = c(
      "#.##..##.",
      "..#.##.#.",
      "##......#",
      "##......#",
      "..#.##.#.",
      "..##..##.",
      "#.#.##.#.",
      "",
      "#...##..#",
      "#....#..#",
      "..##..###",
      "#####.##.",
      "#####.##.",
      "..##..###",
      "#....#..#"
    )
  )
  l[[example]]
}
