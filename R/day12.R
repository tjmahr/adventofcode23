#' Day 12: Hot Springs
#'
#' [Hot Springs](https://adventofcode.com/2023/day/12)
#'
#' @name day12
#' @rdname day12
#' @details
#'
#' **Part One**
#'
#' You finally reach the hot springs! You can see steam rising from
#' secluded areas attached to the primary, ornate building.
#'
#' As you turn to enter, the [researcher](11) stops you. \"Wait - I thought
#' you were looking for the hot springs, weren\'t you?\" You indicate that
#' this definitely looks like hot springs to you.
#'
#' \"Oh, sorry, common mistake! This is actually the
#' [onsen](https://en.wikipedia.org/wiki/Onsen){target="_blank"}! The hot
#' springs are next door.\"
#'
#' You look in the direction the researcher is pointing and suddenly notice
#' the [massive metal helixes]{title="I love this joke. I'm not sorry."}
#' towering overhead. \"This way!\"
#'
#' It only takes you a few more steps to reach the main gate of the massive
#' fenced-off area containing the springs. You go through the gate and into
#' a small administrative building.
#'
#' \"Hello! What brings you to the hot springs today? Sorry they\'re not
#' very hot right now; we\'re having a *lava shortage* at the moment.\" You
#' ask about the missing machine parts for Desert Island.
#'
#' \"Oh, all of Gear Island is currently offline! Nothing is being
#' manufactured at the moment, not until we get more lava to heat our
#' forges. And our springs. The springs aren\'t very springy unless
#' they\'re hot!\"
#'
#' \"Say, could you go up and see why the lava stopped flowing? The springs
#' are too cold for normal operation, but we should be able to find one
#' springy enough to launch *you* up there!\"
#'
#' There\'s just one problem - many of the springs have fallen into
#' disrepair, so they\'re not actually sure which springs would even be
#' *safe* to use! Worse yet, their *condition records of which springs are
#' damaged* (your puzzle input) are also damaged! You\'ll need to help them
#' repair the damaged records.
#'
#' In the giant field just outside, the springs are arranged into *rows*.
#' For each row, the condition records show every spring and whether it is
#' *operational* (`.`) or *damaged* (`#`). This is the part of the
#' condition records that is itself damaged; for some springs, it is simply
#' *unknown* (`?`) whether the spring is operational or damaged.
#'
#' However, the engineer that produced the condition records also
#' duplicated some of this information in a different format! After the
#' list of springs for a given row, the size of each *contiguous group of
#' damaged springs* is listed in the order those groups appear in the row.
#' This list always accounts for every damaged spring, and each number is
#' the entire size of its contiguous group (that is, groups are always
#' separated by at least one operational spring: `####` would always be
#' `4`, never `2,2`).
#'
#' So, condition records with no unknown spring conditions might look like
#' this:
#'
#'     #.#.### 1,1,3
#'     .#...#....###. 1,1,3
#'     .#.###.#.###### 1,3,1,6
#'     ####.#...#... 4,1,1
#'     #....######..#####. 1,6,5
#'     .###.##....# 3,2,1
#'
#' However, the condition records are partially damaged; some of the
#' springs\' conditions are actually *unknown* (`?`). For example:
#'
#'     ???.### 1,1,3
#'     .??..??...?##. 1,1,3
#'     ?#?#?#?#?#?#?#? 1,3,1,6
#'     ????.#...#... 4,1,1
#'     ????.######..#####. 1,6,5
#'     ?###???????? 3,2,1
#'
#' Equipped with this information, it is your job to figure out *how many
#' different arrangements* of operational and broken springs fit the given
#' criteria in each row.
#'
#' In the first line (`???.### 1,1,3`), there is exactly *one* way separate
#' groups of one, one, and three broken springs (in that order) can appear
#' in that row: the first three unknown springs must be broken, then
#' operational, then broken (`#.#`), making the whole row `#.#.###`.
#'
#' The second line is more interesting: `.??..??...?##. 1,1,3` could be a
#' total of *four* different arrangements. The last `?` must always be
#' broken (to satisfy the final contiguous group of three broken springs),
#' and each `??` must hide exactly one of the two broken springs. (Neither
#' `??` could be both broken springs or they would form a single contiguous
#' group of two; if that were true, the numbers afterward would have been
#' `2,3` instead.) Since each `??` can either be `#.` or `.#`, there are
#' four possible arrangements of springs.
#'
#' The last line is actually consistent with *ten* different arrangements!
#' Because the first number is `3`, the first and second `?` must both be
#' `.` (if either were `#`, the first number would have to be `4` or
#' higher). However, the remaining run of unknown spring conditions have
#' many different ways they could hold groups of two and one broken
#' springs:
#'
#'     ?###???????? 3,2,1
#'     .###.##.#...
#'     .###.##..#..
#'     .###.##...#.
#'     .###.##....#
#'     .###..##.#..
#'     .###..##..#.
#'     .###..##...#
#'     .###...##.#.
#'     .###...##..#
#'     .###....##.#
#'
#' In this example, the number of possible arrangements for each row is:
#'
#' -   `???.### 1,1,3` - *`1`* arrangement
#' -   `.??..??...?##. 1,1,3` - *`4`* arrangements
#' -   `?#?#?#?#?#?#?#? 1,3,1,6` - *`1`* arrangement
#' -   `????.#...#... 4,1,1` - *`1`* arrangement
#' -   `????.######..#####. 1,6,5` - *`4`* arrangements
#' -   `?###???????? 3,2,1` - *`10`* arrangements
#'
#' Adding all of the possible arrangement counts together produces a total
#' of *`21`* arrangements.
#'
#' For each row, count all of the different arrangements of operational and
#' broken springs that meet the given criteria. *What is the sum of those
#' counts?*
#'
#' **Part Two**
#'
#' As you look out at the field of springs, you feel like there are way
#' more springs than the condition records list. When you examine the
#' records, you discover that they were actually *folded up* this whole
#' time!
#'
#' To *unfold the records*, on each row, replace the list of spring
#' conditions with five copies of itself (separated by `?`) and replace the
#' list of contiguous groups of damaged springs with five copies of itself
#' (separated by `,`).
#'
#' So, this row:
#'
#'     .# 1
#'
#' Would become:
#'
#'     .#?.#?.#?.#?.# 1,1,1,1,1
#'
#' The first line of the above example would become:
#'
#'     ???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3
#'
#' In the above example, after unfolding, the number of possible
#' arrangements for some rows is now much larger:
#'
#' -   `???.### 1,1,3` - *`1`* arrangement
#' -   `.??..??...?##. 1,1,3` - *`16384`* arrangements
#' -   `?#?#?#?#?#?#?#? 1,3,1,6` - *`1`* arrangement
#' -   `????.#...#... 4,1,1` - *`16`* arrangements
#' -   `????.######..#####. 1,6,5` - *`2500`* arrangements
#' -   `?###???????? 3,2,1` - *`506250`* arrangements
#'
#' After unfolding, adding all of the possible arrangement counts together
#' produces *`525152`*.
#'
#' Unfold your condition records; *what is the new sum of possible
#' arrangement counts?*
#'
#' @param x some data
#' @return For Part One, `f12a_count_completions(x)` returns .... For Part Two,
#'   `f12b(x)` returns ....
#' @export
#' @examples
#' f12a_count_completions(example_data_12())
#' f12b()
f12a_count_completions <- function(x) {
  solve_brute_force <- function(data) {
    chars <- data$chars
    target_seq <- data$hash_seq
    try_completion <- function(i, chars, target_seq) {
      chars[i] <- "#"
      guess_seq <- vec_run_lengths_of_value(chars, "#")
      if (length(guess_seq) == length(target_seq)) {
        all(guess_seq == target_seq)
      } else {
        FALSE
      }
    }
    utils::combn(
      data$free_slots,
      data$hash_missing,
      FUN = try_completion,
      chars = data$chars,
      target_seq = data$hash_seq
    )
  }

  x |>
    f12_helper() |>
    lapply(solve_brute_force) |>
    unlist() |>
    sum()
}


#' @rdname day12
#' @export
f12b <- function(x) {
  # The puzzle adds an extra `?` between each fold. So the cases are
  # 1. Set ? to .
  #    [line].[line] with A * A possible solutions
  # 2. Set ? to #
  #    [line#][line] and need to test A possible cases on right
  #    [line][#line] and need to test A possible cases on left
  solve_brute_force <- function(data, size = 5) {
    try_completion <- function(i, chars, target_seq) {
      chars[i] <- "#"
      chars[chars == "?"] <- "."
      guess_seq <- vec_run_lengths_of_value(chars, "#")
      if (length(guess_seq) == length(target_seq)) {
        if (all(guess_seq == target_seq)) {
          paste0(chars, collapse = "")
        } else {
          character(0)
        }
      } else {
        character(0)
      }
    }

    solve_case_from_result <- function(result, data, direction) {
      if (direction == "left") {
        new_line <- paste0(data$line, "#", result)
      } else {
        new_line <- paste0(result, "#", data$line)
      }
      new_data <- new_line |>
        paste0(" ", data$record, ",", data$record) |>
        f12_helper() |>
        vec_element(1)

      utils::combn(
        new_data$free_slots,
        new_data$hash_missing,
        simplify = FALSE
      ) |>
        lapply(
          try_completion,
          chars = new_data$chars,
          target_seq = new_data$hash_seq
        )
    }

    chars <- data$chars
    target_seq <- data$hash_seq

    initial_solve <- utils::combn(
      data$free_slots,
      data$hash_missing,
      simplify = FALSE
    ) |>
      lapply(try_completion, chars = data$chars, target_seq = data$hash_seq) |>
      list_filter(length) |>
      unlist()

    l <- initial_solve |>
      unlist() |>
      lapply(solve_case_from_result, data = data, direction = "left") |>
      lapply(list_filter, length)

    r <- initial_solve |>
      unlist() |>
      lapply(solve_case_from_result, data = data, direction = "right") |>
      lapply(list_filter, length)

    lengths(l)

    x <- rep(length(initial_solve), size)
    l0 <- c(rep(lengths(l)[1], size - 1), 0)
    lr <- c(0, rep(lengths(r)[1], size - 1))
    prod(x + l0 + lr)
  }

  # x <- readLines("./inst/input12.txt")
  # x <- x |> f12_helper()
  # data <- x[[6]]

  # This solves the examples but I don't think the examples
  # have enough "#"-final examples
  x <- example_data_12()
  try <- x |>
    f12_helper() |>
    lapply(solve_brute_force, 5) |>
    lapply(sum) |>
    unlist()
  try
}


f12_helper <- function(x) {
  x |>
    strsplit(" ") |>
    lapply(as.list) |>
    lapply(vec_set_names, c("line", "record")) |>
    # Some helper data
    lapply(function(l) {
      l$chars <- l$line |> strsplit("") |> unlist()
      l$hash_seq <- l$record |> strsplit(",") |> unlist() |> as.numeric()
      l$hash_missing <- sum(l$hash_seq) - sum(l$chars == "#")
      l$free_slots <- l$chars |> vec_which_value("?")
      l
    })
#
#   which(as == "?")
#
#   as <- a$line |> strsplit("") |> unlist()
#   hashes <- a$record |> strsplit(",") |> unlist() |> as.numeric()
#   hashes_need <- sum(hashes)
#   hashes_have <- which(as == "#") |> length()
#   to_add <- hashes_need - hashes_have
#   x <- a
#
  # a <- x
  #
  # i <- c(5, 11, 13, 15)
  #
  # combn(slots, 4)
  # a2 <- as
  #
  #
  # xs <- a2
  # value <- "#"
  # rle(a2)
  #
  # cumsum(a2 != "#")
  #
  # hashed_to_add <- hashes_needed - counts$`#`
  #
  #
  # counts <- as |>
  #   table() |>
  #   as.list()
  #
  #
  # r <- rle(as)
  #
  # length(as)
  # need <- length(as[as == "?"])
  #
  # r$values
  # r$lengths
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day12
#' @export
example_data_12 <- function(example = 1) {
  l <- list(
    a = c(
      "???.### 1,1,3",
      ".??..??...?##. 1,1,3",
      "?#?#?#?#?#?#?#? 1,3,1,6",
      "????.#...#... 4,1,1",
      "????.######..#####. 1,6,5",
      "?###???????? 3,2,1"
    )
  )
  l[[example]]
}
