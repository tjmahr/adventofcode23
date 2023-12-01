#' Day 01: Trebuchet?!
#'
#' [Trebuchet?!](https://adventofcode.com/2023/day/1)
#'
#' @name day01
#' @rdname day01
#' @details
#'
#' **Part One**
#'
#' Something is wrong with global snow production, and you've been
#' selected to take a look. The Elves have even given you a map; on it,
#' they've used stars to mark the top fifty locations that are likely to
#' be having problems.
#'
#' You've been doing this long enough to know that to restore snow
#' operations, you need to check all *fifty stars* by December 25th.
#'
#' Collect stars by solving puzzles. Two puzzles will be made available on
#' each day in the Advent calendar; the second puzzle is unlocked when you
#' complete the first. Each puzzle grants *one star*. Good luck!
#'
#' You try to ask why they can't just use a [weather machine](/2015/day/1)
#' ("not powerful enough") and where they're even sending you ("the
#' sky") and why your map looks mostly blank ("you sure ask a lot of
#' questions")
#' [and]{title="My hope is that this abomination of a run-on sentence somehow conveys the chaos of being hastily loaded into a trebuchet."}
#' hang on did you just say the sky ("of course, where do you think snow
#' comes from") when you realize that the Elves are already loading you
#' into a
#' [trebuchet](https://en.wikipedia.org/wiki/Trebuchet){target="_blank"}
#' ("please hold still, we need to strap you in").
#'
#' As they're making the final adjustments, they discover that their
#' calibration document (your puzzle input) has been *amended* by a very
#' young Elf who was apparently just excited to show off her art skills.
#' Consequently, the Elves are having trouble reading the values on the
#' document.
#'
#' The newly-improved calibration document consists of lines of text; each
#' line originally contained a specific *calibration value* that the Elves
#' now need to recover. On each line, the calibration value can be found by
#' combining the *first digit* and the *last digit* (in that order) to form
#' a single *two-digit number*.
#'
#' For example:
#'
#'     1abc2
#'     pqr3stu8vwx
#'     a1b2c3d4e5f
#'     treb7uchet
#'
#' In this example, the calibration values of these four lines are `12`,
#' `38`, `15`, and `77`. Adding these together produces *`142`*.
#'
#' Consider your entire calibration document. *What is the sum of all of
#' the calibration values?*
#'
#' **Part Two**
#'
#' Your calculation isn\'t quite right. It looks like some of the digits
#' are actually *spelled out with letters*: `one`, `two`, `three`, `four`,
#' `five`, `six`, `seven`, `eight`, and `nine` *also* count as valid
#' \"digits\".
#'
#' Equipped with this new information, you now need to find the real first
#' and last digit on each line. For example:
#'
#'     two1nine
#'     eightwothree
#'     abcone2threexyz
#'     xtwone3four
#'     4nineeightseven2
#'     zoneight234
#'     7pqrstsixteen
#'
#' In this example, the calibration values are `29`, `83`, `13`, `24`,
#' `42`, `14`, and `76`. Adding these together produces *`281`*.
#'
#' *What is the sum of all of the calibration values?*
#'
#' @param x some data
#' @return For Part One, `f01a_sum_hidden_numbers(x)` returns the sum of the
#'   hidden numbers. For Part Two, `f01b_decode_and_sum_hidden_numbers(x)`
#'   returns the sum of the hidden numbers.
#' @export
#' @examples
#' f01a_sum_hidden_numbers(example_data_01())
#' f01b_decode_and_sum_hidden_numbers(example_data_01(2))
f01a_sum_hidden_numbers <- function(x) {
  x |> f01_helper() |> sum()
}


#' @rdname day01
#' @export
f01b_decode_and_sum_hidden_numbers <- function(x) {
  x |> f01_helper2() |> sum()
}


f01_helper <- function(x, fix_words = FALSE) {
  x |>
    chr_replace("\\D", "") |>
    # repeat any single digit
    chr_replace("(^\\d$)", "\\1\\1") |>
    chr_replace("(^\\d).+(\\d$)", "\\1\\2") |>
    as.numeric()
}


f01_helper2 <- function(x) {
  # Find the first and last digits separately by
  # working on reversed strings
  plan <- c(
    one = 1, two = 2, three = 3,
    four = 4, five = 5, six = 6,
    seven = 7, eight = 8, nine = 9
  )
  rev_plan <- plan
  names(rev_plan) <- chr_reverse(names(plan))
  regex <- paste0("(", paste0(names(plan), collapse = "|"), ")")
  rev_regex <- paste0("(", paste0(names(rev_plan), collapse = "|"), ")")

  first <- x |>
    # put underscores in to prevent "twone" collision
    chr_replace(regex, "_\\1_") |>
    chr_replace_many(plan) |>
    chr_replace("\\D", "") |>
    substr(1, 1)

  last <- x |>
    chr_reverse() |>
    chr_replace(rev_regex, "_\\1_") |>
    chr_replace_many(rev_plan) |>
    chr_replace("\\D", "") |>
    substr(1, 1)

  paste0(first, last) |>
    as.numeric()
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day01
#' @export
example_data_01 <- function(example = 1) {
  l <- list(
    a = c(
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    ),
    b = c(
      "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    )
  )
  l[[example]]
}
