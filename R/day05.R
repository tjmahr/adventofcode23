#' Day 05: If You Give A Seed A Fertilizer
#'
#' [If You Give A Seed A Fertilizer](https://adventofcode.com/2023/day/5)
#'
#' @name day05
#' @rdname day05
#' @details
#'
#' **Part One**
#'
#' You take the boat and find the gardener right where you were told he
#' would be: managing a giant \"garden\" that looks more to you like a
#' farm.
#'
#' \"A water source? Island Island *is* the water source!\" You point out
#' that Snow Island isn\'t receiving any water.
#'
#' \"Oh, we had to stop the water because we *ran out of sand* to
#' [filter](https://en.wikipedia.org/wiki/Sand_filter){target="_blank"} it
#' with! Can\'t make snow with dirty water. Don\'t worry, I\'m sure we\'ll
#' get more sand soon; we only turned off the water a few days\...
#' weeks\... oh no.\" His face sinks into a look of horrified realization.
#'
#' \"I\'ve been so busy making sure everyone here has food that I
#' completely forgot to check why we stopped getting more sand! There\'s a
#' ferry leaving soon that is headed over in that direction - it\'s much
#' faster than your boat. Could you please go check it out?\"
#'
#' You barely have time to agree to this request when he brings up another.
#' \"While you wait for the ferry, maybe you can help us with our *food
#' production problem*. The latest Island Island
#' [Almanac](https://en.wikipedia.org/wiki/Almanac){target="_blank"} just
#' arrived and we\'re having trouble making sense of it.\"
#'
#' The almanac (your puzzle input) lists all of the seeds that need to be
#' planted. It also lists what type of soil to use with each kind of seed,
#' what type of fertilizer to use with each kind of soil, what type of
#' water to use with each kind of fertilizer, and so on. Every type of
#' seed, soil, fertilizer and so on is identified with a number, but
#' numbers are reused by each category - that is, soil `123` and fertilizer
#' `123` aren\'t necessarily related to each other.
#'
#' For example:
#'
#'     seeds: 79 14 55 13
#'
#'     seed-to-soil map:
#'     50 98 2
#'     52 50 48
#'
#'     soil-to-fertilizer map:
#'     0 15 37
#'     37 52 2
#'     39 0 15
#'
#'     fertilizer-to-water map:
#'     49 53 8
#'     0 11 42
#'     42 0 7
#'     57 7 4
#'
#'     water-to-light map:
#'     88 18 7
#'     18 25 70
#'
#'     light-to-temperature map:
#'     45 77 23
#'     81 45 19
#'     68 64 13
#'
#'     temperature-to-humidity map:
#'     0 69 1
#'     1 0 69
#'
#'     humidity-to-location map:
#'     60 56 37
#'     56 93 4
#'
#' The almanac starts by listing which seeds need to be planted: seeds
#' `79`, `14`, `55`, and `13`.
#'
#' The rest of the almanac contains a list of *maps* which describe how to
#' convert numbers from a *source category* into numbers in a *destination
#' category*. That is, the section that starts with `seed-to-soil map:`
#' describes how to convert a *seed number* (the source) to a *soil number*
#' (the destination). This lets the gardener and his team know which soil
#' to use with which seeds, which water to use with which fertilizer, and
#' so on.
#'
#' Rather than list every source number and its corresponding destination
#' number one by one, the maps describe entire *ranges* of numbers that can
#' be converted. Each line within a map contains [three
#' numbers]{title="Don't blame me for the weird order. Blame LXC container.conf UID mappings."}:
#' the *destination range start*, the *source range start*, and the *range
#' length*.
#'
#' Consider again the example `seed-to-soil map`:
#'
#'     50 98 2
#'     52 50 48
#'
#' The first line has a *destination range start* of `50`, a *source range
#' start* of `98`, and a *range length* of `2`. This line means that the
#' source range starts at `98` and contains two values: `98` and `99`. The
#' destination range is the same length, but it starts at `50`, so its two
#' values are `50` and `51`. With this information, you know that seed
#' number `98` corresponds to soil number `50` and that seed number `99`
#' corresponds to soil number `51`.
#'
#' The second line means that the source range starts at `50` and contains
#' `48` values: `50`, `51`, \..., `96`, `97`. This corresponds to a
#' destination range starting at `52` and also containing `48` values:
#' `52`, `53`, \..., `98`, `99`. So, seed number `53` corresponds to soil
#' number `55`.
#'
#' Any source numbers that *aren\'t mapped* correspond to the *same*
#' destination number. So, seed number `10` corresponds to soil number
#' `10`.
#'
#' So, the entire list of seed numbers and their corresponding soil numbers
#' looks like this:
#'
#'     seed  soil
#'     0     0
#'     1     1
#'     ...   ...
#'     48    48
#'     49    49
#'     50    52
#'     51    53
#'     ...   ...
#'     96    98
#'     97    99
#'     98    50
#'     99    51
#'
#' With this map, you can look up the soil number required for each initial
#' seed number:
#'
#' -   Seed number `79` corresponds to soil number `81`.
#' -   Seed number `14` corresponds to soil number `14`.
#' -   Seed number `55` corresponds to soil number `57`.
#' -   Seed number `13` corresponds to soil number `13`.
#'
#' The gardener and his team want to get started as soon as possible, so
#' they\'d like to know the closest location that needs a seed. Using these
#' maps, find *the lowest location number that corresponds to any of the
#' initial seeds*. To do this, you\'ll need to convert each seed number
#' through other categories until you can find its corresponding *location
#' number*. In this example, the corresponding types are:
#'
#' -   Seed `79`, soil `81`, fertilizer `81`, water `81`, light `74`,
#'     temperature `78`, humidity `78`, *location `82`*.
#' -   Seed `14`, soil `14`, fertilizer `53`, water `49`, light `42`,
#'     temperature `42`, humidity `43`, *location `43`*.
#' -   Seed `55`, soil `57`, fertilizer `57`, water `53`, light `46`,
#'     temperature `82`, humidity `82`, *location `86`*.
#' -   Seed `13`, soil `13`, fertilizer `52`, water `41`, light `34`,
#'     temperature `34`, humidity `35`, *location `35`*.
#'
#' So, the lowest location number in this example is *`35`*.
#'
#' *What is the lowest location number that corresponds to any of the
#' initial seed numbers?*
#'
#' **Part Two**
#'
#' Everyone will starve if you only plant such a small number of seeds.
#' Re-reading the almanac, it looks like the `seeds:` line actually
#' describes *ranges of seed numbers*.
#'
#' The values on the initial `seeds:` line come in pairs. Within each pair,
#' the first value is the *start* of the range and the second value is the
#' *length* of the range. So, in the first line of the example above:
#'
#'     seeds: 79 14 55 13
#'
#' This line describes two ranges of seed numbers to be planted in the
#' garden. The first range starts with seed number `79` and contains `14`
#' values: `79`, `80`, \..., `91`, `92`. The second range starts with seed
#' number `55` and contains `13` values: `55`, `56`, \..., `66`, `67`.
#'
#' Now, rather than considering four seed numbers, you need to consider a
#' total of *27* seed numbers.
#'
#' In the above example, the lowest location number can be obtained from
#' seed number `82`, which corresponds to soil `84`, fertilizer `84`, water
#' `84`, light `77`, temperature `45`, humidity `46`, and *location `46`*.
#' So, the lowest location number is *`46`*.
#'
#' Consider all of the initial seed numbers listed in the ranges on the
#' first line of the almanac. *What is the lowest location number that
#' corresponds to any of the initial seed numbers?*
#'
#' @param x some data
#' @return For Part One, `f05a_find_lowest_location(x)` returns the lowest
#'   location value. For Part Two, `f05b_find_lowest_location(x)` returns the
#'   lowest location value.
#' @export
#' @examples
#' f05a_find_lowest_location(example_data_05())
#' f05b_find_lowest_location(example_data_05())
f05a_find_lowest_location <- function(x) {
  data <- f05_helper(x)
  s <- data$seeds
  for (func in data$maps) {
    s <- func(s)
  }
  min(s)
}


#' @rdname day05
#' @export
f05b_find_lowest_location <- function(x) {
  data <- f05_helper(x)

  # Find the smallest interval length
  smallest_range <- data$data |>
    lapply(function(x) x[, 3]) |>
    unlist() |>
    min()

  # Divide into smaller pieces. Now we can never "jump" over any outcome
  # intervals
  smallest_range <- smallest_range %/% 10
  if (smallest_range < 1000) smallest_range <- 1

  i <- seq_along(data$seeds)
  starts <- data$seeds[which(i %% 2 == 1)]
  lengths <- data$seeds[which(i %% 2 == 0)]

  # Sample points within each starting interval
  sample_points_in_range <- function(start, length, step_size) {
    start + c(seq(0, to = length, by = step_size), length)
  }

  batches <- list_map2(
    starts,
    lengths,
    sample_points_in_range,
    step_size = smallest_range
  )

  # Find outcomes of samples
  outcomes <- batches |>
    lapply(
      function(x) {
        for (func in data$maps) x <- func(x)
        x
      }
    )

  # Find which sample had lowest outcome
  batch_w_lowest_outcome <- outcomes |>
    lapply(min) |>
    unlist() |>
    which.min()

  sample_w_lowest_outcome <- outcomes |>
    vec_element(batch_w_lowest_outcome) |>
    which.min()

  sample_start <- batches |>
    vec_element(batch_w_lowest_outcome) |>
    vec_element(sample_w_lowest_outcome)

  # and then check all values around that sample
  values_to_check <- seq(
    sample_start - smallest_range,
    sample_start + smallest_range
  )

  s <- values_to_check
  for (func in data$maps) s <- func(s)
  min(s)
}


f05_helper <- function(x) {
  create_range_mapper <- function(start_dest, start_source, length) {
    function(xs) {
      offsets <- start_dest - start_source
      adjustments <- xs |>
        as.list() |>
        lapply(rep, length(start_source)) |>
        lapply(
          function(x) which(start_source <= x & x < start_source + length)
        ) |>
        lapply(
          function(x) if (length(x)) offsets[x] else 0
        ) |>
        unlist()

      xs + adjustments
    }
  }

  seeds <- x[1] |>
    strsplit(" ") |>
    unlist() |>
    vec_index(-1) |>
    as.numeric()

  section_starts <- c(1, which(x == "") + 1)
  section_ends <- c(which(x == "") - 1, length(x))

  sections <- list_map2(section_starts, section_ends, seq) |>
    lapply(vec_index_into, x)

  section_data <- sections[-1] |>
    lapply(function(x) {
      x[-1] |>
        strsplit(" ") |>
        unlist() |>
        as.numeric() |>
        matrix(ncol = 3, byrow = TRUE)
    })

  section_maps <- section_data |>
    lapply(function(m) create_range_mapper(m[, 1], m[, 2], m[, 3]))

  list(
    seeds = seeds,
    data = section_data,
    maps = section_maps
  )
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day05
#' @export
example_data_05 <- function(example = 1) {
  l <- list(
    a = c(
      "seeds: 79 14 55 13",
      "",
      "seed-to-soil map:",
      "50 98 2",
      "52 50 48",
      "",
      "soil-to-fertilizer map:",
      "0 15 37",
      "37 52 2",
      "39 0 15",
      "",
      "fertilizer-to-water map:",
      "49 53 8",
      "0 11 42",
      "42 0 7",
      "57 7 4",
      "",
      "water-to-light map:",
      "88 18 7",
      "18 25 70",
      "",
      "light-to-temperature map:",
      "45 77 23",
      "81 45 19",
      "68 64 13",
      "",
      "temperature-to-humidity map:",
      "0 69 1",
      "1 0 69",
      "",
      "humidity-to-location map:",
      "60 56 37",
      "56 93 4"
      )
  )
  l[[example]]
}
