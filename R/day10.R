#' Day 10: Pipe Maze
#'
#' [Pipe Maze](https://adventofcode.com/2023/day/10)
#'
#' @name day10
#' @rdname day10
#' @details
#'
#' **Part One**
#'
#' You use the hang glider to ride the hot air from Desert Island all the
#' way up to the floating metal island. This island is surprisingly cold
#' and there definitely aren\'t any thermals to glide on, so you leave your
#' hang glider behind.
#'
#' You wander around for a while, but you don\'t find any people or
#' animals. However, you do occasionally find signposts labeled \"[Hot
#' Springs](https://en.wikipedia.org/wiki/Hot_spring){target="_blank"}\"
#' pointing in a seemingly consistent direction; maybe you can find someone
#' at the hot springs and ask them where the desert-machine parts are made.
#'
#' The landscape here is alien; even the flowers and trees are made of
#' metal. As you stop to admire some metal grass, you notice something
#' metallic scurry away in your peripheral vision and jump into a big pipe!
#' It didn\'t look like any animal you\'ve ever seen; if you want a better
#' look, you\'ll need to get ahead of it.
#'
#' Scanning the area, you discover that the entire field you\'re standing
#' on is [densely packed with
#' pipes]{title="Manufactured by Hamilton and Hilbert Pipe Company"}; it
#' was hard to tell at first because they\'re the same metallic silver
#' color as the \"ground\". You make a quick sketch of all of the surface
#' pipes you can see (your puzzle input).
#'
#' The pipes are arranged in a two-dimensional grid of *tiles*:
#'
#' -   `|` is a *vertical pipe* connecting north and south.
#' -   `-` is a *horizontal pipe* connecting east and west.
#' -   `L` is a *90-degree bend* connecting north and east.
#' -   `J` is a *90-degree bend* connecting north and west.
#' -   `7` is a *90-degree bend* connecting south and west.
#' -   `F` is a *90-degree bend* connecting south and east.
#' -   `.` is *ground*; there is no pipe in this tile.
#' -   `S` is the *starting position* of the animal; there is a pipe on
#'     this tile, but your sketch doesn\'t show what shape the pipe has.
#'
#' Based on the acoustics of the animal\'s scurrying, you\'re confident the
#' pipe that contains the animal is *one large, continuous loop*.
#'
#' For example, here is a square loop of pipe:
#'
#'     .....
#'     .F-7.
#'     .|.|.
#'     .L-J.
#'     .....
#'
#' If the animal had entered this loop in the northwest corner, the sketch
#' would instead look like this:
#'
#'     .....
#'     .S-7.
#'     .|.|.
#'     .L-J.
#'     .....
#'
#' In the above diagram, the `S` tile is still a 90-degree `F` bend: you
#' can tell because of how the adjacent pipes connect to it.
#'
#' Unfortunately, there are also many pipes that *aren\'t connected to the
#' loop*! This sketch shows the same loop as above:
#'
#'     -L|F7
#'     7S-7|
#'     L|7||
#'     -L-J|
#'     L|-JF
#'
#' In the above diagram, you can still figure out which pipes form the main
#' loop: they\'re the ones connected to `S`, pipes those pipes connect to,
#' pipes *those* pipes connect to, and so on. Every pipe in the main loop
#' connects to its two neighbors (including `S`, which will have exactly
#' two pipes connecting to it, and which is assumed to connect back to
#' those two pipes).
#'
#' Here is a sketch that contains a slightly more complex main loop:
#'
#'     ..F7.
#'     .FJ|.
#'     SJ.L7
#'     |F--J
#'     LJ...
#'
#' Here\'s the same example sketch with the extra, non-main-loop pipe tiles
#' also shown:
#'
#'     7-F7-
#'     .FJ|7
#'     SJLL7
#'     |F--J
#'     LJ.LJ
#'
#' If you want to *get out ahead of the animal*, you should find the tile
#' in the loop that is *farthest* from the starting position. Because the
#' animal is in the pipe, it doesn\'t make sense to measure this by direct
#' distance. Instead, you need to find the tile that would take the longest
#' number of steps *along the loop* to reach from the starting point -
#' regardless of which way around the loop the animal went.
#'
#' In the first example with the square loop:
#'
#'     .....
#'     .S-7.
#'     .|.|.
#'     .L-J.
#'     .....
#'
#' You can count the distance each tile in the loop is from the starting
#' point like this:
#'
#'     .....
#'     .012.
#'     .1.3.
#'     .234.
#'     .....
#'
#' In this example, the farthest point from the start is *`4`* steps away.
#'
#' Here\'s the more complex loop again:
#'
#'     ..F7.
#'     .FJ|.
#'     SJ.L7
#'     |F--J
#'     LJ...
#'
#' Here are the distances for each tile on that loop:
#'
#'     ..45.
#'     .236.
#'     01.78
#'     14567
#'     23...
#'
#' Find the single giant loop starting at `S`. *How many steps along the
#' loop does it take to get from the starting position to the point
#' farthest from the starting position?*
#'
#' **Part Two**
#'
#' You quickly reach the farthest point of the loop, but the animal never
#' emerges. Maybe its nest is *within the area enclosed by the loop*?
#'
#' To determine whether it\'s even worth taking the time to search for such
#' a nest, you should calculate how many tiles are contained within the
#' loop. For example:
#'
#'     ...........
#'     .S-------7.
#'     .|F-----7|.
#'     .||.....||.
#'     .||.....||.
#'     .|L-7.F-J|.
#'     .|..|.|..|.
#'     .L--J.L--J.
#'     ...........
#'
#' The above loop encloses merely *four tiles* - the two pairs of `.` in
#' the southwest and southeast (marked `I` below). The middle `.` tiles
#' (marked `O` below) are *not* in the loop. Here is the same loop again
#' with those regions marked:
#'
#'     ...........
#'     .S-------7.
#'     .|F-----7|.
#'     .||OOOOO||.
#'     .||OOOOO||.
#'     .|L-7OF-J|.
#'     .|II|O|II|.
#'     .L--JOL--J.
#'     .....O.....
#'
#' In fact, there doesn\'t even need to be a full tile path to the outside
#' for tiles to count as outside the loop - squeezing between pipes is also
#' allowed! Here, `I` is still within the loop and `O` is still outside the
#' loop:
#'
#'     ..........
#'     .S------7.
#'     .|F----7|.
#'     .||OOOO||.
#'     .||OOOO||.
#'     .|L-7F-J|.
#'     .|II||II|.
#'     .L--JL--J.
#'     ..........
#'
#' In both of the above examples, *`4`* tiles are enclosed by the loop.
#'
#' Here\'s a larger example:
#'
#'     .F----7F7F7F7F-7....
#'     .|F--7||||||||FJ....
#'     .||.FJ||||||||L7....
#'     FJL7L7LJLJ||LJ.L-7..
#'     L--J.L7...LJS7F-7L7.
#'     ....F-J..F7FJ|L7L7L7
#'     ....L7.F7||L7|.L7L7|
#'     .....|FJLJ|FJ|F7|.LJ
#'     ....FJL-7.||.||||...
#'     ....L---J.LJ.LJLJ...
#'
#' The above sketch has many random bits of ground, some of which are in
#' the loop (`I`) and some of which are outside it (`O`):
#'
#'     OF----7F7F7F7F-7OOOO
#'     O|F--7||||||||FJOOOO
#'     O||OFJ||||||||L7OOOO
#'     FJL7L7LJLJ||LJIL-7OO
#'     L--JOL7IIILJS7F-7L7O
#'     OOOOF-JIIF7FJ|L7L7L7
#'     OOOOL7IF7||L7|IL7L7|
#'     OOOOO|FJLJ|FJ|F7|OLJ
#'     OOOOFJL-7O||O||||OOO
#'     OOOOL---JOLJOLJLJOOO
#'
#' In this larger example, *`8`* tiles are enclosed by the loop.
#'
#' Any tile that isn\'t part of the main loop can count as being enclosed
#' by the loop. Here\'s another example with many bits of junk pipe lying
#' around that aren\'t connected to the main loop at all:
#'
#'     FF7FSF7F7F7F7F7F---7
#'     L|LJ||||||||||||F--J
#'     FL-7LJLJ||||||LJL-77
#'     F--JF--7||LJLJ7F7FJ-
#'     L---JF-JLJ.||-FJLJJ7
#'     |F|F-JF---7F7-L7L|7|
#'     |FFJF7L7F-JF7|JL---7
#'     7-L-JL7||F7|L7F-7F7|
#'     L.L7LFJ|||||FJL7||LJ
#'     L7JLJL-JLJLJL--JLJ.L
#'
#' Here are just the tiles that are *enclosed by the loop* marked with `I`:
#'
#'     FF7FSF7F7F7F7F7F---7
#'     L|LJ||||||||||||F--J
#'     FL-7LJLJ||||||LJL-77
#'     F--JF--7||LJLJIF7FJ-
#'     L---JF-JLJIIIIFJLJJ7
#'     |F|F-JF---7IIIL7L|7|
#'     |FFJF7L7F-JF7IIL---7
#'     7-L-JL7||F7|L7F-7F7|
#'     L.L7LFJ|||||FJL7||LJ
#'     L7JLJL-JLJLJL--JLJ.L
#'
#' In this last example, *`10`* tiles are enclosed by the loop.
#'
#' Figure out whether you have time to search for the nest by calculating
#' the area within the loop. *How many tiles are enclosed by the loop?*
#'
#' @param x some data
#' @return For Part One, `f10a_find_farthest_distance(x)` returns finds the
#'   distance of the farthest point on the loop. For Part Two, `f10b(x)` returns
#'   ....
#' @export
#' @examples
#' f10a_find_farthest_distance(example_data_10())
#' # f10b()
f10a_find_farthest_distance <- function(x) {
  state <- x |> f10_helper()
  state$step_num - 10
}


#' @rdname day10
#' @export
f10b <- function(x) {

}


f10_helper <- function(x) {
  # Use lots of index look ups
  tile_types <- c("|", "L", "J", "F", "7", "-", ".")
  northy <- c("|", "L", "J")
  southy <- c("|", "F", "7")
  easty  <- c("-", "F", "L")
  westy  <- c("-", "J", "7")
  not_northy <- tile_types |> setdiff(northy) |> sort()
  not_southy <- tile_types |> setdiff(southy) |> sort()
  not_easty  <- tile_types |> setdiff(easty) |> sort()
  not_westy  <- tile_types |> setdiff(westy) |> sort()
  dir_friends <- list(N = southy, S = northy, E = westy, W = easty)

  # Convert a direction into a change in row, col
  map_dir_to_offset <- function(dirs) {
    map <- c(-1, 0, 1, 0, 0, 1, 0, -1) |>
      matrix(ncol = 2, byrow = FALSE) |>
      mat_set_rownames(c("N", "E", "S", "W"))
    map[dirs, ]
  }

  # Given direction into a tile and the tile's symbol, find outgoing direction
  map_incoming_to_outgoing <- function(dir_incoming, symbol) {
    flip_dir <- function(xs) {
      c("E" = "W", "W" = "E", "N" = "S", "S" = "N") |>
        vec_index(xs) |>
        unname()
    }
    dir_out <- list(
      "-" = c("E", "W"),
      "|" = c("N", "S"),
      "F" = c("S", "E"),
      "7" = c("S", "W"),
      "L" = c("N", "E"),
      "J" = c("N", "W")
    )

    dir_out |>
      vec_index(symbol) |>
      list_map2(flip_dir(dir_incoming), setdiff) |>
      unlist(use.names = FALSE)
  }

  # Remove all path tiles that lead to somewhere invalid
  simplify_grid <- function(m) {
    # invoke a function over and over until it doesn't change the input
    vec_fixed_point <- function(x, func, ..., .limit = 10000) {
      for (i in seq_len(.limit)) {
        old_x <- x
        x <- func(x, ...)
        if (all(old_x == x)) break
      }
      x
    }

    # Convert rows and tiles to strings and modify with regexes
    re_group <- function(xs) sprintf("([%s])", paste0(xs, collapse = ""))
    re_group_pair <- function(xs, ys) paste0(re_group(xs), re_group(ys))

    fix_east_west <- function(x) {
      x |>
        chr_replace(re_group_pair(easty, not_westy), ".\\2") |>
        chr_replace(re_group_pair(not_easty, westy), "\\1.") |>
        chr_replace(paste0("^", re_group(westy)), ".") |>
        chr_replace(paste0(re_group(easty), "$"), ".")
    }
    fix_north_south <- function(x) {
      x |>
        chr_replace(re_group_pair(southy, not_northy), ".\\2") |>
        chr_replace(re_group_pair(not_southy, northy), "\\1.") |>
        chr_replace(paste0("^", re_group(northy)), ".") |>
        chr_replace(paste0(re_group(southy), "$"), ".")
    }

    fix_grid <- function(mat) {
      mat |>
        apply(1, paste0, collapse = "") |>
        vec_fixed_point(fix_east_west) |>
        strsplit("") |>
        list_invoke(rbind) |>
        apply(2, paste0, collapse = "") |>
        vec_fixed_point(fix_north_south) |>
        strsplit("") |>
        list_invoke(cbind)
    }

    m |>
      vec_fixed_point(fix_grid)
  }

  # Get valid neighbors of S for initial steps
  setup_paths <- function(mat) {
    # Look at all four neighbors
    steps <- which(mat == "S", arr.ind = TRUE) |>
      rep(4) |>
      matrix(ncol = 2, byrow = TRUE) |>
      mat_set_colnames(c("i", "j"))
    offsets <- map_dir_to_offset(c("N", "E", "S", "W"))

    # Drop out of bounds ones
    data <- as.data.frame(steps + offsets)
    data$dir <- rownames(offsets)
    oob1 <- data$i < 1 | data$j < 1
    oob2 <- data$i > nrow(mat) | data$j > ncol(mat)
    data$valid <- !oob1 & !oob2

    # Drop ones with invalid symbols
    for (row_i in seq_len(nrow(data))) {
      row <- data[row_i, ]
      if (!row$valid) next

      data[row_i, "valid"] <- mat[row$i, row$j] |>
        is.element(dir_friends[[row$dir]])
    }

    data <- data[data$valid, c("i", "j", "dir")]
    data$symbol <- mat[data[c("i", "j")] |> as.matrix()]
    data
  }

  # Take one step away from origin along each path
  take_step <- function(mat, paths, step_num = 1) {
    mat_i <- paths[, 1:2] |> as.matrix()
    mat[mat_i] <- step_num + 1

    old_dir <- paths$dir
    paths$dir <- map_incoming_to_outgoing(old_dir, paths$symbol)
    new_locations <- mat_i + map_dir_to_offset(paths$dir)
    paths$symbol <- mat[new_locations]
    paths$i <- new_locations[, 1]
    paths$j <- new_locations[, 2]

    paths <- paths[paths$symbol %in% c(tile_types), ]
    list(mat = mat, paths = paths, step_num = step_num + 1)
  }

  m <- x |>
    strsplit("") |>
    list_invoke(rbind) |>
    simplify_grid()

  state <- list(
    mat = m,
    paths = setup_paths(m),
    step_num = 10
  )

  while (nrow(state$paths)) {
    state <- list_invoke(state, take_step)
  }
  state$initial_mat <- m
  state
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day10
#' @export
example_data_10 <- function(example = 1) {
  l <- list(
    a = c(
      "-L|F7",
      "7S-7|",
      "L|7||",
      "-L-J|",
      "L|-JF"
    ),
    b = c(
      "7-F7-",
      ".FJ|7",
      "SJLL7",
      "|F--J",
      "LJ.LJ"
    )
  )
  l[[example]]
}
