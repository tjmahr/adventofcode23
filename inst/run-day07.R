library(adventofcode23)
x <- readLines("./inst/input07.txt")

p1 <- f07a_calculate_winnings(x)
p2 <- f07b_calculate_jk_winnings(x)

stopifnot(p1 == aoc_solutions$day07a)
stopifnot(p2 == aoc_solutions$day07b)
