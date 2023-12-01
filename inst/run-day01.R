library(adventofcode23)
x <- readLines("./inst/input01.txt")

p1 <- f01a_sum_hidden_numbers(x)
p2 <- f01b_decode_and_sum_hidden_numbers(x)

stopifnot(p1 == aoc_solutions$day01a)
stopifnot(p2 == aoc_solutions$day01b)
