x <- readLines("./inst/input04.txt")

(p1 <- solve04a(x))
(p2 <- solve04b(x))

stopifnot(p1 == aoc_solutions$day04a)
stopifnot(p2 == aoc_solutions$day04b)
