x <- readLines("./inst/input11.txt")

(p1 <- solve11a(x))
options(digits = 22)
(p2 <- solve11b(x))

stopifnot(p1 == aoc_solutions$day11a)
stopifnot(p2 == aoc_solutions$day11b)
