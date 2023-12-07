x <- readLines("./inst/input07.txt")

(p1 <- solve07a(x))
(p2 <- solve07b(x))

stopifnot(p1 == aoc_solutions$day07a)
stopifnot(p2 == aoc_solutions$day07b)
