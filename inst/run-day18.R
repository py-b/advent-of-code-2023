x <- readLines("./inst/input18.txt")

(p1 <- solve18a(x))
(p2 <- solve18b(x))

stopifnot(p1 == aoc_solutions$day18a)
stopifnot(p2 == aoc_solutions$day18b)
