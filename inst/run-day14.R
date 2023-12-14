x <- readLines("./inst/input14.txt")

(p1 <- solve14a(x))
(p2 <- solve14b(x))

stopifnot(p1 == aoc_solutions$day14a)
stopifnot(p2 == aoc_solutions$day14b)
