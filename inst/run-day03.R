x <- readLines("./inst/input03.txt")

(p1 <- solve03a(x))
(p2 <- solve03b(x))

stopifnot(p1 == aoc_solutions$day03a)
stopifnot(p2 == aoc_solutions$day03b)
