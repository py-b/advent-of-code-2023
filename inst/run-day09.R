x <- readLines("./inst/input09.txt")

(p1 <- solve09a(x))
(p2 <- solve09b(x))

stopifnot(p1 == aoc_solutions$day09a)
stopifnot(p2 == aoc_solutions$day09b)
