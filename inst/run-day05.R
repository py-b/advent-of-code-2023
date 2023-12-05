x <- readLines("./inst/input05.txt")

(p1 <- solve05a(x))
# (p2 <- solve05b(x))

stopifnot(p1 == aoc_solutions$day05a)
# stopifnot(p2 == aoc_solutions$day05b)
