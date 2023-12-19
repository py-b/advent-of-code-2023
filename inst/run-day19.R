x <- readLines("./inst/input19.txt")

(p1 <- solve19a(x))
# (p2 <- solve19b(x))

stopifnot(p1 == aoc_solutions$day19a)
# stopifnot(p2 == aoc_solutions$day19b)
