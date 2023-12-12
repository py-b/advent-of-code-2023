x <- readLines("./inst/input12.txt")

(p1 <- solve12a(x))
(p2 <- solve12b(x))

stopifnot(p1 == aoc_solutions$day12a)
stopifnot(p2 == aoc_solutions$day12b)


brute_force_12(
  strsplit("?###????????", "")[[1]],
  c(3,2,1)
)
