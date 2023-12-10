x <- readLines("./inst/input10.txt")

(p1 <- solve10a(x))
(p2 <- solve10b(x))

stopifnot(p1 == aoc_solutions$day10a)
stopifnot(p2 == aoc_solutions$day10b)


# Visualize ---------------------------------------------------------------

x |>
  stringr::str_replace_all(
    c("-" = "\u2500",
      "\\|" = "\u2502",
      "F" = "\u250C",
      "J"= "\u2518",
      "7" = "\u2510",
      "L" = "\u2514",
      "S" = "o")
  ) |>
  cat(sep = "\n")
