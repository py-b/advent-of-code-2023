#' Day 11: Cosmic Expansion
#'
#' [Cosmic Expansion](https://adventofcode.com/2023/day/11)
#'
#' @name day11
#' @rdname day11
#' @param x raw universe
#' @export
#' @examples
#' solve11a(example_data_11())
#' solve11b(example_data_11())

solve11a <- function(x) solve11(x, expand_factor = 2)

#' @rdname day11
#' @export

solve11b <- function(x) solve11(x, expand_factor = 10^6)


# Helpers ----------------------------------------------------------------------

solve11 <- function(x, expand_factor) {

  universe <- parse11(x)
  galaxies <- galaxies_pos(universe, expand_factor = expand_factor)

  res <- 0

  for (i in seq(nrow(galaxies)))
    for (j in seq(nrow(galaxies)))
      if (j > i) res <- res + manhattan(galaxies[i, ], galaxies[j, ])

  unname(res)

}

parse11 <- function(x) do.call(rbind, strsplit(x, ""))

manhattan <- function(g0, g1)
  abs(g1["row"] - g0["row"]) + abs(g1["col"] - g0["col"])

new_g_pos <- function(x, y, expand_factor = 2, empty_rows, empty_cols)
  c(
    x + sum(empty_rows < x) * (expand_factor - 1),
    y + sum(empty_cols < y) * (expand_factor - 1)
  )

galaxies_pos <- function(universe, expand_factor = 2) {

  galaxies <- which(universe == "#", arr.ind = TRUE)

  empty_rows <- universe |> apply(1, \(x) all(x == ".")) |> which()
  empty_cols <- universe |> apply(2, \(x) all(x == ".")) |> which()

  # update all positions due to universe expansion
  for (i in seq(nrow(galaxies))) {
    galaxies[i, ] <-
      new_g_pos(
        galaxies[i, "row"],
        galaxies[i, "col"],
        expand_factor,
        empty_rows,
        empty_cols
      )
  }

  galaxies

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day11
#' @export

example_data_11 <- function(example = 1) {
  l <- list(
    c(
      "...#......",
      ".......#..",
      "#.........",
      "..........",
      "......#...",
      ".#........",
      ".........#",
      "..........",
      ".......#..",
      "#...#....."
    )
  )
  l[[example]]
}
