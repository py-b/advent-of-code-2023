#' Day 06: Wait For It
#'
#' [Wait For It](https://adventofcode.com/2023/day/6)
#'
#' @name day06
#' @rdname day06
#' @param x raw races
#' @export
#' @examples
#' solve06a(example_data_06())
#' solve06b(example_data_06())

solve06a <- function(x) solve06(x)

#' @rdname day06
#' @export

solve06b <- function(x) solve06(x, collapse = TRUE)


# Helpers ----------------------------------------------------------------------

parse06 <- function(x, collapse) {
  if (collapse) x <- gsub(" ", "", x)
  races <- x |> str_extract_all("\\d+") |> lapply(as.numeric)
  list(time = races[[1]], record = races[[2]])
}

solve06 <- function(x, collapse = FALSE) {
  races <- parse06(x, collapse = collapse)
  mapply(winning_strats, races$time, races$record) |> prod()
}

winning_strats <- function(race_time, record) {
  # solve `hold * (racetime - hold) > record` for `hold`
  delta <- race_time^2 - 4 * record
  roots <- (-race_time + c(1, -1) * sqrt(delta)) / -2
  # integers strictly between roots
  max(ceiling(roots[2]) - floor(roots[1]) - 1, 0)
}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day06
#' @export

example_data_06 <- function(example = 1) {
  l <- list(
    c(
      "Time:      7  15   30",
      "Distance:  9  40  200"
    )
  )
  l[[example]]
}
