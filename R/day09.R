#' Day 09: Mirage Maintenance
#'
#' [Mirage Maintenance](https://adventofcode.com/2023/day/9)
#'
#' @name day09
#' @rdname day09
#' @param x raw OASIS report
#' @importFrom utils read.table
#' @export
#' @examples
#' solve09a(example_data_09())
#' solve09b(example_data_09())

solve09a <- function(x) x |> parse09() |> extrapolate_nexts() |> sum()

#' @rdname day09
#' @export

solve09b <- function(x) x |> parse09() |> extrapolate_prevs() |> sum()


# Helpers -----------------------------------------------------------------

parse09 <- \(x) read.table(text = x)

extrapolate_nexts <- \(report) apply(report, 1, extrapolate)
extrapolate_prevs <- \(report) apply(report, 1, \(.) rev(.) |> extrapolate())

extrapolate <- function(history) {

  tails <- c()

  repeat {
    tails <- c(tails, tail(history, 1))
    if (length(unique(history)) == 1) break
    history <- diff(history)
  }

  sum(tails)

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day09
#' @export

example_data_09 <- function(example = 1) {
  l <- list(
    c(
      "0 3 6 9 12 15",
      "1 3 6 10 15 21",
      "10 13 16 21 30 45"
    )
  )
  l[[example]]
}
