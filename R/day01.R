#' Day 01: Trebuchet?!
#'
#' [Trebuchet?!](https://adventofcode.com/2023/day/1)
#'
#' @name day01
#' @rdname day01
#'
#' @param x some data
#' @export
#' @examples
#' solve01a(example_data_01("a"))
#' solve01b(example_data_01("b"))

solve01a <- function(x)
  gsub("[^0-9]", "", x) |> strsplit("") |> sapply(calibrate) |> sum()

#' @rdname day01
#' @export

solve01b <- function(x)
  x |> replace_numstr() |> solve01a()


# Utils -------------------------------------------------------------------

#' @importFrom utils head tail
calibrate <- function(x)
  c(head(x, 1), tail(x, 1)) |>
  paste(collapse = "") |>
  as.integer()

replace_numstr <- function(x) {
  x <- gsub("oneight",   "18", x)
  x <- gsub("twone",     "21", x)
  x <- gsub("threeight", "38", x)
  x <- gsub("fiveight",  "58", x)
  x <- gsub("sevenine",  "79", x)
  x <- gsub("eightwo",   "82", x)
  x <- gsub("eighthree", "83", x)
  x <- gsub("nineight",  "98", x)
  x <- gsub("one",        "1", x)
  x <- gsub("two",        "2", x)
  x <- gsub("three",      "3", x)
  x <- gsub("four",       "4", x)
  x <- gsub("five",       "5", x)
  x <- gsub("six",        "6", x)
  x <- gsub("seven",      "7", x)
  x <- gsub("eight",      "8", x)
  x <- gsub("nine",       "9", x)
  x
}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day01
#' @export

example_data_01 <- function(example = 1) {
  l <- list(
    a = c(
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    ),
    b = c(
      "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    )
  )
  l[[example]]
}
