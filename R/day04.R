#' Day 04: Scratchcards
#'
#' [Scratchcards](https://adventofcode.com/2023/day/4)
#'
#' @name day04
#' @rdname day04
#' @param x raw cards
#' @export
#' @examples
#' solve04a(example_data_04())
#' solve04b(example_data_04())

solve04a <- function(x)
  x |>
    parse04() |>
    lapply(winning_numbers) |>
    lengths() |>
    card_value() |>
    sum()

#' @rdname day04
#' @export

solve04b <- function(x)
  x |>
    parse04() |>
    lapply(winning_numbers) |>
    lengths() |>
    unstack()


# Helpers ----------------------------------------------------------------------

parse04 <- function(x)
  x |>
    strsplit("[:|]") |>
    lapply(
      \(cd) cd[-1] |> setNames(c("win", "have")) |> trimws() |> strsplit(" +")
    )

winning_numbers <- function(card) intersect(card$win, card$have)

card_value <- function(n_win) floor(2 ^ (n_win - 1))

unstack <- function(n_copies) {

  id <- seq_along(n_copies)

  stack <- id
  cards_count <- 0

  while (length(stack)) {

    # 1. pop card
    id1 <- stack[1]
    stack <- stack[-1]
    cards_count <- cards_count + 1

    # 2. push copies
    n_to_stack <- n_copies[id1]
    if (n_to_stack > 0) stack <- c(id[id1 + seq(n_to_stack)], stack)

  }

  cards_count

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day04
#' @export

example_data_04 <- function(example = 1) {
  l <- list(
    c(
      "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    )
  )
  l[[example]]
}
