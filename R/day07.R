#' Day 07: Camel Cards
#'
#' [Camel Cards](https://adventofcode.com/2023/day/7)
#'
#' @name day07
#' @rdname day07
#' @param x some data
#' @export
#' @examples
#' solve07a(example_data_07())
#' solve07b(example_data_07())

solve07a <- function(x) solve07(x)

#' @rdname day07
#' @export

solve07b <- function(x) solve07(x, jokers = TRUE)


# Helpers -----------------------------------------------------------------

solve07 <- function(x, jokers = FALSE) {

  hands <- parse07(x)

  # strength

  hands$strength <- strength(hands$hand)

  if (jokers) {
    jokers_count <- stringr::str_count(hands$hand, "J")
    hands$strength <- mapply(update_strength, hands$strength, jokers_count)
  }

  # cards value

  if (jokers)
    cards <- c("J", 2:9, "T", "Q", "K", "A")
  else
    cards <- c(2:9, "T", "J", "Q", "K", "A")

  for (i in 1:5) {
    card_i <- paste0("card", i)
    hands[[card_i]] <- ordered(substr(hands$hand, i, i), levels = cards)
  }

  # sort on strength, then on card values

  line_order <- with(hands, order(strength, card1, card2, card3, card4, card5))
  hands <- hands[line_order, ]
  hands$rank <- seq(nrow(hands))

  # result

  with(hands, sum(rank * bid))

}

parse07 <- function(x) {
  utils::read.table(text = x, col.names = c("hand", "bid"))
}

strength <- function(hand) {

  # Strength representation (will arrange nicely with character sort)
      # "5"     : five of a kind
      # "41"    : four of a kind
      # "32"    : full house
      # "311"   : three of a kind
      # "221"   : two pairs
      # "21111" : one pair
      # "11111" : high card

  hand |>
    strsplit("") |>
    sapply(\(cards) cards |> table() |> sort() |> rev() |> paste(collapse = ""))

}

update_strength <- function(strength, jokers) {

  if (jokers == 0)         return(strength)
  if (strength == "11111") return("2111")
  if (strength == "2111")  return("311")
  if (strength == "221")   return(if (jokers == 1) "32" else "41")
  if (strength == "311")   return("41")
  # other strengths ("32", "41" and "5") upgrade to 5 when they have "J"s
  return("5")

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day07
#' @export

example_data_07 <- function(example = 1) {
  l <- list(
    c(
      "32T3K 765",
      "T55J5 684",
      "KK677 28",
      "KTJJT 220",
      "QQQJA 483"
    )
  )
  l[[example]]
}
