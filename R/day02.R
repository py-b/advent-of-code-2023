#' Day 02: Cube Conundrum
#'
#' [Cube Conundrum](https://adventofcode.com/2023/day/2)
#'
#' @name day02
#' @rdname day02
#' @param x raw games
#' @export
#' @importFrom stats setNames
#' @importFrom stringr str_match
#' @examples
#' solve02a(example_data_02())
#' solve02b(example_data_02())

solve02a <- \(x) x |> parse02() |> sapply(valid_game) |> which() |> sum()

#' @rdname day02
#' @export

solve02b <- \(x) x |> parse02() |> sapply(game_power) |> sum()


# Helpers ----------------------------------------------------------------------

parse02 <- function(raw_games)
  lapply(
    strsplit(raw_games, "[;:] "),
    \(sets) sets |> tail(-1) |> strsplit(", ") |> lapply(normalize_set)
  )

normalize_set <- function(raw_set) {

  t <- str_match(raw_set, "(\\d+) (r|g|b)")
  set <- setNames(as.integer(t[, 2]), t[, 3])

  if (is.na(set["r"])) set["r"] <- 0
  if (is.na(set["g"])) set["g"] <- 0
  if (is.na(set["b"])) set["b"] <- 0

  set

}

valid_set  <- \(set)  set["r"] <= 12 && set["g"] <= 13 && set["b"] <= 14
valid_game <- \(game) all(sapply(game, valid_set))

game_power <- function(game) {

  max_r <- max(sapply(game, \(set) set["r"]))
  max_g <- max(sapply(game, \(set) set["g"]))
  max_b <- max(sapply(game, \(set) set["b"]))

  max_r * max_g * max_b

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day02
#' @export

example_data_02 <- function(example = 1) {
  l <- list(
    c(
     "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
     "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
     "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
     "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
     "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )
  )
  l[[example]]
}
