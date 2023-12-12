#' Day 12: Hot Springs
#'
#' [Hot Springs](https://adventofcode.com/2023/day/12)
#'
#' @name day12
#' @rdname day12
#' @param x raw springs
#' @export
#' @examples
#' solve12a(example_data_12())
#' solve12b(example_data_12())

solve12a <- function(x) {

  spring_data <- parse12(x)
  spring_data |> sapply(\(.) brute_force_12(.$springs, .$broken)) |> sum()

}

#' @rdname day12
#' @export

solve12b <- function(x) {

  # example_data_12() |> parse12() |> lapply(unfold)

}


# Helpers ----------------------------------------------------------------------

parse12 <- function(x)
  lapply(
    strsplit(x, " "),
    function(.)
      list(
        springs = strsplit(.[1], "")[[1]],
        broken  = as.integer(strsplit(.[2], ",")[[1]])
      )
  )

broken_match <- function(springs, broken) {
  # no "?" in springs for example .#...#....###.

  broken0 <- rle(springs)

  if (broken0$values[1] == "#")
    broken0 <- broken0$lengths[c(TRUE, FALSE)]
  else
    broken0 <- broken0$lengths[c(FALSE, TRUE)]

  identical(broken0, as.integer(broken))

}

brute_force_12 <- function(springs, broken) {

  res <- 0

  unknown <- which(springs == "?")
  broken_to_add <- sum(broken) - sum(springs == "#")

  possib <- combn(length(unknown), broken_to_add)

  for (p in seq(ncol(possib))) {
    to_test <- springs
    to_test[to_test == "?"] <- "."
    to_test[unknown[possib[,p]]] <- "#"
    if (broken_match(to_test, broken)) res <- res + 1
  }

  res

}

unfold <- function(sp) {
  list(
    springs = sp$springs |> c("?") |> rep(5) |> head(-1),
    broken  = rep(sp$broken, 5)
  )
}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day12
#' @export

example_data_12 <- function(example = 1) {
  l <- list(
    c(
      "???.### 1,1,3",
      ".??..??...?##. 1,1,3",
      "?#?#?#?#?#?#?#? 1,3,1,6",
      "????.#...#... 4,1,1",
      "????.######..#####. 1,6,5",
      "?###???????? 3,2,1"
    )
  )
  l[[example]]
}
