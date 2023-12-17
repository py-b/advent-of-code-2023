#' Day 15: Lens Library
#'
#' [Lens Library](https://adventofcode.com/2023/day/15)
#'
#' @name day15
#' @rdname day15
#' @param x raw instructions
#' @importFrom stringr str_match
#' @export
#' @examples
#' solve15a(example_data_15())
#' solve15b(example_data_15())

solve15a <- function(x)
  x |> strsplit(",") |> unlist() |> hash15() |> sum()

#' @rdname day15
#' @export

solve15b <- function(x) {

  instructions <- parse15(x)

  instructions |>
    fill_boxes() |>
    vapply(box_value, numeric(1)) |>
    sum()

}


# Helpers ----------------------------------------------------------------------

hash15 <- function(text) {

  vapply(
    text,
    function(t) {
      hash <- 0L
      for (ascii in utf8ToInt(t)) hash <- ((hash + ascii) * 17) %% 256
      as.integer(hash)
    },
    FUN.VALUE = integer(1)
  )

}

parse15 <- function(x) {

  instructions <-
    x |>
    strsplit(",") |>
    unlist() |>
    str_match("(?<label>[a-z]+)[=-](?<focal>[0-9]*)") |>
    as.data.frame()

  instructions$hash  <- hash15(instructions$label)
  instructions$focal <- as.integer(instructions$focal)

  instructions[c("label", "focal", "hash")]

}

fill_boxes <- function(instructions) {

  boxes <- vector(mode = "list", length = 256)

  for (i in seq(nrow(instructions))) {

    label <- instructions$label[i]
    index <- instructions$hash[i] + 1
    focal <- instructions$focal[i]

    if (is.na(focal)) {
      if (is.null(boxes[[index]])) next
      boxes[[index]][[label]] <- NULL
    } else {
      boxes[[index]][[label]] <- focal
    }

  }

  Filter(\(.) length(.) > 0, boxes)

}

box_value <- function(box) {
  box_number <- hash15(names(box)[1])
  focals <- unlist(box)
  sum((box_number + 1) * focals * seq(box))
}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day15
#' @export

example_data_15 <- function(example = 1) {
  l <- list(
    c(
      "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    )
  )
  l[[example]]
}
