#' Day 18: Lavaduct Lagoon
#'
#' [Lavaduct Lagoon](https://adventofcode.com/2023/day/18)
#'
#' @name day18
#' @rdname day18
#' @param x raw dig plan
#' @export
#' @examples
#' solve18a(example_data_18())
#' solve18b(example_data_18())

solve18a <- function(x) {
  dig_plan <- parse18(x)
  perimeter <- perimeter_grid(dig_plan$direction, dig_plan$length)
  sum(perimeter != ".") + n_inside_18(perimeter)
}

# #' @rdname day18
# #' @export
#
# solve18b <- function(x) {
#
# }


# Helpers ----------------------------------------------------------------------

parse18 <- function(x)
  read.table(
    text = x,
    col.names = c("direction", "length", "hexa"),
    comment.char = ""
  )

get_next_point <- function(point, direction) {
  drow <- switch(direction, "U" = -1, "D" = 1, "L" =  0, "R" = 0)
  dcol <- switch(direction, "U" =  0, "D" = 0, "L" = -1, "R" = 1)
  c(point[1] + drow, point[2] + dcol)
}

turn_shape <- function(dir1, dir2) {
  # same turn codification as in ?day10
  if (dir1 == "R" && dir2 == "D") return("7")
  if (dir1 == "U" && dir2 == "L") return("7")
  if (dir1 == "D" && dir2 == "L") return("J")
  if (dir1 == "R" && dir2 == "U") return("J")
  if (dir1 == "L" && dir2 == "U") return("L")
  if (dir1 == "D" && dir2 == "R") return("L")
  if (dir1 == "U" && dir2 == "R") return("F")
  if (dir1 == "L" && dir2 == "D") return("F")
  stop("impossible turn")
}

perimeter_grid <- function(directions, lengths) {

  turns_shapes <- c(
    turn_shape(tail(directions, 1), head(directions, 1)),
    mapply(turn_shape, head(directions, -1), tail(directions, -1))
  )
  shapes <- rep("#", sum(lengths))
  shapes[c(1, head(cumsum(lengths) + 1, -1))] <- turns_shapes

  # repeat directions to iterate moves
  directions <- rep(directions, lengths)

  # matrix of perimeter points
  points <- matrix(c(1, 1), nrow = 1)
  for (d in directions)
    points <- rbind(points, get_next_point(tail(points, 1), d))
  points <- head(points, -1) # duplicated first point

  # reframe so that topleft corner is (1,1)
  min_row <- min(points[, 1])
  min_col <- min(points[, 2])
  points <- cbind(points, points[, 1] - min_row + 1)
  points <- cbind(points, points[, 2] - min_col + 1)

  # empty grid
  max_row <- max(points[, 3])
  max_col <- max(points[, 4])
  grid <- matrix(".", nrow = max_row, ncol = max_col)

  # fill grid with shapes
  for (i in 1:nrow(points)) grid[points[i, 3], points[i, 4]] <- shapes[i]

  grid

}

n_inside_18 <- function(grid) {

  res <- 0

  for (row in seq(nrow(grid))) {

    # make one string to handle turn patterns more easily
    simple_row <- paste(grid[row, , drop = TRUE], collapse = "")

    # count as one in or out
    simple_row <- gsub("(F#*7|L#*J)", "||", simple_row)
    # count as in and out (or out and in)
    simple_row <- gsub("(L#*7|F#*J)", "|",  simple_row)
    # remaining "#" (isolated ones)
    simple_row <- gsub("#", "|",  simple_row)

    # finally ready to count with parity rule
    #   https://iq.opengenus.org/inside-outside-test
    inside <- FALSE # will change everytime a vertical border is met
    for (x in strsplit(simple_row, "")[[1]]) {
      if (x == "|")
        inside <- !inside
      else
        if (inside) res <- res + 1
    }

  }

  res

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day18
#' @export

example_data_18 <- function(example = 1) {
  l <- list(
    c(
      "R 6 (#70c710)",
      "D 5 (#0dc571)",
      "L 2 (#5713f0)",
      "D 2 (#d2c081)",
      "R 2 (#59c680)",
      "D 2 (#411b91)",
      "L 5 (#8ceee2)",
      "U 2 (#caa173)",
      "L 1 (#1b58a2)",
      "U 2 (#caa171)",
      "R 2 (#7807d2)",
      "U 3 (#a77fa3)",
      "L 2 (#015232)",
      "U 2 (#7a21e3)"
    )
  )
  l[[example]]
}
