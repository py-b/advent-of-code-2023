#' Day 03: Gear Ratios
#'
#' [Gear Ratios](https://adventofcode.com/2023/day/3)
#'
#' @name day03
#' @rdname day03
#' @param x raw data
#' @importFrom stringr str_extract_all
#' @export
#' @examples
#' solve03a(example_data_03())
#' solve03b(example_data_03())

solve03a <- function(x) {

  numbers <- x |> str_extract_all("[0-9]+") |> unlist() |> as.integer()
  with_symbol <- is_part_number(numbers_location(x), grid = parse03(x))

  sum(numbers[with_symbol])

}

#' @rdname day03
#' @export

solve03b <- function(x) {

  # General idea :
  #  - finding the coordinates of the * adjacent for each number (NA if no *)
  #  - multiply the numbers which have the same * coords (ignore the others)

  numbers <- x |> str_extract_all("[0-9]+") |> unlist() |> as.integer()
  stars_id <- stars_location(numbers_location(x), grid = parse03(x))

  share_star <- !is.na(stars_id) &
                (duplicated(stars_id) | duplicated(stars_id, fromLast = TRUE))

  numbers[share_star] |>
    split(stars_id[share_star]) |>
    sapply(prod) |>
    sum()

}


# Helpers ----------------------------------------------------------------------

parse03 <- \(x) do.call(rbind, strsplit(x, ""))

numbers_location <- function(raw_data) {

  infos <- gregexpr("[0-9]+", raw_data)

  x       <- rep(seq(infos), lengths(infos))
  y_start <- unlist(infos)
  digits  <- infos |> lapply(attr, "match.length") |> unlist()

  res <- Map(
    function(x, y, d) matrix(c(x, x, y, y + d - 1), nrow = 2),
    x,
    y_start,
    digits
  )

  res[digits > 0]

}

adjacent_box <- function(number_location, dim_grid) {

  list(
    xmin = max(number_location[1, 1] - 1, 1),
    xmax = min(number_location[1, 1] + 1, dim_grid[1]),
    ymin = max(number_location[1, 2] - 1, 1),
    ymax = min(number_location[2, 2] + 1, dim_grid[2])
  )

}

is_part_number <- function(numbers_locations, grid) {

  boxes <- lapply(numbers_locations, adjacent_box, dim_grid = dim(grid))

  sapply(
    boxes,
    function(b) any(grepl("[^0-9.]", grid[b$xmin:b$xmax, b$ymin:b$ymax]))
  )

}

stars_location <- function(numbers_locations, grid) {

  boxes <- lapply(numbers_locations, adjacent_box, dim_grid = dim(grid))

  sapply(
    boxes,
    function(b) {
      coord <-
        which(
          grid[b$xmin:b$xmax, b$ymin:b$ymax] == "*",
          arr.ind = TRUE
        )
      if (!nrow(coord)) return(NA_character_)
      paste(
        coord[ , "row"] + b$xmin - 1,
        coord[ , "col"] + b$ymin - 1
      )
    }
  )

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name)
#' @rdname day03
#' @export

example_data_03 <- function(example = 1) {
  l <- list(
    c(
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    )
  )
  l[[example]]
}
