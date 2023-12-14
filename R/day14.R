#' Day 14: Parabolic Reflector Dish
#'
#' [Parabolic Reflector Dish](https://adventofcode.com/2023/day/14)
#'
#' @name day14
#' @rdname day14
#' @param x some data
#' @export
#' @examples
#' solve14a(example_data_14())
#' solve14b(example_data_14())

solve14a <- function(x) parse14(x) |> tilt_platform_north() |> platform_load()

#' @rdname day14
#' @export

solve14b <- function(x) {

  platform <- parse14(x)

  platform_ids <- NULL

  # search for a cycle of spin cycles

  found <- FALSE
  n <- 1

  while (!found) {

    platform <- spin_cycle(platform)

    current_id <- flat_pf(platform)

    if (current_id %in% platform_ids) {
      found <- TRUE
      init <- which(platform_ids == current_id)
      cycle_length <- n - init
      remainder <- (10^9 - init) %% cycle_length
    }

    platform_ids <- c(platform_ids, current_id)
    n <- n + 1

  }

  # we can then skip the other cycles and
  # just perform the right number (remainder) of spin-cycles to get to 10^9

  for (n in seq(remainder)) platform <- spin_cycle(platform)

  platform_load(platform)

}


# Helpers ----------------------------------------------------------------------

parse14 <- function(x) do.call(rbind, strsplit(x, ""))

col_load <- function(col) sum(length(col) + 1 - which(col == "O"))

platform_load <- function(platform) platform |> apply(2, col_load) |> sum()

tilt_col <- function(col) {

  N <- length(col)

  rounded <- N + 1 - which(col == "O")
  no_cube <- N + 1 - seq(N)[col != "#"]
  no_cube_groups <- funprog::group_if(no_cube, \(a, b) a - b == 1)

  lapply(
    no_cube_groups,
    function(group) {
      in_group <- sum(rounded %in% group)
      if (in_group == 0) indexes <- 0 else indexes <- seq(in_group)
      group[indexes]
    }
  )

}

tilt_platform_north <- function(platform) {

  N <- nrow(platform)
  tilt_infos <- platform |> apply(2, tilt_col) |> lapply(unlist)

  coord_rounded <- function(row, col) lapply(N + 1 - row, function(r) c(r, col))
  rounded <-
    Map(coord_rounded, tilt_infos, seq(tilt_infos)) |>
    unlist(recursive = FALSE)

  platform[platform != "#"] <- "."
  for (r in rounded) platform[r[1], r[2]] <- "O"

  platform

}

rotate90_clock <- function(m) t(apply(m, 2, rev))
rotate90_trigo <- function(m) apply(t(m), 2, rev)
rotate180      <- function(m) {m[] <- rev(m) ; m}

tilt_platform_west  <- \(.)
  . |> rotate90_clock() |> tilt_platform_north() |> rotate90_trigo()

tilt_platform_south <- \(.)
  . |> rotate180() |> tilt_platform_north() |> rotate180()

tilt_platform_east  <- \(.)
  . |> rotate90_trigo() |> tilt_platform_north() |> rotate90_clock()

spin_cycle <- function(platform)
  platform |>
    tilt_platform_north() |>
    tilt_platform_west() |>
    tilt_platform_south() |>
    tilt_platform_east()

flat_pf <- function(platform) paste(platform, collapse = "")

show_platform <- \(mat)
  sprintf("%03d ", rev(seq(nrow(mat)))) |>
    cbind(mat) |>
    apply(1, paste, collapse = "") |>
    cat(sep = "\n")


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day14
#' @export

example_data_14 <- function(example = 1) {
  l <- list(
    c(
      "O....#....",
      "O.OO#....#",
      ".....##...",
      "OO.#O....O",
      ".O.....O#.",
      "O.#..O.#.#",
      "..O..#O..O",
      ".......O..",
      "#....###..",
      "#OO..#...."
    )
  )
  l[[example]]
}
