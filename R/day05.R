#' Day 05: If You Give A Seed A Fertilizer
#'
#' [If You Give A Seed A Fertilizer](https://adventofcode.com/2023/day/5)
#'
#' @name day05
#' @rdname day05
#' @param x seed data
#' @export
#' @examples
#' solve05a(example_data_05())
#' solve05b(example_data_05())

solve05a <- function(x) {

  d <- parse05(x)
  min_location(d$seeds, d$maps)

}

#' @rdname day05
#' @export

solve05b <- function(x) {

  # naive solution
  # will work on test data, BUT NOT ON REAL DATA

  d <- parse05(x)

  seed_starts <- d$seeds[c(TRUE, FALSE)]
  seed_ranges <- d$seeds[c(FALSE, TRUE)]

  seeds <- Map(\(s, n) seq(s, length.out = n), seed_starts, seed_ranges)

  min_location(unlist(seeds), d$maps)

}


# Helpers ----------------------------------------------------------------------

normalize_map <- function(m)
  if (length(m))
    m |> as.list() |> setNames(c("dest_start", "source_start", "length"))

parse05 <- function(x) {

  x_int <- str_extract_all(x, "\\d+") |> lapply(as.numeric)

  seeds <- x_int[[1]]

  x_int <- x_int |> tail(-2) |> lapply(normalize_map)
  maps <- split(x_int, cumsum(lengths(x_int) == 0))
  maps <- Filter(\(l) length(l) > 0, lapply(maps,\(m) m[lengths(m) > 0]))
  names(maps) <- c(
    "seed_soil", "soil_fertilizer", "fertilizer_water", "water_light",
    "light_temperature", "temperature_humidity", "humidity_location"
  )

  list(seeds = seeds, maps = maps)

}

source_to_destination <- function(val, map) {

  for (r in map)
    if (r$source_start <= val && val < r$source_start + r$length)
      return(r$dest_start + val - r$source_start)

  val

}

seed_to_location <- function(seed, maps)
  seed |>
    source_to_destination(maps$seed_soil) |>
    source_to_destination(maps$soil_fertilizer) |>
    source_to_destination(maps$fertilizer_water) |>
    source_to_destination(maps$water_light) |>
    source_to_destination(maps$light_temperature) |>
    source_to_destination(maps$temperature_humidity) |>
    source_to_destination(maps$humidity_location)

min_location <- function(seeds, maps)
  seeds |>
    vapply(
      seed_to_location,
      maps = maps,
      FUN.VALUE = numeric(1)
    ) |>
    min()


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day05
#' @export

example_data_05 <- function(example = 1) {
  l <- list(
    c(
      "seeds: 79 14 55 13",
      "",
      "seed-to-soil map:",
      "50 98 2",
      "52 50 48",
      "",
      "soil-to-fertilizer map:",
      "0 15 37",
      "37 52 2",
      "39 0 15",
      "",
      "fertilizer-to-water map:",
      "49 53 8",
      "0 11 42",
      "42 0 7",
      "57 7 4",
      "",
      "water-to-light map:",
      "88 18 7",
      "18 25 70",
      "",
      "light-to-temperature map:",
      "45 77 23",
      "81 45 19",
      "68 64 13",
      "",
      "temperature-to-humidity map:",
      "0 69 1",
      "1 0 69",
      "",
      "humidity-to-location map:",
      "60 56 37",
      "56 93 4"
    )
  )
  l[[example]]
}
