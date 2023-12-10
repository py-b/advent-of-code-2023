#' Day 10: Pipe Maze
#'
#' [Pipe Maze](https://adventofcode.com/2023/day/10)
#'
#' @name day10
#' @rdname day10
#' @param x raw pipes
#' @export
#' @examples
#' solve10a(example_data_10())
#' solve10b(example_data_10())

solve10a <- function(x) {

  pipes <- parse10(x)
  travel_pipes(pipes, value = "count") / 2

}

#' @rdname day10
#' @export

solve10b <- function(x) {

  pipes <- parse10(x)

  path <- travel_pipes(pipes, value = "coords")

  grid <- matrix(".", nrow = nrow(pipes), ncol = ncol(pipes))
  for (p in path) grid[p$row, p$col] <- pipes[p$row, p$col]

  start_pipe <- start_infos(pipes)$pipe
  n_inside(grid, start_pipe)

}


# Helpers ----------------------------------------------------------------------

parse10 <- function(x) do.call(rbind, strsplit(x, ""))

start_infos <- function(pipes) {

  # pipe shape of the start (S) & coords of its two connections

  start_coord <- which(pipes == "S", arr.ind = TRUE)[1, ] |> unname()
  row0 <- start_coord[1]
  col0 <- start_coord[2]

  connect <- NULL

  if (row0 > 1 && pipes[row0 - 1, col0] %in% c("|", "F", "7"))
    connect$north <- list(row = row0 - 1, col = col0)

  if (col0 < ncol(pipes) && pipes[row0, col0 + 1] %in% c("-", "J", "7"))
    connect$east <- list(row = row0, col = col0 + 1)

  if (row0 < nrow(pipes) && pipes[row0 + 1, col0] %in% c("|", "L", "J"))
    connect$south <- list(row = row0 + 1, col = col0)

  if (col0 > 1 && pipes[row0, col0 - 1] %in% c("-", "L", "F"))
    connect$west <- list(row = row0, col = col0 - 1)

  if (!is.null(connect$north) && !is.null(connect$east) ) pipe <- "L"
  if (!is.null(connect$north) && !is.null(connect$south)) pipe <- "|"
  if (!is.null(connect$north) && !is.null(connect$west) ) pipe <- "J"
  if (!is.null(connect$east)  && !is.null(connect$south)) pipe <- "F"
  if (!is.null(connect$east)  && !is.null(connect$west) ) pipe <- "-"
  if (!is.null(connect$north) && !is.null(connect$east) ) pipe <- "L"
  if (!is.null(connect$south) && !is.null(connect$west) ) pipe <- "7"

  list(pipe = pipe, connect = connect)

}

next_pipe_coord <- function(prev, symbol, current) {

  from_north_or_south <- current$col == prev$col
  from_west           <- current$col > prev$col
  from_north          <- current$row > prev$row

  switch(symbol,
    "-" =
      list(
        row = current$row,
        col = current$col + if (from_west) 1 else -1
      ),
    "|" =
      list(
        row = current$row + if (from_north) 1 else -1,
        col = current$col
      ),
    "L" =
      list(
        row = current$row + if (from_north_or_south) 0 else -1,
        col = current$col + if (from_north_or_south) 1 else  0
      ),
    "J" =
      list(
        row = current$row + if (from_north_or_south)  0 else -1,
        col = current$col + if (from_north_or_south) -1 else  0
      ),
    "7" =
      list(
        row = current$row + if (from_north_or_south)  0 else 1,
        col = current$col + if (from_north_or_south) -1 else 0
      ),
    "F" =
      list(
        row = current$row + if (from_north_or_south) 0 else 1,
        col = current$col + if (from_north_or_south) 1 else 0
      )
  )

}

travel_pipes <- function(pipes, value = c("count", "coords")) {

  value <- match.arg(value)

  start_p <- which(pipes == "S", arr.ind = TRUE)[1, ] |> as.list()

  previous_p <- start_p
  current_p <- start_infos(pipes)$connect[[1]]

  if (value == "count") res <- 0 else res <- list()
  symbol <- ""

  while (symbol != "S") {
    symbol <- pipes[current_p$row, current_p$col]
    save_previous_p <- current_p
    current_p <- next_pipe_coord(previous_p, symbol, current_p)
    previous_p <- save_previous_p
    if (value == "count")
      res <- res + 1
    else
      res <- append(res, list(previous_p))
  }

  res

}

n_inside <- function(grid, start_pipe) {

  res <- 0

  grid[grid == "S"] <- start_pipe

  for (row in seq(nrow(grid))) {

    simple_row <- grid[row, , drop = TRUE]

    # searching vertical pipes on rows, hence ignore horizontal pipes
    simple_row <- simple_row[simple_row != "-"]

    # make one string to handle corner patterns more easily
    simple_row <- paste(simple_row, collapse = "")

    # count as one in or out
    simple_row <- gsub("(L7|FJ)", "|",  simple_row)
    # count as in and out (or out and in)
    simple_row <- gsub("(F7|LJ)", "||", simple_row)

    # uncollapse
    simple_row <- strsplit(simple_row, "")[[1]]

    # finally ready to count with parity rule
    # https://iq.opengenus.org/inside-outside-test

    inside <- FALSE # will change everytime a vertical border is met

    for (x in simple_row) {
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
#' @rdname day10
#' @export

example_data_10 <- function(example = 1) {
  l <- list(
    a1 = c(
      "-L|F7",
      "7S-7|",
      "L|7||",
      "-L-J|",
      "L|-JF"
    ),
    a2 = c(
      "7-F7-",
      ".FJ|7",
      "SJLL7",
      "|F--J",
      "LJ.LJ"
    ),
    b1 = c(
      "...........",
      ".S-------7.",
      ".|F-----7|.",
      ".||.....||.",
      ".||.....||.",
      ".|L-7.F-J|.",
      ".|..|.|..|.",
      ".L--J.L--J.",
      "..........."
    ),
    b2 = c(
      ".F----7F7F7F7F-7....",
      ".|F--7||||||||FJ....",
      ".||.FJ||||||||L7....",
      "FJL7L7LJLJ||LJ.L-7..",
      "L--J.L7...LJS7F-7L7.",
      "....F-J..F7FJ|L7L7L7",
      "....L7.F7||L7|.L7L7|",
      ".....|FJLJ|FJ|F7|.LJ",
      "....FJL-7.||.||||...",
      "....L---J.LJ.LJLJ..."
    ),
    b3 = c(
      "FF7FSF7F7F7F7F7F---7",
      "L|LJ||||||||||||F--J",
      "FL-7LJLJ||||||LJL-77",
      "F--JF--7||LJLJ7F7FJ-",
      "L---JF-JLJ.||-FJLJJ7",
      "|F|F-JF---7F7-L7L|7|",
      "|FFJF7L7F-JF7|JL---7",
      "7-L-JL7||F7|L7F-7F7|",
      "L.L7LFJ|||||FJL7||LJ",
      "L7JLJL-JLJLJL--JLJ.L"
    )
  )
  l[[example]]
}
