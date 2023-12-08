#' Day 08: Haunted Wasteland
#'
#' [Haunted Wasteland](https://adventofcode.com/2023/day/8)
#'
#' @name day08
#' @rdname day08
#' @param x raw network
#' @export
#' @examples
#' solve08a(example_data_08("a"))
#' solve08b(example_data_08("b"))

solve08a <- function(x) {

  network <- parse08(x)
  N <- length(network$instructions)

  node <- "AAA"
  n_instructions <- 0

  while (node != "ZZZ") {
    i <- network$instructions[n_instructions %% N + 1]
    node <- network$nodes[[node]][[i]]
    n_instructions <- n_instructions + 1
  }

  n_instructions

}

#' @rdname day08
#' @export

solve08b <- function(x) {

  # assumes :
  #   instructions list is not made of sub-cycles
  #   there is no introductory sequence before entering a cycle

  network <- parse08(x)
  start_nodes <- grep("A$", names(network$nodes), value = TRUE)

  start_nodes |> sapply(cycle_length, network = network) |> Reduce(f = lcm)

}


# Helpers ----------------------------------------------------------------------

gcd <- function(a, b) if (b == 0) a  else gcd(b, a %% b)

lcm <- function(a, b) a * b / gcd(a, b)

parse08 <- function(x) {

  instructions <- x[1] |> strsplit("") |> unlist()

  tab_nodes <- x |> tail(-2) |> str_match("(...) = .(...), (...).")
  nodes <- apply(tab_nodes, 1, \(.) list(L = .[3], R = .[4]))
  names(nodes) <- tab_nodes[, 2]

  list(instructions = instructions, nodes = nodes)

}

apply_all_instructions <- function(node, network) {

  # apply all instructions for a node in a map, return final node

  for (i in network$instructions) node <- network$nodes[[node]][[i]]
  node

}

cycle_length <- function(node, network) {

  # apply all instructions as many time as needed to get a final "Z"

  apply_times <- 0

  while (substr(node, 3, 3) != "Z") {
    node <- apply_all_instructions(node, network)
    apply_times <- apply_times + 1
  }

  apply_times * length(network$instructions)

}


# Test data ---------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day08
#' @export

example_data_08 <- function(example = 1) {
  l <- list(
    c(
      "RL",
      "",
      "AAA = (BBB, CCC)",
      "BBB = (DDD, EEE)",
      "CCC = (ZZZ, GGG)",
      "DDD = (DDD, DDD)",
      "EEE = (EEE, EEE)",
      "GGG = (GGG, GGG)",
      "ZZZ = (ZZZ, ZZZ)"
    ),
    a = c(
      "LLR",
      "",
      "AAA = (BBB, BBB)",
      "BBB = (AAA, ZZZ)",
      "ZZZ = (ZZZ, ZZZ)"
    ),
    b = c(
      "LR",
      "",
      "11A = (11B, XXX)",
      "11B = (XXX, 11Z)",
      "11Z = (11B, XXX)",
      "22A = (22B, XXX)",
      "22B = (22C, 22C)",
      "22C = (22Z, 22Z)",
      "22Z = (22B, 22B)",
      "XXX = (XXX, XXX)"
    )
  )
  l[[example]]
}
