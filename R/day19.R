#' Day 19: Aplenty
#'
#' [Aplenty](https://adventofcode.com/2023/day/19)
#'
#' @name day19
#' @rdname day19
#' @param x some data
#' @importFrom stringr str_extract_all str_match
#' @export
#' @examples
#' solve19a(example_data_19())
#' solve19b(example_data_19())

solve19a <- function(x) {

  data19    <- parse19(x)
  parts     <- data19$parts
  workflows <- data19$workflows

  accepted <- vapply(parts, workflow_accepted, workflows, FUN.VALUE = TRUE)

  parts[accepted] |> unlist() |> sum()

}

#' @rdname day19
#' @export

solve19b <- function(x) {

}


# Helpers ----------------------------------------------------------------------

parse19 <- function(x) {

  blank <- which(x == "")

  parts <-
    x |>
    tail(length(x) - blank) |>
    str_extract_all("\\d+") |>
    lapply(function(.) setNames(as.list(as.integer(.)), c("x", "m", "a", "s")))

  workflows <- x |> head(blank - 1) |> str_match("([a-z]+)\\{(.+)\\}")

  rules <-
    workflows[, 3] |>
    strsplit(",") |>
    lapply(
      function(r)
        r |> strsplit(":") |> setNames(c(rep("", length(r) - 1), "otherwise"))
    )

  list(
    workflows = setNames(rules, workflows[, 2]),
    parts = parts
  )

}

next_workflow <- function(part, workflow) {

  for (rule in head(workflow, -1)) {
    if (eval(str2lang(rule[1]), envir = part)) return(rule[2])
  }
  workflow$otherwise

}

workflow_accepted <- function(part, workflows) {

  workflow_id <- "in"
  while (!workflow_id %in% c("A", "R")) {
    workflow_id <- next_workflow(part, workflows[[workflow_id]])
  }
  workflow_id == "A"

}


# Test data --------------------------------------------------------------------

#' @param example Which example data to use (by position or name).
#' @rdname day19
#' @export

example_data_19 <- function(example = 1) {
  l <- list(
    c(
      "px{a<2006:qkq,m>2090:A,rfg}",
      "pv{a>1716:R,A}",
      "lnx{m>1548:A,A}",
      "rfg{s<537:gd,x>2440:R,A}",
      "qs{s>3448:A,lnx}",
      "qkq{x<1416:A,crn}",
      "crn{x>2662:A,R}",
      "in{s<1351:px,qqz}",
      "qqz{s>2770:qs,m<1801:hdj,R}",
      "gd{a>3333:R,R}",
      "hdj{m>838:A,pv}",
      "",
      "{x=787,m=2655,a=1222,s=2876}",
      "{x=1679,m=44,a=2067,s=496}",
      "{x=2036,m=264,a=79,s=2244}",
      "{x=2461,m=1339,a=466,s=291}",
      "{x=2127,m=1623,a=2188,s=1013}"
    )
  )
  l[[example]]
}
