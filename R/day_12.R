library(tidyverse)

read_day12 <- function(path) {
  edges <- read_lines(path) %>%
    map(str_split, "-") %>%
    flatten()
  edges <- append(edges, map(edges, rev)) %>%
    discard(~.x[[2]] == "end") %>% # create sinking states
    discard(~.x[[1]] == "start")
  
  nodes <- edges %>%
    flatten() %>%
    unique()
  
  list(edges = edges,
       nodes = nodes)
}

# ---- part 1 ----

connects_to <- function(node, graph) {
  edges <- graph$edges
  
  candidates <- keep(edges, ~.x[[1]] == node) %>%
    map(pluck, 2) %>%
    as.character()
  
  if (length(candidates) == 0) {
    NA
  } else {
    candidates
  }
}

# paths are vectors of locations
step_through <- function(paths, graph, prune_strat = is_bad_path) {
  endpoints <- map(paths, get_endpoint)
  connections <- map(endpoints, connects_to, graph)
  check_endpoints <- all(map_lgl(endpoints, ~length(.x) == 1 && .x == "start"))
  if (check_endpoints) {
    return(paths)
  } else {
    new_paths <- expand_paths(paths, connections) %>%
      prune_paths(prune_strat)
    step_through(new_paths, graph, prune_strat)
  }
}

get_endpoint <- function(path) {
  path[length(path)]
}

expand_paths <- function(paths, connections) {
  map2(paths, connections,
       ~map(.y, function(y) {if (is.na(y)) .x else c(.x, y)})) %>%
    flatten()
}

prune_paths <- function(paths, prune_strat) {
  paths %>% discard(prune_strat)
}

is_bad_path <- function(path) {
  small_caves <- path %>%
    discard(~.x=="start") %>%
    discard(~.x=="end") %>%
    discard(~str_detect(.x, "[A-Z]"))
  
  # it's a bad path if the there are duplicates of small caves
  # if there are duplicates, then this will be true
  length(small_caves) != length(unique(small_caves)) 
}

part1 <- function(path) {
  graph <- read_day12(path)
  length(step_through(list("end"), graph))
}

part1("data/12.txt") # 4167

# --- part 2 ----


is_bad_path_2 <- function(path) {
  small_caves <- path %>%
    discard(~.x=="start") %>%
    discard(~.x=="end") %>%
    discard(~str_detect(.x, "[A-Z]"))
  
  # it's a bad path if the there are duplicates of small caves
  # if there are duplicates, then this will be true
  (length(small_caves) - length(unique(small_caves))) > 1
}

part2 <- function(path) {
  graph <- read_day12(path)
  length(step_through(list("end"), graph, is_bad_path_2))
}


part2("data/12.txt") # 98441   but this solution is really slow
