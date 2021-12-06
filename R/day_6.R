library(tidyverse)

# ---- part 1 ----

read_day6 <- function(path) {
  path %>%
    read_file() %>%
    str_split(",", simplify = TRUE) %>%
    as.numeric() %>%
    tibble(age = .) %>%
    group_by(age) %>%
    count()
}

next_generation <- function(generation) {
  generation %>%
    mutate(age = map(age, step_generation)) %>%
    unnest_longer(age) %>%
    group_by(age) %>%
    summarise(n = sum(n))
}

step_generation <- function(x) {
  x_next = (x - 1)
  if (x_next == -1) {
    list(6, 8)
  } else {
    list(x_next)
  }
}

forward_n_generations <- function(generation, n) {
  reduce(1:n, function(x, y) {next_generation(x)}, .init = generation)
}

part1 <- function(path, n) {
  read_day6(path) %>%
    forward_n_generations(n) %>%
    pull(n) %>%
    sum()
}

part1("data/6_1.txt", n = 80) # 380758

# ---- part 2 ----
part1("data/6_1.txt", n = 256) # 1710623015163
