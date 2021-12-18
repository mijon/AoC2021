library(tidyverse)

read_day13 <- function(path) {
  input <- read_file(path) %>%
    str_split("\\n\\n") %>%
    pluck(1)
  
  points <- input[[1]] %>% read_lines() %>%
    as_tibble() %>%
    separate(col = "value",
             into = c("x", "y"),
             sep = ",") %>%
    mutate(across(everything(), parse_number))
  
  folds <- input[[2]] %>%
    read_lines() %>%
    str_remove("fold along ") %>%
    as_tibble() %>%
    separate(col = "value",
             into = c("f_direction", "f_value"),
             sep = "=") %>%
    mutate(f_value = parse_number(f_value))
  
  list(points = points,
       folds = folds)
}

# --- part 1 ----

plot_points <- function(points) {
  points %>%
    ggplot(aes(x = x, y = y)) + 
    geom_tile(fill = "#009900", colour = "white") +
    coord_fixed() +
    theme_void() +
    theme(panel.background = element_rect(fill = "#0f0f23")) +
    scale_y_reverse()
}

fold <- function(points, instruction) {
  f_direction <- rlang::sym(instruction$f_direction)
  f_value <- instruction$f_value
  
  points %>% mutate(!!f_direction := if_else(!!f_direction > f_value,
                                               2 * f_value - !!f_direction,
                                               !!f_direction))
}

part1 <- function(path) {
  input <- read_day13(path)
  points <- input$points
  folds <- input$folds %>% transpose()
  
  fold(points, folds[[1]]) %>%
    distinct() %>%
    nrow()
}

part1("data/13.txt") # 607

# ---- part2 ----

part2 <- function(path) {
  input <- read_day13(path)
  points <- input$points
  folds <- input$folds %>% transpose()
  
  reduce(folds, fold, .init = points) %>%
    plot_points()
}


part2("data/13.txt")# CPZLPFZL
