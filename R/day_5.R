library(tidyverse)

# ---- Part 1 ----
read_day5 <- function(path) {
  read_lines(path) %>%
    map(parse_points) %>%
    keep(is_h_or_v) %>%
    map(make_line)
}


parse_points <- function(string) {
  str_split(string, " -> ")[[1]] %>%
    str_split(",") %>%
    flatten() %>%
    set_names("x1", "y1", "x2", "y2") %>%
    map(as.numeric)
}

is_h_or_v <- function(p) {
  p$x1 == p$x2 || p$y1 == p$y2
}

make_line <- function(p) {
  if (p$x1 == p$x2) {
    expand_ys(p)
  } else {
    expand_xs(p)
  }
}

expand_ys <- function(p) {
  min_y = min(p$y1, p$y2)
  max_y = max(p$y1, p$y2)
  
  tibble(x = p$x1,
         y = seq(min_y, max_y))
}


expand_xs <- function(p) {
  min_x = min(p$x1, p$x2)
  max_x = max(p$x1, p$x2)
  
  tibble(x = seq(min_x, max_x),
         y = p$y1)
}

plot_output <- function(df) {
  df %>% ggplot(aes(x = x, y = y, fill = n, label = n)) +
    geom_tile() +
    scale_y_reverse() +
    coord_fixed() +
    geom_label() +
    scale_fill_viridis_c() +
    theme(panel.background = element_rect(fill = "black"), legend.position = "none")
}

part1 <- function(path, draw_plot = FALSE) { # draw_plot makes it slower, but prettier
  output <- read_day5(path) %>%
    bind_rows() %>%
    count(x, y, sort = TRUE)
  
  if (draw_plot) {
    print(plot_output(output))
  }
  
  output %>% filter(n >= 2) %>%
    nrow()
}


part1("data/5_1.txt", draw_plot = FALSE) # 7412

# --- Part 2 ----

read_day5_2 <- function(path) {
  read_lines(path) %>%
    map(parse_points) %>%
    map(make_line_diags)
}

make_line_diags <- function(p) {
  if (p$x1 == p$x2) {
    expand_ys(p)
  } else if (p$y1 == p$y2) {
    expand_xs(p)
  } else {
    expand_diag(p)
  }
}

expand_diag <- function(p) {
  tibble(x = p$x1:p$x2,
         y = p$y1:p$y2)
}



part2 <- function(path, draw_plot = FALSE) { # draw_plot makes it slower, but prettier
  output <- read_day5_2(path) %>%
    bind_rows() %>%
    count(x, y, sort = TRUE)
  
  if (draw_plot) {
    print(plot_output(output))
  }
  
  output %>% filter(n >= 2) %>%
    nrow()
}

part2("data/5_1.txt") # 20012
