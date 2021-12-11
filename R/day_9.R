library(tidyverse)
options(scipen=999)

read_day9 <- function(path) {
  scan(path, what = "character") %>%
    map(~str_split(.x, "") %>%
          pluck(1) %>%
          map_dbl(as.numeric) %>%
          as.matrix) %>%
    reduce(cbind) %>%
    t()
}

shift_down <- function(mat, infill = Inf) {
  mask <- matrix(infill, nrow = nrow(mat), ncol = ncol(mat))
  mask[2:nrow(mask), 1:ncol(mask)] <- mat[1:(nrow(mat)-1), 1:ncol(mat)]
  mask
}

shift_up <- function(mat, infill = Inf) {
  mask <- matrix(infill, nrow = nrow(mat), ncol = ncol(mat))
  mask[1:(nrow(mask)-1), 1:ncol(mask)] <- mat[2:nrow(mat), 1:ncol(mat)]
  mask
}

shift_left <- function(mat, infill = Inf) {
  mask <- matrix(infill, nrow = nrow(mat), ncol = ncol(mat))
  mask[1:nrow(mask), 1:(ncol(mask)-1)] <- mat[1:nrow(mat), 2:ncol(mat)]
  mask 
}

shift_right <- function(mat, infill = Inf) {
  mask <- matrix(infill, nrow = nrow(mat), ncol = ncol(mat))
  mask[1:nrow(mask), 2:ncol(mask)] <- mat[1:nrow(mat), 1:(ncol(mat)-1)]
  mask 
}


find_lows <- function(m) {
  l <- shift_left(m)
  r <- shift_right(m)
  u <- shift_up(m)
  d <- shift_down(m)
  
  tested <- (m < 1) & (m < r) & (m < u) & (m < d)
  which(tested, arr.ind = TRUE)
}

part1 <- function(path) {
  input_mat <- read_day9(path)
  
  lows <- input_mat %>%
    find_lows()
  
  sum(1 + input_mat[lows])
}

part1("data/9_1.txt") # 539

# ---- part 2 ----

m <- read_day9("data/9_1.txt")
known <- find_lows(m) %>%
  as_tibble() %>%
  mutate(group = 1:n())

gen_candidates <- function(r, c, m) {
  tribble(
     ~row,  ~col,
        r, c + 1,
        r, c - 1,
    r + 1,     c,
    r - 1,     c
  ) %>%
    filter(row >= 1, row <= nrow(m),
           col >= 1, col <= ncol(m))
}

check_values <- function(points, m) {
  tmp <- points %>% mutate(value = map2_dbl(row, col, ~m[.x, .y])) 
  # print(tmp)
  tmp %>%
    filter(value < 9) %>%
    select(-value)
}

move_frontier <- function(state, m) {
  frontier <- state$frontier
  known <- state$known
  
  frontier %>%
    mutate(candidates = map2(row, col, gen_candidates, m) %>%
             map(check_values, m)) %>%
    select(group, candidates) %>%
    unnest(candidates) %>%
    bind_rows() %>%
    distinct() %>%
    setdiff(known)
}

# state is a list(known, frontier)
step <- function(state, m) {
  
  new_frontier <- move_frontier(state, m)
  # print(nrow(new_frontier))
  new_state <- list(
    known = bind_rows(state$known, state$frontier) %>% distinct(),
    frontier = new_frontier)
  
  if (nrow(new_frontier) == 0) {
    return(new_state)
  } else {
    step(new_state, m)
  }
}

find_basin <- function(df, m) {
  step(list(known = df, frontier = df), m)$known
}

part2 <- function(path) {
  input <- read_day9(path) 
  
  basins <- input %>%
    find_lows() %>%
    as_tibble() %>%
    mutate(group = 1:n(),
           basin = group) %>%
    group_by(basin) %>%
    nest() %>% ungroup() %>%
    mutate(all_in_basin = map(data, find_basin, m)) %>%
    select(all_in_basin) %>%
    unnest(all_in_basin)
  
  basins %>%
    count(group, sort = TRUE) %>%
    slice_head(n = 3) %>%
    pull(n) %>%
    prod()
}

# This takes a while
part2("data/9_1.txt") # 73690

