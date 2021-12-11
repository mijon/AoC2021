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

part1("data/9_1.txt")

