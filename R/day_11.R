library(tidyverse)

read_day11 <- function(path) {
  output <- scan(path, what = "character") %>%
    map(~str_split(.x, "") %>%
          pluck(1) %>%
          map_dbl(as.numeric) %>%
          as.matrix) %>%
    reduce(cbind) %>%
    t()
  
  attr(output, "flashes") <- 0
  output
}


step_octopuses <- function(grid) {
  grid %>%
    up_by_one() %>%
    flash_cascade() %>%
    reset_octopuses()
}


up_by_one <- function(grid) {
  grid + 1
}

flash_cascade <- function(grid) {
  to_flash <- which(grid > 9, arr.ind = TRUE)
  
  if (length(to_flash) == 0) {
    return(grid)
  } else {
    adjacents <- remove_to_flash(get_adjacents(to_flash, grid),
                                 to_flash) %>%
      as.matrix()
    grid[to_flash] <- -Inf
    for (i in seq_len(nrow(adjacents))) {
      grid[adjacents[i,1], adjacents[i,2]] <-
        grid[adjacents[i,1], adjacents[i,2]] + 1
    }
    flash_cascade(grid)
  }
}

reset_octopuses <- function(grid) {
  n_flashed <- sum(is.infinite(grid))
  grid[which(is.infinite(grid), arr.ind = TRUE)] <- 0
  attr(grid, "flashes") <- attr(grid, "flashes") + n_flashed
  grid
}

get_adjacents <- function(points, grid) {
  reduce(map(seq_len(nrow(points)),
             ~generate_adjacents(points[.x,], grid)),
         rbind) 
}

generate_adjacents <- function(point, grid) {
  max_row <- nrow(grid)
  max_col <- ncol(grid)
  
  i <- point[[1]]
  j <- point[[2]]
  
  tibble(
    row = c(i-1, i-1, i-1, i, i, i+1, i+1, i+1),
    col = c(j-1, j, j+1, j-1, j+1, j-1, j, j+1)) %>%
    filter(row >= 1, col >= 1,
           row <= max_row, col <= max_col)
}

remove_to_flash <- function(adjacents_df, to_flash_mat) {
  to_flash_df <- as_tibble(to_flash_mat)
  
  add_test <- function(dfr) {
    dfr %>% mutate(tester = paste(row, col))
  }
  
  add_test(adjacents_df) %>%
    filter(!tester %in% add_test(to_flash_df)$tester) %>%
    select(-tester)
}


part1 <- function(path, n) {
  output <- read_day11(path)
  for (i in 1:n) {
    output <- step_octopuses(output)
  } 
  output
}

part1("data/11.txt", 100) # 1719


# ---- part 2 ----
part2 <- function(path) {
  output <- read_day11(path)
  
  check_part2 <- function(grid, n = 0) {
    if (all(grid == 0)) {
      return(n)
    } else {
      check_part2(step_octopuses(grid), n + 1)
    }
  }
  
  check_part2(output)
}

part2("data/11.txt") # 232