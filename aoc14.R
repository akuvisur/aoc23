require(dplyr)

file <- "input_data/input14test.txt"

input <- readLines(file) %>% sapply(strsplit, NULL) %>% unlist() %>% matrix(ncol=length(readLines(file))) %>% t()

sum <- 0
grid <- input

new_grid <- matrix(nrow=nrow(input),ncol=ncol(input),".")

for (c in 1:ncol(input)) {
  max_val <- nrow(input)
  rock_count <- 0
  for (r in 1:nrow(input)) {
    cell <- input[r,c]
    if (cell == "O") {
      #print(paste("cell at ",r,c, " added ",(max_val-rock_count), " max_val = ", max_val, " rock count=",rock_count))
      sum <- sum+(max_val-rock_count)
      rock_count <- rock_count + 1
      new_grid[(nrow(input)-max_val+rock_count),c] <- "O"
    }
    else if (cell == "#") {
      new_grid[r,c] <- "#"
      max_val <- nrow(input)-r
      rock_count <- 0
    }
    
  }
}
sum

# part 2 tests
# rotate c-clockwise(?)
rotate <- function(x) t(apply(x, 2, rev))
#apply(new_grid,2,rev)
rotate(new_grid)

grids <- list()
grids <- append(grids, new_grid)

for (i in 1:4000000000) {
  if (i %% 50000 == 0) print(i)
  
  grid <- rotate(new_grid)
  
  if (i %% 4 == 0) {
    for (g in grids) {
      if (all(grid == g)) {
        print("duplicate!")
        break
      }
    }
  }
  else {
    grids[[length(grids)+1]] <- grid
  }
  new_grid <- matrix(nrow=nrow(input),ncol=ncol(input),".")
  for (c in 1:ncol(grid)) {
    max_val <- nrow(grid)
    rock_count <- 0
    for (r in 1:nrow(grid)) {
      cell <- grid[r,c]
      if (cell == "O") {
        #print(paste("cell at ",r,c, " added ",(max_val-rock_count), " max_val = ", max_val, " rock count=",rock_count))
        #sum <- sum+(max_val-rock_count)
        rock_count <- rock_count + 1
        new_grid[(nrow(grid)-max_val+rock_count),c] <- "O"
      }
      else if (cell == "#") {
        new_grid[r,c] <- "#"
        max_val <- nrow(grid)-r
        rock_count <- 0
      }
      
    }
  }
}
new_grid

input2 <- new_grid
sum <- 0
for (c in 1:ncol(input2)) {
  max_val <- nrow(input2)
  rock_count <- 0
  for (r in 1:nrow(input2)) {
    cell <- input2[r,c]
    if (cell == "O") {
      #print(paste("cell at ",r,c, " added ",(max_val-rock_count), " max_val = ", max_val, " rock count=",rock_count))
      sum <- sum+(max_val-rock_count)
      rock_count <- rock_count + 1
      new_grid[(nrow(input2)-max_val+rock_count),c] <- "O"
    }
    else if (cell == "#") {
      new_grid[r,c] <- "#"
      max_val <- nrow(input2)-r
      rock_count <- 0
    }
    
  }
}
sum
