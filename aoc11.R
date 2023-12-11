#aoc11
require(dplyr)
require(purrr)

file <- "input11test.txt"
input <- readLines(file) %>% sapply(strsplit, NULL) %>% unlist() %>% matrix(ncol=length(readLines(file))) %>% t()

# inserts AFTEr n
insertEmptyRow <- function(x,n) {
  return(
    rbind(
      x[1:n, , drop = FALSE],rep("$",times=ncol(x)),x[(n+1):nrow(x), , drop=FALSE]
    )
  )
}

# inserts AFTER n
insertEmptyCol <- function(x,n) {
  return(
    cbind(
      x[,1:n , drop = FALSE],rep("$",times=nrow(x)),x[,(n+1):ncol(x), drop=FALSE]
    )
  )
}

distance <- function(y1,x1,y2,x2) {
  return((abs(y2-y1)+abs(x2-x1)))
}

empty_rows <- c()
for (r in 1:nrow(input)) {
  if (all(input[r,] == ".")) {
    empty_rows <- append(empty_rows,r)
  }
}

empty_cols <- c()
for (c in 1:ncol(input)) {
  if (all(input[,c] == ".")) {
    empty_cols <- append(empty_cols,c)
  }
}
#copy input with no added rows for part 2
input2 <- input

for (r in rev(empty_rows)) {
  input <- insertEmptyRow(input,r)
}

for (c in rev(empty_cols)) {
  input <- insertEmptyCol(input,c)
}

galaxies <- as.data.frame(which(input == "#", arr.ind=TRUE))

combinations <- expand.grid(galaxy1 = 1:nrow(galaxies), galaxy2 = 1:nrow(galaxies)) %>% subset(galaxy1 != galaxy2) %>%
  mutate(galaxy_comb = paste(pmin(galaxy1, galaxy2), pmax(galaxy1, galaxy2), sep = "-")) %>%
  distinct(galaxy_comb, .keep_all = TRUE) %>%
  select(-galaxy_comb)

distances <- pmap_dbl(list(combinations$galaxy1,combinations$galaxy2), ~distance(galaxies$row[.x[1]], galaxies$col[.x[1]], 
                                                                                 galaxies$row[.y[1]], galaxies$col[.y[1]]))
sum(distances)

# part 2
galaxies2 <- as.data.frame(which(input2 == "#", arr.ind=TRUE))

# visualisation
input2[empty_rows,] <- "$"
input2[,empty_cols] <- "$"

distance2 <- function(y1,x1,y2,x2) {
  empty_vastness_of_space_x <- sum(seq(from=x1,to=x2) %in% empty_cols)
  empty_vastness_of_space_y <- sum(seq(from=y1,to=y2) %in% empty_rows)
  dist <- abs(x1-x2)+abs(y1-y2)+((empty_vastness_of_space_x+empty_vastness_of_space_y)*999999)
  return(dist)
}

combinations <- expand.grid(galaxy1 = 1:nrow(galaxies2), galaxy2 = 1:nrow(galaxies2)) %>% subset(galaxy1 != galaxy2) %>%
  mutate(galaxy_comb = paste(pmin(galaxy1, galaxy2), pmax(galaxy1, galaxy2), sep = "-")) %>%
  distinct(galaxy_comb, .keep_all = TRUE) %>%
  select(-galaxy_comb)

distances2 <- pmap_dbl(list(combinations$galaxy1,combinations$galaxy2), ~distance2(galaxies2$row[.x[1]], galaxies2$col[.x[1]], 
                                                                                 galaxies2$row[.y[1]], galaxies2$col[.y[1]]))
combinations$distance <- distances2

sum(distances2)
