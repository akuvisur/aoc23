#aoc 10
require(dplyr)

getDirForChar <- function(x) {
  if (x == "|") {
    list(UP,DOWN)
  }
  else if (x == "-") {
    list(LEFT,RIGHT)
  }
  else if (x == "L") {
    list(UP,RIGHT)
  }
  else if (x == "J") {
    list(UP,LEFT)
  }
  else if (x == "7") {
    list(LEFT,DOWN)
  }
  else if (x == "F") {
    list(DOWN,RIGHT)
  }
}

file <- "input10.txt"
input <- readLines(file) %>% sapply(strsplit, NULL) %>% unlist() %>% matrix(ncol=length(readLines(file))) %>% t()

DOWN <- c(1,0)
UP <- c(-1,0) 
LEFT <- c(0,-1)
RIGHT <- c(0,1)

start <- list(which(input == "S", arr.ind=TRUE)[1],which(input == "S", arr.ind=TRUE)[2]) %>% unlist()

#UP
upcoords <- input[start[1]+UP[1],start[2]+UP[2]]
downcoords <- input[start[1]+DOWN[1],start[2]+DOWN[2]]
leftcoords <- input[start[1]+LEFT[1],start[2]+LEFT[2]]
rightcoords <- input[start[1]+RIGHT[1],start[2]+RIGHT[2]]

# stupid method for starting step
if (upcoords %in% c("|","7","F")) {
  step <- c(start[1]+UP[1],start[2]+UP[2])
}
if (downcoords %in% c("|","L","J")) {
  step <- c(start[1]+DOWN[1],start[2]+DOWN[2])
}
if (leftcoords %in% c("-","F","L")) {
  step <- c(start[1]+LEFT[1],start[2]+LEFT[2])
}
if (rightcoords %in% c("-","J","7")) {
  step <- c(start[1]+RIGHT[1],start[2]+RIGHT[2])
}
# already took starting step
num_steps <- 1
prev_step <- start

route <- data.frame(
  x=numeric(),
  y=numeric(),
  symbol=character(),
  dir=numeric()
)

route[nrow(route)+1,] <- c(
  x=start[2],
  y=start[1],
  symbol=input[start[1],start[2]],
  dir=0
)

prev_step <- start
while(TRUE) {
  num_steps <- num_steps + 1
  if (input[step[1],step[2]] == "S") {
    break
  }
    
  route[nrow(route)+1,] <- c(
    x=step[2],
    y=step[1],
    symbol=input[step[1],step[2]],
    dir=prev_step[1]-step[1]
  )

  a <- getDirForChar(input[step[1],step[2]])
  s1 <- step + a[[1]]
  s2 <- step + a[[2]]
  if (all(s1 == prev_step)) {
    prev_step <- step
    step <- s2
  }
  else if (all(s2 == prev_step)) {
    prev_step <- step
    step <- s1
  }
}
as.integer(num_steps/2)

# part 2
route$x <- as.numeric(route$x)
route$y <- as.numeric(route$y)

num_cells <- 0
# input2 is for visualising
input2 <- input
for (cur_y in 1:nrow(input)) {
  for (cur_x in 1:ncol(input)) {
    walls_r <- 0
    walls_l <- 0
    check_route <- subset(route, y == cur_y & x == cur_x) 
    if (nrow(check_route) == 1) {
      input2[cur_y,cur_x] <- 1
      next
    }
    
    ss_right <- subset(route, y == cur_y & x > cur_x & symbol %in% c("|","F","L","7","J","-"))
    ss_left <- subset(route, y == cur_y & x < cur_x & symbol %in% c("|","F","L","7","J","-"))
    
    if (nrow(ss_right) > 0) {
      for (r in 1:nrow(ss_right)) {
        if (ss_right$x[r] == 1) next
        chk_symbol <- -1
        while(TRUE) {
          if (input[ss_right$y[r],ss_right$x[r]+chk_symbol] == "-") {
            chk_symbol <- chk_symbol-1
          }
          else {
            break
          }
        }
        if (ss_right$symbol[r] == "J" & input[ss_right$y[r],ss_right$x[r]+chk_symbol] == "F") walls_r <- walls_r + 1
        if (ss_right$symbol[r] == "7" & input[ss_right$y[r],ss_right$x[r]+chk_symbol] == "L") walls_r <- walls_r + 1
        
      }
    }
    if (nrow(ss_left) > 0) {
      for (r in 1:nrow(ss_left)) {
        if (ss_left$x[r] == 1) next
        chk_symbol <- -1
        while(TRUE) {
          if (input[ss_left$y[r],ss_left$x[r]+chk_symbol] == "-") {
            chk_symbol <- chk_symbol-1
          }
          else {
            break
          }
        }
        if (ss_left$symbol[r] == "7" & input[ss_left$y[r],ss_left$x[r]+chk_symbol] == "L") walls_l <- walls_l + 1
        if (ss_left$symbol[r] == "J" & input[ss_left$y[r],ss_left$x[r]+chk_symbol] == "F") walls_l <- walls_l + 1
      }
    }
    ss_up <- subset(route, y > cur_y & x == cur_x)
    ss_down <- subset(route, y < cur_y & x == cur_x)
    
    if (nrow(ss_right)*nrow(ss_left)*nrow(ss_up)*nrow(ss_down) == 0) {
      input2[cur_y,cur_x] <- 0
      next
    }
    
    ss_right <- subset(subset(route, y == cur_y & x > cur_x & symbol %in% c("|")))
    ss_left <- subset(subset(route, y == cur_y & x < cur_x & symbol %in% c("|")))
    
    if ((nrow(ss_right)+walls_r) %% 2 == 1) {
      input2[cur_y,cur_x] <- 2
      num_cells <- num_cells + 1
    }
    else if ((nrow(ss_left)+walls_l) %% 2 == 1) {
      input2[cur_y,cur_x] <- 2
      num_cells <- num_cells + 1
    }
  }
}
num_cells
