#aoc 10
require(dplyr)
require(stringr)

# change file in both locations
input <- readLines("input10.txt") %>% sapply(strsplit, NULL) %>% unlist() %>% matrix(nrow=length(readLines("input10.txt"))) %>% t()

# x = colnum
# y = rownum
dirmap <- data.frame(
  symbol=character(),
  x=numeric(),
  y=numeric(),
  allowed_x1=numeric(),
  allowed_y1=numeric(),
  allowed_x2=numeric(),
  allowed_y2=numeric()
)
allowed <- c("|","-","L","J","7","F","S")

for (y in 1:nrow(input)) {
  for (x in 1:ncol(input)) {
    if (!input[y,x] %in% allowed) {
      dirmap[nrow(dirmap)+1,] <- c(
        symbol=input[y,x],
        x=x,
        y=y,
        allowed_x1=NA,
        allowed_y1=NA,
        allowed_x2=NA,
        allowed_y2=NA
      )
    }
    else {
      if(input[y,x] == "S") {
        dirmap[nrow(dirmap)+1,] <- c(
          symbol="S",
          x=x,
          y=y,
          allowed_x1=NA,
          allowed_y1=NA,
          allowed_x2=NA,
          allowed_y2=NA
        )
      }
      if(input[y,x] == "|") {
        dirmap[nrow(dirmap)+1,] <- c(
          symbol=input[y,x],
          x=x,
          y=y,
          allowed_x1=x+0,
          allowed_y1=y+1,
          allowed_x2=x+0,
          allowed_y2=y-1
        ) 
      }
      else if(input[y,x] == "-") {
        dirmap[nrow(dirmap)+1,] <- c(
          symbol=input[y,x],
          x=x,
          y=y,
          allowed_x1=x+-1,
          allowed_y1=y+0,
          allowed_x2=x+1,
          allowed_y2=y+0
        ) 
      }
      else if(input[y,x] == "L") {
        dirmap[nrow(dirmap)+1,] <- c(
          symbol=input[y,x],
          x=x,
          y=y,
          allowed_x1=x+0,
          allowed_y1=y-1,
          allowed_x2=x+1,
          allowed_y2=y+0
        ) 
      }
      else if(input[y,x] == "J") {
        dirmap[nrow(dirmap)+1,] <- c(
          symbol=input[y,x],
          x=x,
          y=y,
          allowed_x1=x+-1,
          allowed_y1=y+0,
          allowed_x2=x+0,
          allowed_y2=y-1
        )  
      }
      else if(input[y,x] == "7") {
        dirmap[nrow(dirmap)+1,] <- c(
          symbol=input[y,x],
          x=x,
          y=y,
          allowed_x1=x+0,
          allowed_y1=y+1,
          allowed_x2=x-1,
          allowed_y2=y+0
        )
      }
      else if(input[y,x] == "F") {
        dirmap[nrow(dirmap)+1,] <- c(
          symbol=input[y,x],
          x=x,
          y=y,
          allowed_x1=x+1,
          allowed_y1=y+0,
          allowed_x2=x+0,
          allowed_y2=y+1
        )
      }
    }
  }
}

start <- list(which(input == "S", arr.ind=TRUE)[1],which(input == "S", arr.ind=TRUE)[2]) %>% unlist()
step1 <- subset(dirmap, allowed_x1 == start[2] & allowed_y1 == start[1])
step2 <- subset(dirmap, allowed_x2 == start[2] & allowed_y2 == start[1])

steps <- c()
stepcount <- 0
while(TRUE) {
  stepcount <- stepcount + 1
  print(paste("iter ",stepcount))
  
  step_check1 <- c(step1$x,step1$y)
  if (list(step_check1) %in% steps) {
    print("found?!?")
    break
  }
  steps <- append(steps, list(step_check1))
  
  step_check2 <- c(step2$x,step2$y)
  if (list(step_check2) %in% steps) {
    print("found?!?")
    break
  }
  
  steps <- append(steps, list(step_check2))
  
  # get next steps
  st1_ss1 <- subset(dirmap, allowed_x1 == step1$x & allowed_y1 == step1$y)
  st1_ss2 <- subset(dirmap, allowed_x2 == step1$x & allowed_y2 == step1$y)
  step1 <- rbind(st1_ss1,st1_ss2)
  # remove iterated steps
  row_to_remove <- NA
  for (step in steps) {
    for (i in 1:nrow(step1)) {
      print(paste(step1$x[i],step1$y[i],sep=":"))
      if (step1$x[i] == step[1] & step1$y[i] == step[2]) {
        print("hep!")
        row_to_remove <- i
      }
    }
  }
  if (!is.na(row_to_remove)) {
    step1 <- step1[-row_to_remove,]
  }
  
  st2_ss1 <- subset(dirmap, allowed_x1 == step2$x & allowed_y1 == step2$y)
  st2_ss2 <- subset(dirmap, allowed_x2 == step2$x & allowed_y2 == step2$y)
  step2 <- rbind(st2_ss1,st2_ss2)
  # remove iterated steps
  # remove iterated steps
  row_to_remove <- NA
  for (step in steps) {
    for (i in 1:nrow(step2)) {
      print(paste(step2$x[i],step2$y[i],sep=":"))
      if (step2$x[i] == step[1] & step2$y[i] == step[2]) {
        print("hep!")
        row_to_remove <- i
      }
    }
  }
  if (!is.na(row_to_remove)) {
    step2 <- step2[-row_to_remove,]
  }
}
