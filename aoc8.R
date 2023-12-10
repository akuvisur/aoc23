#part 1
require(stringr)
require(dplyr)

input <- readLines("input8.txt")

instructions <- input[1]
split <- str_extract_all(input[3:length(input)], "[A-Z]+")

dirmap <- data.frame(
  from=character(),
  left=character(),
  right=character(),
stringsAsFactors = FALSE)

for (i in 1:length(split)) {
  dirmap[nrow(dirmap)+1,] <- c(
    from=split[[i]][1],
    left=split[[i]][2],
    right=split[[i]][3]
  )
}

i <- 1
numsteps <- 0
step <- "AAA"
while (i > 0) {
  numsteps <- numsteps + 1
  dir <- substr(instructions,i,i)
  #print(paste("from",step,"dir",dir,sep = " "))
  if (dir == "L") {
    step <- subset(dirmap, from==step)$left
  }
  else {
    step <- subset(dirmap, from==step)$right
  } 
  if (i == nchar(instructions)) {
    i <- 1
  }
  else i <- i + 1
  if (step == "ZZZ") break
}
numsteps

### part 2

steps <- dirmap[substr(dirmap$from,3,3)=="A",]$from
goals <- dirmap[substr(dirmap$from,3,3)=="Z",]$from
i <- 1
step_counts <- c()
goals_ordered <- c()
for (step in steps) {
  numsteps <- 0
  while (i > 0) {
    numsteps <- numsteps + 1
    dir <- substr(instructions,i,i)
    if (dir == "L") {
      step <- subset(dirmap, from==step)$left
    }
    else {
      step <- subset(dirmap, from==step)$right
    } 
    
    if (i == nchar(instructions)) {
      i <- 0
    }
    i <- i + 1
    
    if (step %in% goals) {
      print(step)
      goals_ordered <- append(goals_ordered,step)
      step_counts <- append(step_counts, numsteps)
      break
    }
    #if (step == "ZZZ") break
  }
}

goal_counts <- c()
for (step in goals_ordered) {
  goal <- step
  print(step)
  numsteps <- 0
  while (i > 0) {
    numsteps <- numsteps + 1
    dir <- substr(instructions,i,i)
    if (dir == "L") {
      step <- subset(dirmap, from==step)$left
    }
    else {
      step <- subset(dirmap, from==step)$right
    } 
    
    if (i == nchar(instructions)) {
      i <- 0
    }
    i <- i + 1
    
    if (step == goal) {
      print(step)
      goal_counts <- append(goal_counts, numsteps)
      break
    }
    #if (step == "ZZZ") break
  }
}

cycles <- data.frame(first=step_counts,cycle=goal_counts)

require(gmp)
goal_counts <- as.bigz(goal_counts)
lcm_result <- Reduce(lcm.bigz, goal_counts)
lcm_result

require(pracma)
require(purrr)

Lcm(step_counts,step_counts+goal_counts)

smallest <- function(x) {
  combn(x, 2, simplify = F) %>% 
    map(~gcd(.[1], .[2])) %>% 
    Reduce(function(x,y) gcd(x, y),.)
}

smallest(goal_counts)

# lcm for goal_counts
a <- c()
for (i in 1:6) {
  for (j in 2:6) {
    if (i == j) next
    a <- append(a, Lcm(goal_counts[i],goal_counts[j]))
  }
}
Lcm(a)
