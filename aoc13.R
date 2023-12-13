#aoc13
require(dplyr)

file <- "input_data/input13.txt"
input <- readLines(file)

getSplitCol <- function(f) {
  for (r in 1:(ncol(f)-1)) {
    #print(r)
    max_to_consider <- min(ncol(f)-r,r)
    #max_to_consider <- 2
    start <- max(1,r-max_to_consider+1)
    end <- min(length(f[1,]),r+max_to_consider)
    
    left_side <- f[,start:(r)]
    right_side <- f[,(r+1):end]
    if (is.null(ncol(left_side)) || is.null(ncol(right_side))) {
      if (all(left_side==right_side)) {
        return(r)
      }
    }
    else {
      right_side_rev <- right_side[,ncol(right_side):1]
      if (all(left_side == right_side_rev)) {
        return(r)
      }
    }
  }
}

iter <- which(nzchar(input) == FALSE)
sumr <- 0
sumc <- 0
for (i in 1:(length(iter)+1)) {
  start <- max(0,iter[i-1]+1)
  end <- iter[i]-1
  if (is.na(end)) end <- length(input)
  grid <- input[start:end] %>% sapply(strsplit, NULL) %>% unlist() %>% matrix(nrow=nchar(input[start:end][1])) %>% t()
  
  c <- getSplitCol(grid)
  r <- getSplitCol(t(grid))
  
  if (!is.null(r)) sumr <- sumr + r
  if (!is.null(c)) sumc <- sumc + c 
}

sumc+(100*sumr)




