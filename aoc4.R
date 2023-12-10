require(stringr)
require(dplyr)
inputs <- read.csv("input4.txt",header=FALSE)

wins <<- data.frame(
  row=numeric(),
  wins=numeric(),
  stringsAsFactors = FALSE
)
mapWins <- function() {
  for (row in seq(1,nrow(inputs))) {
    
    winning_numbers <- c()
    picked_numbers <- c()
    
    winsplit <- inputs[row,] %>% substr(1,str_locate(inputs[row,],"\\|")-1) %>% str_trim() %>% strsplit(" ")
    winsplit <- winsplit[[1]]
    
    for (i in seq(1,length(winsplit))) {
      if (!is.na(as.integer(winsplit[i]))) {
        winning_numbers <- append(winning_numbers,as.integer(winsplit[i]))
      }
    }
    picksplit <- inputs[row,] %>% substr(str_locate(inputs[row,],"\\|")+1,nchar(inputs[row,])) %>% str_trim() %>% strsplit(" ")
    picksplit <- picksplit[[1]]
    
    for (i in seq(1,length(picksplit))) {
      if (!is.na(as.integer(picksplit[i]))) {
        picked_numbers <- append(picked_numbers,as.integer(picksplit[i]))
      }
    }
    
    wins[nrow(wins)+1,] <<- c(
      row=row,wins=sum(picked_numbers %in% winning_numbers)
    )
      
  }
}
mapWins()

hits <- wins
hits$hits <- 0
getCardCount <- function(start,end) {
  if (start > nrow(inputs)) return()
  
  end <- min(end,nrow(inputs))
  
  for (row in seq(start,end)) {
    cards <<- cards + 1
    if (wins[wins$row == row,]$wins > 0) {
      hits[hits$row == row,]$hits <<- hits[hits$row == row,]$hits+1
      getCardCount(row+1,row+wins[wins$row == row,]$wins)
    }  
  }
  
}
cards <<- 0
getCardCount(1,nrow(inputs))

getPoints <- function(s,e) {
  
  for (row in seq(s,max(e,206))) {
    cards <<- cards + 1
    winning_numbers <- c()
    picked_numbers <- c()
    
    winsplit <- inputs[row,] %>% substr(1,str_locate(inputs[row,],"\\|")-1) %>% str_trim() %>% strsplit(" ")
    winsplit <- winsplit[[1]]
    
    for (i in seq(1,length(winsplit))) {
      if (!is.na(as.integer(winsplit[i]))) {
        winning_numbers <- append(winning_numbers,as.integer(winsplit[i]))
      }
    }
    picksplit <- inputs[row,] %>% substr(str_locate(inputs[row,],"\\|")+1,nchar(inputs[row,])) %>% str_trim() %>% strsplit(" ")
    picksplit <- picksplit[[1]]
    
    for (i in seq(1,length(picksplit))) {
      if (!is.na(as.integer(picksplit[i]))) {
        picked_numbers <- append(picked_numbers,as.integer(picksplit[i]))
      }
    }
    if (any(picked_numbers %in% winning_numbers)) {
      if (!row %in% wins$row) {
        wins[nrow(wins)+1,] <<- c(
          row=row,wins=sum(picked_numbers %in% winning_numbers)
        )
      }
      getPoints(row+1,row+sum(picked_numbers %in% winning_numbers))
    }
    else {
      #print(paste("no wins for",row))
    }
  }
  return()
}

total_points <- 0
# part 1
p <- getPoints(1,206)

# part 2
cards <<- 0
mapWins()
getCardCount(1,206)


factorial <- function(n, acc = 1) {
  if (n <= 1) acc
  else "factorial(n - 1, acc * n)"
}

# netcode
require(dplyr)
input <- readLines("input4.txt")

input

winning <- input %>%
  stringr::str_extract("\\:.*\\|") %>%
  stringr::str_extract_all("\\d+") %>%
  lapply(as.integer)

numbers <- input %>%
  stringr::str_extract("\\|.*") %>%
  stringr::str_extract_all("\\d+") %>%
  lapply(as.integer)

cards <- rep(1, length(input))

for (i in seq_along(cards)) {
  if (i == 0) next
  n <- cards[i]
  
  wins <- sum(winning[[i]] %in% numbers[[i]])
  
  cards[seq_len(wins) + i] <- cards[seq_len(wins) + i] + n
}

sum(cards)
