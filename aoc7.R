#aoc7
require(stringr)
require(dplyr)

cards <- c("A","K","Q","J","T","9","8","7","6","5","4","3","2")

# assign general hand value to each hand 
# 1 = 5x3
# 2 = 4x3
# 3 = fh
# 4 = 3x3
# 5 = two pair
# 6 = one pair
# 7 = hc
getHandValue <- function(x) {
  value <- 7
  for (card in cards) {
    if (str_count(x,card) == 5) {
      value <- 1
    }
    else if (str_count(x,card) == 4) {
      if (value > 2) value <- 2
    }
    else if (str_count(x,card) == 3) {
      # full house check
      for (c2 in cards) {
        if (str_count(x,c2) == 2) {
          if (value > 3) value <- 3
        }
      }
      if (value > 4) value <- 4
    }
    else if (str_count(x,card) == 2) {
      # two pair check
      for (c2 in cards) {
        if (str_count(x,c2) == 2 & card != c2) {
          if (value > 5) value <- 5
        }
      if (value > 6) value <- 6
      }
       
    }
  }
  return(value)
}

# order hand values with order()
input <- readLines("input7.txt")

hands <- data.frame(
  hand=character(),
  value=numeric(),
  bet=numeric(),
  stringsAsFactors = FALSE
)

for (row in input) {
  hand <- strsplit(row, " ")[[1]][1]
  bet <- strsplit(row, " ")[[1]][2]
  hands[nrow(hands)+1,] <- c(
    hand=hand,
    value=as.numeric(getHandValue(hand)),
    bet=bet
  )
}




hands[c("c1","c2","c3","c4","c5")] <- str_split_fixed(hands$hand, "", 5)
for (col in 4:8) {
  hands[,col] <- as.factor(hands[,col])
  hands[,col] <- factor(hands[,col],levels=c("A","K","Q","J","T","9","8","7","6","5","4","3","2"),ordered=TRUE)
}

hands <- hands[order(hands$value,hands$c1,hands$c2,hands$c3,hands$c4,hands$c5),]
sum <- 0
for (i in nrow(hands):1) {
  sum <- sum + (nrow(hands)-i+1)*as.numeric(hands[i,]$bet)
}
sum

# --- PT2
cards <- c("A","K","Q","T","9","8","7","6","5","4","3","2")

getHandValue2 <- function(x) {
  value <- 7
  jokers <- str_count(x,"J")
  for (card in cards) {
    if (str_count(x,card) + jokers == 5) {
      value <- 1
    }
    else if (str_count(x,card) + jokers == 4) {
      if (value > 2) value <- 2
    }
    else if (str_count(x,card) + jokers == 3) {
      # full house check
      for (c2 in cards) {
        if (str_count(x,c2) == 2 & card != c2) {
          if (value > 3) value <- 3
        }
      }
      if (value > 4) value <- 4
    }
    else if (str_count(x,card) + jokers == 2) {
      # two pair check
      for (c2 in cards) {
        if (str_count(x,c2) == 2 & card != c2) {
          if (value > 5) value <- 5
        }
        if (value > 6) value <- 6
      }
      
    }
  }
  return(value)
}

hands2 <- data.frame(
  hand=character(),
  value=numeric(),
  bet=numeric(),
  stringsAsFactors = FALSE
)

for (row in input) {
  hand <- strsplit(row, " ")[[1]][1]
  bet <- strsplit(row, " ")[[1]][2]
  hands2[nrow(hands2)+1,] <- c(
    hand=hand,
    value=as.numeric(getHandValue2(hand)),
    bet=bet
  )
}

hands2[c("c1","c2","c3","c4","c5")] <- str_split_fixed(hands2$hand, "", 5)

for (col in 4:8) {
  hands2[,col] <- factor(hands2[,col],levels=c("A","K","Q","T","9","8","7","6","5","4","3","2","J"),ordered=TRUE)
}

hands2 <- hands2[order(hands2$value,hands2$c1,hands2$c2,hands2$c3,hands2$c4,hands2$c5),]
hands2$blop <- seq(nrow(hands2),1)

sum <- 0
for (i in nrow(hands2):1) {
  sum <- sum + (nrow(hands2)-i+1)*as.numeric(hands2[i,]$bet)
}
sum
