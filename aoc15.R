#aoc 15
require(dplyr)

getHash <- function(val,x) {
  x <- val+utf8ToInt(x)
  x <- x*17
  x <- x %% 256
  return(x)
}

test <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

input <- readLines("input_data/input15.txt")

sum <- 0
for (part in strsplit(input,",")[[1]]) {
  val <- 0
  print(part)
  for (c in strsplit(part,NULL)[[1]]) {
    #print(c)
    val <- getHash(val,c)
  }
  sum <- sum + val
  print(val)
}
sum
