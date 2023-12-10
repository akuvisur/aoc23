#aoc9
require(dplyr)

interpolate <- function(x) {
  steps <- c()
  while(!all(x == 0)) {
    steps <- append(steps,list(x))
    x <- diff(x)
  }
  steps <- append(steps,list(x))
  steps
}

extrapolate <- function(x) {
  val <- 0
  for (i in length(x):1) {
    val <- val + tail(x[[i]],.1)
  }
  val
}

vsplit <- function(v, n) {
  l = length(v)
  r = l/n
  return(lapply(1:n, function(i) {
    s = max(1, round(r*(i-1))+1)
    e = min(l, round(r*i))
    return(v[s:e])
  }))
}

input <- readLines("input9.txt")
d <- input %>% strsplit(" ") %>% unlist() %>% as.numeric() %>% vsplit(length(input)) 
res <- c()
for (row in d) {
  res <- append(res,extrapolate(interpolate(row)))
}
sum(res)

# part 2

res2 <- c()
for (row in d) {
  res2 <- append(res2,extrapolate(interpolate(rev(row))))
}
sum(res2)
