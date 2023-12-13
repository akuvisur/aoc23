#aoc 12
require(gsubfn)
require(stringr)
require(purrr)

input <- readLines("input_data/input12.txt")

instructions <- vapply(strsplit(input," "),'[',FUN.VALUE = character(2))[2,] 
data <- vapply(strsplit(input," "),'[',FUN.VALUE = character(2))[1,]

# d = data, i = instructions
verifyRow <- function(d,i) {
  i <- strsplit(i,",")[[1]] %>% as.numeric()
  tag_lens <- str_extract_all(d, "#+")[[1]]
  return(length(tag_lens) == length(i) & all(tag_lens %>% sapply(nchar) == i))
} 

sum <- 0
for (r in 1:length(data)) {
  hashtag_count <- strsplit(instructions[r],",")[[1]] %>% as.numeric() %>% as.numeric() %>% sum()
  to_be_added <- hashtag_count - sum(nchar(str_extract_all(data[r],"#+")[[1]]))
  
  positions <- str_locate_all(data[r],"\\?")[[1]][,1]
  
  p <- combn(positions,to_be_added) %>% t()
  for (r2 in 1:nrow(p)) {
    d <- data[r]
    pp <- p[r2,]
    for (pos in pp) {
      substr(d,pos,pos) <- "#"  
    }
    if (verifyRow(d,instructions[r])) sum <- sum + 1
  }
}
sum

