#aoc5
require(stringr)
require(dplyr)
# no longer works for part 1
input5 <- readLines("input5.txt")

maps <- data.frame(
  source=character(),
  target=character(),
  source_range_s=character(),
  source_range_e=character(),
  target_range_s=character(),
  target_range_e=character(),
  stringsAsFactors = FALSE
)

# create maps from destination(source) to target and range of values
for (row in 2:length(input5)) {
  currow <- input5[row]
  if (nchar(currow) < 1) next
  if (!is.na(str_extract(currow,"[a-z]+-to"))) {
    source <- str_extract(currow,"[a-z]+-to") %>% unlist() %>% as.character()
    source <- strsplit(source,"-")[[1]][1]
    target <- str_extract(currow,"to-[a-z]+") %>% unlist() %>% as.character()
    target <- strsplit(target, "-")[[1]][2]
    next
  }
  values <- strsplit(currow, " ") %>% unlist() %>% as.numeric()
  #target_range <- paste(values[1],(values[1]+values[3]),sep = ",")
  #source_range <- paste(values[2],values[2]+values[3],sep = ",")
  maps <- rbind(maps,c(source=source,target=target,source_range_s=values[1],source_range_e=values[1]+values[3],
                       target_range_s=values[2],target_range_e=values[2]+values[3]))
}
names(maps) <- c("source","target","source_range_s","source_range_e", "target_range_s", "target_range_e")
for (c in 3:6) {
  maps[,c] <- as.numeric(maps[,c])
}
maps <- maps[order(maps$target,maps$source_range_s,maps$source_range_e,maps$target_range_s),]

# n = to search
# s = source
# t = target
mapToTarget <- function(n,s,t) {
  map_subset <- subset(maps,source==s & target==t)
  for (r in 1:nrow(map_subset)) {
    row <- map_subset[r,]
    source_range <- unlist(str_extract_all(row$source_range,"\\d+"))
    target_range <- unlist(str_extract_all(row$target_range,"\\d+"))
    if (between(n,source_range[1],source_range[2])) {
      # value from target range is a naive calculation because of the order of sequences
      dist <- as.numeric(target_range[1])+(as.numeric(n)-as.numeric(source_range[1]))
      return(dist)
    }
  }
  # if not found return original value
  return(n)
  
}

targets <- str_extract_all(input5, "[a-z]+-to-[a-z]+") %>% unlist()
seeds <- str_extract_all(input5[1],"\\d+") %>% unlist() 

min_dist <- Inf
for (seed in seeds) {
  dist <- seed
  for (st in targets) {
    s <- strsplit(st,"-to-")[[1]][1]
    t <- strsplit(st,"-to-")[[1]][2]
    dist <- mapToTarget(as.numeric(dist),s,t)
  }
  min_dist <<- min(dist,min_dist)
}

#---- PART 2
seeds2 <- str_extract_all(input5[1],"\\d+ \\d+") %>% unlist() 

head(maps)

# map ranges are always |--*--*----*--|
seeds_ranges <- data.frame(
  start=numeric(),end=numeric()
)
for (seed in seeds2) {
  start <- strsplit(seed, " ")[[1]][1]
  end <- strsplit(seed, " ")[[1]][2]
  seeds_ranges[nrow(seeds_ranges)+1,] <- c(start=start,end=end) 
}
seeds_ranges$start <- as.numeric(seeds_ranges$start)
seeds_ranges$end <- as.numeric(seeds_ranges$end)

seeds_ranges <- seeds_ranges[order(seeds_ranges$start),]
seeds_ranges

# s = start var
# t = target var
# s = search dataframe of starting and ending points
mapToTargetRange <- function(s,t,search) {
  map_subset <- subset(maps,source==s & target==t)
  targets <- data.frame(
    start=numeric(),
    end=numeric()
  )
  target_start <- min(map_subset$source_range_s)
  target_end <- max(map_subset$source_range_e)
  # below target
  if (max(search$end) < target_start) {
    return(search)
  }
  else if (min(search$start) > target_end) {
    return(search)
  }
  # check each row in the "soil" ranges
  for (r in 1:nrow(map_subset)) {
    row <- map_subset[r,]
    print(row)
    # and for each of them search the "seed" ranges to find matches
    for (r2 in 1:nrow(search)) {
      row2 <- search[r2,]
      
    }
  }
  # if not found return original value
  return(search)
}

min_dist <- Inf
mapToTargetRange("seed","soil",seeds_ranges)

