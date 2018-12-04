n <- 1100
data_file_location <- "C:/code/AdventOfCode/Day3/data.txt"
cloth_list <- read.table(data_file_location,header=FALSE,sep=" ",stringsAsFactors = FALSE)
cloth_list <- cloth_list[,c(3,4)]

cloth_list[,1] <- sub(":","",cloth_list[,1])
c1 <- sub(".*,","",cloth_list[,1])
c2 <- sub(",.*","",cloth_list[,1])
c3 <- sub(".*x","",cloth_list[,2])
c4 <- sub("x.*","",cloth_list[,2])

cloth_list <- matrix(as.numeric(c(c2,c1,c4,c3)),ncol=4)

max_width <- max(apply(cloth_list,1,function(x) x[[1]]+x[[3]]))
max_height <- max(apply(cloth_list,1,function(x) x[[2]]+x[[4]]))

print(c(max_width,max_height))

#part1

Grid <- matrix(0, n, n)

for (i in 1:dim(cloth_list)[1]){
  vec <- cloth_list[i,]
  for(j in (vec[1]+1):(vec[1]+vec[3])){
    for(k in (vec[2]+1):(vec[2]+vec[4])){
      Grid[j,k] <- Grid[j,k]+1
    }
  }
}

# Loop over Grid
for(row in 1:nrow(Grid)) {
  for(col in 1:ncol(Grid)) {
    if(Grid[row, col]<=1) Grid[row, col] <- 0
    else Grid[row, col] <- 1
  }
}

sum(Grid)


image(Grid)

#part2

overlap_YN <- rep(1,dim(cloth_list)[1])

for (i in 1:dim(cloth_list)[1]){
  vec <- cloth_list[i,]
  for(j in (vec[1]+1):(vec[1]+vec[3])){
    for(k in (vec[2]+1):(vec[2]+vec[4])){
      if(Grid[j,k]==1) {
        overlap_YN[i] <- 0
        break}
    }
  }
}

which(overlap_YN==1)