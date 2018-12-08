library("dplyr")

#import data file

data_file_location <- "C:/code/AdventOfCode/Day6/data.txt"

coordinates <- read.table(data_file_location, sep = ",")

#part 1

range_x <- c(min(coordinates[1]),max(coordinates[1]))

range_y <- c(min(coordinates[2]),max(coordinates[2]))

distance <- function(a,b) {abs(a[1]-b[1])+abs(a[2]-b[2])}
n <- nrow(coordinates)
grid <- matrix(c(0,0,0),1)

for (i in range_x[1]:range_x[2]){
  for (j in range_y[1]:range_y[2]){
    d <- rep (0,n)
    for (k in 1:n){
      d[k] <- distance(c(i,j),unlist(coordinates[k,]))
    }
    closest <- which(d==min(d))
    if (length(closest)==1) {grid <- rbind(grid,c(i,j,closest))
    } else grid <- rbind(grid,c(i,j,0))
    print(c(i,j))
  }
}

border1 <- grid[grid[,1]==range_x[1],3]
border2 <- grid[grid[,1]==range_x[2],3]
border3 <- grid[grid[,2]==range_y[1],3]
border4 <- grid[grid[,2]==range_y[2],3]

b <- unique(c(border1,border2,border3,border4))

clean_grid <- grid[!(grid[,3] %in% b),]

unique_numbers <- unique(clean_grid[,3])


sum_vector <- sapply(unique_numbers,function(x) sum(clean_grid[,3]==x))

sum_vector[which(sum_vector==max(sum_vector))]

#part 2

dist_sum <- vector()

for (i in range_x[1]:range_x[2]){
  for (j in range_y[1]:range_y[2]){
    d <- rep (0,n)
    for (k in 1:n){
      d[k] <- distance(c(i,j),unlist(coordinates[k,]))
    }
    dist_sum <- c(dist_sum,sum(d))
  }
}

sum(dist_sum < 10000)
