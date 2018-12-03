library(stringr)

#import data file

data_file_location <- "C:/code/AdventOfCode/Day2/data.txt"
IDs <- as.vector(unlist(read.table(data_file_location),use.names = FALSE))

#define some variables

alphabet <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
counts <- matrix(nrow=length(IDs),ncol=length(alphabet))



#loop a loop to count number of matches per string

i <- 1
a <- 1 


for (i in 1:length(IDs)){
  
    for (a in 1:length(alphabet)){
      counts[i,a] <- str_count(IDs[i],pattern=alphabet[a])
      print(i)
      print(a)
    }
}
  

i <-1
two <- 1:length(IDs)
three <- 1:length(IDs)

for (i in 1:length(IDs)){
  two[i] <- 2 %in% counts[i,]
  three[i] <- 3 %in% counts[i,]
  
}

checksums <- sum(two)*sum(three)

print(checksums)



