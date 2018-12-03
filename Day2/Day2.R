library(stringr)

#import data file

data_file_location <- "C:/code/AdventOfCode/Day2/data.txt"
IDs <- as.vector(unlist(read.table(data_file_location),use.names = FALSE))

#part 1

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


#part 2

compare_string <- matrix(0, length(IDs), length(IDs))


for (i in 1:length(IDs)){
  for (j in 1:length(IDs)){
    n <- 0
    for (k in 1:nchar(IDs[j])){
      if(i==j) break
      char1 <- substring(IDs[i],k,k)
      char2 <- substring(IDs[j],k,k)
      if (char1!=char2) n <- n+1
      if (n==2) break
      if (k==nchar(IDs[j])) compare_string[i,j] <- 1
    }
  }
}

compare_sum <- apply(compare_string,1,sum)

nmbrs <- IDs[which(compare_sum==1)]

str <- ""

for (k  in 1:nchar(IDs[1])){
  char1 <- substring(nmbrs[1],k,k)
  char2 <- substring(nmbrs[2],k,k)
  if (char1==char2) str <- paste(str, char1, sep="")
}

print(str)





