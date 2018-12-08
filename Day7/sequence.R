#import data file

data_file_location <- "C:/code/AdventOfCode/Day7/data.txt"

input <- read.table(data_file_location, sep = " ",stringsAsFactors = FALSE)


#part 1

from_vector <- as.vector(unlist(input[2]))
to_vector <- as.vector(unlist(input[8]))

steps <- sort(unique(c(from_vector,to_vector)))

sequence <- vector()

while(length(steps)>0){
  letter <- steps[which(!(steps %in% to_vector))][1]
  sequence <- c(sequence,letter)
  steps <- steps[steps!=letter]
  to_vector <- to_vector[from_vector!=letter]
  from_vector <- from_vector[from_vector!=letter]
}