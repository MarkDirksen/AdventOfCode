library(readr)

data_file_location <- "C:/code/AdventOfCode/Day5/data.txt"

polymer_input <- read_file(data_file_location)

#polymer_input<-"VvfFUuCcvVLlZzJjPpHSshgSsGjLoOlJrRuUQCchHiIRra"

#part 1

polymer <- sapply(1:nchar(polymer_input),function(i) substring(polymer_input, i, i))

i <- 1

while (TRUE){
  letter1 <- polymer[i]
  letter2 <- polymer[i+1]
  if(grepl("^[[:upper:]]+$", (letter1))) { test <- tolower(letter1)==letter2
  } else {test <- toupper(letter1)==letter2}
  if (test==TRUE){
    if(i==1) polymer <- polymer[(i+2):length(polymer)]
    if(((i+1)!=length(polymer))&&(i!=1)) polymer <- polymer[c(1:(i-1),(i+2):length(polymer))]
    if((i+1)==length(polymer)) polymer <- polymer[1:(i-1)]
    if(i!=1) i <- i-1
  } else {i <- i+1}
  print(length(polymer))
  if (i==length(polymer)) break
}

result <- paste(polymer,collapse="") 

print(result)

nchar(result)

