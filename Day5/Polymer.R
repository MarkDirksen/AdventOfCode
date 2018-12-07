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

#part 2

alphabet_lower <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")
alphabet_upper <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
polymer_result <- rep(0,26)

for (k in 1:length(alphabet_lower)){
  
  stripped <- polymer[which(!(polymer==alphabet_lower[k]|polymer==alphabet_upper[k]))]
  
  i <-1
  
  while (TRUE){
    letter1 <- stripped[i]
    letter2 <- stripped[i+1]
    if(grepl("^[[:upper:]]+$", (letter1))) { test <- tolower(letter1)==letter2
    } else {test <- toupper(letter1)==letter2}
    if (test==TRUE){
      if(i==1) stripped <- stripped[(i+2):length(stripped)]
      if(((i+1)!=length(stripped))&&(i!=1)) stripped <- stripped[c(1:(i-1),(i+2):length(stripped))]
      if((i+1)==length(stripped)) stripped <- stripped[1:(i-1)]
      if(i!=1) i <- i-1
    } else {i <- i+1}
    print(c(k,length(stripped)))
    if (i==length(stripped)) break
  }
  
  polymer_result[k] <- length(stripped)
  
}

c(alphabet_lower[which(polymer_result==min(polymer_result))],min(polymer_result))









