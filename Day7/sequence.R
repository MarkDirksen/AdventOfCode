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

paste(sequence,collapse="") 

#part2

from_vector <- as.vector(unlist(input[2]))
to_vector <- as.vector(unlist(input[8]))

steps <- sort(unique(c(from_vector,to_vector)))
steps_fixed <- steps

seconds_left <- 60+1:26

work_schedule <- matrix(, nrow = 0, ncol = 6)

t <- 0

while(sum(seconds_left)>0){
  letters <- steps[which(!(steps %in% to_vector))]
  count <- 1
  row <- c(t,rep(0,5))
  
  for ( i in 1:5){
    if(t==0){
      if(length(letters)>=count) row[i+1]<-letters[count]
      seconds_left[steps_fixed==row[i+1]] <- seconds_left[steps_fixed==row[i+1]]-1
      count <- count+1
    }else{
    if((work_schedule[t,i+1]==0)|(seconds_left[match(work_schedule[t,i+1], steps_fixed)]==0)) {
      if(length(letters)>=count) row[i+1]<-letters[count]
      seconds_left[steps_fixed==row[i+1]] <- seconds_left[steps_fixed==row[i+1]]-1
      count <- count+1
    }
      if((work_schedule[t,i+1]!=0)&&(!(seconds_left[match(work_schedule[t,i+1], steps_fixed)]==0))){
        row[i+1] <- work_schedule[t,i+1]
        seconds_left[steps_fixed==row[i+1]] <- seconds_left[steps_fixed==row[i+1]] -1
      }
      }
  }
  
  work_schedule <- rbind(work_schedule,row)
  
  steps <- steps[!(steps%in%work_schedule[t+1,2:6])]
  
  finished_steps <- steps_fixed[seconds_left==0]
  
  to_vector <- to_vector[!(from_vector%in%finished_steps)]
  from_vector <- from_vector[!(from_vector%in%finished_steps)]
  
  t <- t+1
  print(seconds_left)
}

nrow(work_schedule)

#1106 too low
