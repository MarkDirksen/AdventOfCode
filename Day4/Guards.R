#import and clean data

data_file_location <- "C:/code/AdventOfCode/Day4/data.txt"
guard_list <- read.table(data_file_location,header=FALSE,sep="|",stringsAsFactors = FALSE, comment.char = "~")


extract1 <- function(i) regmatches(guard_list[i,],regexpr("\\[.*\\]", guard_list[i,]))
DateTime <- sapply(1:nrow(guard_list),extract1)


extract2 <- function(i) sub(".*? ","",regmatches(guard_list[i,],regexpr("\\].*", guard_list[i,])))
Activity <- sapply(1:nrow(guard_list),extract2)

#part 1

ordering <- order(DateTime)

DateTime <- DateTime[ordering]
Activity <- Activity[ordering]

sleep_time <- matrix(c(0,0),nrow=2)


for (i in 1:length(DateTime)){
  if(grepl("Guard .*? begins shift", Activity[i])==1){guard <- as.numeric(sub("#","",sub(" ","",regmatches(Activity[i],regexpr("#[0-9]* ", Activity[i])))))}
  if(!(guard%in% sleep_time[1,])) sleep_time <- cbind(sleep_time,c(guard,0))
  if(Activity[i]=="wakes up") {
    end <- as.numeric(sub(":","",regmatches(DateTime[i],regexpr(":[0-9]*", DateTime[i]))))
    start <- as.numeric(sub(":","",regmatches(DateTime[i-1],regexpr(":[0-9]*", DateTime[i-1]))))
    sleep_time[2,which(sleep_time[1,]==guard)] <- sleep_time[2,which(sleep_time[1,]==guard)] + (end-start)
  }
}


sleepy_guard <- sleep_time[1,which(sleep_time[2,]==max(sleep_time[2,]))]


minutes <- rep(0,60)


for (i in 1:length(DateTime)){
  if(grepl("Guard .*? begins shift", Activity[i])==1){guard <- as.numeric(sub("#","",sub(" ","",regmatches(Activity[i],regexpr("#[0-9]* ", Activity[i])))))}
  if(guard==sleepy_guard){
    
    if(Activity[i]=="wakes up") {
      end <- as.numeric(sub(":","",regmatches(DateTime[i],regexpr(":[0-9]*", DateTime[i]))))
      start <- as.numeric(sub(":","",regmatches(DateTime[i-1],regexpr(":[0-9]*", DateTime[i-1]))))
      minutes[start:(end-1)] <- minutes[start:(end-1)]+1
      print(start:(end-1))
    }
  }
}

minute <- which(minutes==max(minutes))

minute * sleepy_guard

#part 2


for (i in 1:length(DateTime)){
  if(grepl("Guard .*? begins shift", Activity[i])==1){guard <- as.numeric(sub("#","",sub(" ","",regmatches(Activity[i],regexpr("#[0-9]* ", Activity[i])))))}
  if(guard==sleepy_guard){
    
    if(Activity[i]=="wakes up") {
      end <- as.numeric(sub(":","",regmatches(DateTime[i],regexpr(":[0-9]*", DateTime[i]))))
      start <- as.numeric(sub(":","",regmatches(DateTime[i-1],regexpr(":[0-9]*", DateTime[i-1]))))
      minutes[which(sleep_time[1,]==guard),start:(end-1)] <- minutes[which(sleep_time[1,]==guard),start:(end-1)]+1
      print(start:(end-1))
    }
  }
}

