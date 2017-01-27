rm(list=ls());
graphics.off();

dir = "/Users/Looi/Desktop/Watersheds/Water Level Management Plans"

start.y = 1943
end.y = 1951

setwd(dir)

start.d = as.Date(paste(start.y, "-01-01", sep = ""))
end.d = as.Date(paste(end.y, "-12-31", sep = ""))

dates = seq.Date(start.d, end.d, by = "day")

month = format(dates, "%m")
day = format(dates, "%d")

d.len = length(dates)

planks = rep(0, d.len)

for(p in 1:d.len){
  
  if(month[p] == "06" && day[p] == "01"){
     
    planks[p] = -11.43
    
  } else if (month[p] == "06" && day[p] == "15") {
    
    planks[p] = -11.43
    
  } else if (month[p] == "06" && day[p] == "30") {
    
    planks[p] = -11.43
    
  } else if (month[p] == "09" && day[p] == "15"){
    
    planks[p] = 34.29
    
  }
    
}
  
Flash.boards = data.frame(planks, planks, planks, planks)

colnames(Flash.boards) = c("DATES", "Carpenters", "Butterfield", "Delaney")

Flash.boards$DATES = dates

for (t in 1:3){
  
  Flash.boards[,t+1] = planks
  
}

file.name.final = paste("Water Level Management Plan ", 
                        start.y, "-", end.y, ".csv", sep = "")

write.table(Flash.boards, file.name.final, row.names = F, quote = F, sep = ",")
