# Basic Summary Statistics on Field Data

main.dir = paste("/Users/Looi/Dropbox/NOAA_Wetlands_Ceili-Alex/",
                 "Alex's Folder/Wetland Model/Watersheds", sep = "")
setwd(main.dir)

file.name = "Raw Field Data.csv"

field.data.raw = read.csv(file.name)

field.data = field.data.raw[,-14:15]

sites = c("BF", "DR", "DB", "CB", "DL", "PT")

num.sites = length(sites)

mon.col = rep(0, 12)

field.folder.name = "Field Nutrient Data"
folder.check = list.files(pattern = field.folder.name)
if(length(folder.check) == 0){
  dir.create(field.folder.name)
}

month.lab = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

setwd(paste(main.dir, "/", field.folder.name, sep = ""))

# Create seperate tables for each wetland site
for(s in 1:num.sites){

  table.name = paste(sites[s], ".FN", sep = "")
  
  sites.vec = which(sites[s] == field.data$Site)
  
  SFND = field.data[sites.vec,]
  
  assign(table.name, SFND)
  
  mean.month.Nut = data.frame(mon.col, mon.col, mon.col, mon.col)
  colnames(mean.month.Nut) = c("Site", "Month", "TP", "TN" )
  
  for(m in 1:12){
    
    TFM = which(field.data$Month == m)
    
    if(length(TFM) == 0){
    
      m.TPval = NA
      m.TNval = NA
      
    } else {
    
      m.vec = TFM
      
      m.TNval = mean(c(SFND$TDN[m.vec]), na.rm = T)
      m.TPval = mean(c(SFND$TP[m.vec]), na.rm = T)
    
    }
    
    mean.month.Nut$Site[m] = sites[s]
    mean.month.Nut$Month[m] = m
    mean.month.Nut$TP[m] = m.TPval
    mean.month.Nut$TN[m] = m.TNval
    
  }
  
  SFND.csv = SFND
  
  mean.month.name = paste(sites[s], ".mean.nut", sep = "")
  assign(mean.month.name, mean.month.Nut)
  
  file.name = paste(sites[s], "Monthly Mean Nutrient Field Data.csv")
  file.raw = paste(sites[s], "Nutrient Field Data.csv")
  
  write.csv(mean.month.Nut, file = file.name, row.names = F)
  write.csv(SFND.csv, file = file.raw, row.names = F)
  # plot Total Nitrogen
  plot(mean.month.Nut$TN, type = "b", xaxt = "n", 
       ylab = "[N] microMoles/L", col = 2,
       xlab = "Month", main = paste(sites[s], "Mean Monthly [N]"))
  axis(side = 1, at = c(1:12), labels = month.lab)
  # plot Total Phosphorus
  plot(mean.month.Nut$TP, type = "b", xaxt = "n", 
       ylab = "[P] microMoles/L", col = 2,
       xlab = "Month", main = paste(sites[s], "Mean Monthly [P]"))
  axis(side = 1, at = c(1:12), labels = month.lab)
  
  plot(SFND$Month, SFND$TP, xlab = "Month", ylab = "[P] microMoles/L",
       col = 2, main = paste(sites[s], "Mean Monthly [P]"),
       xaxt = "n")
  axis(side = 1, at = c(1:12), labels = month.lab)
  
  plot(SFND$Month, SFND$TDN, xlab = "Month", ylab = "[N] microMoles/L",
       col = 2, main = paste(sites[s], "Mean Monthly [N]"),
       xaxt = "n")
  axis(side = 1, at = c(1:12), labels = month.lab)
  
}

