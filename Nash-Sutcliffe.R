# Nash-Sutcliffe Efficiencies
##################################################
rm(list = ls())
graphics.off()
require(lubridate)
########################################################################################
# plot the mean water outflow, runoff, backflow vs. distance to river, watershed area

# Read in water level data from the model
field.dir = paste("C:/Users/Alexander Looi/Google Drive/Dropbox/", 
                  "NOAA_Wetlands_Ceili-Alex/Alex's Folder/",
                  "Water level files/WL Data",sep = "")
setwd(field.dir)

site_names = c("Carpenters", "CB", "Delaney", "DL", "Butterfield", "BF")

for(s in seq(2, 6, by = 2)){
  
  temp.WL = read.csv(list.files(pattern = site_names[s])[2], header = T)
  
  uni.date = as.character(unique(temp.WL$Date))
  
  field.table = data.frame(matrix(NA, length(uni.date), 3))
  
  colnames(field.table) = c("Date", "WL", "sd")
  
  for(d in 1:length(uni.date)){
    d.vec = which(uni.date[d] == as.character(temp.WL$Date))
    WL.mean = mean(temp.WL$WL[d.vec], na.rm = T)
    WL.sd = sd(temp.WL$WL[d.vec], na.rm = T)
    field.table[d,1] = uni.date[d]
    field.table[d,2] = WL.mean
    field.table[d,3] = WL.sd
  }
  
  field.table[d,1] = mdy(field.table[d,1], tz = "EST")
  
  assign(paste(site_names[s], ".WLraw", sep = ""), field.table)
  
}

# Read in water level data from level loggers
model.dir = paste("C:/Users/Alexander Looi/Google Drive/",
                  "Dropbox/NOAA_Wetlands_Ceili-Alex/",
                  "Alex's Folder/Wetland Model/",
                  "Watersheds/Wetland Sites", sep = "")

site.direct = list.files(pattern = "TIBS")

setwd(model.dir)

for(s in seq(1, 5, by = 2)){
  
  list.files(pattern = site_names[s])
  
  site.dir = paste(model.dir, "/", 
                   list.files(pattern = site_names[s]), sep = "")
  setwd(site.dir)
  
  file.name = list.files(pattern = "2005-2014 model output.csv")
  
  temp.table = read.csv(file.name, header = T)
  
  assign(paste(site_names[s+1], ".WLmodel", sep = ""), temp.table)
  
  setwd(model.dir)
  
}

# find matching dates for waterlevel data and model data
for(s in seq(2, 6, by = 2)){
  
  temp.field = get(paste(site_names[s], ".WLraw", sep = ""))
  temp.model = get(paste(site_names[s], ".WLmodel", sep = ""))
  table.name = paste(site_names[s], ".match", sep = "")
  m.Dates = ymd(temp.model$Dates, tz = "EST")
  f.Date = mdy(temp.field$Date, tz = "EST")
  
  table.match = data.frame(matrix(NA, length(temp.field[,1]), 5))
  
  colnames(table.match) = c("Date", "nd.WL", "peak.WL", "Field.WL", "F.SD")
  
  for(d in 1:(length(table.match[,1])-1)){
    
    d.vec = which(f.Date[d] == m.Dates)
    table.match[d,1] = as.character(f.Date[d])
    table.match[d,2] = temp.model$nd.WL[d.vec]
    table.match[d,3] = temp.model$peak.WL[d.vec]
    table.match[d,4] = temp.field$WL[d]
    table.match[d,5] = temp.field$sd[d]
  
  }
  
  assign(table.name, table.match)

}

NSE = function(sim, obs){
  numer = sum((obs-sim)^2, na.rm = T)
  denom = sum((obs-mean(obs, na.rm = T))^2, na.rm = T)
  E = 1-(numer/denom)
  return(E)
}

require(pgirmess)

# plot NSE, model data, level logger data
CB.match = CB.match[order(CB.match$Date),]
CB.match$Date = ymd(CB.match$Date, tz = "EST")

# plot CB
plot.dates = seq.POSIXt(ymd("2012-03-09 EST", tz = "EST"), 
                        ymd("2013-10-29 EST", tz = "EST"), 
                        by = "day")

CB.full = data.frame(matrix(NA, length(plot.dates), 3))
colnames(CB.full) = c("Date", "field.WL", "model.WL")

dates.vec = which(plot.dates %in% CB.match$Date)

CB.full$Date = plot.dates

CB.full$field.WL[dates.vec] = CB.match$Field.WL[1:416]
CB.full$model.WL[dates.vec] = CB.match$nd.WL[1:416]

# friedman.test(cbind(CB.full$field.WL, CB.full$model.WL)[1:416,])

##########################################################################
# Plot Delaney Branch
DL.match = DL.match[order(DL.match$Date),]

plot(ymd(DL.match$Date, tz = "EST"), c(DL.match$peak.WL)-.20, ylim = c(74.0, 75.75), type = "l")
lines(ymd(DL.match$Date, tz = "EST"), DL.match$Field.WL, col = 2)
friedman.test(cbind(DL.match$Field.WL, DL.match$nd.WL))
NSE(c(DL.match$nd.WL), DL.match$Field.WL)

BF.match = BF.match[order(BF.match$Date),]
BF.match$Date = ymd(BF.match$Date, tz = "EST")
plot(BF.match$Date, c(BF.match$nd.WL), ylim = c(74.0, 75.75), type = "l")
lines(BF.match$Date,BF.match$Field.WL, col = 2)
friedman.test(cbind(BF.match$Field.WL, BF.match$nd.WL))
NSE(BF.match$nd.WL, BF.match$Field.WL)
############################################################################
setwd(field.dir)
mag = 1.5
png("CB NSE plot.png", height = 950, width = 1500)

layout(matrix(c(1,2), 2, 1), heights = c(4, 1))
par(mar = c(6.5, 5.5, 1, 1), cex = mag)
# plot the CB field and model data
plot(CB.full$Date, CB.full$field.WL, type = "l",
     xaxt = "n", yaxt = "n", ylim = c(74.5, 75.75),
     xlab = "", ylab = "", lwd = 2)
lines(CB.full$Date, CB.full$model.WL, 
      col = 2, lwd = 2, lty = 2)

# plot the DL field and model data
lines(ymd(DL.match$Date, tz = "EST"), c(DL.match$nd.WL)-.20, 
      lwd = 2, col = 3)
lines(ymd(DL.match$Date, tz = "EST"), c(DL.match$Field.WL), 
      lwd = 2, col = 4, lty = 2)

# plot the BF field and model data
lines(ymd(BF.match$Date, tz = "EST"), c(BF.match$nd.WL), 
      lwd = 2, col = "blueviolet")
lines(ymd(BF.match$Date, tz = "EST"), c(BF.match$Field.WL), 
      lwd = 2, col = "mediumturquoise", lty = 2)
# y.min and y.max calculations
y.min = min(c(CB.full$field.WL, CB.full$model.WL), na.rm = T)
y.max = max(c(CB.full$field.WL, CB.full$model.WL), na.rm = T)
# y ticks and axis
y.ticks.place = seq(74.5, 75.75, length.out = 8)
y.axis = round(y.ticks.place, digits = 1)
# x ticks and axis
x.ticks.place = seq(CB.full$Date[1], 
                    CB.full$Date[length(CB.full$Date)], 
                    length.out = 15)
# x axis labels
m = month(CB.full$Date[round(seq(1, length(CB.full$Date), 
                                 length.out = 15), digits = 0)],label = T)
y = year(CB.full$Date[round(seq(1, length(CB.full$Date), 
                                length.out = 15), digits = 0)])
x.axis = paste(m, "-", y, sep = "")

# put the axis on the figure
axis(side = 2, at = y.ticks.place, labels = y.axis, las = 2, cex.axis = mag-.5)
axis(side = 1, at = x.ticks.place, labels = x.axis, las = 2, cex.axis = mag-.5)
CB.NSE = round(NSE(CB.full$model.WL, CB.full$field.WL), digits = 3)
DL.NSE = round(NSE(DL.match$nd.WL-.20, DL.match$Field.WL), digits = 3)
BF.NSE = round(NSE(BF.match$nd.WL, BF.match$Field.WL), digits = 3)
# label text
mtext(side = 2, line = 4, text = "meters IGLD", cex = mag+1)
mtext(side = 1, line = 5.5, text = "Month", cex = mag+1.1)

# NSE x pos
# text(x.ticks.place[2], 74.5, paste("CB NSE =", CB.NSE))
# text(x.ticks.place[2], 74.6, paste("DL NSE =", DL.NSE))
# text(x.ticks.place[2], 74.7, paste("BF NSE =", BF.NSE))
# Legend
plot.new()
par(mar = c(0, 0, 0, 0), cex = mag+.25)

legend("center", c("CB model", "CB Field",
                   "DL model", "DL Field",
                   "BF model", "BF Field"), bty ="n", y.intersp = 3,
       lty = c(1, 2, 1, 2, 1, 2), ncol = 3, lwd = 2,
       col = c(1, 2, 3, 4, "blueviolet", "mediumturquoise"))

dev.off()
##########################################################################
