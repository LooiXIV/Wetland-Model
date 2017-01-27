rm(list = ls())
graphics.off()
########################################################################################
# plot the mean water outflow, runoff, backflow vs. distance to river, watershed area

dir = paste("C:/Users/Alexander Looi/Google Drive/",
            "Dropbox/NOAA_Wetlands_Ceili-Alex/",
            "Alex's Folder/Wetland Model/",
            "Watersheds/Wetland Sites", sep = "")

setwd(dir)

site_names = c("Carpenters", "CB", "Delaney", "DL", "Butterfield", "BF",
               "Deferno", "DB", "Plum Tree", "PT", "Dorr Road", "DR")
Matrix_Sites = matrix(site_names, 2, 6, byrow = F)
rownames(Matrix_Sites) = c("Full","Abrv.")
colnames(Matrix_Sites) = c("With WCS", "With WCS", "With WCS", 
                           "Without WCS", "Without WCS", "Without WCS")

site.abv = c("BF", "CB", "DB", "DL", "DR", "PT")

WAT = read.csv("Wetland Attributes Table.csv", header = T)

wetlands = list.files(pattern = "TIBS")

wl = length(wetlands)

csv.names = c("Post_High", "Post_Low", "Pre_High", "Pre_Low")
TP = c("Post MSD High Prcp.", "Post MSD Low Prcp.", 
       "Pre MSD High Prcp.", "Pre MSD Low Prcp.")

# "backflow", "Precip.", "runoff", "ET", "EV", "Outflows", "num days"

plot.y = "Outflows"
letter = c("A", "B", "C", "D")
# plot three types of plots:
# 1) backflow vs. Distance to SL
# 2) backflow vs. watershed Area
# 3) outflow vs. Distance to SL
# 4) outflow vs. watershed Area
# y.lab = c("Prop. Backflow", "Prop. Outflow",
#           "Prop. Outlfow", "Prop. Backflow")

x.lab = c("Distance to SL", "Watershed Area")
inflow.types = c("backflow", "Precip.", "runoff")
outflow.types = c("ET", "EV", "Outflows")
Len.Prop = c(" Proportion", " Total Volume")
Propor = c(T,F)
mag = 1.45

for(n in 1:2){
  
  # create a file name for the plot
  plot.file = paste(plot.y, Len.Prop[n], ".png", sep = "")
  png(filename = plot.file, height = 1150, width = 800)
  # New layout
  layout(rbind(1,2,3), heights = c(7,7,2))
  for(p in 1:2){
    for(w in 1:wl){
      
      # navigate to the correct folder
      WL.dir = paste(dir, "/", wetlands[w], sep = "")
      setwd(WL.dir)
      limb.fold = list.files(pattern = "Limb Analysis")
      WL.limb = paste(WL.dir, "/", limb.fold, sep = "")
      setwd(WL.limb)
      limb.csvs = list.files(pattern = ".csv")
      
      # Read in all the limb data then add the 
      # Appropriate limb data to the plot.
      # Runoff, days of backflow and outflow, outflow
      
      for (l in 1:4){
        
        temp.table = read.csv(limb.csvs[l], header = T)
        
        temp.table[1:6, 2:4] = temp.table[1:6, 2:4]
        
        temp.table$Total = rowSums(temp.table[, 2:4])
        
        all.out = sum(temp.table[4:6,2:4])
        all.in = sum(temp.table[1:3,2:4])
        
        assign(csv.names[l],temp.table)
        
        if(x.lab[p] == "Distance to SL"){
          x = WAT[2,w+1]
          x.max = 11000
          x.min = 0
          text.min = 0
          x.label = "Distance to SL in meters"
        } else {
          x = WAT[1,w+1]/10^6
          x.max = 5
          x.min = 0
          text.min = 0
          x.label = expression("Watershed Area (10"^"6"*" m"^"2"*")")
        }
  
        x.ticks = seq(x.min, x.max, by = (x.max-x.min)/5)
        
        if(any(plot.y==inflow.types)){
          in.out = "inflow"  
        } else {
          in.out = "outflow" 
        }
        
        if(Propor[n]){
          if(in.out == "outflow"){
            denom.y = sum(temp.table$Total[4:6])
          } else {
            denom.y = sum(temp.table$Total[1:3])  
          }
          axis.lab = paste("Proportion of Total Outflows")
          y.min = 0
          y.max = 1.4
          multi = 0.2
          margins = c(3, 1, 1, 5)
        } else {
          axis.lab = expression("Total Vol. 10"^"6"*" Liters")
          y.min = 0
          y.max = 20 # millions of Liters?????
          multi = 2.5
          denom.y = 1
          margins = c(3, 5, 1, 1)
        }
        y.ticks = seq(y.min, y.max, by = (y.max-y.min)/5)
        x.ticks = seq(x.min, x.max, by = (x.max-x.min)/5)
        y = temp.table$Total[which(plot.y == temp.table$X)]/denom.y
        if(w == 1 & l == 1){
          
          # Maybe have backflow and outflow as proportion of total outputs
          # and inflow?
          y.pos = (y.max+y.min)/2 
          x.p = jitter(x)
          par(mar = margins, cex = mag)
          plot(x.p, y, ylim = c(y.min, y.max),
               xlim = c(x.min,x.max),
               ylab = "", xlab = "", 
               yaxt = "n", xaxt = "n",
               pch = 21, bg = l, cex = mag)
          
          if(Propor[n]){
            axis.side = 4
            x.pos = par("usr")[2] + 0.55*(x.max-x.min)/5
            ang = -90
          } else {
            axis.side = 2
            x.pos = par("usr")[1] - 0.55*(x.max-x.min)/5
            ang = 90
          }
          
          axis(side = axis.side, las = 2, at = y.ticks, 
               labels = round(y.ticks, digits = 1), cex.axis = mag-0.4)
          axis(side = 1, las = 1, at = x.ticks, 
               labels = round(x.ticks, digits = 1), cex.axis = mag-0.4)
          mtext(side = 1, text = x.label, line = 2.75, cex = mag+0.4)
          
          
          text(x.pos, y.pos, label = axis.lab, srt = ang, cex = mag, xpd = T)
        } else {
          x.p = jitter(x)
          points(x.p, y, pch = 21, bg = l, cex = mag)
        }
      }
      
      text(x, c(y+multi), site.abv[w], cex = mag-.25)
      
    }
    # text(text.min, y.max, letter[p], cex = mag)
  }
  plot.new()
  par(mar = c(0,0,0,0), cex = mag+0.5)
  legend("center", pch = 21, pt.bg = c(1:4), 
         legend = TP, ncol = 2, bty ="n", y.intersp = 3)
  dev.off()
  graphics.off()
  setwd(dir)
}