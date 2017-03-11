# df <- data.frame(cbind(1:10, 11:20))
# names(df) <- c("y","x")
# 
# par(mar = c(7, 7, 7, 7))
# plot(y ~ x, data = df, ylab = "", xlab = "independent")
# axis(side = 4, at = c(1:10))
# mtext(side = 2, line = 3.5, text = y.lab, cex = mag+.5)
# text(par("usr")[2] + 2.2, 5.5, srt=-90, labels = y.lab, 
#      xpd = TRUE, cex = mag+0.5)

#####################
# Boxplot function #
#####################
model.boxplots = function(Box.data, y.min, y.max, y.lab, 
                          means, sds, 
                          folder.name, file.name, YI){
  # Check to see if the directory was already created
  check.folder = list.files(pattern = folder.name)
  if (length(check.folder) == 0){
    dir.create(folder.name) 
  } 
  fig.let = c("A", "B", "C", "D")
  # set to new WD
  setwd(paste(site.wd, "/", folder.name, sep = ""))
  png(filename = file.name,
      width = p.width, height = p.length)
  if(YI %% 2  == 0){
    s.n = 4
    mar.vals = c(5, 1, 2, 7)
    ang = -90
  } else {
    mar.vals = c(5, 7, 2, 1)
    s.n = 2
    ang = 0
  }  
  par(cex = 1.25, mar = mar.vals)
  x.axis = boxplot(Box.data, xaxt = "n", ylim = c(y.min, y.max),
                   xlab = "", ylab = "", pch = point.types, yaxt = "n")
  abline(h = 0)
  # plot the means and SD
  lines(means, col = 6, type = "b", lwd = 2, pch = point.types)
  arrows(c(1:12), means-sds, c(1:12), col = 6, lwd = 2,
         means+sds, length=0.05, angle=90, code=3)
  # Create new figure labels
  y.pos = (y.max+y.min)/2
  if(YI %% 2  == 0){
    text(par("usr")[2] + 1.2, y.pos, srt = -90, labels = y.lab, 
         xpd = TRUE, cex = mag+0.35)
  } else {
    text(par("usr")[1] - 1.2, y.pos, srt = 90, labels = y.lab, 
         xpd = TRUE, cex = mag+0.35)
  }
  # Axis
  y.rang = (y.max-y.min)/5
  if(y.rang < 1) {digit = 1} else {digit = 0}
  y.ticks = seq(y.min, y.max, by = y.rang)
  axis(side = s.n, at = y.ticks, labels = y.ticks, 
       las = 2, cex.axis = mag)
  axis(side = 1, at = 1:12, label = month.names, cex.axis = mag)
  mtext(side = 1, line = 2.75, text = "Month", cex = mag+.5)
  text(0.35, c(y.max-(0.05*y.max)), fig.let[YI], cex = mag+1)
  dev.off()
  graphics.off()
}

##############################
# Nutrient plotting Function #
##############################

Nutrient.Month.plot = function(Nut.avg, Nut.sd, y.min, y.max, y.lab, 
                               FieldN.Data, FieldN.Data.m,
                               FieldN.raw.D, FieldN.raw.M,
                               folder.nut.name, file.name, mag){
  if(sw %% 2  == 0){
    s.n = 4
    mar.vals = c(5, 1, 2, 7)
    ang = -90
  } else {
    mar.vals = c(5, 7, 2, 1)
    s.n = 2
    ang = 0
  }
  
  # Check to see if the directory already exists if it does not
  # exist then create a new directory
  check.folder = list.files(pattern = folder.nut.name)
  if(length(check.folder) == 0) {
    dir.create(folder.nut.name)
  }
  fig.let = c("A","B","C", "D", "E", "F")
  # set the new work directory
  setwd(paste(site.wd, "/", folder.nut.name, sep = ""))
  Nut.Max = max(Nut.avg+Nut.sd, FieldN.Data, na.rm = T)
  png(filename = file.name,
      width = p.width, height = p.length)
  par(cex = mag, mar = mar.vals)
  x.axis = plot(Nut.avg, type = "b", pch = point.types,
       xlab = "", ylab = "",
       xaxt="n", yaxt = "n", col = "2",
       ylim = c(y.min, y.max), lwd = 2)
  abline(h = 0)
  y.pos = (y.max+y.min)/2
  # Create new figure labels
  if(sw %% 2  == 0){
    text(par("usr")[2] + 1.2, y.pos, srt=-90, labels = y.lab, 
         xpd = TRUE, cex = mag+0.25)
  } else {
    text(par("usr")[1] - 1.2, y.pos, srt=90, labels = y.lab, 
         xpd = TRUE, cex = mag+0.25)
    # mtext(side = s.n, line = 3.5, text = y.lab, cex = mag+0.5)
  }
  y.rang = (y.max-y.min)/5
  if(y.rang < 1) {digit = 1} else {digit = 0}
  mtext(side = 1, line = 2.75, text = "Month", cex = mag+1)
  # Axis 
  y.ticks = seq(y.min, y.max, by = round((y.max-y.min)/5, digits = digit))
  axis(side = s.n, at = y.ticks, labels = y.ticks, 
       las = 2, cex.axis = mag)
  axis(side = 1, at = 1:12, label = month.names, cex.axis = mag)
  # add raw data
  points(FieldN.Data.m, FieldN.Data, col = 4, pch = point.types, cex = mag)
  points(FieldN.raw.M, FieldN.raw.D, col = 4, pch = point.types, cex = mag)
  arrows(c(1:12),Nut.avg-Nut.sd, c(1:12), Nut.avg+Nut.sd, 
         length=0.05, angle=90, code=3, lwd = 2)
  text(0.9, c(y.max-0.025*y.max), fig.let[sw], cex = mag+1)
  dev.off()
  graphics.off()
}