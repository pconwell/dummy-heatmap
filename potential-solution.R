plotTheHeat <- function(mat, colors) {
  
  figs <- rbind(c(0.0, 0.8, 0.8, 1.0), # top
                c(0.8, 1.0, 0.0, 0.8), # right
                c(0.0, 0.8, 0.0, 0.8)  # main
  )
  colnames(figs) <- c("W", "E", "S", "N")
  rownames(figs) <- c("top", "right", "main")
  
  screenIDs <- split.screen(figs)
  names(screenIDs) <- rownames(figs)
  
  screen(screenIDs["top"])
  par(mar=c(0,0,0,0))
  cols <- colors(20)[cut(colSums(mat),20)]
  xs   <- seq_len(ncol(mat))
  ys   <- colSums(mat)
  plot.new()
  plot.window(xlim=c(0, ncol(mat)), ylim=c(0, max(ys)*1.5))
  rect(xs-0.95, 0, xs-0.05, ys, col=cols, border="white", lwd=0.5)
  text(xs-0.4, ys, round(ys), pos=3, offset=1, srt=90, cex=0.8, font=2)
  
  screen(screenIDs["right"])
  par(mar=c(0,0,0,0))
  cols <- colors(20)[cut(rowSums(mat),20)]
  xs   <- rowSums(mat)
  ys   <- seq_len(nrow(mat))
  plot.new()
  plot.window(xlim=c(0, max(xs)*1.5), ylim=c(nrow(mat), 0))
  rect(0, ys-0.9, xs, ys-0.1, col=cols, border="white", lwd=0.5)
  text(0, ys-0.5, round(xs), pos=4, cex=0.8, col="white", font=2)
  
  screen(screenIDs["main"])
  par(mar=c(0,0,0,0))
  cols <- colors(20)[cut(mat,20)]
  xs   <- col(mat)
  ys   <- row(mat)
  plot.new()
  plot.window(xlim=c(0, max(xs)), ylim=rev(c(0, max(ys))))
  rect(xs-1, ys-1, xs, ys, col=cols, border="white", lwd=0.5)
  text(xs-0.5, ys-0.5, round(mat), cex=0.8, col="white", font=2)
  text(xs[1,]-0.5, 0, colnames(mat), pos=3, cex=0.8, xpd=TRUE)
  text(ncol(mat), ys[,1]-0.5, rownames(mat), pos=4, cex=0.8, xpd=TRUE)
  close.screen(screenIDs)
}

dat <- read.csv("./crashes.csv", as.is=TRUE)
dat$week <- weekdays(as.Date(dat$collision_date, format="%m/%d/%Y"), abbreviate=TRUE)
dat$week <- factor(dat$week, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
dat$hour <- format(as.POSIXct(dat$collision_date, format="%m/%d/%Y %H:%M"), "%H")
mat <- table(dat$week, dat$hour)


cols <- colorRampPalette(c("#adddd1", "#3e98af", "#375980"))
#png("heat.png", width=1000, height=500)
plotTheHeat(mat, cols)
#dev.off()
