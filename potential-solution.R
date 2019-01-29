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
  rect(xs-0.9, 0, xs-0.1, ys, col=cols)
  text(xs-0.4, ys, round(ys), pos=3, offset=1, srt=90, cex=0.8)
  
  screen(screenIDs["right"])
  par(mar=c(0,0,0,0))
  cols <- colors(20)[cut(rowSums(mat),20)]
  xs   <- rowSums(mat)
  ys   <- seq_len(nrow(mat))
  plot.new()
  plot.window(xlim=c(0, max(xs)*1.5), ylim=c(nrow(mat), 0))
  rect(0, ys-0.9, xs, ys-0.1, col=cols)
  text(0, ys-0.5, round(xs), pos=4, cex=0.8, col="white")
  
  screen(screenIDs["main"])
  par(mar=c(0,0,0,0))
  cols <- colors(20)[cut(mat,20)]
  xs   <- col(mat)
  ys   <- row(mat)
  plot.new()
  plot.window(xlim=c(0, max(xs)), ylim=rev(c(0, max(ys))))
  rect(xs-1, ys-1, xs, ys, col=cols)
  text(xs-0.5, ys-0.5, round(mat), cex=0.8, col="white")
  text(xs[1,]-0.5, 0, colnames(mat), pos=3, cex=0.8, xpd=TRUE)
  text(ncol(mat), ys[,1]-0.5, rownames(mat), pos=4, cex=0.8, xpd=TRUE)
  close.screen(screenIDs)
}

dat <- read.csv("./crashes.csv", as.is=TRUE)
dat$week <- weekdays(as.Date(dat$collision_date, format="%m/%d/%Y"), abbreviate=FALSE)
dat$hour <- format(as.POSIXct(dat$collision_date, format="%m/%d/%Y %H:%M"), "%H")
dat$week <- factor(dat$week, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
dat[order(dat$week), ]
mat <- tapply(dat$number_injured, list(dat$week, dat$hour), sum)

png("heat.png", width=1000, height=500)
plotTheHeat(mat, colorRampPalette(blues9[-1]))
dev.off()
