plotTheHeat <- function(dat) {
  
  plot_w = 0.85
  plot_h = 0.75
  
  
  dat$week <- weekdays(as.Date(dat$collision_date, format="%m/%d/%Y"), abbreviate=TRUE)
  dat$week <- factor(dat$week, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
  dat$hour <- format(as.POSIXct(dat$collision_date, format="%m/%d/%Y %H:%M"), "%H")
  dat$hour <- factor(dat$hour, levels=c('00', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23'))
  mat <- table(dat$hour, dat$week)
  
  colors <- colorRampPalette(c("#adddd1", "#3e98af", "#375980"))
  
  
  
  figs <- rbind(c(0.0, plot_w, plot_h, 1.0), # top
                c(plot_w, 1.0, 0.0, plot_h), # right
                c(0.0, plot_w, 0.0, plot_h)  # main
  )
  colnames(figs) <- c("W", "E", "S", "N")
  rownames(figs) <- c("top", "right", "main")
  
  screenIDs <- split.screen(figs)
  names(screenIDs) <- rownames(figs)
  
  screen(screenIDs["top"])
  par(mar=c(0,0,0,plot_w))
  cols <- colors(20)[cut(colSums(mat),20)]
  xs   <- seq_len(ncol(mat))
  ys   <- colSums(mat)
  plot.new()
  plot.window(xlim=c(0, ncol(mat)), ylim=c(0.0, max(ys)*2.5))
  rect(xs-0.95, 0.0, xs-0.05, ys+0.0, col=cols, border="white", lwd=0.5)
  text(xs-0.4, ys, round(ys), pos=3, offset=1.0, srt=90, cex=0.75, font=2)
  
  screen(screenIDs["right"])
  par(mar=c(0,1-plot_w,1-plot_h,0))
  cols <- colors(20)[cut(rowSums(mat),20)]
  xs   <- rowSums(mat)
  ys   <- seq_len(nrow(mat))
  plot.new()
  plot.window(xlim=c(0, max(xs)*2.5), ylim=c(nrow(mat), 0))
  rect(0, ys-0.9, xs, ys-0.1, col=cols, border="white", lwd=0.75)
  text(0, ys-0.5, round(xs), pos=4, cex=0.75, col="black", font=2)
  
  screen(screenIDs["main"])
  par(mar=c(0,0,1-plot_h,plot_h))
  cols <- colors(20)[cut(mat,20)]
  xs   <- col(mat)
  ys   <- row(mat)
  plot.new()
  plot.window(xlim=c(0, max(xs)), ylim=rev(c(0, max(ys))))
  rect(xs-1, ys-1, xs, ys, col=cols, border="white", lwd=0.5)
  text(xs-0.5, ys-0.5, round(mat), cex=0.8, col="white", font=0)
  text(xs[1,]-0.5, 0, colnames(mat), pos=3, cex=0.8, xpd=TRUE)
  text(ncol(mat), ys[,1]-0.5, rownames(mat), pos=4, cex=0.8, xpd=TRUE)
  close.screen(screenIDs)
}
