# trajectory object
# members:
#     - track.id: id of the track
#     - xy: matrix of 2n columns with object coordinates
#     - time: 1-t vector with time samples
#     - arena.width: width of arena in degrees
#     - arena.height: height of arena in degrees
#     - x.coord: indexes of x coordinates
#     - y.coord: indexes of y coordinates


video.from.trajectory <- function(traj, tgt = 4, eye.x = NULL, eye.y = NULL, pred.x = NULL, pred.y = NULL, out.file = "traj.mp4"){
  plot.frame <- function(i, blackbac = T) {
    f <- traj$xy[i,]
    
    plot.default(f[x.coords], f[y.coords], col="green", pch = 16, xlab = "x", ylab = "y", xlim = c(-traj$arena.width, traj$arena.width), ylim = c(-traj$arena.height, traj$arena.height))
    if (blackbac) {
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "black")
    }
    #par(bg="white")
    points(f[tgt.x.coords], f[tgt.y.coords], col="green", pch=16)
    points(f[dist.x.coords], f[dist.y.coords], col="grey", pch=16)
    if (!is.null(eye.x) & !is.null(eye.y)){
      points(eye.x[i], eye.y[i], col="red", pch=16)
    }
    if (!is.null(pred.x) & !is.null(pred.y)){
      points(pred.x[i], pred.y[i], col="yellow", pch=16)
    }
    
    
  }
  
  #if (!file.exists(out.dir)) {
  #  cat("Creating output directory '",out.dir,"'",sep="")
  #  dir.create(out.dir,recursive=T)
  #}
  
  tgt <- 1:(2*tgt)
  x.coords <- traj$x.coords
  y.coords <- traj$y.coords
  tgt.x.coords <- intersect(tgt,x.coords)
  tgt.y.coords <- intersect(tgt,y.coords)
  dist.x.coords <- setdiff(tgt,x.coords)
  dist.y.coords <- setdiff(tgt,y.coords)
  
  require("animation")
  
  n <- nrow(traj$xy)
    
  oopt <- ani.options(interval = 0.02, nmax = n,autoplay=F)
  
  for (i in 1:ani.options("nmax")) {
    ## draw your plots here, then pause for a while with ani.pause()
    plot.frame(i,blackbac=T)
    ani.pause()
    
  }
  #ani.options(oopt)
  dev.hold()
  #file.copy(file.path(tempdir(),out.file),file.path(out.dir,out.file))
}

# plots trajectories object
plot.trajectory <- function(traj,tgt=4) {
  # helper function to extract legend
  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  stopifnot(is.trajectory(traj))
  require(ggplot2)
  require(gridExtra)
  xy <- data.frame(traj$xy, time = traj$time)
  n  <- nrow(xy)
  
  tgt <- 1:(2*tgt)
  x.coords <- traj$x.coords
  y.coords <- traj$y.coords
  
  xy.long <- long.xy(xy, x.coords)
  xy.start <- subset(xy.long, time == min(xy.long[1,]$time))
  xy.end <- subset(xy.long, time == max(xy.long[,]$time))
  
  ar.w <- traj$arena.width
  ar.h <- traj$arena.height
  
  
  p.xy <- ggplot(xy.long, aes(x, y, col = dot)) +
    geom_path(size = 0.8, linetype = 2) +
    geom_point(data = xy.start, aes(x, y, col = dot), size = 5, shape = 1) +
    geom_point(data = xy.end, aes(x, y, col = dot), size = 5) +
    ggtitle(paste0("Trajectory ", traj$track.id)) +
    scale_x_continuous("x (in deg)", breaks = round(seq(-ar.w, ar.w, by = 5), 1), limits = c(-ar.w, ar.w)) +
    scale_y_continuous("y (in deg)", breaks = round(seq(-ar.h, ar.h, by = 5), 1), limits = c(-ar.h, ar.h))
  p.tx <- ggplot(xy.long, aes(time, x, col = dot)) +
    geom_path(size = 0.8, linetype = 2) +
    geom_point(data = xy.start, aes(time, x, col = dot), size = 5, shape = 1) +
    geom_point(data = xy.end, aes(time, x, col = dot), size = 5) +
    ggtitle("x in time") +
    scale_x_continuous("time (in s)") +
    scale_y_continuous("x (in deg)", breaks = round(seq(-ar.w, ar.w, by = 5), 1), limits = c(-ar.w, ar.w))
  p.ty <- ggplot(xy.long, aes(time, y, col = dot)) +
    geom_path(size = 0.8, linetype = 2) +
    geom_point(data = xy.start, aes(time, y, col = dot), size = 5, shape = 1) +
    geom_point(data = xy.end, aes(time, y, col = dot), size = 5) +
    ggtitle("y in time") +
    scale_x_continuous("time (in s)") +
    scale_y_continuous("y (in deg)", breaks = round(seq(-ar.h, ar.h, by = 5), 1), limits = c(-ar.h, ar.h)) +
    guides(col = guide_legend(nrow = 4))
  
  mylegend <- g_legend(p.ty)
  grid.arrange(arrangeGrob(p.xy + theme(legend.position = "none"),
                           p.tx + theme(legend.position = "none"),
                           p.ty + theme(legend.position = "none"),
                           mylegend, ncol = 2))
}

# function for casting trajectory to long format (useful for plotting)
long.xy <- function (xy, x.coords) {
  xy.x  <- melt(xy, measure.vars = x.coords, variable.name = "dot.x", value.name = "x.coord")
  xy.xy <- melt(xy.x, id.vars = c("dot.x", "x.coord", "time"), variable.name = "dot.y", value.name = "y.coord")
  levels(xy.xy$dot.x) <- 1:length(levels(xy.xy$dot.x))
  levels(xy.xy$dot.y) <- 1:length(levels(xy.xy$dot.y))
  xy.xy <- subset(xy.xy, dot.x == dot.y)
  xy.xy$dot.x <- NULL
  xy.xy <- xy.xy[,c(3,1,4,2)]
  colnames(xy.xy) <- c("dot","x","y","time")
  
  return(xy.xy)
}

print.trajectory <- function(traj) {
  cat("Track id: ", traj$track.id, "\n")
  cat("Dot positions:\n")
  print.data.frame(head(traj$xy), row.names = F)
  cat("...\n")
  cat("X coords:\n")
  cat("  ", traj$x.coords, "\n")
  cat("Y coords:\n")
  cat("  ",traj$y.coords, "\n")
  cat("Time:\n")
  
  cat("  ",head(traj$time), "...", "", traj$time[length(traj$time)], "\n")
  
  cat("Arena: \n")
  cat("   width: ",traj$arena.width, "   height: ", traj$arena.height, "\n")
  
}

is.trajectory <- function(traj){
  
  return(class(traj) == "trajectory")
}
  
is.valid.trajectory <- function(traj){
  stopifnot(is.trajectory(traj))
  ret.val <- T
  x.coords <- traj$x.coords
  y.coords <- traj$y.coords
  
  xy <- traj$xy
  time <- traj$time
  diff.time <- zapsmall(diff(time), digits = 4)
  ret.val <- ret.val & length(intersect(x.coords, y.coords)) == 0
  ret.val <- ret.val & all(sort(union(x.coords, y.coords)) == 1:ncol(xy))
  
  ret.val <- ret.val & max(xy[x.coords]) <= traj$arena.width & min(xy[x.coords]) >= -traj$arena.width
  ret.val <- ret.val & max(xy[y.coords]) <= traj$arena.height & min(xy[y.coords]) >= -traj$arena.height
  ret.val <- ret.val & nrow(xy) == length(time)
  ret.val <- ret.val & max(diff.time) == min(diff.time)
  return(ret.val)
}  

get.trajectory <- function(track, trackData) {
  df.traj <- trackData[trackData$track == track,]
  traj <- list()
  class(traj) <- "trajectory"
  track <- unique(df.traj$track)
  stopifnot(length(track) == 1)
  traj$track <- track
  traj$xy <- df.traj[,2:17]
  row.names(traj$xy) <- NULL
  traj$time <- df.traj$time
  
  x.coords <- seq(2,16,2)
  y.coords <- seq(3,17,2)
  
  traj$arena.width <- 15
  traj$arena.height <- 15
  traj$x.coords <- x.coords-1
  traj$y.coords <- y.coords-1
  stopifnot(is.valid.trajectory(traj))
  
  return(traj)
}
