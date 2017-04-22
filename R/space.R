#' Creates steps for 3D gaussian
#' @description
#' Computes all spatiotemporal maps and stores them into RData files. Trajectories from response data.frame will be used and data taken from eyeData data.frame
#' @param response data.frame with participant responses
#' @param eyeData data.frame with eye gaze data
#' @param prefix we can store data with optional prefix, default is ""
#' @param spaces.dir path to the directory, where scanpaths will be stored. Default value is taken from config
#' @author Filip Dechterenko
#' @export
prepare.all.spaces <- function(response, eyeData, prefix = "", spaces.dir = NULL) {
  
  G <- gaussian.mask()
    
  if (missing(spaces.dir)) {
    spaces.dir <- get("spaces-dir", pkg_globals)
  }
  
  if (!file.exists(spaces.dir)){
    
    cat(sprintf("* Directory %s don't exist, creating..\n", spaces.dir))
    
    dir.create(spaces.dir, recursive = T)
  }
  
  cat("Preparing all scanpath spaces\n")
  n  <- nrow(response)
  tm <- create.time.measure(n)
  for (i in 1:n) {
    r     <- response[i,]
    id    <- r$id
    trial <- r$trial
    
    cat(sprintf(" * id: %d, tr: %d", id, trial))
    
    sp <- get.scanpath(id, trial, eyeData)
    SV <- smooth.space(sp, G)
    
    
    save(SV, file = get.stored.space.name(id, trial, prefix, spaces.dir))
    tm <- update.tm(tm)
    print(tm)
    
  }
}

#' Tests, if coordinates of the bin are present in spatiotemporal map
#' @description
#' Tests, if coordinates of the bin are present in spatiotemporal map
#' @param space spatiotemporal map
#' @param point vector of coordinates of the bin
#' @author Filip Dechterenko
#' @export
contains <- function(space, point) {
  return((point[1] %in% space$x.steps & 
            point[2] %in% space$y.steps & 
            point[3] %in% space$z.steps))
}


#' Creates steps for spatiotemporal maps
#' @description
#' Creates steps for spatiotemporal maps using parameters from config
#' @author Filip Dechterenko
#' @export
make.steps<-function() {
  xystep <- get("xystep", pkg_globals) 
  tstep  <- get("tstep", pkg_globals) 
  x <- seq(from = get("arenamin", pkg_globals), to = get("arenamax", pkg_globals), by = xystep);
  y = x; # we are assuming rectangular steps
  t <- seq(from = 0, to = get("tmax", pkg_globals) , by = tstep);
  steps = list(x = x, y = y, t = t, xystep = xystep, tstep = tstep); class(steps)="steps"
  return(steps)
}

#' Removes all data from spatiotemporal map
#' @description
#' Removes all data from spatiotemporal map
#' @param space spatiotemporal map
#' @author Filip Dechterenko
#' @export
clear.space = function(space) {
  stopifnot(is.space(space))
  space$data <- space$data * 0
  return(space)
}

#' Creates empty spatiotemporal map
#' @description
#' Creates empty spatiotemporal map with specified steps
#' @param xsteps steps for spatiotemporal map in x coordinate
#' @param ysteps steps for spatiotemporal map in y coordinate
#' @param tsteps steps for spatiotemporal map in t coordinate
#' @author Filip Dechterenko
#' @export
empty.space <- function(xsteps = NULL, ysteps = NULL, tsteps = NULL) {
  if (is.null(xsteps) | is.null(ysteps) | is.null(tsteps)) {
    steps  <- make.steps()
    xsteps <- steps$x; ysteps <- steps$y; tsteps <- steps$t
  }
  
  nx <- length(xsteps); ny <- length(ysteps); nt <- length(tsteps)
  data <- rep(0, nx * ny * nt) # 1:(nx*ny*nz)#
  dim(data) <- c(nx, ny, nt)
  space <- list(data = data,
               x.steps = xsteps, y.steps = ysteps, z.steps = tsteps)
  class(space) = "space"
  return(space)
}

#' Adds small spatiotemporal map (stm) to the big stm at specified point
#' @description
#' Adds small spatiotemporal map (stm) to the big stm at specified point
#' @param big spatiotemporal map
#' @param a small spatiotemporal map which will be added 
#' @param p coordinates where wil be stm a added to big
#' @param checks boolean, if we should run tests. We might want to set this to F to reduce running time. Default value is T
#' @author Filip Dechterenko
#' @export
add = function(big, a, p,checks=T) {
  # p is point in big, where [0,0,0] will be added
  if(checks) {
    stopifnot(is.space(big))
    stopifnot(is.space(a))
    stopifnot(is.compatible(big, a))
    stopifnot(contains(big, p))
  }
  xi <- center.axis(a$x.steps, big$x.steps, 0, p[1])
  yi <- center.axis(a$y.steps, big$y.steps, 0, p[2])
  zi <- center.axis(a$z.steps, big$z.steps, 0, p[3])
  big$data[xi$tgt, yi$tgt, zi$tgt] <- 
    big$data[xi$tgt, yi$tgt, zi$tgt] + a$data[xi$src, yi$src, zi$src]
  return(big)
}


#' Convolves scanpath with Gaussian
#' @description
#' Convolves scanpath with Gaussian. First, we bin scanpath to create discrete spatiotemporal map (stm). 
#' We add Gaussians to each position at stm
#' @param spath scanpath, which will be convolved
#' @param G small Gaussian used for convlution
#' @param checks boolean, if we should run tests. We might want to set this to F to reduce running time. Default value is T
#' @author Filip Dechterenko
#' @export
smooth.space <- function(spath, G, checks = T) {
  V <- empty.space()
  df <- bin.trajectory(spath)
  for (i in 1:nrow(df)) {
    r <- df[i,]
    G2 <- G; G2$data <- G2$data * r$value
    V <- add(V, G2, c(r[1], r[2], r[3]), checks)
  }
  
  return(V)
}

#' Sum spatiotemporal fixation maps
#' @description
#' Sum spatiotemporal fixation maps
#' @param SVs list of spatiotemporal fixation maps
#' @author Filip Dechterenko
#' @export
sum.spaces<-function(SVs) {
   return(Reduce("+", SVs, empty.space()))
  
}


#' Plus operator for two spatiotemporal fixation maps
#' @description
#' Plus operator for two spatiotemporal fixation maps
#' @param V1 first spatiotemporal fixation map
#' @param V2 second spatiotemporal fixation map
#' @author Filip Dechterenko
#' @export
`+.space`<-function(V1, V2){
   stopifnot(is.summable(V1, V2))
  V1$data <- V1$data + V2$data
  return(V1)
}

#' Visualizes scanpath space
#' @description
#' Visualizes scanpath space using ggplot. Three projections to 2D plane are shown (xy,yt and xt) 
#' @param space spatiotemporal map
#' @author Filip Dechterenko
#' @export
plot.space <- function(space) {
  layout(matrix(c(1,2,3,4),2,2, byrow = T))
  opar <- par(mar = c(2,2,2,2))
  xy <- apply(space$data, c(1:2), sum)
  xz <- apply(space$data, c(1,3), sum)
  yz <- apply(space$data, c(3,2), sum)
  mini <- min(c(xy,xz,yz)); maxi = max(c(xy,xz,yz)); 
  limit <- max(abs(c(mini,maxi)))
  breaks <- seq(-limit - 1, limit + 1, length.out = 22)
  colors <- c("red","white","blue")
  colors <- colorRampPalette(c("red", "white", "blue"))(21)
  xs <- step.size(space$x.steps); xr <- c(-xs, +xs)
  ys <- step.size(space$y.steps); yr <- c(-ys, +ys)
  zs <- step.size(space$z.steps); zr <- c(-zs, +zs)
  
  image(space$x.steps, space$y.steps, xy, 
        breaks = breaks, col = colors, axes = T, 
        xlim = range(space$x.steps) + xr, ylim = range(space$y.steps) + yr,
        xlab = "", ylab = "", main = "XY"); box()
  image(space$z.steps, space$y.steps, yz, 
        breaks = breaks, col = colors, axes = T, 
        xlim = range(space$z.steps) + zr, ylim = range(space$y.steps) + yr,
        xlab = "", ylab = "", main = "ZY"); box()
  image(space$x.steps, space$z.steps, xz, 
        breaks = breaks, col = colors, axes = T, 
        xlim = range(space$x.steps) + xr, ylim = range(space$z.steps) + zr,
        xlab = "", ylab = "", main = "XZ");   box()
  par(opar)
  layout(1)
}

#' Creates small gaussian
#' @description
#' Creates small gaussian in form of spatiotemporal fixation map 
#' @param steps objects with step sizes for individual dimension
#' @param xsd standard deviation of gaussian in x coord. Default value is taken from config
#' @param ysd standard deviation of gaussian in y coord. Default value is taken from config
#' @param tsd standard deviation of gaussian in t coord. Default value is taken from config
#' @author Filip Dechterenko
#' @export
gaussian.mask <- function(steps = NULL, xsd = NULL, ysd = NULL, tsd = NULL) {
  if(is.null(xsd) | is.null(ysd) | is.null(tsd)) {
    xsd <- get("xsd", pkg_globals)
    ysd <- get("ysd", pkg_globals)
    tsd <- get("tsd", pkg_globals)
  }
  
  if (is.null(steps)){
    steps <- make.steps.gauss(sdx = xsd, sdy = ysd, sdt = tsd)
  }
  
  xs <- steps$x; ys <- steps$y; zs <- steps$t
  g0 <- empty.space(xs, ys, zs)
  m  <- mesh.grid(g0)
  
  g0$data <- dnorm(m$x, 0, xsd) * dnorm(m$y, 0, ysd) * dnorm(m$z, 0, tsd)
  return(g0)
}

#' Creates steps for 3D gaussian
#' @description
#' creates steps which will be used for creating 3D gaussian. Those steps will be used for creating spatiotemporal map consisting of gaussian filter
#' @param xystep size of step in spatial coordinates. Default value is taken from config
#' @param tstep size of step in spatial coordinates. Default value is taken from config
#' @param sdx standard deviation of gaussian in x coord. Default value is taken from config
#' @param sdy standard deviation of gaussian in y coord. Default value is taken from config
#' @param sdt standard deviation of gaussian in t coord. Default value is taken from config
#' @author Filip Dechterenko
#' @export
make.steps.gauss <- function(xystep = NULL, tstep = NULL, sdx = NULL, sdy = NULL, sdt = NULL) {
  if(is.null(xystep)) {xystep<-get("xystep", pkg_globals)}
  if(is.null(tstep)) {tstep<-get("tstep", pkg_globals)}
  
  if (is.null(sdx)){sdx <- get("xsd", pkg_globals)}
  if (is.null(sdy)){sdy <- get("ysd", pkg_globals)}
  if (is.null(sdt)){sdt <- get("tsd", pkg_globals)}
  
  sd3x <- ceiling(3 * sdx / xystep) * xystep
  sd3y <- ceiling(3 * sdy / xystep) * xystep
  sd3t <- ceiling(3 * sdt / tstep) * tstep
  
  x <- seq(-sd3x, sd3x, by = xystep)
  y <- seq(-sd3y, sd3y, by = xystep)
  t <- seq(-sd3t, sd3t, by = tstep)
  
  steps <- list(x = x, y = y, t = t, xystep = xystep, tstep = tstep); class(steps)="steps"
  return(steps)
}

#' Tests if object is of class space
#' @description Simply test, if object is of class space
#' 
#' @param obj an object to be tested
#' @author Filip Dechterenko
#' @export
is.space <- function(obj) {
  return(class(obj) == "space")
}


#' Tests if object is of class space
#' @description Simply test, if object is of class space
#' 
#' @param obj an object to be tested
#' @author Filip Dechterenko
#' @export
get.coords <- function(space, index, values = T) {
  stopifnot(is.space(space))
  dims <- dim(space$data)
  nx <- dims[1]; ny <- dims[2]; nz <- dims[3]
  dd <- expand.grid(space$x.steps, space$y.steps, space$z.steps)
  colnames(dd) <- c("x","y","z")
  if (values) dd$value <- as.numeric(space$data)
  return(dd[index,])
}

#' Computes step size per dimension
#' @description
#' Computes step size per one dimension of object steps
#' @param steps one dimension of object steps
#' @author Filip Dechterenko
#' @export
step.size = function(steps) {
  ds<-  unique(zapsmall(diff(steps)))
  stopifnot(length(ds) == 1)
  return(ds)
}

#' Test, if two spatiotemporal map are compatible
#' @description
#' Test, if two spatiotemporal map (stm) are compatible (if their step sizes are gcd)
#' @param a first stm
#' @param a second stm
#' @author Filip Dechterenko
#' @export
is.compatible <- function(a,b) {
  ok <- T
  # same class
  ok <- ok & is.space(a) & is.space(b)
  # same steps
  ok <- ok & step.size(a$x.steps) == step.size(b$x.steps)
  ok <- ok & step.size(a$y.steps) == step.size(b$y.steps)
  ok <- ok & step.size(a$z.steps) == step.size(b$z.steps)
  return(ok)
}

#' Test, if two spatiotemporal map are summable
#' @description
#' Test, if two spatiotemporal map (stm) are summable
#' @param a first stm
#' @param a second stm
#' @author Filip Dechterenko
#' @export
is.summable = function(a,b) {
  ok <- T
  # same class
  ok <- ok & is.space(a) & is.space(b)
  # same steps
  ok <- ok & all(a$x.steps == b$x.steps)
  ok <- ok & all(a$y.steps == b$y.steps)
  ok <- ok & all(a$z.steps == b$z.steps)
  return(ok)
}

#' Creates meshgrid for spatiotemporal map
#' @description
#' Creates meshgrid for spatiotemporal map
#' @param s spatiotemporal map
#' @author Filip Dechterenko
#' @export
mesh.grid <- function(s) {
  stopifnot(is.space(s))
  dims <- dim(s$data)
  coo <- get.coords(s, T, values=F)
  xx <- coo$x; dim(xx) = dims
  yy <- coo$y; dim(yy) = dims
  zz <- coo$z; dim(zz) = dims
  return(list(x = xx, y = yy, z = zz))
}

#' Centers two spatiotemporal map at each other
#' @description
#' Centers two spatiotemporal map at each other.Returns part of a2, when a1 is placed over it, p1 over p2
#' @param a1 first spatiotemporal map
#' @param a2 second spatiotemporal map
#' @param p1 point in first spatiotemporal map
#' @param p2 point in second spatiotemporal map
#' @author Filip Dechterenko
#' @export
center.axis = function(a1, a2, p1, p2) {
  # returns part of a2, when a1 is placed over it, p1 over p2
  n1 <- length(a1); w1 = which(a1==p1)
  n2 <- length(a2); w2 = which(a2==p2)
  ix <- (1:n1) - w1 + w2
  ix <- ix[ix>0 & ix <= n2]
  ix1 <- ix - w2 + w1
  return(list(src = ix1, tgt = ix))
}

#' Normalize values in spatiotemporal map
#' @description
#' Normalize values in spatiotemporal map (z-transformation)
#' @param V spatiotemporal map
#' @author Filip Dechterenko
#' @export
normalize.space <- function(V) {
  V$data <- (V$data - mean(V$data)) / sd(V$data)
  return(V)
}

#' Loads spatiotemporal map
#' @description
#' Loads spatiotemporal map (stm) from file based on id, trial and optionaly directory, where are stm stored
#' @param id integer of participant id
#' @param trial integer of trial id
#' @param spaces.dir path to the directory, where stnm will be stored. Default value is taken from config
#' @author Filip Dechterenko
#' @export
load.space<-function(id, trial, prefix = "", spaces.dir = NULL) {
  
  load(get.stored.space.name(id, trial, prefix, spaces.dir))
  # SV object should be loaded
  return(SV)
}

#' Loads spatiotemporal map from file
#' @description
#' Loads spatiotemporal map object in form of discrete spatiotemporal map from RData file specified by the path. 
#' @param filename path to the spatiotemporal map
#' @author Filip Dechterenko
#' @export
load.space2 <- function(filename) {
  
  load(filename)
  # SV object should be loaded
  return(SV)
}

#' Loads spaces specified by list
#' @description
#' Loads spaces specified by list of values in format id.trial
#' @param list1 of ids in format id.trial
#' @author Filip Dechterenko
#' @export
load.spaces <- function(list1) {
  out <- plyr::llply(list1, function(x) {
    eye1.id <- unlist(strsplit(as.character(x), "[.]"))[1]
    eye1.tr <- unlist(strsplit(as.character(x), "[.]"))[2]
    SV <- load.space(as.numeric(eye1.id), as.numeric(eye1.tr))
    return(SV)
  })
  return(out)
}

#' Gets filename of stored spatiotemporal map
#' @description
#' Construct filename for stored spatiotemporal map based on id, trial, prefix and directory, where are spatiotemporal maps stored
#' @param id integer with participant id
#' @param trial integer with trial id
#' @param prefix optional prefix of the spatiotemporal map
#' @param space.dir directory with stored spatiotemporal maps. Default value is taken from config
#' @author Filip Dechterenko
#' @export
get.stored.space.name <- function(id, trial, prefix, spaces.dir = NULL){
  
  if (prefix != "") {
    prefix <- paste0(prefix, "-")
  }  
  if(is.null(spaces.dir)) {
    spaces.dir <- get("spaces-dir", pkg_globals)
  }
  
  
  fname <- sprintf("%sSV-%d-%d.Rdata", prefix, id, trial)
  
  return(file.path(spaces.dir, fname))
}

#' Flips spatiotemporal map by x and/or y axis
#' @description
#' Flips spatiotemporal map by x or y axis
#' @param V spatiotemporal object which will be flipped
#' @param type variable determining, around which axis we will flip map. 
#' @author Filip Dechterenko
#' @export
flip.space <- function(V, type = 1) {
  stopifnot(type == 1 | type == 2)
  V$data[,,] <- flipdim(V$data[,,], type)
  
  return(V)
}

#' Moves spatiotemporal map
#' @description
#' Moves spatiotemporal map by x and/or y coordinates
#' @param SV spatiotemporal object which will be moevd
#' @param dir.x integer meaning how many bins we should move in x axis
#' @param dir.y integer meaning how many bins we should move in y axis
#' @author Filip Dechterenko
#' @export
move.space <- function(SV, dir.x = 0, dir.y = 0) {
  data <- SV$data
  ####
  # move x
  ####
  if(abs(dir.x) > 0){
    nx <- dim(data)[1]
    if (dir.x > 0) {
      dir <- "R"
    }
    else {
      dir <- "L"
    }
    dir.x <- abs(dir.x)
  
    # name indexes for better abstraction
    start.part <- 1:(nx - dir.x)
    end.part <- (dir.x + 1):nx
    without.start.part<- 1:dir.x
    without.end.part <- (nx - dir.x + 1):nx
    if (dir == "R") { # we will move data right
      data[end.part,,] <- data[start.part,,]
      data[without.start.part,,] <- 0
    }
    else { # we will move data left
      data[start.part,,] <- data[end.part,,]
      data[without.end.part,,] <- 0
    }
  }
  ####
  # move y
  ####
  if(abs(dir.y) > 0) {
    ny <- dim(data)[2]
    if (dir.y > 0) {
      dir <- "R"
    }
    else {
      dir <- "L"
    }
    dir.y <- abs(dir.y)
  
    # name indexes for better abstraction
    start.part <- 1:(ny - dir.y)
    end.part <- (dir.y + 1):ny
    without.start.part <- 1:dir.y
    without.end.part <- (ny - dir.y + 1):ny
    if (dir == "R") { # we will move data right    
      data[,end.part,] <- data[, start.part,]
      data[,without.start.part,] <- 0
    }
    else { # we will move data left
      data[,start.part,] <- data[,end.part,]
      data[,without.end.part,] <- 0
    }
  }
  SV$data <- data
  
  return(SV)
}
