#' Prepare all scanpaths 
#' @description
#' Computes all scanpaths and stores them into RData files. Trajectories from response data.frame will be used and data taken from eyeData data.frame
#' @param response data.frame with participant responses
#' @param eyeData data.frame with eye gaze data
#' @param prefix we can store data with optional prefix, default is ""
#' @param spaces.dir path to the directory, where scanpaths will be stored. Default value is taken from config
#' @author Filip Dechterenko
#' @export
prepare.all.scanpaths <- function(response, eyeData, prefix = "", spaces.dir = NULL) {
  cat("Preparing all scanpath spaces\n")
  n  <- row(response)
  tn <- create.time.measure(n)
  for (i in 1:n) {
    r  <- response[i,]
    id <- r$id
    trial <- r$trial
    
    cat(sprintf(" * id: %d, tr: %d", id, trial))
    
    sp <- get.scanpath(id, trial, eyeData)
    V  <- scanpath.to.space(sp)
        
    save(V, file = get.stored.scanpath.name(id, trial, prefix, spaces.dir))
    
    tm <- update(tm)
    print(tm)   
    
  }
}

#' Gets scanpaths from data.frame 
#' @description
#' Selects scanpath from data.frame based on id and trial
#' @param id integer with participant id
#' @param trial integer with trial id
#' @param eyeData data.frame with eye gaze data
#' @author Filip Dechterenko
#' @export
get.scanpath <- function(eid, etrial, eyeData) {
  tr <- filter(eyeData, id == eid, trial == etrial)
  spath <- list("x" = tr$x, "y" = tr$y, "t" = tr$time)
  class(spath) <- "spath"
  return(spath)
}

#' Gets filename of stored scanpath 
#' @description
#' Construct filename for stored scanpath based on id, trial, prefix and directory, where are scanpaths stored
#' @param id integer with participant id
#' @param trial integer with trial id
#' @param prefix optional prefix of the scanpath
#' @param space.dir directory with stored scanpaths. Default value is taken from config
#' @author Filip Dechterenko
#' @export
get.stored.scanpath.name <- function(id, trial, prefix = "", spaces.dir = NULL){
  if (prefix != "") {
    prefix <- paste0(prefix, "-")
  }
  if(is.null(spaces.dir)) {
    space.dir <- get("spaces-dir", pkg_globals)
  }
  
  fname <- sprintf("%sV-%d-%d.Rdata", prefix, id, trial)
  
  return(file.path(spaces.dir, fname))
}

#' Bins trajectory
#' @description
#' Bins trajectory to bins specified by parameters in config
#' @param spath scanpath object which will be binned into discrete spatiotemporal map.
#' @author Filip Dechterenko
#' @export
bin.trajectory <- function(spath) {
  steps <- make.steps()
  
  sx <- spath$x; sy <- spath$y; st <- spath$t / 1000;
  xyfac <- 1 / steps$xystep
  tfac  <- 1 / steps$tstep
  sx1 <- round(sx * xyfac) / xyfac;
  sy1 <- round(sy * xyfac) / xyfac;
  st1 <- round(st * tfac) / tfac;
    
  df <- data.frame(sx1, sy1, st1, value <- 1)
  df2 <- df %>% group_by(sx1, sy1, st1) %>% summarise(value = sum(value))
  return(df2)
}

#' Test if object is of class scanpath
#' @description
#' Test if object is of class scanpath
#' @param obj object which will be tested for scanpath membership
#' @author Filip Dechterenko
#' @export
is.spath <- function(obj) {
  return(class(obj) == "spath")
}

#' Converts scanpath to space
#' @description
#' Converts scanpath to disrete spatiotemporal map
#' @param spath scanpath, which will be converted
#' @param min.x minimum value in x coordinate, default value is set from config
#' @param min.y minimum value in y coordinate, default value is set from config
#' @param min.t minimum value inty coordinate, default value is 0
#' @author Filip Dechterenko
#' @export
scanpath.to.space <- function(spath, min.x = NULL, min.y = NULL, min.t = 0) {
  V <- empty.space()
  stopifnot(step.size(V$x.steps) == step.size(V$y.steps))
  
  if(is.null(min.x)) {
    min.x <- get("arenamin", pkg_globals)
  }
  if(is.null(min.y)) {
    min.y <- get("arenamin", pkg_globals)
  }
  
  xystep <- step.size(V$x.steps); tstep <- step.size(V$z.steps);
  
  
  sx <- spath$x; sy <- spath$y; st <- spath$t/1000;
  sx1 <- valueToIndex(sx, min.x, xystep);
  sy1 <- valueToIndex(sy, min.y, xystep);
  st1 <- valueToIndex(st, min.t, tstep);
  
  df <- data.frame(sx1, sy1, st1, value = 1)
  df <- df %>% group_by(sx1, sy1, st1) %>% summarise(value = sum(value))
  
  V$data[as.matrix(df[,1:3])]=df$value

  return(V)
}

#' Gets index for given value
#' @description
#' Gets index for given value based on minimum value and stepsize
#' @param x value to be converted to index
#' @param min.x minimum value of x
#' @param step.x stepsize of x
#' @author Filip Dechterenko
#' @export
valueToIndex<-function(x,min.x,step.x) {
  return(round((x - min.x) / step.x) + 1);
}

#' Loads scanpath from file
#' @description
#' Loads scanpath from file based on id, trial and optionaly directory, where are scanpaths stored
#' @param id integer of participant id
#' @param trial integer of trial id
#' @param spaces.dir path to the directory, where scanpaths will be stored. Default value is taken from config
#' @author Filip Dechterenko
#' @export
load.scanpath <- function(id, trial, spaces.dir) {
  
  load(get.stored.scanpath.name(id,trial,spaces.dir))
  # V object should be loaded
  return(V)
}

#' Loads scanpath from file
#' @description
#' Loads scanpath object in form of discrete spatiotemporal map from RData file specified by the path. 
#' @param filename path to the scanpath
#' @author Filip Dechterenko
#' @export
load.scanpath2 <- function(filename) {
  
  load(filename)
  # V object should be loaded
  return(V)
}

#' Flips scanpath by x and/or y axis
#' @description
#' Flips scanpath by x and/or y axis
#' @param spath scanpath object which will be flipped
#' @param flip.x logical, if we should flip x axis
#' @param flip.y logical, if we should flip y axis
#' @author Filip Dechterenko
#' @export
flip.scanpath <- function(spath, flip.x = F, flip.y = F) {
  if (flip.x == T) {
    spath$x <- spath$x * -1
  }
  if(flip.y == T) {
    spath$y <- spath$y * -1
  }
  return(spath)
    
}

