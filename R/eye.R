# eye object - simple form
# members:
#     - id: id of the subject
#     - trial: id of the trial 
#     - xyt: dested data frame with three columns corresponding to the x,y and time coordinates


#' Print an eye object
#'
#' @description Pretty prints eye object, it skips most of the eye gaze data to make it more readable
#'
#' @param eye an object of class eye
#' @author Filip Dechterenko
#' @export
print.eye<-function(eye) {
  # prints eye object
  cat("Id: ", eye$id,"\n")
  cat("Trial: ", eye$trial,"\n")
  cat("Eye positions:\n")
  print(as_data_frame(eye$xyt[,c("x", "y")]))
  cat("...\n")
  
  cat("Time:\n")
  t <- eye$xyt[, "t"]
  cat("  ",head(t),"...","",last(t),"\n")
    
}
#' Visualize an eye object
#' 
#' @description
#' Visualizes eye gaze data using nice plots from ggplot. Three projections to 2D plane are shown (xy, yt and xt) 
#' 
#' @param eye an object of class eye
#' @author Filip Dechterenko
#' @export
plot.eye<-function(eye, side_by_side = F) {
  # helper function to extract legend
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  stopifnot(is.eye(eye))
  #require(ggplot2)
  require(gridExtra)
  
  xyt <- eye$xyt
  n <- nrow(xyt)
  
  ar.w <- attr(eye,"arena.width")
  ar.h <- attr(eye,"arena.height")
  
  
  p.xy <- ggplot(xyt,aes(x = x, y = y))+
    geom_path(size=0.8)+
    ggtitle(paste0("Eye - id: ", eye$id, ", trial: ", eye$trial))+
    scale_x_continuous("x (in deg)",breaks = round(seq(-ar.w, ar.w, by = 5),1),limits=c(-ar.w,ar.w))+
    scale_y_continuous("y (in deg)",breaks = round(seq(-ar.h, ar.h, by = 5),1),limits=c(-ar.h,ar.h)) + 
    theme(aspect.ratio = 1)
  p.tx <- ggplot(xyt,aes(x = t,y = x))+
    geom_path(size=0.8)+
    ggtitle("x in time")+
    scale_x_continuous("time (in s)")+
    scale_y_continuous("x (in deg)",breaks = round(seq(-ar.w, ar.w, by = 5),1),limits=c(-ar.w,ar.w)) + 
    theme(aspect.ratio = 1)
  p.ty <- ggplot(xyt,aes(x = t, y = y))+
    geom_path(size=0.8)+
    ggtitle("y in time")+
    scale_x_continuous("time (in s)") +
    scale_y_continuous("y (in deg)",breaks = round(seq(-ar.h, ar.h, by = 5),1),limits=c(-ar.h,ar.h))+
    guides(col = guide_legend(nrow=4)) + 
    theme(aspect.ratio = 1)
  
  if (side_by_side) {
    n_col <- 3
  } else {
    n_col <- 2
  }
    
    
  grid.arrange(arrangeGrob(p.xy + theme(legend.position = "none"),
                           p.tx + theme(legend.position = "none"),
                           p.ty + theme(legend.position = "none"),
                           ncol = n_col))
  
}
#' Tests if object is of class eye
#' @description Simply test, if object is of class eye
#' 
#' @param obj an object to be tested
#' @author Filip Dechterenko
#' @export
is.eye<-function(obj){
  # test, if object is of class "eye"
  return(class(obj)=="eye")
}

#' Tests if object is a valid instance of eye object
#' @description This function tests, if object eye has a correct structure for an eye object. 
#' It checks following:
#' * eye gaze data are inside arena
#' * time vector has same length as eye gaze data
#' * differences between time samples are all the same
#' 
#' @param eye an eye object to be tested 
#' @author Filip Dechterenko
#' @export
is.valid.eye<-function(eye){
  # test, if object "eye" has all parameters valid
  stopifnot(is.eye(eye))
  
  ret.val <- T
  
  xy <- eye$xyt[,c("x", "y")]
  t  <- eye$xyt[,"t"]
  diff.time <- zapsmall(diff(t), digits = 4)
  ar_width <- attr(eye, "arena.width")
  ar_height <- attr(eye, "arena.height")
  # eye gaze data inside arena
  ret.val <- ret.val & max(xy$x, na.rm = T) <= ar_width  & min(xy$x,na.rm=T) >= -ar_width
  ret.val <- ret.val & max(xy$y, na.rm = T) <= ar_height & min(xy$y,na.rm=T) >= -ar_height
  
  # all differences between time samples are the same
  ret.val <- ret.val & max(diff.time) == min(diff.time)
  
  return(ret.val)
} 

#' Converts an eye to the scanpath
#' @description Converts an object of class eye to the object of class scanpath
#' 
#' @param eye an object of class eye
#' @author Filip Dechterenko
#' @export
as.scanpath<-function(eye) {
  stopifnot(class(eye) == "eye")
  
  # converts eye to scanpath
  ix <- complete.cases(eye$xy)
  eye.clean <- eye$xyt[ix,]
  spath <- list("x" = eye.clean$x, "y" = eye.clean$y, "t" = eye.clean$t)
  class(spath) <- "spath"
  return(spath)
}

#' Creates eye object from data frame
#' @description
#'   We need to have data.frame with eye data. This function selects data from data.frame specified by trial and id
#' @param id integer of participant id
#' @param trial integer of trial number
#' @param eye.data data.frame of class valid.eyes
#' @author Filip Dechterenko
#' @export
get.eye <- function(eid, etrial, eye.data, ...) {
  df.eye <-  dplyr::filter(eye.data, id == eid, trial == etrial)
  return(as.eye(df.eye,...))
}

#' Converts data frame to eye object
#' @description 
#' Data frame should contain data only with one id and one trial, otherwise it stops. This is a ideal function to by used with dplyr pipeline.
#'
#' @param df.eye data frame that will be converted to eye object
#' @param idname name of column that should be treated as id
#' @param trialname name of column that should be treated as trial id
#' @param timename name of column that should be treated as time coordinate
#' @param xname name of column that should be treated as x coordinate
#' @param yname name of column that should be treated as y coordinate
#'
#' @return
#' object of type eye
#' @export
#'
#' @examples
#' scanpatterns.set.parameters(list("max-time" = 40))
#' df <- data_frame(id = rep(1, 10), trial = rep(4,10), t = seq(1, 40, by = 4), x = runif(10, -5, 5), y = runif(10, -5, 5))
as.eye <- function(df.eye, idname = "id", trialname = "trial", timename = "t", xname = "x", yname = "y") {
  stopifnot(length(setdiff(c(idname,trialname, xname, yname, timename), colnames(df.eye))) == 0)
  stopifnot(length(unique(df.eye[[idname]])) == 1)
  stopifnot(length(unique(df.eye[[trialname]])) == 1)
  
  eid <- first(df.eye[[idname]])
  etrial <- first(df.eye[[trialname]])
  eye <- list()
  class(eye) <- "eye"
  
  eye$id <- eid
  eye$trial <- etrial
  #print(tr.id)
  
  if(get("max-time", pkg_globals) == -1) {
    min.time <- min(df.eye[[timename]])
    max.time <- max(df.eye[[timename]])
    etime <- seq(from = min.time, to = max.time, length.out = nrow(df.eye))
  } else {
    etime <- seq(from = get("min-time", pkg_globals),to=get("max-time", pkg_globals), by = get("step-time", pkg_globals))
  }
  
  
  eye$xyt <- as.data.frame(matrix(nrow = length(etime), ncol = 3))
  eye$xyt[,3] <- etime
  eye$xyt[etime %in% df.eye[[timename]], 1] <- df.eye[[xname]]
  eye$xyt[etime %in% df.eye[[timename]], 2] <- df.eye[[yname]]
  
  row.names(eye$xyt) <- NULL
  colnames(eye$xyt) <- c("x","y","t")
  
  attr(eye, which = "arena.width")  <- get("arenamax", pkg_globals)
  attr(eye, which = "arena.height") <- get("arenamax", pkg_globals)
  
  stopifnot(is.valid.eye(eye))
  
  return(eye)
}

#' Loads an eye object from file
#' @description
#' Loads eye object from RData file specified by id and trial. Pth to the directory with eye objects can be specified, or it uses default value from global parameters.
#' Eyes objects are expected to be stored in file eye%d_%d where first number is id and second is trial. If not, use function load.eye2 instead
#' @param id integer specifying participant
#' @param tr integer specifying trial
#' @param eye.dir optional parametr specifying path to the eye data. If missing, default value is used
#' @author Filip Dechterenko
#' @export
load.eye<-function(id, tr, eye.dir=NULL) {
  # loads eye object from file
  ne <- new.env()
  if (missing(eye.dir)) {
    path <- file.path(get("eye-dir", pkg_globals),sprintf("eye%d_%d.RData",id,tr))
  }
  else {  path<-file.path(eye.dir,sprintf("eye%d_%d.RData",id,tr))  }
  load(path, envir = ne)
  if(length(ls(env = ne)) > 1) {
    if(!exists(ne$eye)) {
      stop(sprintf("There were 2 or more objects in '%s' file and none of them was object named 'eye' ",path))
    }
    else {
      out<-ne$eye
      warning(sprintf("There were 2 or more objects in '%s' file. One of them was object named 'eye', so we loaded it ",path))
    }
  }
  if(length(ls(env = ne)) == 0) {error(sprintf("There were no object in '%s' file.",path))}
  if(length(ls(env = ne) == 1)) {
    out<-get(ls(env = ne)[1], ne)
  }
  return(out)
}

#' Loads an eye object from file
#' @description
#' Loads eye object from RData file specified by the path. 
#' @param filename path to the eye data
#' @author Filip Dechterenko
#' @export
load.eye2<-function(filename) {
  # loads eye object from file
  load(filename)
  return(eye)
}

#' Saves an eye object into the file
#' @description
#' Saves an eye object as RData file into directory with eye data 
#' @param eye object to be stored
#' @param eye.dir optional parametr specifying path to the eye data. If missing, default value from global variables is used.
#' @author Filip Dechterenko
#' @export
save.eye<-function(eye, eye.dir=NULL) {
  # saves eye object into file
  
  if (missing(eye.dir)) {
    path <- file.path(get("eye-dir", pkg_globals),sprintf("eye%d_%d.RData",eye$id,eye$trial))
  }
  else {
    path <- file.path(eye.dir, sprintf("eye%d_%d.RData",eye$id,eye$trial))
  }
  save(eye, file = path)
}


#' Saves all eye objects from into the file
#' @description
#' Saves all eye objects from valid.eyes data frame as RData files into directory with eye data 
#' @param df data.frame of class valid.eyes 
#' @param eye.dir optional parametr specifying path to the eye data. If missing, default value from global variables is used.
#' @param verbose boolean parameter specifying, if the output should be verbose. Default value is True
#' @author Filip Dechterenko
#' @export
save.all.eye <- function(df, eye.dir = NULL, verbose = T){
  if(verbose) {
    progress = "time"  
    cat("Saving all eye object as .RData..\n")
  }
  else {
    progress = "none"
  }
  if (missing(eye.dir)) {
    eye.dir <- get("eye-dir", pkg_globals)
  }
    
  if (!file.exists(eye.dir)){
    if (verbose){
      cat(sprintf("* Directory %s don't exist, creating..\n", eye.dir))
    }
    dir.create(eye.dir, recursive = T)
  }
  .saveeye <- function(dfx) {
    id <- unique(dfx$id)
    tr <- unique(dfx$trial)
    eye <- get.eye(id,tr,dfx)
    save.eye(eye, eye.dir)
    return(data.frame(x = id)) # dirty trick how to return data.frame for dplyr, all stuff is done as side effect
  }
  df %>% group_by(id, trial) %>% do({.saveeye(.)})
  
  if(verbose){
    cat("All eye object saved into directory cache/eye ..\n")
  }
}

#' Descriptive statistics for an eye object
#' @description
#' Descriptive statistics for an eye object. It produces min, max in x and y coordinates. Mean differences between time points and convex hull
#' @param eye an object of class eye
#' @author Filip Dechterenko
#' @export
describe.eye<-function(eye) {
  
  cha <- function(x, y){
    chull(x, y) -> i
    return(splancs::areapl(cbind(x[i],y[i])))
  }
  dist.between <- function(x, y) {
    return(sqrt(diff(x) ^ 2 + diff(y) ^ 2))
  }
  
  ok <- complete.cases(eye$xyt$x, eye$xyt$y)
  
  xyt <- eye$xyt
  if("track.id" %in% names(eye)) {
    df <- data_frame(id = eye$id,
                     trial = eye$trial,
                     track.id = eye$track.id,
                     mean.x = mean(xyt$x, na.rm = T),
                     mean.y = mean(xyt$y, na.rm = T),
                     sd.x   = mean(xyt$x, na.rm = T),
                     sd.y   = mean(xyt$y, na.rm = T),
                     max.x  = max(xyt$x, na.rm = T),
                     max.y  = max(xyt$y, na.rm = T),
                     min.x  = min(xyt$x, na.rm = T),
                     min.y  = min(xyt$y, na.rm = T),
                     conv.hull       = cha(xyt$x[ok], xyt$y[ok]),
                     mean.difference = mean(dist.between(xyt$x, xyt$y), na.rm = T),
                     sd.difference   = sd(dist.between(xyt$x, xyt$y), na.rm = T),
                     total.length    = sum(dist.between(xyt$x, xyt$y), na.rm = T))
  } else {
    df <- data_frame(id = eye$id,
                     trial = eye$trial,
                     mean.x = mean(xyt$x, na.rm = T),
                     mean.y = mean(xyt$y, na.rm = T),
                     sd.x   = mean(xyt$x, na.rm = T),
                     sd.y   = mean(xyt$y, na.rm = T),
                     max.x  = max(xyt$x, na.rm = T),
                     max.y  = max(xyt$y, na.rm = T),
                     min.x  = min(xyt$x, na.rm = T),
                     min.y  = min(xyt$y, na.rm = T),
                     conv.hull       = cha(xyt$x[ok], xyt$y[ok]),
                     mean.difference = mean(dist.between(xyt$x, xyt$y), na.rm = T),
                     sd.difference   = sd(dist.between(xyt$x, xyt$y), na.rm = T),
                     total.length    = sum(dist.between(xyt$x, xyt$y), na.rm = T))
    
  }
           
  return(df)
}

#' Moves eye gaze data in space (cart coord). Move is specified by carthesian coordinates
#' @description
#' Moves eye gaze data in space (in x and/or y coordinate)
#' @param eye an object of class eye
#' @param delta.x distance in x coordinate specifying the move in x coord
#' @param delta.y distance in y coordinate specifying the move in y coord
#' @author Filip Dechterenko
#' @export
move.eye <- function(eye, delta.x = 0, delta.y = 0) {
  eye$xyt$x <- eye$xyt$x + delta.x
  eye$xyt$y <- eye$xyt$y + delta.y
  return(eye)
}

#' Moves eye gaze data in space (polar coord)
#' @description
#' Moves eye gaze data in space. Move is specified by polar coordinates
#' @param eye an object of class eye
#' @param dist distance from the origin, default=0
#' @param angle argument specifying rotation from the top,default=0
#' @param deg are we using degrees or radians? Defualt are degrees
#' @author Filip Dechterenko
#' @export
move.eye.polar<-function(eye, dist = 0, angle = 0, deg = T) {
  if(deg) {
    angle <- deg2rad(angle)
  }   
    
  delta.xy <- pol2cart(c(angle, dist))
  
  eye$xyt$x <- eye$xyt$x + delta.xy[1]
  eye$xyt$y <- eye$xyt$y + delta.xy[2]
  return(eye)
}

#' Flips eye by x and/or y axis
#' @description
#' Flips eye by x and/or y axis
#' @param eye an object of class eye
#' @param flip.x logical, if we should flip x coord
#' @param flip.y logical, if we should flip y coord
#' @author Filip Dechterenko
#' @export
flip.eye<-function(eye, flip.x = F, flip.y = F) {
  stopifnot(class(flip.x) == "logical")
  stopifnot(class(flip.y) == "logical")
  if (flip.x) {
    eye$xyt$x <- (-eye$xyt$x)
  }
  if (flip.y) {
    eye$xyt$y <- (-eye$xyt$y)
  }
  
  return(eye)
}
#' Randomly move proportion of trajectory far away
#' @description
#' Randomly move proportion of trajectory far away  (by width of the arena)
#' @param eye an object of class eye
#' @param ratio fraction of trajectory which should be moved away
#' @author Filip Dechterenko
#' @export
incoherence.eye<-function(eye, ratio) {
  stopifnot(ratio <= 1 | ratio >= 0)
  if (ratio > 0) { # it behaves strange when we are computing 1:0
    n <- nrow(eye$xyt)
    n.incoh <- round(n * ratio)
    ix <- sample(n)
    movdeg <- attr(eye, "arena.width")
    eye$xyt[ix[1:n.incoh],c("x","y")] <- eye$xyt[ix[1:n.incoh],c("x","y")] + 10
  }
  return(eye)
}
