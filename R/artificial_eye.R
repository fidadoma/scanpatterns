#' Generates random scan pattern as random walk
#'
#' @param id this parameter normally denotes id of the subject, here it can be used for defining the variability
#' @param trial this parameter normally denotes id of the trial, here it can be used for defining the variability
#' @param track.id this parameter normally denotes id of the trajectory, here it can be used for defining the variability
#' @param t max time coordinate. Default 1000
#' @param arenamin min coordinates for arena (assuming rectangular coordinates)
#' @param arenamax min coordinates for arena (assuming rectangular coordinates)
#' @param interp.fac interpolation factor. Default 1 (no interpolation between samples). Larger number denotes number of samples that are created between the generated samples.
#' @param starting.point starting point for random walk. Default is center c(0,0)
#' @param sigma 2x2 matrix setting standard deviation for bivariate Gaussian distribution, default is 0.05 * I, where I is an identity matrix
#'
#' @return
#' @export
#'
#' @examples
generate.random.eye <- function(id = NULL, trial = NULL, track.id, t = 1000, arenamin = -15, arenamax = 15, interp.fac = 1, starting.point = c(0,0), sigma = matrix(c(0.05, 0, 0, 0.05), 2, 2)) {
  if(any(starting.point<arenamin) | any(starting.point>arenamax)) {warning("starting point is outside arena, it is not ensured that it will converge",immediate.=T)}
  if(t < 2) {stop("t should be greater than 1")}
  if(length(starting.point) != 2) {stop("mu should have two numbers")}
  if(any(dim(sigma) != c(2,2))) {stop("sigma should be 2x2 matrix")}
  require(MASS); require(pracma)
  mu <- starting.point
  
  t.small <- round(t / interp.fac)
  x <- numeric(t.small)
  y <- numeric(t.small)
  last.point <- mvrnorm(1, mu, sigma)
  
  
  for (i in 1:t.small) {
    should.repeat <- T
    while(should.repeat) {
      curr.point <- mvrnorm(1, last.point, sigma)
      
      if (any(curr.point > arenamax | curr.point < arenamin)){
        should.repeat <- T
      }
      else {
        should.repeat <- F
      }
    }
    
    x[i] <- curr.point[1]
    y[i] <- curr.point[2]
    
    last.point <- curr.point
  }
  
  t.resampled <- seq(0, t, length.out = t.small)
  x <- interp1(t.resampled, x, 1:t)
  y <- interp1(t.resampled, y, 1:t)
  stopifnot(length(x) == t)
  
  eye <- list()
  class(eye) <- "eye"
  
  eye$id    <- id
  eye$trial <- trial
  
  eye$track.id <- track.id
  
  eye$time <- 1:t
  
  eye$xyt <- data.frame(x,y,1:t)
  
  row.names(eye$xyt) <- NULL
  colnames(eye$xyt)=c("x","y","t")
  
  attr(eye, which = "arena.width")  <- get("arenamax", pkg_globals)
  attr(eye, which = "arena.height") <- get("arenamax", pkg_globals)
  
  stopifnot(is.valid.eye(eye))
  return(eye)
}


#' Saves all random scan patterns as eye objects and stm objects into file for faster calculations
#' @note This function will be removed in the future as it is too specific
#' @param traj list with generated scan patterns
#' @param spaces.dir output dir for stm maps, default is taken from the package
#' @param eye.dir ouput dir for eye.dir, default is taken from the package
#' @param verbose whether the function should be verbose
#'
#' @return
#' @export
#'
#' @examples
save.spaces.and.eye <- function(traj, spaces.dir = NULL, eye.dir = NULL, verbose=T){
  
  stopifnot(max(laply(traj,function(e) max(e$xyt[,c("x","y")]))) < get("arenamax", pkg_globals))
  stopifnot(min(laply(traj,function(e) min(e$xyt[,c("x","y")]))) > get("arenamin", pkg_globals))
  
  if(is.null(spaces.dir)){
    spaces.dir <- get("spaces-dir", pkg_globals)
  }
  if(is.null(eye.dir)){
    eye.dir <- get("eye-dir", pkg_globals)
  }
  
  if(!file.exists(spaces.dir)){
    dir.create(spaces.dir, recursive=T)
  }
  if(!file.exists(eye.dir)){
    dir.create(eye.dir, recursive=T)
  }  
  
  # compute scanpath spaces and scanpaths
  
  G <- gaussian.mask()
  
  if (verbose) {
    progress="time"
  }
  else {
    progress="none"
  }
  plyr::l_ply(traj, function(eye) {
    #if (eye$track.id==0) {
    
    sp <- as.scanpath(eye)
    V  <- scanpath.to.space(sp)
    SV <- smooth.space(sp,G)  
    
    save(V,   file = create.filename.eye_V_SV(eye, spaces.dir, "V-"))
    save(SV,  file = create.filename.eye_V_SV(eye, spaces.dir, "SV-"))
    save(eye, file = create.filename.eye_V_SV(eye, eye.dir))
    #}
  }, .progress = progress)
}


#' Creates filename for object for saving
#'
#' @param var1 object for which the name will be generated
#' @param ... additional parameters that are passed to the next function in the case when the object is not of type eye
#'
#' @return
#' @export
#'
#' @examples
create.filename.eye_V_SV <- function(var1, ...) {
  if (class(var1) == "eye") {
    return(create.filename.eye_V_SV2(var1$id, var1$trial, var1$track.id, ...))
  }
  else {
    return(create.filename.eye_V_SV2(var1, ...))
  }
}


#' Creates filename for object for saving
#'  @description General function for any object 
#'
#' @param id id part of the name
#' @param trial trial part of the name 
#' @param track.id track.id part of the name
#' @param eye.dir ouptu directory
#' @param type type of the object (first part of the name)
#'
#' @return
#' @export
#'
#' @examples
create.filename.eye_V_SV2<-function(id, trial, track.id, eye.dir = NULL, type = "eye-") {
  if (is.null(eye.dir)) {
    eye.dir <- get("eye-dir", pkg_globals)
  }
  
  return(file.path(eye.dir, sprintf("%s%d-%.2f-%.2f.RData", type, id, trial, track.id)))
    
  
}
