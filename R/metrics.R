# methods for computing metrics 

###################################
##            NSS                ##
###################################

#' Computes nss distance metric 
#' @description
#' Computes nss distance metric for eye object against spatiotemporal map
#' @param V scanpathwhich will be tested
#' @param SV stm map
#' @author Filip Dechterenko
#' @export
compute.nss.from.space <- function(V, SV) {
  stopifnot(is.spath(V) | is.space(V))
  if (class(V) == "spath") {
    V <- scanpath.to.space(V)
  }
  return(sum(c(V$data) * c((normalize.space(SV))$data)) / sum(c(V$data)))
}

#' Computes nss distance metric 
#' @description
#' Computes nss distance metric for one eye object against another 
#' @param eye1 corresponding scanpath will be loaded for this eye object
#' @param eye2 corresponding stm will be loaded for this eye object and scanpath will be compared against it
#' @author Filip Dechterenko
#' @export
nss.dist <- function(eye1, eye2){

  SV <- load.space(eye1$id, eye1$trial)
  V  <- load.scanpath(eye2$id, eye2$trial)
  
  compute.nss.from.space(V, SV)
  
}

#' Computes correlation distance metric for two lists
#' @description
#' Computes correlation distance metric between two lists of spatiotemporal maps (stm) or ids of eye gaze data.
#' In second case corresponding stm are loaded from path specified in config or in parameter
#' @param list1 first list with stm or ids in format id.trial
#' @param list2 second list with stm or ids in format id.trial
#' @param spaces.dir optional parameter with path to the stored stm maps. If missing, default value is used from config
#' @author Filip Dechterenko
#' @export
corr.for.two.list <- function(list1, list2, spaces.dir = NULL){
  
  EV1 <- empty.space()
  EV2 <- empty.space()
  plyr::l_ply(list1, function(x) {
    
    if (class(x) == "space") { # we have list of spaces
      EV1 <<- x + EV1
    }
    else if(class(x) == "character") { # we have list of ids
      eye1.id <- unlist(strsplit(as.character(x), "[.]"))[1]
      eye1.tr <- unlist(strsplit(as.character(x), "[.]"))[2]
      SV <- load.space(as.numeric(eye1.id), as.numeric(eye1.tr), spaces.dir)
      EV1 <<- SV+EV1
    }
    
  })
  
  plyr::l_ply(list2, function(x) {
    if (class(x) == "space") { # we have list of spaces
      EV2 <<- x+EV2
    }
    else if(class(x) == "character") { # we have list of ids
      eye1.id <- unlist(strsplit(as.character(x), "[.]"))[1]
      eye1.tr <- unlist(strsplit(as.character(x), "[.]"))[2]
      SV <- load.space(as.numeric(eye1.id),as.numeric(eye1.tr),spaces.dir)
      EV2 <<- SV+EV2
    }
  })
  
  return(.cdm(c(EV1$data), c(EV2$data)))
  
  
}

#' Computes correlation distance metric 
#' @description
#' Computes correlation distance metric between two spatiotemporal maps or two eye gaze data (in second case corresponding stm are loaded)
#' @usage
#' corr.dist(SV1,SV2)
#' corr.dist(eye1,eye2)
#' @param SV1 first spatiotemporal map
#' @param SV2 second spatiotemporal map
#' @param eye1 first eye gaze data
#' @param eye2 second eye gaze data
#' @author Filip Dechterenko
#' @export
corr.dist<-function(eye1, eye2) {
  if (class(eye1) == "eye") {
    SV1 <- load.space(eye1$id, eye1$trial)
  } 
  else {
    SV1 <- eye1
  }
  if (class(eye2) == "eye") {
    SV2 <- load.space(eye2$id, eye2$trial)
  } 
  else {
    SV2 <- eye2
  }
  
  
  
  return(.cdm(SV1$data, SV2$data))
}

#' Internal method for correlation distance metric
#' @description
#' Computes correlation c between two vectors and return 1-c (in case when c>0)or 0 (otherwise) 
#' @param x first vector
#' @param y second vector
#' @author Filip Dechterenko
.cdm<-function(x, y) {
  if (var(x, y) != 0) {
    c <- cor(x, y)
    if (c < 0) {
      c <- 0
    }
    return(1 - c)
  }
  else {
    return(0)
  }
}

###################################
##        Mean distance          ##
###################################


mean_distance <- function(eye1, eye2) {
  df1 <- eye1$xyt 
  df2 <- eye2$xyt 
  
  d <- df1 %>% full_join(df2, by = "t") %>% mutate(d = sqrt((x.x - x.y) ^ 2 + (y.x - y.y) ^ 2)) %>% summarize(d = mean(d, na.rm = T))%>% .$d
  
  return(d)
}

###################################
##       Median distance         ##
###################################

median_distance <- function(eye1, eye2) {
  df1 <- eye1$xyt 
  df2 <- eye2$xyt 
  d <- df1 %>% full_join(df2, by = "t") %>% mutate(d = sqrt((x.x - x.y) ^ 2 + (y.x - y.y) ^ 2)) %>% summarize(d = median(d, na.rm = T))%>% .$d
  
  return(d)
}


#' Levenshtein distance
#' Computes levenshtein distance for two eye objects
#'
#' @param eye1 - object of class eye
#' @param eye2 - object of class eye 
#' @param gr - grid for levenshtein distance
#'
#' @return Returns Levenshtein distance
#' @export
#'
#' @examples
levenhstein_distance <- function(eye1, eye2, gr, treat_missing = "spec_char") {
  df1 <- eye1$xyt 
  df2 <- eye2$xyt 
  
  gridsize <- diff(gr$x[1:2]) 
  
  roundprec <- round(1/gridsize)
  
  df1 <- df1 %>% mutate(rx = plyr::round_any(x,gridsize), ry = plyr::round_any(y,gridsize))
  df2 <- df2 %>% mutate(rx = plyr::round_any(x,gridsize), ry = plyr::round_any(y,gridsize))
  s1 <- df1 %>% left_join(gr, by = c("rx"="x","ry"="y")) %>% as_data_frame() %>% select(rx,ry,chr,ix)
  s2 <- df2 %>% left_join(gr, by = c("rx"="x","ry"="y")) %>% as_data_frame() %>% select(rx,ry,chr,ix)
  s1 <- s1$chr
  s2 <- s2$chr
  n <- length(s1)
  m <- length(s2)
  d <- matrix(data = rep(-1, n*m), nrow = n, ncol = m)
  
  if(treat_missing == "spec_char") {
    s1[is.na(s1)] <- ".."
    s2[is.na(s2)] <- ".."
  }
  
  for (i in 2:n) {
    d[i,1] = d[i-1,1] + 1
  }
  for (j in 2:m) {
    d[1,j] = d[1,j - 1] + 1
  }
  for (i in 2:n) {
    for (j in 2:m) {
      d[i,j] = min(d[i-1,j] + 1, d[i,j-1] + 1, d[i-1,j-1] + (s1[i-1] != s2[j-1]))
    }
  }
  return((d[n,m]+1)/max(n,m))
}

correlation_distance <- function(eye1, eye2) {
  if (class(eye1) == "space" & class(eye2) == "space") {
    SV1 <- eye1
    SV2 <- eye2
    
  } else {
    G <- gaussian.mask()
    if(class(eye1) == "eye") {
      sp1 <- as.scanpath(eye1)  
      if(max(abs(sp1$x)) > 15 | max(abs(sp1$y)) > 15){
        return(NA)
      }
      SV1 <- smooth.space(sp1, G)
    }
    if(class(eye2) == "eye") { # class of eye2 is eye
      sp2 <- as.scanpath(eye2)
      if(max(abs(sp2$x)) > 15 | max(abs(sp2$y)) > 15){
        return(NA)
      }
      SV2 <- smooth.space(sp2, G)
    }
  }
  
  cd <- .cdm(c(SV1$data),c(SV2$data))
  return(cd)
}



frechet_distance <- function(eye1, eye2) {
  df1 <- eye1$xyt 
  df2 <- eye2$xyt 
  
  require(pdist)  
  xy1 <- df1 %>% select(x,y) %>% as.matrix()
  xy2 <- df2 %>% select(x,y) %>% as.matrix()
  distm <- pdist(xy1,xy2)
  #eucdist <- function(i,j){
  #  return(sqrt((df1$x[i] - df2$x[j])^2 + (df1$y[i] - df2$y[j])^2))
  #}
  
  n  <- nrow(df1)
  m  <- nrow(df2)
  ca <- matrix(data = rep(-1, n * m), nrow = n, ncol = m)
  ca[1,1] <- distm[1, 1]
  for (i in 2:n) {
    ca[i,1] <- max(ca[i - 1, 1], distm[i,1])#eucdist(i, 1))
  }
  for (j in 2:m) {
    ca[1,j] <- max(ca[1, j - 1], distm[1,j])#eucdist(1, j))
  }
  for (i in 2:n) {
    for (j in 2:m) {
      ca[i,j] <- max(min(ca[i - 1, j],ca[i, j - 1], ca[i - 1, j - 1]), distm[i, j])
    }
  }
  return(ca[n,m])
}

create_grid <- function(gridsize = 0.25, minx = -15, maxx = 15, miny = -15, maxy = 15){
  xAOI <- seq(minx, maxx, by = gridsize)
  yAOI <- seq(miny, maxy, by = gridsize)
  nx   <- length(xAOI)
  ny   <- length(yAOI)
  a <- rawToChar(as.raw(1:nx+as.numeric(charToRaw("a"))-1)) %>% strsplit("") %>% unlist()
  b <- rawToChar(as.raw(1:nx+as.numeric(charToRaw("a"))-1)) %>% strsplit("") %>% unlist()
  gr <- expand.grid(a,b, stringsAsFactors = F) %>% as_data_frame() %>% transmute(chr = stringr::str_c(Var1,Var2)) 
  xy <- expand.grid(xAOI, yAOI) %>% transmute(x = Var1, y = Var2)
  
  gr <- gr %>% mutate(ix = 1:n()) %>% cbind(xy) # grid computed
  return(gr)
}
