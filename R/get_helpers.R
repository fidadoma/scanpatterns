get.trial <- function(id, tr, eyeData) {
  return(droplevels(subset(eyeData, id == id & trial == tr)))
}




