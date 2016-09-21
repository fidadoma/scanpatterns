# valid.eyes object - it is basically data.frame with stored eyes as rows
# we should be able to decompose it to the individual eye objects

#' Converts data frame to valid.eyes data.frame
#' @description
#' Converts data frame to valid.eyes data.frame. Slices data frame per each pair id, trial and tries to convert each slice into eye object. If all conversions succeed, class valid.eyes is added to the data frame. If data.frame could not be converted, method returns NULL
#' @param df data.frame which should contain valid eyes
#' @author Filip Dechterenko
#' @export
as.valid.eyes.data.frame<-function(df) {
  stopifnot(any(class(df)=="data.frame"))
  stopifnot(c("id","trial","track","x","y","time") %in% colnames(df))
  df2<-df
  df2$uniq.id<-paste(df2$id,df2$trial,sep=".")
  is.valid.eyes<-T
  
  is.valid.eyes<-all(daply(df2,.(uniq.id),function(dfx) {   
    return(is.convertible.to.eye(dfx))
    }))
  if(is.valid.eyes) {
    class(df)<-union(class(df),"valid.eyes")
    return(df)
  }
  else {
    return(NULL)
  }
  
}

#' Test if object is of class valid.eyes
#' @description
#' Test if object is of class valid.eyes
#' @param obj object, which should be tested for membership in both classes data.frame and valid.eyes
#' @author Filip Dechterenko
#' @export
is.valid.eyes<-function(obj){
  return(any(class(obj)=="data.frame")&any(class(obj)=="valid.eyes"))
}