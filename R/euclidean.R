#' euclidean's algorithm
#' it finds the GCD of two numbers
#' @param x the first number
#' @param y the second number
#' @return the GCD of two numbers
#' @references
#' https://en.wikipedia.org/wiki/Euclidean_algorithm.
#' @export
euclidean<-function(x,y){
  if(!(is.numeric(x)) | !(is.numeric(y))){
    stop("invalid")
  }
  else{
    while(y!=0){
      r<-x%%y
      x<-y
      y<-r
    }
  }
  return(x)
}

