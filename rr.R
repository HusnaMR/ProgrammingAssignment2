@@ -1,15 +1,33 @@
  ## Put comments here that give an overall description of what your
  ## functions do
  
  ## Write a short comment describing this function
  ##  This function sets the special "matrix", then, get the value, sets the inv matrix and gets the inv matrix
  
  makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    get<-function(){x}
    setInverse<-function(inverse){inv<<-inverse}
    getInverse<-function(){inv}
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
    
  }
  
  
  ## Write a short comment describing this function
  ## This function first check if the inverse has been calculated, and if it is it returns the value, if it isn´t, calculates it
  
  cacheSolve <- function(x, ...) {
    inv<-x$getInverse()
    if(!is.null(inv)){
      message("getting cahced data")
      return(inv)
    }
    mat<-x$get()
    inv<-solve(mat,...)
    x$setInverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
  }
  