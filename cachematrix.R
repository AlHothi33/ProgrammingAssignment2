## The first function makeCacheMatrix uses lexical scoping to create a matrix that stores the input
##matrix. The function will also store the inverse of the function once it is calculated. Note
#that the function on its own will not compute anything as it will work with another function
#cacheSolve

makeCacheMatrix<-function(x=matrix()){
  cachedinverse<-NULL
  setMatrix<-function(newValue)
  {
    x <<-newValue
    cachedinverse<<- NULL #this clears the cache of a previous inverse calculation
  }
  
  getMatrix<-function()x
  
  setInverse <- function(inverse) cachedinverse <<-inverse
  
  getInverse<- function() cachedinverse
  
  list( #this list is the output of the makeCacheMatrix, where it returns a list of 
        #of four functions which will then be evaluated by cacheSolve. 
    setMatrix=setMatrix,
    getMatrix=getMatrix,
    setInverse=setInverse,
    getInverse=getInverse
  )
  
}
## The cacheSolve function below is created to work in conjunction with the makeCacheMatrix
##function created above. The below function's primary goal is to compute the inverse of the
##matrix stored within makeCacheMatrix

cacheSolve<- function(x,...){
  cachedinverse<-x$getInverse()
  if (!is.null(cachedinverse)){. #the function will first check if there a cached inverse in
                                  #makeCacheMatrix
    message("getting cached data")
    return(cachedinverse)
  }
  data<-x$getMatrix() #In case there is no cached inverse, the function will compute as shown.
  cachedinverse<-solve(data,...)
  x$setInverse(cachedinverse)
  cachedinverse
}
