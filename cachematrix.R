## I set the input x as a matrix
## and then set the solved value "inv" as a null
##then I changed every reference to "mean" to "inverse"

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<-function(y){
      x<<-y
      inv<<-NULL
  }
  get<-function() {x}
  setInverse<-function(inverse) {inv<<-inverse}
  getInverse<-function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
 