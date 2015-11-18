## Function that uses scoping lexical to maintain global state in x and m for matrix generated

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y                 ## Mantains global state of x in other execution environment
  m<<-NULL              ## Preserves global state of m in other execution environment
}
get<-function() x
setmatrix<-function(solve) m <<- solve  ## Assigns inverse of the matrix to cache variable
getmatrix<-function() m                 ## Returns inverse of the matrix
## Returns a list of operations
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## Function that computes inverse of the matrix if the operation was not executed before over the input matrix

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()  ## Obtains m value of the parent execution environment
## If inverse of the matrix was computed before, this section returns the result
      if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
## If inverse of the matrix was not computed before, this section computes the inverse of the input matrix
    data1 <- x$get()
    m <- solve(data1, ...)
    x$setmatrix(m)
    m
}
