#creates a matrix object that can cache its inverse
makeCacheMatrix <-function(x = matrix()) {
    m <- NULL
    set <- function(y) {
          x <<-y
          m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#If no inverse is cached, the inverse will be computed and cached
#otherwise, return the cached inverse
cacheSolve <- function(x) {
  m <- x$getInverse()
  
  if(!is.null(m)) {
    #has cache data
    message("getting cached data")
    return(m)
  }
  
  #no cache data, compute the inverse and cache the data
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}