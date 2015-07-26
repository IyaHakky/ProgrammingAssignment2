## makeCacheMatrix allows to create a matrix as an object and than it is able to cache it's inverse  
## The function works similar to "makeVector" function from the example. 


makeCacheMatrix <- function(matr = matrix()) {
  solvedMatrix <- NULL
  setMatrix <- function(newMatrix) {
    matr <<- newMatrix
    solvedMatrix <<- NULL
  }
  getMatrix <- function() matr
  setSolved <- function(InversedMatrix) solvedMatrix <<- InversedMatrix
  getSolved <- function() solvedMatrix
  list(set = setMatrix, get = getMatrix,
       setSolved = setSolved,
       getSolved = getSolved)
}


## cacheSolve checks first if the inverse to the matrix, returned by makeCacheMatrix above, has been already calculated. 
## If so, it takes the data from cache and thraws a message "getting cached data". Else it calculates the 
## inverse of x and that stores it in cashe. The function works similar to cachemean function from the example description.

cacheSolve <- function(CacheMatrix, ...) {
  SolvedMatrix <- CacheMatrix$getSolved()
  if(!is.null(SolvedMatrix)) {
    message("getting cached data")
    return(SolvedMatrix)
  }
  data <- CacheMatrix$get()
  SolvedMatrix <- solve(data, ...)
  CacheMatrix$setSolved(SolvedMatrix)
  SolvedMatrix
}
