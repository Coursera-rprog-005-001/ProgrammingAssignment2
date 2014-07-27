## The first function is used to instanciate a matrix object.
## In order to not waste resources the inverse of this matrix
## will only be computed once, then cached thanks to the function "cacheSolve".

## taking a matrix as argument this function will cache the inverse of said matrix.
makeCacheMatrix <- function(M = matrix()) {
  M.Inv <- NULL # The inverse is set to NULL for now
  set <- function(y) {
    # "<<-" is a super-assignment.
    # Here it creates a variable in the global environment.
    M <<- y
    M.Inv <<- NULL
  }
  #Retun the matrix
  get <- function() M
  #Set the inverse of the matrix
  setInv <- function(Inv) M.Inv <<- Inv
  #Return the Inverse of the matrix
  getInv <- function() M.Inv
  #The function returns a list of objects (functions) to be used.
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Takes as argument an object from the function "makeCacheMatrix".
## Solves the inverse of a matrix, caching the result in the above function.
cacheSolve <- function(M, ...) {
  M.Inv <- M$getInv() # The inverse solution is the value from the former function.
  #If the value is not NULL, the inverse has been solved it is simply returned.
  if(!is.null(M.Inv)) {
    message("getting cached data")
    return(M.Inv)
  }
  data <- M$get() #Get the original matrix
  M.Inv <- solve(data, ...) #Solve  the inverse of the matrix.
  M$setInv(M.Inv) #Set the object Inv value as the new inverse solution.
  M.Inv #Return the Inversed matrix
}
