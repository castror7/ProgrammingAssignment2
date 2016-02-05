makeCacheMatrix <- function(mtrx = matrix()) 
{
  tmp = NULL
  set = function(st) 
  {
    mtrx <<- st
    tmp <<- NULL
  }
  get = function() mtrx
  setinv = function(inverse) tmp <<- inverse 
  getinv = function() tmp
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(mtrx, ...) 
{
  tmp = mtrx$getinv()
  if (!is.null(tmp))
  {
    message("getting cached data")
    return(tmp)
  }
  chd = mtrx$get()
  tmp = solve(chd, ...)
  mtrx$setinv(tmp)
  return(tmp)
}
