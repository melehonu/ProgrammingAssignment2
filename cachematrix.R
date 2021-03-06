## Put comments here that give an overall description of what your
## functions do

## This function is used to create a special matrix object that can cache its inverse.
## 1st I assign a blank matrix defined by the variable x.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m<<- matrix
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Here I wrote a function to solve for the inverse of my matrix x. And asked it to return the answer
## by getting it from the cached data if it exists or solving for the inverse of the matrix if not.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x $setmatrix(m)
  m
}
## Now I am setting up a 2 by 2 matrix with the values 1 to 4 and caching its inverse
> a <- makeCacheMatrix()
> a $set(matrix(1:4,2,2))
> cacheSolve(a)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
