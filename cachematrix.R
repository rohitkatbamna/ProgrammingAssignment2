###############################################################################
## Put comments here that give an overall description of what your
## functions do
###############################################################################
## Well the makeCacheMatrix function makes a Cache matrix function which is
## some what similar to matrix function accept for Caching matrix which
## are already done and then cachesolve takes this Cache matrix to find inverse
## of it.
###############################################################################
## Write a short comment describing this function
###############################################################################
## makeCacheMatrix takes a vector which are to be values of matrix and ... takes
## all the other values of matrix like rows,columns etc. Like the example shown
## this function stores matrix for caching if it has to be reterived]
###############################################################################
makeCacheMatrix <- function(x,...)
{
    m <- NULL
    set <- function(y,...)
    {
        x <<- matrix(y,...) #making x to free variable in makeCacheMatrix
        m <<- NULL #making m to free variable in makeCacheMatrix
    }
    get <- function() matrix(x,...)
    setinv <- function(solve,...) m <<- matrix(solve,...) # the thinking here
    # solve is a variable
    # rather than a function
    # as solve is looked
    # frist in makeCacheMatrix
    # then in Global Env.
    # then in pakages
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
###############################################################################
## Write a short comment describing this function
###############################################################################
## cachesolve take the above makeCacheMatrix function and ... argments for solve
## function just like in the example shown it checks to see if the matrix
## stored in the function of matrix or not and if it is not finds the inverse
## matrix of it by calling the matrix and putting in data function and then
## putting this matrix to find a inverse matrix.
###############################################################################
cacheSolve <- function(x,...)
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
