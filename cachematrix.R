## The task of the two functions is to demonstrate "object" caching in R. The makeCacheMatrix function is able to cache 
## inverse of a matrix. The cacheSolve function takes as an argument the makeCacheMatrix object and see if the inverse of
## a matrix has already been cached by makeCacheMatrix object. If it is already calculated then the cachesolve function
## just returns the cached value rather than calculating the value itself. If the inverse matrix value is not cached then
## the cacheSolve funtion will calculate the inverse of the matrix and cache the inverse value in the makeCacheMatrix and
## then return the inverse matrix

##Note: It is possible to not "properly" use these two functions. For exmaple if you use makeCacheMatrix$setInverseMatrix
##function to put some "junk" value in it then the cachesolve funciton will not be able to detect that as the only
##check cacheSolve funciton has is to see if the cahce evalue is NULL or not

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(arg_matrix = matrix()){
        
        inverseMatrix <- NULL
        getMatrix <- function() arg_matrix
        getInverseMatrix <- function() inverseMatrix
        setInverseMatrix <- function(invMat) inverseMatrix <<- invMat
        
        list(getMatrix = getMatrix, getInverseMatrix = getInverseMatrix,
             setInverseMatrix = setInverseMatrix)}


## The cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x) {
        
        inverseMatrix <- x$getInverseMatrix()
        
        if(!is.null(inverseMatrix)) {
                message("Getting cached matrix data")
                return (inverseMatrix)
        }
        
        tmp_Matrix <- x$getMatrix()
        
        inverseMatrix <- solve(tmp_Matrix)
        
        x$setInverse(inverseMatrix)
        inverseMatrix
}
