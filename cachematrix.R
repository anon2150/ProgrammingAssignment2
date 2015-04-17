###########################################################################
#       ASSIGNMENT 2 - LEXICAL SCOPING
###########################################################################


##  This function creates a special "matrix" object 
##  that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse        
        
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_m_inv <- function(solve) m <<- solve
        get_m_inv <- function() m
        list(set = set, get=get,
             set_m_inv = set_m_inv,
             get_m_inv = get_m_inv)
}

##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated (and the
##  matrix has not changed), then the cachesolve should retrieve the inverse
##  from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_m_inv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_m_inv(m)
        m
}
