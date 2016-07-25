# Example of operations:
# > my_matrix <- matrix(c(1, 2, 3, 4), 2, 2)
# > my_matrix
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > a <- makeCacheMatrix(my_matrix)
# > cacheSolve(a)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(a)
# matrix is unchanged, getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

#To create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x)
{
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) m <<- Inverse
    getInverse <- function() m
    list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}

#To compute the inverse of a special "matrix" object,
#If inverse already computed, return previous calculation
cacheSolve <- function(x)
{
    m <- x$getInverse()
    if(!is.null(m)) {
        message("matrix is unchanged, getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}

#ignore