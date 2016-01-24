## makeCacheMatrix takes the matrix as input; creates the inverse of
## the matrix and caches it in global env.
## cacheSolve function tries to access the inverse matrix if it is 
## available in the global cache and if not calculates the inverse
## and pushes that into the global cache.

## makeCacheMatrix take the matrix as an input and creates an object
## that returns functions to set inverse matrix,get inverse matrix and 
## inverse matrix. If there is no input passed to the function returns
## empty matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix<<-solve(x)
    set_inv_matrix<-function(x = matrix()) inv_matrix<<-x
    get_inv_matrix<-function() inv_matrix
    list(set_inv_matrix=set_inv_matrix,
         get_inv_matrix=get_inv_matrix,inv_matrix=inv_matrix)
}


## cacheSolve takes object created by makeCacheMatrix and gives 
## inverted matrix as an output from cache. If not present it calcula
## -tes and pushes the inverted matrix to the cache for future use.
## inputs are (obj_created_makeCacheMatrix,matrix) output 
## (inverted_matrix)


cacheSolve <- function(x, ...) {
    m<-x$get_inv_matrix()
    if(!all(is.na(m))) {
        message("getting cached matrix inverse")
        return(m)
    }
    inv_mat<-solve(...)
    x$set_inv_matrix(inv_mat)
    inv_mat
    ## Return a matrix that is the inverse of 'x'
}
