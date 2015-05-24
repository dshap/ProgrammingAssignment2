## Speed up matrix inverse calculation using cache

#  Create a set of functions for inverted matrix cache
makeCacheMatrix <- function( x = matrix() ) {

	# Cache storage
    inv_cache <- NULL;
  
  	# Matrix set
    set <- function(y) {
      x <<- y;
      inv_cache <<- NULL;
    }

    # Matrix get
    get <- function() {
    	x;
    }

    # Matrix inversion cache update
    setInvCache <- function( inv ) {
    	inv_cache <<- inv;
    }

    # Matrix inversion cache read
    getInvCache <- function() {
    	inv_cache;
    }

    # Announce functions to the working environment
    list(
    	set = set,
    	get = get,
    	setInvCache = setInvCache,
    	getInvCache = getInvCache
    );

}

# Calculate the inverse of the matrix with cache
cacheSolve <- function( x, ... ) {
    
    # Try to get cached data
    inv <- x$getInvCache();

    # Cache exists
    if ( !is.null(inv) ) {
    	message( "getting cached data" );
        return( inv );
    }

    # No cached data, calculate matrix inverse
    data <- x$get();
    inv <- solve( data, ... );

    # Update cache
    x$setInvCache( inv );

    # Return the result
    return( inv );
}


## Testing 
#
# source("cachematrix.R");
# m <- makeCacheMatrix();
# m_sz = sample( 2:5, 1 );
# m$set( matrix( round( runif( m_sz * m_sz, 1, 9 ) ), m_sz, m_sz ) );

## Source data
# m$get()
#
##      [,1] [,2] [,3]
## [1,]    2    5    6
## [2,]    8    3    2
## [3,]    5    4    5


## First run
#
# cacheSolve(m);
#
##            [,1]        [,2]       [,3]
## [1,] -0.2058824  0.02941176  0.2352941
## [2,]  0.8823529  0.58823529 -1.2941176
## [3,] -0.5000000 -0.50000000  1.0000000


## Second run
#
# cacheSolve(m);
#
## getting cached data
##            [,1]        [,2]       [,3]
## [1,] -0.2058824  0.02941176  0.2352941
## [2,]  0.8823529  0.58823529 -1.2941176
## [3,] -0.5000000 -0.50000000  1.0000000
