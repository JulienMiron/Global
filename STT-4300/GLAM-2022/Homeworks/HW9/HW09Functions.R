
#========== HW09 Functions ========== 



# "Tent" basis#========== 

tf <- function(x,xj,j){
  dj <- xj * 0
  dj[j] <- 1
  approx(xj, dj, x)$y
}


# "Tent" design matrix:

tf.X <- function(x,xj){
  
  # Arguments : a vector of observations and a vector of knots.
  
  nk <- length(xj)
  n <- length(x)
  X <- matrix(NA, n, nk)
  for(j in 1:nk){
    X[,j] <- tf(x,xj,j)
  }
  X
}



# Cubic splines : #========== 

b_cub <- function(z,knot){abs(z-knot)^3}


# Cubic splines design matrix:

cub_X <- function(x,xj){
  
  # Arguments : a vector of observations and a vector of knots.
  
  nk <- length(xj)
  n <- length(x)
  X <- matrix(data = 1,nrow =  n, ncol = (nk+2))
  X[,2] <- x
  for(j in 1:nk){
    X[,(j+2)] <- b_cub(x,xj[j])
  }
  X
}

