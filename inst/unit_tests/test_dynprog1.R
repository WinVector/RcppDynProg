
test_dynprog1 <- function() {
  x <- matrix(c(1,1,5,1,1,0,5,0,1), nrow=3)
  RUnit::checkTrue(RcppDynProg:::test_solvers(x, 3))
  
  x2 <- matrix(c(0.165439415490255, 0.982703358167782, 0.507731057703495, 
                    0.423818744486198, 0.866322142072022, 0.807251449674368, 0.0862272190861404, 
                    0.823423527413979, 0.907007380854338), nrow=3)
  RUnit::checkTrue(RcppDynProg:::test_solvers(x2, 3))
  
  x3 <- matrix(c(0.15401595, 0.04822183, 0.19091068, 0.45166874, 0.91731301, 0.65618810,
                 0.35495444, 0.12153691, 0.56267106), nrow=3)
  RUnit::checkTrue(RcppDynProg:::test_solvers(x3, 3))
  
  set.seed(1515)
  for(i in 1:10) {
    x3 <- matrix(runif(9), nrow=3)
    RUnit::checkTrue(RcppDynProg:::test_solvers(x3, 3))
  }
  
  set.seed(16515)
  for(i in 1:10) {
    x3 <- matrix(runif(16), nrow=4)
    RUnit::checkTrue(RcppDynProg:::test_solvers(x3, 4))
  }
  
  
  
  
  # should be non-increasing in k
  # expected loss of using the mean of other points to
  # estimate each point
  inflated_var <- function(x) {
    meanx <- mean(x)
    n <- length(x)
    (n/(n-1))*sum((x-meanx)^2) # + 10
  }
  
  set.seed(253)
  g <- 10
  d <- data.frame(
    x = 1:(3*g),
    y = c(rnorm(g), 1 + rnorm(g), -1 + rnorm(g)))
  
  n <- nrow(d)
  x <- matrix(0, nrow=n, ncol=n)
  for(i in 1:n) {
    x[i,i] <- 100000 # big penalty
    if(i<n) {
      for(j in (i+1):n) {
        cost <- inflated_var(d$y[i:j])
        x[i,j] <- cost
        x[j,i] <- x[i,j]
      }
    }
  }
  
  slast <- NA
  for(k in 1:nrow(x)) {
    s1 <- solve_interval_partition_R(x, k)
    sc1 <- score_solution(x, s1)
    s2 <- solve_interval_partition(x, k)
    sc2 <- score_solution(x, s2)
    if(!(abs(sc1-sc2)<=1e-5)) {
      stop("R and C++ solutions disagree")
    }
    if(!is.na(slast)) {
      if(sc1>slast) {
        stop("solution was not monotone in k")
      }
    }
    slast <- sc1
  }

  invisible(NULL)
}

  
