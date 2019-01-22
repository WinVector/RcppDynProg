

#' @importFrom utils combn
NULL

#' @importFrom wrapr stop_if_dot_args
NULL



#' Increasing whole-number sequence.
#'
#' Return an in increaing whole-number sequence from a to b inclusive (return integer(0) if none such). Allows for safe iteraton.
#' 
#' TODO: switch to wrapr version after next wrapr release.
#'
#' @param a scalar lower bound
#' @param b scalar upper bound
#' @return whole number sequence
#'
#' @examples
#'
#' # print 3, 4, and then 5
#' for(i in seqi(3, 5)) {
#'    print(i)
#' }
#'
#' # empty
#' for(i in seqi(5, 2)) {
#'    print(i)
#' }
#'
#' @noRd
#'
seqi <- function(a, b) {
  a = ceiling(a)
  b = floor(b)
  if(a>b) {
    return(integer(0))
  }
  base::seq(a, b, by = 1L)
}


#' Build all partitions into intervals.
#' 
#' @param n integer, sequence lenght to choose from.
#' @param kmax int, maximum number of segments in solution.
#' @return list of all partitions.
#' 
#' @examples 
#' 
#' all_partitions(4, 2)
#' 
#' @keywords internal
#' 
#' @export
#' 
all_partitions <- function(n, kmax = n) {
  # get shape of problem
  kmax <- min(kmax, n)
  syms <- seqi(2, n)
  
  res <- list(c(1, n+1))
  for(kf in seqi(1, kmax-1)) {
    ci = combn(syms, kf)
    for(j in seq_len(ncol(ci))) {
      soln <- sort(c(1, ci[, j, drop=TRUE], n+1))
      res <- c(res, list(soln))
    }
  }
  res
}



is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

#' compute the price of a partition solution (and check is valid).
#' 
#' @param x NumericMatix, for j>=i x(i,j) is the cost of partition element [i,...,j] (inclusive).
#' @param solution vector of indices
#' @return price
#' 
#' @examples 
#' 
#' x <- matrix(c(1,1,5,1,1,0,5,0,1), nrow=3)
#' s <- c(1, 2, 4)
#' score_solution(x, s)
#' 
#' @export
#' 
score_solution <- function(x, solution) {
  n <- nrow(x)
  ls <- length(solution)
  if(ls<2) {
    stop("solutions must have length at least 2")
  }
  if(ls>(n+1)) {
    stop("soltuions must have length no more than nrow(x)+1")
  }
  if(solution[1]!=1) {
    stop("solution[1] must equal 1")
  }
  if(solution[ls]!=(n+1)) {
    stop("solution[length(solution)] must equal nrow(x)+1")
  }
  if(!isTRUE(all(solution[-1]>solution[-ls]))) {
    stop("solution indices must be increasing")
  }
  if(!isTRUE(all(is.wholenumber(solution)))) {
    stop("solution must be wholenumbers")
  }
  score <- 0
  for(i in seqi(1, ls-1)) {
    score <- score + x[solution[i], solution[i+1]-1]
  }
  return(score)
}




test_solvers <- function(x, k) {
  msg <- NULL
  tryCatch({
    sl <- all_partitions(nrow(x), k)
    if(length(sl)<1) {
      stop("brute force didn't return any solutions")
    }
    for(si in sl) {
      if(!(length(si)<=(k+1))) {
        stop("brute solution too long")
      }
    }
    sc <- vapply(
      sl, 
      function(si) {
        score_solution(x, si)
      }, numeric(1))
    sm <- min(sc)
 
    
    soln1 <- solve_interval_partition_R(x, k)
    score1 <- score_solution(x, soln1)
    if(!(length(soln1)<=(k+1))) {
      stop("soln1 too long")
    }
    if(!(abs(score1-sm)<=1e-5)) {
      stop("R solution has wrong score")
    }
    
    soln2 <- solve_interval_partition_k(x, k)
    score2 <- score_solution(x, soln2)
    if(!(length(soln2)<=(k+1))) {
      stop("soln2 too long")
    }
    if(!(abs(score2-sm)<=1e-5)) {
      stop("C++ k solution has wrong score")
    }
    
    soln3 <- solve_interval_partition(x, k)
    score3 <- score_solution(x, soln3)
    if(!(length(soln3)<=(k+1))) {
      stop("soln3 too long")
    }
    if(!(abs(score3-sm)<=1e-5)) {
      stop("C++ solution has wrong score")
    }
    
    if(k>=nrow(x)) {
      soln4 <- solve_interval_partition_no_k(x)
      score4 <- score_solution(x, soln4)
      if(!(length(soln4)<=(k+1))) {
        stop("soln4 too long")
      }
      if(!(abs(score4-sm)<=1e-5)) {
        stop("C++ no_k solution has wrong score")
      }
    }
  },
  error = function(e) { msg <<- paste(as.character(e), sep = " ") }
  )
  if(!is.null(msg)) {
    return(msg)
  }
  
  return(TRUE)
}

