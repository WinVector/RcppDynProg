

#' @importFrom stats approx lm predict
NULL

#' Solve for a piecewise linear partiton.
#' 
#' Solve for a good set of right-exclusive x-cuts such that the 
#' overall graph of y~x is well-approximated by a piecewise linear
#' function.
#' 
#' @param x numeric, input variable (no NAs).
#' @param y numeric, result variable (no NAs, same length as x).
#' @param ... not used, force later arguments by name.
#' @param w numeric, weights (no NAs, positive, same length as x).
#' @param penalty per-segment cost penalty.
#' @param min_n_to_chunk minimum n to subdivied problem.
#' @param min_seg positive integer, minimum segment size.
#' @param max_k maximum segments to divide into.
#' @param costs_fn function to produce cost matrix.
#' @param cost_f function to score intervals
#' @return a data frame appropriate for stats::approx().
#' 
#' @noRd
#' 
solve_for_partition_xs <- function(x, y, 
                                   ...,
                                   w = NULL,
                                   penalty = 0.0,
                                   min_n_to_chunk = 1000,
                                   min_seg = 1,
                                   max_k = length(x),
                                   cost_fn = lin_costs, cost_f = lin_cost) {
  wrapr::stop_if_dot_args(substitute(list(...)), "RcppDynProg::solve_for_partition_xs")
  n <- length(x)
  if(n<1) {
    return(NULL)
  }
  if(length(unique(x))<=1) {
    return(c(1, n+1))
  }
  if(length(w)<=0) {
    w = 1 + numeric(n)
  }
  chunk_size <- max(1, round(sqrt(n)))
  if(n<min_n_to_chunk) {
    chunk_size <- 1
  }
  d <- data.frame(x = x, y = y)
  d$orig_index <- seq_len(n)
  d <- d[order(d$x), , drop = FALSE]
  # build a smaller problem
  indices = sort(unique(c(1, seq(1, n, by = chunk_size), n)))
  # can only start indices where x differs
  is_dup <- c(FALSE, x[indices[-1]]==x[indices[-length(indices)]])
  indices <- indices[!is_dup]
  indices = sort(unique(c(1, indices, n)))
  # solve dynamic program
  xmat <- cost_fn(x, y, w, min_seg, indices)
  xmat <- xmat + penalty
  soln1 <- solve_interval_partition(xmat, max_k)
  if(length(soln1)<=2) {
    return(c(1, n+1))
  }
  # translate back to original indices
  soln1 <- c(indices[soln1[-length(soln1)]], n+1)
  # now polish interior boundaries
  # only run once under assumption dynprog has isolated
  # most long-range effects
  soln2 <- soln1
  R_INDEX_SHIFT = -1
  for(trial in seqi(1,5)) {
    old_soln = soln2
    for(ii in seqi(2, length(soln2)-1)) {
      low <- max(soln2[ii-1]+(1+min_seg), soln2[ii]-chunk_size)
      high <- min(soln2[ii+1]-(1+min_seg), soln2[ii]+chunk_size)
      if((low<high)&&(low<=soln2[ii])&&(high>=soln2[ii])) {
        best_can <- -1
        best_cost <- 0
        for(candidate in seqi(low, high)) {
          costi <- cost_f(x, y, w, min_seg, R_INDEX_SHIFT + soln2[ii-1], R_INDEX_SHIFT + candidate-1) + 
            cost_f(x, y, w, min_seg, R_INDEX_SHIFT + candidate, R_INDEX_SHIFT + soln2[ii+1]-1)
          if((best_can<0)||(costi<best_cost)) {
            best_can <- candidate
            best_cost <- costi
          }
        }
        if(best_can>0) {
          soln2[ii] <- best_can
        }
      }
    }
    if(isTRUE(all.equal(old_soln, soln2))) {
      break
    }
  }
  soln2
}

#' Solve for a piecewise linear partiton.
#' 
#' Solve for a good set of right-exclusive x-cuts such that the 
#' overall graph of y~x is well-approximated by a piecewise linear
#' function.
#' 
#' @param x numeric, input variable (no NAs).
#' @param y numeric, result variable (no NAs, same length as x).
#' @param ... not used, force later arguments by name.
#' @param w numeric, weights (no NAs, positive, same length as x).
#' @param penalty per-segment cost penalty.
#' @param min_n_to_chunk minimum n to subdivied problem.
#' @param min_seg positive integer, minimum segment size.
#' @param max_k maximum segments to divide into.
#' @return a data frame appropriate for stats::approx().
#' 
#' @examples 
#' 
#' solve_for_partition(1:8, c(1, 2, 3, 4, 4, 3, 2, 1))
#' 
#' @export
#' 
solve_for_partition <- function(x, y,
                                ...,
                                w = NULL,
                                penalty = 0.0,
                                min_n_to_chunk = 1000,
                                min_seg = 1,
                                max_k = length(x)) {
  wrapr::stop_if_dot_args(substitute(list(...)), "RcppDynProg::solve_for_partition")
  n <- length(x)
  if(n<1) {
    return(NULL)
  }
  if(length(unique(x))<=1) {
    return(data.frame(x = mean(x), y = mean(y)))
  }
  if(length(w)<=0) {
    w = 1 + numeric(n)
  }
  soln2 <- solve_for_partition_xs(x, y, 
                                  w = w,
                                  penalty = penalty, 
                                  min_n_to_chunk = min_n_to_chunk,
                                  min_seg = min_seg,
                                  max_k = max_k,
                                  cost_fn = lin_costs, cost_f = lin_cost)
  d <- data.frame(x = x, y = y, w = w)
  d$orig_index <- seq_len(n)
  d <- d[order(d$x), , drop = FALSE]
  # solve for linear funciton in each region to get endpoint values
  d$group <- as.character(findInterval(d$x, d$x[soln2[-length(soln2)]]))
  dlist <- base::split(d, d$group)
  points <- lapply(
    dlist,
    function(di) {
      mi <- lm(y~x, data=di, weights = di$w)
      di$pred <- predict(mi, newdata = di)
      dmin <- di[which(di$x<=min(di$x))[[1]], c("x", "pred", "group"), drop = FALSE]
      dmin$what <- "left"
      dmax <- di[which(di$x>=max(di$x))[[1]], c("x", "pred", "group"), drop = FALSE]
      dmax$what <- "right"
      ri <- rbind(dmin, dmax)
      rownames(ri) <- NULL
      ri
    }
  )
  points <- do.call(rbind, points)
  rownames(points) <- NULL
  points <- points[order(points$x), , drop = FALSE]
  points
}


#' Solve for a piecewise constant partiton.
#' 
#' Solve for a good set of right-exclusive x-cuts such that the 
#' overall graph of y~x is well-approximated by a piecewise linear
#' function.
#' 
#' @param x numeric, input variable (no NAs).
#' @param y numeric, result variable (no NAs, same length as x).
#' @param ... not used, force later arguments by name.
#' @param w numeric, weights (no NAs, positive, same length as x).
#' @param penalty per-segment cost penalty.
#' @param min_n_to_chunk minimum n to subdivied problem.
#' @param min_seg positive integer, minimum segment size.
#' @param max_k maximum segments to divide into.
#' @return a data frame appropriate for stats::approx().
#' 
#' @examples
#' 
#' solve_for_partitionc(1:8, c(-1, -1, -1, -1, 1, 1, 1, 1))
#' 
#' @export
#' 
solve_for_partitionc <- function(x, y,
                                 ...,
                                 w = NULL,
                                 penalty = 0.0,
                                 min_n_to_chunk = 1000,
                                 min_seg = 1,
                                 max_k = length(x)) {
  wrapr::stop_if_dot_args(substitute(list(...)), "RcppDynProg::solve_for_partitionc")
  n <- length(x)
  if(n<1) {
    return(NULL)
  }
  if(length(unique(x))<=1) {
    return(data.frame(x = mean(x), y = mean(y)))
  }
  if(length(w)<=0) {
    w = 1 + numeric(n)
  }
  ord <- order(x)
  fn <- function(x, y, w, min_seg, indices) { const_costs(y, w, min_seg, indices) }
  f <- function(x, y, w, min_seg, i, j) { const_cost(y, w, min_seg, i, j) }
  soln2 <- solve_for_partition_xs(x[ord], y[ord], 
                                  w = w[ord],
                                  penalty = penalty, 
                                  min_n_to_chunk = min_n_to_chunk,
                                  min_seg = min_seg,
                                  max_k = max_k,
                                  cost_fn = fn, cost_f = f)
  # solve for constant funciton in each region to get endpoint values
  d <- data.frame(x = x, y = y, w = w)
  d$orig_index <- seq_len(n)
  d <- d[order(d$x), , drop = FALSE]
  d$group <- as.character(findInterval(d$x, d$x[soln2[-length(soln2)]]))
  dlist <- base::split(d, d$group)
  points <- lapply(
    dlist,
    function(di) {
      di$pred <- sum(di$y*di$w)/sum(di$w)
      dmin <- di[which(di$x<=min(di$x))[[1]], c("x", "pred", "group"), drop = FALSE]
      dmin$what <- "left"
      dmax <- di[which(di$x>=max(di$x))[[1]], c("x", "pred", "group"), drop = FALSE]
      dmax$what <- "right"
      ri <- rbind(dmin, dmax)
      rownames(ri) <- NULL
      ri
    }
  )
  points <- do.call(rbind, points)
  rownames(points) <- NULL
  points <- points[order(points$x), , drop = FALSE]
  points
}
