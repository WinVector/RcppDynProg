

#' solve_interval_partition (R version)
#' 
#' Solve a for a minimal cost partition of the integers [1,...,nrow(x)] problem where for j>=i x(i,j).
#' is the cost of choosing the partition element [i,...,j]. 
#' Returned solution is an ordered vector v of length k where: v[1]==1, v[k]==nrow(x)+1, and the 
#' partition is of the form [v[i], v[i+1]) (intervals open on the right).
#' 
#' @param x NumericMatix, for j>=i x(i,j) is the cost of partition element [i,...,j] (inclusive).
#' @param kmax int, maximum number of steps in solution.
#' @return dynamic program solution.
#' 
#' @keywords internal
#' 
#' @examples 
#' 
#' x <- matrix(c(1,1,5,1,1,0,5,0,1), nrow=3)
#' k <- 3
#' solve_interval_partition_R(x, k)
#' solve_interval_partition(x, k)
#' 
#' @export
#' 
solve_interval_partition_R <- function(x, kmax) {
  # for cleaner notation
  # solution and x will be indexed from 1 using
  # R_INDEX_DELTA
  # intermediate arrays will be padded so indexing
  # does not need to be shifted
  R_INDEX_DELTA = 0L;
  R_SIZE_PAD = 0L;
  
  # get shape of problem
  n = nrow(x);
  if(kmax>n) {
    kmax = n;
  }
  
  # get some edge-cases
  if((kmax<=1)||(n<=1)) {
    solution = integer(2);
    solution[1 + R_INDEX_DELTA] = 1;
    solution[2 + R_INDEX_DELTA] = n+1;
    return(solution);
  }
  
  # best path cost up to i (row) with exactly k-steps (column)
  path_costs = matrix(0.0, n + R_SIZE_PAD, kmax + R_SIZE_PAD);
  # how many steps we actually took
  k_actual = matrix(0L, n + R_SIZE_PAD, kmax + R_SIZE_PAD);
  # how we realized each above cost
  prev_step = matrix(0L, n + R_SIZE_PAD, kmax + R_SIZE_PAD);
  
  # fill in initial path and costs tables k = 1 case
  for(i in seqi(1, n)) {
    prev_step[i, 1] = 1L;
    path_costs[i, 1] = x[1 + R_INDEX_DELTA, i + R_INDEX_DELTA];
    k_actual[i, 1] = 1L;
  }
  # refine dynprog table
  for(ksteps in seqi(2, kmax)) {
    # compute larger paths
    for(i in seqi(1, n)) {
      # no split case
      pick = i;
      k_seen = 1;
      pick_cost = x[1 + R_INDEX_DELTA, i + R_INDEX_DELTA];
      # split cases
      for(candidate in seqi(1, i-1)) {
        cost = path_costs[candidate, ksteps-1] +
          x[candidate + 1 + R_INDEX_DELTA, i + R_INDEX_DELTA];
        k_cost = k_actual[candidate, ksteps-1] + 1;
        if((cost<=pick_cost) &&
           ((cost<pick_cost)||(k_cost<k_seen))) {
          pick = candidate;
          pick_cost = cost;
          k_seen = k_cost;
        }
      }
      path_costs[i, ksteps] = pick_cost;
      prev_step[i, ksteps] = pick;
      k_actual[i, ksteps] = k_seen;
    }
  }
  
  # now back-chain for solution
  k_opt = k_actual[n, kmax];
  solution = integer(k_opt+1);
  solution[1 + R_INDEX_DELTA] = 1L;
  solution[k_opt + 1 + R_INDEX_DELTA] = n+1L;
  i_at = n;
  k_at = k_opt;
  while(k_at>1) {
    prev_i = prev_step[i_at, k_at];
    solution[k_at + R_INDEX_DELTA] = prev_i + 1;
    i_at = prev_i;
    k_at = k_at - 1;
  }
  
  return(solution);
}

