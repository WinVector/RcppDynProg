#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 24 15:36:22 2018

@author: John Mount
"""

import numpy

# import numpy
# from DynProg import *
# x = numpy.array([1,1,5,1,1,0,5,0,1])
# x.shape = (3,3)
# solve_dynamic_program(x, 3)
#
#' solve_dynamic_program
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
def solve_dynamic_program(x, kmax):
    """x n by n inclusive interval cost array, kmax maximum number of steps to take"""
    # for cleaner notation
    # solution and x will be indexed from 1 using
    # R_INDEX_DELTA
    # intermediate arrays will be padded so indexing
    # does not need to be shifted
    R_INDEX_DELTA = -1
    R_SIZE_PAD = 1
  
    # get shape of problem
    n = x.shape[0]
    if kmax>n:
        kmax = n
 
    # get some edge-cases
    if (kmax<=1) or (n<=1):
         return [1, n+1]

    # best path cost up to i (row) with exactly k-steps (column)
    path_costs = numpy.zeros((n + R_SIZE_PAD, kmax + R_SIZE_PAD))
    # how many steps we actually took
    k_actual = numpy.zeros((n + R_SIZE_PAD, kmax + R_SIZE_PAD))
    # how we realized each above cost
    prev_step = numpy.zeros((n + R_SIZE_PAD, kmax + R_SIZE_PAD))
  
    # fill in path and costs tables
    for i in range(1, n+1):
        prev_step[i, 1] = 1
        path_costs[i, 1] = x[1 + R_INDEX_DELTA, i + R_INDEX_DELTA]
        k_actual[i, 1] = 1

    # refine dynprog table
    for ksteps in range(2, kmax+1):
        # compute larger paths
        for i in range(1, n+1):
            # no split case
            pick = i
            k_seen = 1
            pick_cost = x[1 + R_INDEX_DELTA, i + R_INDEX_DELTA]
            # split cases
            for candidate in range(1, i):
                cost = path_costs[candidate, ksteps-1] + \
                    x[candidate + 1 + R_INDEX_DELTA, i + R_INDEX_DELTA]
                k_cost = k_actual[candidate, ksteps-1] + 1
                if (cost<=pick_cost) and \
                    ((cost<pick_cost) or (k_cost<k_seen)):
                    pick = candidate
                    pick_cost = cost
                    k_seen = k_cost
            path_costs[i, ksteps] = pick_cost
            prev_step[i, ksteps] = pick
            k_actual[i, ksteps] = k_seen
 
    # now back-chain for solution
    k_opt = int(k_actual[n, kmax])
    solution = [0]*(k_opt+1)
    solution[1 + R_INDEX_DELTA] = int(1)
    solution[k_opt + 1 + R_INDEX_DELTA] = int(n+1)
    i_at = n
    k_at = k_opt
    while k_at>1:
        prev_i = int(prev_step[i_at, k_at])
        solution[k_at + R_INDEX_DELTA] = int(prev_i + 1)
        i_at = prev_i
        k_at = k_at - 1
    return solution


