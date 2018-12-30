#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Dec 30 08:46:34 2018

@author: johnmount
"""

import numpy

def xlin_fits_py(x, y, w):
  """return all out of sample fits of y~a*x+b weighted by w, all values numpy arrays"""
  n = len(y)
  # build fitting data
  regularization = 1.0e-5
  xx_0_0 = numpy.zeros(n) + numpy.sum(w*1)
  xx_1_0 = numpy.zeros(n) + numpy.sum(w*x)
  xx_0_1 = numpy.zeros(n) + numpy.sum(w*x)
  xx_1_1 = numpy.zeros(n) + numpy.sum(w*x*x)
  xy_0 = numpy.zeros(n) + numpy.sum(w*y)
  xy_1 = numpy.zeros(n) + numpy.sum(w*x*y)
  xx_1_0 = xx_1_0 + regularization
  xx_0_1  = xx_0_1  + regularization
  # pull out k'th observation
  xxk_0_0 = xx_0_0 - w*1
  xxk_1_0 = xx_1_0 - w*x
  xxk_0_1 = xx_0_1 - w*x
  xxk_1_1 = xx_1_1 - w*x*x
  xyk_0 = xy_0 - w*y
  xyk_1 = xy_1 - w*x*y
  # solve linear system
  det = xxk_0_0*xxk_1_1 - xxk_0_1*xxk_1_0
  c0 = (xxk_1_1*xyk_0 - xxk_0_1*xyk_1)/det
  c1 = (-xxk_1_0*xyk_0 + xxk_0_0*xyk_1)/det
  # form estimate
  y_est = c0 + c1*x
  return(y_est)
  
# x = numpy.asarray([1 ,2, 3, 4])
# y = numpy.asarray([1, 2, 2, 1])
# w = numpy.asarray([1, 1, 1, 1])
# xlin_fits_py(x, y, w)
## array([2.666715  , 1.28571541, 1.28571214, 2.66666833])