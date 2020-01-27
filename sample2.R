## Copyright (C) 2015-2020 Marek Gagolewski
## http://www.gagolewski.com
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
## THE SOFTWARE.


#' @title
#' Random Samples and Permutations
#'
#' @author
#' Marek Gagolewski
#'
#' @description
#' A version of sample() which does not generate a permutation
#' of \code{1:x} if \code{length(x) == 1}.
#'
#' @param x a vector of elements from which to choose
#' @param size the number of items to choose
#' @param replace should sampling be with replacement?
#' @param prob a vector of probabilities corresponding to each element in \code{x}
#'
#' @return a vector of length \code{size}
sample2 <- function(x, size, replace=FALSE, prob=NULL) {
   if (missing(size))
      size <- length(x)
   x[sample.int(length(x), size, replace, prob)]
}
