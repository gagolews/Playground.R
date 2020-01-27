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
#' Image Deskewer
#'
#' @author
#' Marek Gagolewski
#'
#' @description
#' An image deskewer (to apply, e.g., on the MNIST handwritten digits dataset).
#'
#' @param I a numeric matrix representing a grayscale image,
#'        say, with 0.0=black and 1.0=white
#' @param bg background colour (for extrapolation)
#'
#' @return a new image of the same size as the input one
deskew <- function(I, bg=0.0) {
    # A call to akima::interp.old() below constitutes the slow part.
    stopifnot(is.numeric(I), is.matrix(I))
    stopifnot(is.numeric(bg), length(bg)==1)
    nx <- ncol(I) # Ox goes columnwise
    ny <- nrow(I) # Oy goes rowwise
    ss <- sum(I)
    sx <- colSums(I)
    sy <- rowSums(I)

    # Centroid:
    mux <- sum(sx*(1:nx))/ss
    muy <- sum(sy*(1:ny))/ss
    # 1st Central Moments
    #cxx <- sum(sx*(1:nx-mux)^2)/ss # 1st central moment
    cyy <- sum(sy*(1:ny-muy)^2)/ss
    cxy <- sum(outer(1:ny-muy, 1:nx-mux)*I)/ss
    #c <- matrix(c(cxx, cxy, cxy, cyy), nrow=2)

    # Transformation matrix (inverted shear)
    T <- matrix(c(1, 0, -cxy/cyy, 1), nrow=2)

    # Get the modified coordinates for the input image (translation&shear)
    xxyy <- as.matrix(expand.grid(1:ny-muy, 1:nx-mux)) # +center
    xxyy <- xxyy %*% T

    # To get the output image, interpolate by querying over an equidistant grid
    I2 <- akima::interp.old(xxyy[,1], xxyy[,2], I, 1:ny-ny/2, 1:nx-nx/2)$z
    I2[is.na(I2)] <- bg
    I2
}
