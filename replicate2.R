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
#' Repeatedly Evaluate an Expression
#'
#' @author
#' Marek Gagolewski
#'
#' @description
#' A version of \code{replicate()} together with  \code{vapply()}
#' featuring a nice progress bar.
#'
#' @param M the number of replications
#' @param expr the expression to evaluate repeatedly
#' @param FUN.VALUE a (generalized) vector;
#'        a template for the return value from \code{FUN}
#'
#' @return a vector or array of type matching the \code{FUN.VALUE}
#'
#' @example
#' replicate2(100, {Sys.sleep(0.1); runif(1)}, double(1))
replicate2 <- compiler::cmpfun(function(M, expr, FUN.VALUE) {

   rpkg_progressbar_create <- function(min, max, cur,
               width=floor(getOption("width")*0.8), update_interval=1) {
      stopifnot(is.numeric(min), length(min)==1, is.finite(min))
      stopifnot(is.numeric(max), length(max)==1, is.finite(max), min < max)
      stopifnot(is.numeric(cur), length(cur)==1, is.finite(cur), min <= cur, cur <= max)
      stopifnot(is.numeric(width), length(width)==1, is.finite(width), width > 0)
      stopifnot(is.numeric(update_interval), length(update_interval)==1,
         is.finite(update_interval), update_interval > 0)
      curtime <- Sys.time()
      structure(as.environment(list(
         min=min,
         max=max,
         cur=cur,
         t_start=curtime,
         t_last=curtime,
         width=width,
         update_interval=update_interval
      )), class="rpkg_progressbar")
   }

   rpkg_progressbar_update <- function(pb, cur, output=stdout()) {
      stopifnot(inherits(pb, "rpkg_progressbar"))
      stopifnot(is.numeric(cur), length(cur)==1, is.finite(cur), pb$min <= cur, cur <= pb$max)

      curtime <- Sys.time()
      pb$cur <- cur
      elapsed <- as.numeric(difftime(curtime, pb$t_last, units="secs"))
      if (pb$max == pb$cur || elapsed > pb$update_interval) {
         pb$t_last <- curtime
         elapsed2 <- as.numeric(difftime(curtime, pb$t_start, units="secs"))

         w <- pb$width-20 # circa about
         p <- (pb$cur-pb$min)/(pb$max-pb$min)
         eta <- round((1-p)*elapsed2/p, 0)
         h <- eta %/% 3600
         m <- (eta-h*3600) %/% 60
         s <- eta-h*3600-m*60


         cat(sprintf("\r[%s%s] ETA %02.0f:%02.0f:%02.0f",
            stringi::stri_dup("=", floor(w*p)),
            stringi::stri_dup(" ", w-floor(w*p)),
            h, m, s
         ), file=output)
      }

      if (pb$max == pb$cur) cat("\n", file=output)
      invisible(NULL)
   }

   res <- matrix(FUN.VALUE, nrow=length(FUN.VALUE), ncol=M)
   pb <- rpkg_progressbar_create(0, M, 0)
   rpkg_progressbar_update(pb, 0)
   i <- 1
   FUN <- compiler::cmpfun(eval.parent(substitute(function(...) expr)))
   while (i <= M) {
      res[,i] <- FUN()
      rpkg_progressbar_update(pb, i)
      i <- i+1
   }
   dimnames(res) <- list(names(FUN.VALUE), NULL)
   res
})
