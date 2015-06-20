# Copyleft (C) 2015, M. Gagolewski
# Licensed under the MIT License

# a version of sample() which does not generate a permutation
# of 1:x if length(x) == 1
sample2 <- function(x, size, replace = FALSE, prob = NULL) {
   if (missing(size))
      size <- length(x)
   x[sample.int(length(x), size, replace, prob)]
}
