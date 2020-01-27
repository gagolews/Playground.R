# Playground.R

> Copyright (C) 2015-2020, [Marek Gagolewski](http://www.gagolewski.com).
Licensed under the MIT License, see `LICENSE` for details.

A few handy R and Rcpp functions:

* `replicate2()` — a version of `replicate()`+`vapply()`+a nice progress bar
* `sample2()` — a version of `sample()` which doesn't treat scalar `x` as `1:x`
* `cgal_qp_solver()` — an interface to the Quadratic (and Linear) programming
solver from the `CGAL` library
* `deskew()` — an image deskewer (apply, e.g., on the MNIST handwritten
digits dataset)
