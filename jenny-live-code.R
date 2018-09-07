#' ---
#' title: "Live code from function writing workshop at R-Ladies Buenos Aires"
#' author: "Jenny Bryan"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---

#' Use `rmarkdown::render()` on this or, in RStudio, click on the "Compile Report" spiral-notebook icon.
#'
#' ## Where to find this document
#'
#' Shortlink humans can type:
#'
#'   * <http://bit.ly/jenny-live-code>
#'
#' Horrible link that reveals how this is done:
#'
#'   * <https://www.dropbox.com/s/2b8mi4rir23pvnx/jenny-live-code.R?raw=1>
#'
#' Using the `raw=1` query trick for rendering a DropBox-hosted file in the
#' browser:
#'
#'   * <https://www.dropbox.com/en/help/desktop-web/force-download>
#' learned from [Michael Levy](https://twitter.com/ucdlevy).
#'
#' How this works:
#'
#'   * I code live in an R script locally. I save often.
#'   * This file lives in a directory synced to DropBox.
#'   * You open the DropBox file at <http://bit.ly/jenny-live-code> and refresh
#'   as needed.
#'   * Should allow you to see, copy, paste everything I've typed and save the
#'   entire transcript at the end. This file is highly perishable, so save your
#'   own copy if you want it.
#'   * Every now and then the refresh won't work. Just re-open from from the
#'   bit.ly link: <http://bit.ly/jenny-live-code>
#'
#+ setup, include = FALSE
knitr::opts_chunk$set(error = TRUE, collapse = TRUE)

#+ live-code

#' ## Workshop material starts here
library(gapminder)

#' Play with some top-level code to get maximum life expectancy, minimum life
#' expectancy, and the max - min.
min(gapminder$lifeExp)
max(gapminder$lifeExp)
range(gapminder$lifeExp)

max(gapminder$lifeExp) - min(gapminder$lifeExp)
range(gapminder$lifeExp)[2] - range(gapminder$lifeExp)[1]
diff(range(gapminder$lifeExp))

max_minus_min <- function(x) {
  max(x) - min(x)
}
max_minus_min(gapminder$lifeExp)

#' Try out your new function on new inputs, where you know
#' the correct answer, at least approximately
max_minus_min(1:10)
max_minus_min(runif(n = 1000))

max_minus_min(gapminder$gdpPercap)
max_minus_min(gapminder$year)

#' Try your new function on weird input
max_minus_min(gapminder)
max_minus_min(gapminder$country)
max_minus_min("r-ladies")

max_minus_min(gapminder[c("lifeExp", "gdpPercap", "pop")])

#' How do we check for weird input?
mmm <- function(x) {
  stopifnot(is.numeric(x))
  max(x) - min(x)
}

#' Let's try it out!
#'
#' These should work.
mmm(gapminder$lifeExp)
mmm(1:10)
mmm(runif(n = 1000))
mmm(gapminder$gdpPercap)
mmm(gapminder$year)

#' These should NOT work.
mmm(gapminder)
mmm(gapminder$country)
mmm("r-ladies")
mmm(gapminder[c("lifeExp", "gdpPercap", "pop")])

#' You can work a little harder and make a nicer error message.
mmm2 <- function(x) {
  if (!is.numeric(x)) {
    stop("I am so sorry, but this function only works for a numeric input `x`. Please try again.")
  }
  max(x) - min(x)
}
mmm2(gapminder)

mmm3 <- function(x) {
  if (!is.numeric(x)) {
    stop("I am so sorry, but this function only works for a numeric input `x`. You have provided an object of class: ", class(x)[1])
  }
  max(x) - min(x)
}
mmm3(gapminder)
mmm3("hi there")
mmm3(rnorm)

#' Food and drink break here :)

#' Let's go back to our basic function.
mmm <- function(x) {
  stopifnot(is.numeric(x))
  max(x) - min(x)
}

#' Generalize to taking the difference between any two
#' quantiles, i.e. not just max - min.
quantile(gapminder$lifeExp)
quantile(gapminder$lifeExp, probs = c(0, 1))
range(gapminder$lifeExp)
quantile(gapminder$lifeExp, probs = 0.5)
median(gapminder$lifeExp)
quantile(gapminder$lifeExp, probs = c(0.25, 0.75))

the_probs <- c(0.25, 0.75)
the_quantiles <- quantile(gapminder$lifeExp, probs = the_probs)
max(the_quantiles) - min(the_quantiles)
IQR(gapminder$lifeExp)

qdiff1 <- function(x, probs) {
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}
qdiff1(gapminder$lifeExp, probs = c(0, 1))
qdiff1(gapminder$lifeExp, probs = c(0.25, 0.75))

#' Argument names matter.
qdiff2 <- function(laura, naty) {
  the_quantiles <- quantile(laura, probs = naty)
  max(the_quantiles) - min(the_quantiles)
}
qdiff2(gapminder$lifeExp, naty = c(0, 1))

#' Convention: most people only use `return()` for early returns.
qdiff2 <- function(laura, naty) {
  the_quantiles <- quantile(laura, probs = naty)
  return(max(the_quantiles) - min(the_quantiles))
}
qdiff2(gapminder$lifeExp, naty = c(0, 1))

#' Default arguments
qdiff1(gapminder$lifeExp)

qdiff3 <- function(x, probs = c(0, 1)) {
  ## WHAT SORT OF VALIDITY CHECKS WOULD YOU DO HERE?
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}
qdiff3(gapminder$lifeExp)

#' Possible new validity checks
#' probs: make sure is numeric and in [0, 1]
#' probs: check length (maybe check of length 2, definitely at least 2)

#' Re-define qdiff3().
qdiff3 <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}

#' What about `NA`s?
z <- gapminder$lifeExp
z[3] <- NA
quantile(z)

qdiff4 <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs, na.rm = TRUE)
  max(the_quantiles) - min(the_quantiles)
}
qdiff4(z)

#' That's nice but I think we should expose `na.rm`.
qdiff5 <- function(x, probs = c(0, 1), na.rm = FALSE) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs, na.rm = na.rm)
  max(the_quantiles) - min(the_quantiles)
}
qdiff5(z)
qdiff5(z, na.rm = TRUE)

#' The mysterious `...` argument.
qdiff6 <- function(x, probs = c(0, 1), na.rm = FALSE, ...) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs, na.rm = na.rm, ...)
  max(the_quantiles) - min(the_quantiles)
}
qdiff6(gapminder$lifeExp)
qdiff6(gapminder$lifeExp, type = 1)
qdiff6(gapminder$lifeExp, type = 4)

set.seed(1234)
z <- rnorm(100)
qdiff6(z, type = 1)
qdiff6(z, type = 4)
all.equal(qdiff6(z, type = 1), qdiff6(z, type = 3))

#' OK we had trouble getting `type` to actually show us anything interesting.
#' The STAT 545 lessons show this better than we did live.

#' What does the `...` do? A simpler example.
f <- function(...) {
  list(...)
}
f(a = "a", b = 1.4, c = mean)

#' Testing
library(testthat)

expect_error(
  qdiff6("nope nope nope"),
  "is.numeric(x) is not TRUE",
  fixed = TRUE
)

expect_equal(
  qdiff6(c(1, 2), c(0, 1)),
  1
)

expect_true(
  qdiff6(runif(10), c(0, 1)) < 1
)

qdiff7 <- function(x, probs = c(0, 1)) {
  stopifnot(is.numeric(x))
  the_quantiles <- quantile(x, probs = probs)
  max(the_quantiles) - min(the_quantiles)
}
expect_error(
  qdiff7(c(1, 2, NA), c(0, 1)),
  NA
)

#' Question about... what happens if you make an assignment in your function or
#' as the return value of your function?
f <- function(x) {
  y <- x * 2
}
f(2)
z <- f(2)
z

#' Question: what if you want to return more than 1 thing?
f <- function(x) {
  list(x^2, "cow")
}
f(3)
