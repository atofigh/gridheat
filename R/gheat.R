#' Create a new gheat plot
#'
#' \code{gheat} creates a new gheat object.
#'
#' \code{gheat} objects can be used to create heatmap plots
#' incrementally using the \code{+} operator.
#'
#' @param x A matrix, a 2-dimensional array, or an object that can be
#' coerced to a matrix.
#' @param ... Other arguments passed to specific methods
#' @export
#'
#' @examples
#' m <- matrix(rnorm(15), nrow=3)
#' m[2, 2] <- NA
#' gheat(m)
gheat <- function(x, ...)
{
    UseMethod("gheat")
}

#' @rdname gheat
#'
#' @details A default diverging blue-red color scheme will be used if
#' \code{color.scheme} is missing.
#'
#' @param color.scheme (optional) A \code{\link{ColorScheme}} object
#' @S3method gheat matrix
gheat.matrix <- function(x, color.scheme, ...)
{
    if (missing(color.scheme) || is.null(color.scheme))
        color.scheme <- getDefaultDivColorScheme()

    structure(list(x=x, color.scheme=color.scheme),
              class="gheat")
}

#' @rdname gheat
#' @S3method gheat matrix
gheat.array <- function(x, color.scheme, ...)
{
    if (missing(color.scheme) || is.null(color.scheme))
        color.scheme <- getDefaultDivColorScheme()

    structure(list(x=x, color.scheme=color.scheme),
              class="gheat")
}

#' @rdname gheat
#'
#' @details The default method will try to coerce \code{x} to a matrix
#' and will fail if the coercion fails.
#' @S3method gheat default
gheat.default <- function(x, color.scheme, ...)
{
    if (missing(color.scheme) || is.null(color.scheme))
        color.scheme <- getDefaultDivColorScheme()

    x <- try(as.matrix(x), silent=TRUE)
    if (inherits(x, "try-error"))
        stop("Could not coerce 'x' to matrix")

    structure(list(x=x, color.scheme=color.scheme),
              class="gheat")
}
