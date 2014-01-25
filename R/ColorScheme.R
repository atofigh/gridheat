#' Create a ColorScheme object
#'
#' Constructor for the ColorScheme class
#'
#' Constructs a ColorScheme object. The colors in \code{colors}
#' correspond to the \code{length(breaks) + 1} intervals defined by the
#' breakpoints given in \code{breaks}. The intervals are defined as
#' \code{(-Inf, breaks[1])[breaks[1], breaks[2])...[breaks[n-1],
#' breaks[n]](breaks[n], +Inf)}
#'
#' @export
#' @param breaks vector of breakpoints for the color scheme
#' @param colors character vector of length \code{length(breaks) + 1}
#' describing colors
ColorScheme <- function(breaks, colors)
{
    stopifnot(length(breaks) + 1 == length(colors))
    ret <- structure(list(breaks=breaks,
                          colors=colors),
                     class="ColorScheme")
    ret
}
