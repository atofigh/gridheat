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
#' @param breaks Vector of breakpoints for the color scheme
#' @param colors Character vector of length \code{length(breaks) + 1}
#' describing colors
#' @param na.color Color used for NAs
#'
#' @return An object of class ColorScheme, which has three components:
#' \code{breaks}, \code{colors}, and \code{na.color}.
#'
#' @examples
#' breaks <- seq(-1, 1, length.out=10)
#' ColorScheme(breaks, div_gradient_pal()(seq(0, 1, length.out=length(breaks)+1)), "grey30")
ColorScheme <- function(breaks, colors, na.color)
{
    stopifnot(length(breaks) + 1 == length(colors))
    ret <- structure(list(breaks=breaks,
                          colors=colors,
                          na.color=na.color),
                     class="ColorScheme")
    ret
}

getDefaultDivColorScheme <- function()
{
    blue <- hcl(260, 80, 30)
    grey <- hcl(0, 0, 85)
    red <- hcl(0, 80, 30)
    power <- 1.5

    reds <- gradient_n_pal(c(grey, red), space="Lab")(seq(0, 1, length.out=7)^power)[-1]
    blues <- gradient_n_pal(c(grey, blue), space="Lab")(seq(0, 1, length.out=7)^power)[-1]
    colors <- c(rev(blues), reds)
    breaks <- seq(-3, 3, length.out=length(colors) - 1)
    ColorScheme(breaks, colors, "grey35")
}

mapToColor <- function(x, color.scheme)
{
    xi <- findInterval(x, color.scheme$breaks, rightmost.closed=TRUE) + 1
    xi[is.na(xi)] <- length(color.scheme$colors) + 1
    x.colors <- c(color.scheme$colors, color.scheme$na.colors)[xi]
    dim(x.colors) <- dim(x)
    dimnames(x.colors) <- dimnames(x)
    x.colors
}
