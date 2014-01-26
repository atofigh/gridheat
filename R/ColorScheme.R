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
    ColorScheme(breaks, colors)
}
