#' plot a heatmap on the current device
#'
#' @param x A \code{gheat} object
#' @param use.raster Logical. If true, uses \code{\link{grid.raster}} to
#' draw the heatmap. Otherwise, draws the heatmap using rectangles.
#' @param ... Currently unused.
#' @export
print.gheat <- function(x, use.raster=TRUE, ...)
{
    stopifnot(inherits(x, "gheat"))

    x.colors <- mapToColor(x$x, x$color.scheme)

    if (isTRUE(use.raster))
    {
        grid.raster(x.colors, width=1, height=1, interpolate=FALSE)
    }
    else
    {
        pushViewport(viewport(xscale=c(0, ncol(x.colors)), yscale=c(nrow(x.colors),0)))
        grid.rect(col(x), row(x), gp=gpar(col=NA, fill=x.colors, linejoin="mitre"),
                  width=1, height=1, just=c("right", "top"), default.units="native")
        popViewport()
    }
    invisible(NULL)
}
