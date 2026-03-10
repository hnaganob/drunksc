#' Add a color bar to a base R plot layout
#'
#' Draws a standalone color bar as a base R graphics image, suitable
#' for use alongside other plots in a multi-panel layout. The bar can be oriented
#' horizontally or vertically, and the color vector is returned invisibly for reuse.
#'
#' @param zlim A numeric vector of length 2 specifying the minimum and maximum
#'   values of the color scale \code{c(min, max)}. Defaults to \code{c(-1, 1)}.
#' @param color_ramp A character vector of at least 2 valid R colors used to
#'   construct the color gradient via \code{\link[grDevices]{colorRampPalette}}.
#'   Defaults to \code{grDevices::cm.colors(5)}.
#' @param horizontal Logical. If \code{TRUE} (default), the color bar is drawn
#'   horizontally with the axis on the bottom. If \code{FALSE}, it is drawn
#'   vertically with the axis on the right.
#' @param main Optional title displayed above the color bar.
#'
#' @return Invisibly returns a character vector of hex color codes representing
#'   the full color scale.
#'
#' @details
#' Graphics parameters (\code{mar}, \code{mgp}) are modified during the call and
#' restored on exit via \code{\link[base]{on.exit}}, so surrounding plots are
#' unaffected.
#'
#' The color bar is drawn using \code{\link[graphics]{image}} with 101 evenly
#' spaced values across \code{zlim}. Axis ticks are placed at \code{\link[base]{pretty}}
#' intervals.
#'
#' @seealso \code{\link[grDevices]{colorRampPalette}}, \code{\link[graphics]{image}}
#'
#' @examples
#' # Basic horizontal color bar
#' add_colorbar(zlim = c(-5, 5))
#'
#' # Color bar aligned with another plot
#' layout(c(1, 1, 2))
#' image(volcano, col = hcl.colors(20, "BluYl"))
#' add_colorbar(
#'   zlim = range(volcano),
#'   color_ramp = hcl.colors(20, "BluYl"),
#'   horizontal = TRUE,
#'   main = "Elevation"
#' )
#'
#' # Use returned colors to color points consistently
#' cols <- add_colorbar(zlim = c(0, 1), color_ramp = hcl.colors(5, "viridis"))
#' head(cols)
#'
#' @export
add_colorbar <- function(
    zlim = c(-1, 1),
    color_ramp = grDevices::cm.colors(5),
    horizontal = TRUE,
    main = NULL) {
  # validate inputs
  if (!is.numeric(zlim) || length(zlim) != 2) {
    stop("zlim must be a numeric vector of length 2.")
  }
  if (zlim[1] >= zlim[2]) {
    stop("zlim[1] must be less than zlim[2].")
  }
  if (!is.character(color_ramp) || length(color_ramp) < 2) {
    stop("color_ramp must contain at least 2 colors.")
  }
  if (!is.logical(horizontal) || length(horizontal) != 1) {
    stop("horizontal must be TRUE or FALSE.")
  }

  # preserve graphics parameters
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(opar), add = TRUE)

  # generate color scale
  vals <- seq(zlim[1], zlim[2], length.out = 101)
  ramp <- grDevices::colorRampPalette(color_ramp)
  colorbar <- ramp(length(vals))

  # set plot margins
  mar_default <- if (horizontal) c(2, 1, 1, 1) else c(1, 1, 1, 2)
  mar_with_main <- if (horizontal) c(2, 1, 2, 1) else c(1, 1, 2, 2)

  graphics::par(
    mar = if (!is.null(main)) mar_with_main else mar_default,
    mgp = c(1.5, 0.5, 0)
  )

  # draw color bar
  if (isTRUE(horizontal)) {
    graphics::image(
      vals, 1,
      matrix(seq_along(vals), ncol = 1),
      col = colorbar,
      axes = FALSE,
      xlab = "", ylab = ""
    )
    graphics::axis(1, at = pretty(vals, n = 5))
  } else {
    graphics::image(
      1, vals,
      matrix(seq_along(vals), nrow = 1),
      col = colorbar,
      axes = FALSE,
      xlab = "", ylab = ""
    )
    graphics::axis(4, at = pretty(vals, n = 5), las = 2)
  }

  graphics::box(which = "plot", col = "gray")

  if (!is.null(main)) graphics::title(main = main, line = 0.8)

  # return colors invisibly
  invisible(colorbar)
}
