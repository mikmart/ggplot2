#' @include facet-null.r
NULL

#' Facet specification: a single panel.
#'
#' @inheritParams facet_grid
#' @keywords internal
#' @export
#' @examples
#' # facet_null is the default faceting specification if you
#' # don't override it with facet_grid or facet_wrap
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
facet_absolute <- function(shrink = TRUE, y_size = unit(1, "null"), x_size = unit(1, "null"), debug = FALSE) {
  ggproto(NULL, FacetAbsolute,
    shrink = shrink,
    params = list(
      x_size = x_size,
      y_size = y_size,
      debug = debug
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
FacetAbsolute <- ggproto("FacetAbsolute", FacetNull,
  shrink = TRUE,

  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {

    range <- ranges[[1]]

    # Figure out aspect ratio
    aspect_ratio <- theme$aspect.ratio %||% coord$aspect(range)
    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }
    axis_h <- coord$render_axis_h(range, theme)
    axis_v <- coord$render_axis_v(range, theme)

    all <- matrix(list(
      zeroGrob(),  axis_h$top,    zeroGrob(),
      axis_v$left, panels[[1]],   axis_v$right,
      zeroGrob(),  axis_h$bottom, zeroGrob()
    ), ncol = 3, byrow = TRUE)

    if (params$debug) browser()

    z_matrix <- matrix(c(5, 6, 4, 7, 1, 8, 3, 9, 2), ncol = 3, byrow = TRUE)
    grob_widths <- unit.c(grobWidth(axis_v$left), params$x_size * diff(range$x.range), grobWidth(axis_v$right))
    grob_heights <- unit.c(grobHeight(axis_h$top), params$y_size * diff(range$y.range), grobHeight(axis_h$bottom))

    grob_names <- c("spacer", "axis-l", "spacer", "axis-t", "panel", "axis-b", "spacer", "axis-r", "spacer")
    grob_clip <- c("off", "off", "off", "off", coord$clip, "off", "off", "off", "off")

    layout <- gtable_matrix("layout", all,
      widths = grob_widths, heights = grob_heights,
      respect = respect, clip = grob_clip,
      z = z_matrix
    )
    layout$layout$name <- grob_names

    layout
  }
)
