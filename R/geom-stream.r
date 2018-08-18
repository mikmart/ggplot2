#' @include position-stack.r
#' @export
geom_stream <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "stream", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArea,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
position_stream <- function(type = "silhouette", vjust = 1, reverse = FALSE) {
  ggproto(NULL, PositionStream, type = type, vjust = vjust, reverse = reverse)
}

PositionStream <- ggproto(
  "PositionStream", PositionStack,

  setup_params = function(self, data) {
    list(
      var = self$var %||% stack_var(data),
      vjust = self$vjust,
      reverse = self$reverse,
      type = self$type
    )
  },

  compute_panel = function(data, params, scales) {
    if (is.null(params$var)) {
      return(data)
    }

    negative <- data$ymax < 0
    if (any(negative)) {
      stop("all `y` values must be positive", call. = FALSE)
    }

    data$ybaseline <- stream_bl(data, params$type)
    # browser()

    collide(data, NULL, "position_stream", pos_stream,
      vjust = params$vjust,
      reverse = params$reverse,
      type = params$type
    )
    # browser()
  }
)

# From pos_stack() in position-stack.r
#' @noRD
#' @param width unused, for compatibility with `collide` which always passes it
pos_stream <- function(df, vjust = 1, type = "silhouette", width = NULL) {
  n <- nrow(df) + 1
  y <- ifelse(is.na(df$y), 0, df$y)
  heights <- c(0, cumsum(y)) + df$ybaseline[1]

  df$ymin <- pmin(heights[-n], heights[-1])
  df$ymax <- pmax(heights[-n], heights[-1])
  df$y <- (1 - vjust) * df$ymin + vjust * df$ymax
  df
}

#' @noRd
#' @param data full data mapped to aesthetics
stream_bl <- function(data, type = c("silhouette", "wiggle", "weighted")) {
  type <- match.arg(type)
  switch(type,
    silhouette = ave(data$y, data$x, FUN = function(y) -sum(y) / 2),
    wiggle = ave(data$y, data$x, FUN = function(y) {
      -sum(cumsum(y)) / (length(y) + 1)
    }),
    weighted = weighted_baseline(data)
  )
}

weighted_baseline <- function(data) {
  ## TODO: should be cleaner
  deri <- plyr::ddply(data, "group", transform,
                      yderiv = splinefun(x, y)(x, deriv = 1))

  bldf <- plyr::ddply(deri, "x", plyr::summarize,
                      blderiv = -sum((cumsum(yderiv) - yderiv / 2) * y) / sum(y))

  bldf[is.na(bldf$blderiv), "blderiv"] <- 0
  bldf <- transform(bldf, bl = cumsum(c(0, diff(x)) * blderiv))

  out <- dplyr::left_join(data, bldf, by = "x")
  # browser()

  out$bl
}
