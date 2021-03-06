% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale-.r
\name{continuous_scale}
\alias{continuous_scale}
\title{Continuous scale constructor}
\usage{
continuous_scale(aesthetics, scale_name, palette, name = waiver(),
  breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
  limits = NULL, rescaler = rescale, oob = censor,
  expand = waiver(), na.value = NA_real_, trans = "identity",
  guide = "legend", position = "left", super = ScaleContinuous)
}
\arguments{
\item{aesthetics}{The names of the aesthetics that this scale works with.}

\item{scale_name}{The name of the scale that should be used for error messages
associated with this scale.}

\item{palette}{A palette function that when called with a numeric vector with
values between 0 and 1 returns the corresponding output values
(e.g., \code{\link[scales:area_pal]{scales::area_pal()}}).}

\item{name}{The name of the scale. Used as the axis or legend title. If
\code{waiver()}, the default, the name of the scale is taken from the first
mapping used for that aesthetic. If \code{NULL}, the legend title will be
omitted.}

\item{breaks}{One of:
\itemize{
\item \code{NULL} for no breaks
\item \code{waiver()} for the default breaks computed by the
\link[scales:trans_new]{transformation object}
\item A numeric vector of positions
\item A function that takes the limits as input and returns breaks
as output (e.g., a function returned by \code{\link[scales:extended_breaks]{scales::extended_breaks()}})
}}

\item{minor_breaks}{One of:
\itemize{
\item \code{NULL} for no minor breaks
\item \code{waiver()} for the default breaks (one minor break between
each major break)
\item A numeric vector of positions
\item A function that given the limits returns a vector of minor breaks.
}}

\item{labels}{One of:
\itemize{
\item \code{NULL} for no labels
\item \code{waiver()} for the default labels computed by the
transformation object
\item A character vector giving labels (must be same length as \code{breaks})
\item A function that takes the breaks as input and returns labels
as output
}}

\item{limits}{One of:
\itemize{
\item \code{NULL} to use the default scale range
\item A numeric vector of length two providing limits of the scale.
Use \code{NA} to refer to the existing minimum or maximum
\item A function that accepts the existing (automatic) limits and returns
new limits
}}

\item{rescaler}{A function used to scale the input values to the
range [0, 1]. This is always \code{\link[scales:rescale]{scales::rescale()}}, except for
diverging and n colour gradients (i.e., \code{\link[=scale_colour_gradient2]{scale_colour_gradient2()}},
\code{\link[=scale_colour_gradientn]{scale_colour_gradientn()}}). The \code{rescaler} is ignored by position
scales, which ways use \code{\link[scales:rescale]{scales::rescale()}}.}

\item{oob}{Function that handles limits outside of the scale limits
(out of bounds). The default (\code{\link[scales:censor]{scales::censor()}}) replaces out of
bounds values with \code{NA}.}

\item{expand}{For position scales, a vector of range expansion constants used to add some
padding around the data to ensure that they are placed some distance
away from the axes. Use the convenience function \code{\link[=expansion]{expansion()}}
to generate the values for the \code{expand} argument. The defaults are to
expand the scale by 5\% on each side for continuous variables, and by
0.6 units on each side for discrete variables.}

\item{na.value}{Missing values will be replaced with this value.}

\item{trans}{For continuous scales, the name of a transformation object
or the object itself. Built-in transformations include "asn", "atanh",
"boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2",
"logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal",
"reverse", "sqrt" and "time".

A transformation object bundles together a transform, its inverse,
and methods for generating breaks and labels. Transformation objects
are defined in the scales package, and are called \code{<name>_trans} (e.g.,
\code{\link[scales:boxcox_trans]{scales::boxcox_trans()}}). You can create your own
transformation with \code{\link[scales:trans_new]{scales::trans_new()}}.}

\item{guide}{A function used to create a guide or its name. See
\code{\link[=guides]{guides()}} for more information.}

\item{position}{For position scales, The position of the axis.
\code{left} or \code{right} for y axes, \code{top} or \code{bottom} for x axes.}

\item{super}{The super class to use for the constructed scale}
}
\description{
Continuous scale constructor
}
\keyword{internal}
