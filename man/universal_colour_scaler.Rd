% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/universal_colour_scaler.R
\name{universal_colour_scaler}
\alias{universal_colour_scaler}
\title{universal_colour_scaler}
\usage{
universal_colour_scaler(
  x,
  scale = "linear",
  type = "scico",
  palette = "bilbao",
  n_breaks = 5,
  direction = 1,
  alpha = 0,
  begin = 0,
  end = 1,
  mode = "scaled",
  verbose = FALSE,
  minq = 0,
  maxq = 1
)
}
\arguments{
\item{x}{The values for which you wish to generate a colour scale
(a vector or matrix of numeric type)}

\item{scale}{The type of colour scale you would like to generate.
"linear", (the default), "log", "quantile" or a custom numeric vector}

\item{type}{the type of palette you would like to use.
"scico" (the default), "viridis", "brewer" or "custom".
if you specify custom}

\item{palette}{The name of one of the 68 colour palettes from scico,
viridis (defaults to "bilbao" from scico)
\itemize{
\item scico palettes:
"acton", "bamako", "batlow", "berlin", "bilbao", "broc", "brocO", "buda",
"cork", "corkO", "davos", "devon", "grayC", "hawaii", "imola", "lajolla",
"lapaz", "lisbon", "nuuk", "oleron", "oslo", "roma", "romaO", "tofino",
"tokyo", "turku", "vik", "vikO"
\item viridis palettes:
"viridis", "inferno", "magma", "plasma", "cividis"
\item brewer palettes:
"BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
"Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
"Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys",
"Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
"YlGn", "YlGnBu", "YlOrBr", "YlOrRd"
}}

\item{n_breaks}{the number of colour breaks to use from a palette
this will be the number of increments in the linear scale,
the number of natural log spaced steps in a log scale or the number of
evenly separated quantiles in a qualtile scale}

\item{direction}{defaults to 1, -1 inverts the colour scale}

\item{alpha}{transparency (a value between 0, opaque, and 1,
fully transparent, defaults to 0)}

\item{begin}{The beggining to the interval within the palette to sample
colours from (defaults to 0)
NB acts a little differently for brewer's discrete palettes
it rounds to the nearest whole number as a proportion of the number of
breaks you specified and truncates the palette there.}

\item{end}{The end to the interval within the palette to sample
colours from (defaults to 1)}

\item{mode}{the type of output you get is set with mode
"scaled" (the default) outputs a vector the same length as x with
appropriately scaled colour values.
"closure" returns a function to generate new scales based on x
e.g. subsets of x you want to visualize using the same global colour scale
"palette" a vector of the hex codes the names of which are the breaks}

\item{verbose}{verbose mode prints a message with the colours and breaks
as well as plotting a grid of the colours in the palette}

\item{minq}{minimum quantile probability}

\item{maxq}{maximum quantile probability}
}
\description{
A function for generating colour scales using these palettes:
\itemize{
\item viridis
\item scico
\item colour brewer
\item custom hex code based
}
}
