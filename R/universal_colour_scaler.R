#' range_check
#'
#' check if a variable with within a range and error if it is not
#'
#' @param min min
#' @param max max
#' @param var variable name
range_check <- function(min, max, var) {
	if(var > max | var < min) {
		stop(
			paste0(
				var, " must be a value between, ", min, " and ", max, ".\n"
			)
		)
	}
}

#' lseq
#'
#' logarithmic sequence
#'
#' @param from start
#' @param to end
#' @param length_out number of increments
lseq <- function(from = 1, to = 100000, length_out = 6) {
	length_out <- length_out + 1
	dirm <- if(length_out%%2 == 1) {ceiling} else {floor}
	dirp <- if(length_out%%2 == 0) {ceiling} else {floor}
	if(from < 0) {
		c(
			-seq(log(abs(from)), 0, length.out = dirm(length_out/2)),
			seq(0, log(to), length.out = dirp(length_out/2))[-1]
		)
	} else {
		exp(seq(log(from), log(to), length.out = length_out))
	}
}

#' universal_colour_scaler
#'
#' A function for generating colour scales using these palettes:
#' - viridis
#' - scico
#' - colour brewer
#' - custom hex code based
#'
#' @param x The values for which you wish to generate a colour scale
#' (a vector or matrix of numeric type)
#' @param scale The type of colour scale you would like to generate.
#' "linear", (the default), "log", "quantile" or a custom numeric vector
#' @param type the type of palette you would like to use.
#'  "scico" (the default), "viridis", "brewer" or "custom".
#'  if you specify custom
#' @param palette The name of one of the 68 colour palettes from scico,
#'  viridis (defaults to "bilbao" from scico)
#'  - scico palettes:
#'   "acton", "bamako", "batlow", "berlin", "bilbao", "broc", "brocO", "buda",
#'   "cork", "corkO", "davos", "devon", "grayC", "hawaii", "imola", "lajolla",
#'   "lapaz", "lisbon", "nuuk", "oleron", "oslo", "roma", "romaO", "tofino",
#'   "tokyo", "turku", "vik", "vikO"
#'  - viridis palettes:
#'   "viridis", "inferno", "magma", "plasma", "cividis"
#'  - brewer palettes:
#'   "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
#'   "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
#'   "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys",
#'   "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
#'   "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"
#' @param n_breaks the number of colour breaks to use from a palette
#' this will be the number of increments in the linear scale,
#'  the number of natural log spaced steps in a log scale or the number of
#'  evenly separated quantiles in a qualtile scale
#' @param direction defaults to 1, -1 inverts the colour scale
#' @param alpha transparency (a value between 0 and 1, defaults to 1)
#' @param begin The beggining to the interval within the palette to sample
#'  colours from (defaults to 0)
#'  NB acts a little differently for brewer's discrete palettes
#'  it rounds to the nearest whole number as a proportion of the number of
#'  breaks you specified and truncates the palette there.
#' @param end The end to the interval within the palette to sample
#'  colours from (defaults to 1)
#' @param mode the type of output you get is set with mode
#' "scaled" (the default) outputs a vector the same length as x with
#'  appropriately scaled colour values.
#' "closure" returns a function to generate new scales based on x
#' e.g. subsets of x you want to visualize using the same global colour scale
#' "palette" a vector of the hex codes the names of which are the breaks
#' @param verbose verbose mode prints a message with the colours and breaks
#' as well as plotting a grid of the colours in the palette
#'
#' @export
universal_colour_scaler <- function(
	x, scale = "linear", type = "scico", palette = "bilbao", n_breaks = 5,
	direction = 1, alpha = 1, begin = 0, end = 1, mode = "scaled",# closure = FALSE,
	verbose = FALSE
) {
	scales <- c("linear", "log", "quantile")
	types <- c("brewer", "scico", "viridis", "custom")
	modes <- c("closure", "palette", "scaled")

	viridis_pals <- c("viridis", "inferno", "magma", "plasma", "cividis")
	scico_pals <- scico::scico_palette_names()
	brewer_pals <- rownames(RColorBrewer::brewer.pal.info)
	#comb_pals <- c(viridis_pals, scico_pals, brewer_pals)

	colours <- NULL
	breaks <- NULL
	minx <- min(x, na.rm = TRUE)
	maxx <- max(x, na.rm = TRUE)

	range_check(0, 1, alpha)
	range_check(0, 1, begin)
	range_check(0, 1, end)

	if(!direction %in% c(1, -1)) {
		stop("direction must be 1 or -1")
	}

	if(!mode %in% modes) {
		stop(
			paste0(
				"'", mode, "' is not among the valid options: ",
				paste0(modes, collapse = ", ")
			)
		)
	}

	if(is.numeric(scale)) {
		breaks <- scale
	} else if(!scale %in% scales) {
		stop(
			paste0(
				"'", scale, "' is not among the valid options: ",
				paste0(scales, collapse = ", ")
			)
		)
	}

	if(!type %in% types) {
		stop(
			paste0(
				"'", type, "' is not among the valid options: ",
				paste0(types, collapse = ", ")
			)
		)
	}

	if(type == "custom") {
		palettelg <- grepl(
			"^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3}|[A-Fa-f0-9]{8})$",
			palette, perl = TRUE
		)
		if(palette %in% viridis_pals) {
			warning(
				paste0(
					"Looks like you specified a viridis palette (", palette,
					") with the type 'custom' and not a vector of custom ",
					"hex codes!\n DFAULTING TO type: viridis"
				)
			)
			type <- "viridis"
		} else if (palette %in% scico_pals) {
			warning(
				paste0(
					"Looks like you specified a scico palette (", palette,
					") with the type 'custom' and not a vector of custom ",
					"hex codes!\n DFAULTING TO type: scico"
				)
			)
			type <- "scico"
		} else if (palette %in% brewer_pals) {
			warning(
				paste0(
					"Looks like you specified a scico palette (", palette,
					") with the type 'custom' and not a vector of custom ",
					"hex codes!\n DFAULTING TO type: scico"
				)
			)
			type <- "brewer"
		} else if (all(palettelg)) {
			colours <- palette
		} else { #if(!all(palettelg)) {
			stop(
				paste0(
					"Not all of the value in your custom palette are valid",
					" hex codes!\n",
					"These values are invalid: ",
					paste0(palette[!palettelg], collapse = ", ")
				)
			)
		}
	}

	if(type == "scico") {
		# default bilbao?!
		if(!palette %in% scico_pals) {
			stop(
				paste0(
					"'", palette,
					"' is not among the valid options for scico: ",
					paste0(scico_pals, collapse = ", ")
				)
			)
		}
		colours <- scico::scico(
			n = n_breaks, palette = palette, direction = direction,
			#alpha = alpha,
			begin = begin, end = end
		)

	} else if (type == "viridis") {
		# default ?!
		if(!palette %in% viridis_pals) {
			stop(
				paste0(
					"'", palette,
					"' is not among the valid options for viridis: ",
					paste0(viridis_pals, collapse = ", ")
				)
			)
		}
		colours <- viridis::viridis(
			n = n_breaks, direction = direction, option = palette,
			#alpha = alpha,
			begin = begin, end = end
		)
	} else if(type == "brewer") {
		# default ?!
		if(!palette %in% brewer_pals) {
			stop(
				paste0(
					"'", palette,
					"' is not among the valid options for brewer: ",
					paste0(brewer_pals, collapse = ", ")
				)
			)
		}
		# if(alpha != 1) { warning("brewer does not support alpha")}

		max_breaks <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
		if(n_breaks > max_breaks) {
			stop(
				paste0(
					"for the palette: '", palette,
					"' the maximum number of breaks is: ", max_breaks,
					"\nsee `RColorBrewer::brewer.pal.info` for max colours by palette"
				)
			)
		} else if(n_breaks < 3) {
			warning(
				"the minimum value of n_breaks is 3 for brewer palettes"
			)
		}

		colours <- RColorBrewer::brewer.pal(n_breaks, palette)
		if(direction == -1) {
			colours <- rev(colours)
		}

		colours <- colours[round(begin * n_breaks):round(end * n_breaks)]
		n_breaks <- length(colours)
	}



	if(scale == "linear") {
		breaks <- seq(minx, maxx, length.out = n_breaks)
	} else if(scale == "log") {
		breaks <- lseq(from = minx, to = maxx, length_out = n_breaks)
	} else if(scale == "quantile") {
		breaks <- stats::quantile(
			x = x,
			probs = seq(0, 1, length.out = n_breaks),
			na.rm = TRUE
		)
	}

	scale_fun <- circlize::colorRamp2(
		transparency = alpha,
		breaks = breaks,
		colors = colours
	)

	names(colours) <- breaks

	if(verbose == TRUE) {
		message(
			paste0(
				"colours: '", paste0(colours, collapse = "', '"), "'\n",
				"breaks: ", paste0(breaks, collapse = ","), "\n"
			)
		)
		scales::show_col(colours)
	}

	if(mode == "palette") {
		return(colours)
	} else if(mode == "scaled") {
		return(scale_fun(x))
	} else if(mode == "closure") {
		return(scale_fun)
	}
}
