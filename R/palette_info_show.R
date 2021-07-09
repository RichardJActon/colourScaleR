#' pal_info_grob
#'
pal_info_grob <- function(pal_inf) {
	mx <- ifelse(is.finite(pal_inf$maxcolors),pal_inf$maxcolors, 11)
	palette <- colourScaleR::universal_colour_scaler(
		x = 1:mx, type = pal_inf$package,
		palette = pal_inf$palette, n_breaks = mx
	)

	# smooth <- ifelse(pal_inf$category = "qual", FALSE, TRUE)

	grab <- grid::grid.grabExpr(expr = {
		grid::pushViewport(
			grid::viewport(gp = gpar(fill = "white", col = "black", lwd = 2))
		)
		grid::grid.raster(
			width = 0.9, height = 0.6, matrix(palette, nrow = 1),
			interpolate = ifelse(pal_inf$category == "qual", FALSE, TRUE)
		)
		grid::grid.rect(
			width = 0.9, height = 0.6,
			gp = grid::gpar(lwd = 2, col = "black", fill = NA)
		)
		grid::grid.text(
			paste0(pal_inf$package, " : ", pal_inf$palette),
			x = 0.5, y = 0.9#, rot = 90
		)
		grid::grid.text(
			paste0(
				"type: ", pal_inf$category, " max: ", pal_inf$maxcolors, " ",
				ifelse(pal_inf$colorblind == TRUE, "", "(NOT Colour blind safe!)") #(Colour blind friendly)
			), x = 0.5, y = 0.1#, rot = 90
		)
	})
}

#' palette_info_show
#'
#' Visualize palette information
#'
#' @param colour_blind_frieldly TRUE by default
#' @param packages from which packages to display palettes
#' @param type qualitative, divergent, sequential, or split palette type
#' @param mincolours minimum number of colours needed, default 0
#'
#' @export
palette_info_show <- function(
	colour_blind_frieldly = TRUE, packages = NULL,
	type = NULL, mincolours = 0
) {
	#types <- c("qualitative", "divergent", "sequential", "split")
	types <- c("qual", "div", "seq", "split")
	pal_info <- colourScaleR::combined_palette_info

	avail_packages <- c("brewer", "scico", "viridis")

	if(colour_blind_frieldly == TRUE) {
		pal_info <- pal_info[pal_info$colorblind == TRUE,]
	}
	if(!is.null(packages)) {
		if(!all(packages %in% avail_packages)) {
			stop(
				paste0(
					"'", paste0(packages, collapse = ", "),
					"' are not among the valid options: ",
					paste0(avail_packages, collapse = ", ")
				)
			)
		} else {
			pal_info <- pal_info[pal_info$package %in% packages,]
		}
	}
	if(!is.null(type)) {
		if(!all(type %in% types)) {
			stop(
				paste0(
					"'", paste0(type, collapse = ", "),
					"' are not among the valid options: ",
					paste0(types, collapse = ", ")
				)
			)
		} else {
			pal_info <- pal_info[pal_info$package %in% packages,]
		}
	}
	if(!is.null(mincolours)) {
		pal_info <- pal_info[pal_info$maxcolors >= mincolours,]
	}

	lst <- purrr::map(
		#sample(1:68,8),
		1:nrow(pal_info),
		~pal_info_grob(pal_info[.x,])
	)

	ngrobs <- length(lst)
	sqr <- ceiling(sqrt(ngrobs))
	ul <- ifelse(sqr > 5, 5, sqr)
	cols <- ifelse(ngrobs<ul, ngrobs, ul)
	grobs <- gridExtra::arrangeGrob(grobs = lst, ncol = cols)
	gridExtra::grid.arrange(grobs)
}

# palette_info_show(type = "qual")
