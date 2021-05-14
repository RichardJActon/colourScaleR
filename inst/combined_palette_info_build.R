RColorBrewer_colour_info <- as.data.frame(RColorBrewer::brewer.pal.info)
RColorBrewer_colour_info$palette <- rownames(RColorBrewer_colour_info)
rownames(RColorBrewer_colour_info) <- NULL

RColorBrewer_colour_info$package <- "brewer"

scico_palette_info <- data.frame(
	maxcolors = Inf,
	category = c(
		"seq","seq","seq","div","seq",
		"div","div","seq","div","div",
		"seq","seq","seq","div","seq",
		"seq","seq","div","seq","step",
		"seq","div","div","div","seq",
		"seq","div","div"
	),
	colorblind = TRUE,
	palette = scico::scico_palette_names(),
	package = "scico",
	stringsAsFactors = FALSE
)

viridis_palette_info <- data.frame(
	maxcolors = Inf,
	category = rep("div", 5),
	colorblind = TRUE,
	palette = c("viridis", "inferno", "magma", "plasma", "cividis"),
	package = "viridis",
	stringsAsFactors = FALSE
)

combined_palette_info <- do.call("rbind", list(
	RColorBrewer_colour_info, scico_palette_info, viridis_palette_info
))

usethis::use_data(combined_palette_info, overwrite = TRUE)

# combined_palette_info

# combined_palette_show <- function() {
#
# }

# maPalette <- function(low = "white", high = c("green", "red"), mid=NULL, k =50)
# {
# 	low <- col2rgb(low)/255
# 	high <- col2rgb(high)/255
#
# 	if(is.null(mid)){
# 		r <- seq(low[1], high[1], len = k)
# 		g <- seq(low[2], high[2], len = k)
# 		b <- seq(low[3], high[3], len = k)
# 	}
# 	if(!is.null(mid)){
# 		k2 <- round(k/2)
# 		mid <- col2rgb(mid)/255
# 		r <- c(seq(low[1], mid[1], len = k2),
# 			   seq(mid[1], high[1], len = k2))
# 		g <- c(seq(low[2], mid[2], len = k2),
# 			   seq(mid[2], high[2], len = k2))
# 		b <- c(seq(low[3], mid[3], len = k2),
# 			   seq(mid[3], high[3], len = k2))
# 	}
# 	rgb(r, g, b)
# }
