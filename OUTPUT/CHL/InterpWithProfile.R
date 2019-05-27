
#Interpolation function from Morel and Maritorena (2001)

InterpWithProfile <- function(meas, prof) {
	delta <- approx(meas[[1]], approx(prof[[1]], prof[[2]], meas[[1]])$y - meas[[2]], prof[[1]], rule = 1)$y
	data.frame(prof[[1]], prof[[2]] - delta)
}

