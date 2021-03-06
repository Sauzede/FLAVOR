

filtre.mediane <- function(y, m){
 y <- as.numeric(y)
 ny <- length(y)
 res <- rep(NA, ny)
 for (i in 1:ny){
   indi <- (i-m):(i+m)
   if (i<m) indi <- 1:(i+m)
   if (i+m>ny) indi <- i:ny
   tmpi <- y[indi]
   ok <- length(tmpi[!is.na(tmpi) & is.finite(tmpi)]) > 0
   if (ok) res[i] <- median(tmpi, na.rm=T)
 }
 res
}


Z0_func <- function(fluo,depth){

	fluo2 <- filtre.mediane(fluo,10)

	fluo_end=mean(fluo2[(length(fluo2)-10):length(fluo2)])
    
	fluo2 <- fluo2-fluo_end

	depth <- depth[fluo2>0]
	fluo2 <- fluo2[fluo2>0]

	fluo_norm <- (fluo2-min(fluo2))/(max(fluo2)-min(fluo2))
	fluo_norm <- round(fluo_norm,1)

	D <- which(fluo_norm==0 & depth>depth[which.max(fluo_norm)])[1]

	depth_0 <- depth[D]

	return(depth_0)
}
