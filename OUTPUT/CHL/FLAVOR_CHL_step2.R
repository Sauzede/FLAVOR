#Function of the second step of the FLAVOR method : retrieval of a quasi continuous profile of total chlorophyll-a from the ten points predicted by the MLP

source("InterpWithProfile.R")

FLAVOR_CHL_step2 <- function(chlo,z0,fluo,depth){

	fluoo <- NULL
	hplcc <- NULL	

	fluoo$depth <- depth
	fluoo$fluo <- fluo
	hplcc$depth <- seq(0,1.3,0.14)*z0
	hplcc$chla <- chlo

	hplc.interp <- InterpWithProfile(hplcc, fluoo)

	Depth <- 0.1 * max(depth[!is.na(fluo)])

	depth_fin <- which(depth[!is.na(fluo)]> (max(depth[!is.na(fluo)])-Depth))

	fluo_corr <- fluo-mean(fluo[depth_fin] ,na.rm=T)

	alpha <- sum(approx(hplc.interp[[1]],hplc.interp[[2]],seq(1,z0,1),rule=2)$y)/sum(approx(depth,fluo_corr,seq(1,z0,1),rule=2)$y)

	fluo_calib <- fluo_corr*alpha

	return(fluo_calib)
}
