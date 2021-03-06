
source("func_Z0.R")
source("RadLon.R")
source("RadDat.R")

FLAVORinput <- function(fluo,depth,lon,lat,datJ){
	#Computing of the Z0 depth
	z0 <- Z0_func(fluo,depth)

	#Normalization of teh depth
	NP <- depth/z0
	#normalization of the fluorescence values
	fluo_norm <- (fluo-min(fluo,na.rm=T))/(max(fluo,na.rm=T)-min(fluo,na.rm=T))
	c <- approx(NP,fluo_norm,seq(0,1.3,0.14),rule=2)$y;

	lon_rad=RadLon(lon)
	dat_rad=RadDat(datJ)
	Lat_in=lat/90

	#inputs of the MLP
	input <- c(z0,c,sin(dat_rad),cos(dat_rad),sin(lon_rad),cos(lon_rad),Lat_in)
	#Finally, we put the depth at which the chlorophyll is computed
	depth_NP <- seq(0,1.3,0.14)
	
	input_final <- NULL
	
	for(i in 1:length(seq(0,1.3,0.14))){
    		Input_final <- c(input,depth_NP[i])
		input_final <- rbind(input_final,Input_final)
	}

	return(input_final)
}
