

#Example for ten points retrieved by the MLP (CHL_mlp) from a fluorescence profile (fluo)
#The output after the second step is a quasi-continuous profile (same resolution as the initial fluorescence profile) of total chlorophyll-a concentration retrieved with the FLAVOR method (CHL_cal)

source("func_Z0.R")
source("FLAVOR_CHL_step2.R")

#the fluorescence profile is loaded
data <- read.table("profil.dat")
fluo <- data[,1]
depth <- data[,2]

CHL_mlp <- read.table("CHL_mlp.dat")
CHL_mlp <- CHL_mlp[,1]
Z0 <- Z0_func(fluo,depth)

output <- FLAVOR_CHL_step2(CHL_mlp,Z0,fluo,depth)
CHL_cal <- output
plot(CHL_cal,-depth,type="l")
