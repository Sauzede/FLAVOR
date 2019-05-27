

#Example for ten points retrieved by the MLP for each phytoplankton size classes (micro,nano and picophytoplankton -> MICRO_mlp, NANO_mlp and PICO_mlp) from a fluorescence profile (fluo)
#The outputs after the second step are three quasi-continuous profiles (same resolution as the initial fluorescence profile) of total chlorophyll-a concentrations associated with the three phytoplankton size classes retrieved with the FLAVOR method (MICRO_cal, NANO_cal and PICO_cal)

source("func_Z0.R")

#the fluorescence profile is loaded
data <- read.table("profil.dat")
fluo <- data[,1]
depth <- data[,2]

#The ten points retrieved by the MLP are loaded
MICRO_mlp <- read.table("MICRO_mlp.dat")
MICRO_mlp <- MICRO_mlp[,1]
NANO_mlp <- read.table("NANO_mlp.dat")
NANO_mlp <- NANO_mlp[,1]
PICO_mlp <- read.table("PICO_mlp.dat")
PICO_mlp <- PICO_mlp[,1]

Z0 <- Z0_func(fluo,depth)
#To retrieve a quasi-continuous profile, the points are interpolated linearly
depth_MLP  <- seq(0,1.3,0.14)* Z0
MICRO_cal <- approx(c(depth_MLP,1.5*Z0),c(MICRO_mlp,0),depth,rule=2)$y
NANO_cal  <- approx(c(depth_MLP,1.5*Z0),c(NANO_mlp,0),depth,rule=2)$y
PICO_cal  <- approx(c(depth_MLP,1.5*Z0),c(PICO_mlp,0),depth,rule=2)$y
