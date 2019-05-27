##################################################
############# R code for FLAVOR method ###########
##################################################

# The inputs of the neural network are:
# 10 points of the normalized fluorescence profile, depth Z0, location and date (5 values), dimensionless depth (eta) of the output
# The output of the neural network is:
# chlorophyll-a concentration for the depth eta defined in input

# The input file must be a mxn format with m, the number of examples and n=17 (inputs)

rm(list=ls())
graphics.off()

temporaire <- read.table("FLAVOR_PSC_weight_17x9x5x3.sn",skip=1)
poids <- temporaire[,3]
rm(temporaire)

# Number of hidden layers
nc <- 2

# Number of neurons for each layer 

# Number of inputs
ne <- 17
 
# Number of neurons of the first hidden layer
nc1 <- 9

# Number of neurons of the second hidden layer
nc2 <- 5

# Number of outputs
ns <- 3

# Definition of the size of the weights and biases parameters files
b1 <- rep(1,nc1)
b2 <- rep(1,ns)
b3 <- rep(1,nc2)

w1 <- matrix(1, nrow=nc1,ncol=ne)
w2c2 <- matrix(1,nrow=ns,ncol=nc2)
w3 <- matrix(1,nrow=nc2,ncol=nc1)

# WEIGHT PARAMETERS   
   # weight and bias from the input layer to the first hidden layer w1, b1
  
     for(j in 1:nc1){
         b1[j] <- poids[j]
     }    
     
     k <- nc1+nc2+ns+1
     for(i in 1:ne){
         for(j in 1:nc1){
             w1[j,i] <- poids[k]
            k <- k+1
         }    
     }

   # weight and bias from the first hidden layer to the second hidden layer

     for(j in (nc1+1):(nc1+nc2)){
	 b3[j-nc1] <- poids[j]
     }

     for(i in 1:nc1){
         for(j in 1:nc2){
             w3[j,i] <- poids[k]
	    k <- k+1
	 }
     }

   # weight and bias from the second hidden layer to the output layer w3, b3    
 
      for(j in (nc1+nc2):(nc1+nc2+ns)){
            b2[j-(nc1+nc2)] <- poids[j]
        }
   
 
      for(j in 1:nc2){
        for(i in 1:ns){
            w2c2[i,j] <- poids[k]
            k <- k+1
        }
      }

### Mean and standard deviation of the training dataset
### These values are used to normalize the inputs parameters

Moy <- read.table("Mean_Training_FLAVOR_PSC.dat")
Ecart <- read.table("Std_Training_FLAVOR_PSC.dat")

### DATA is the user's input file

data <- read.table("validation_dataset_psc.dat")
#data=read.table("profil_input.dat")

### NORMALISATION OF THE INPUT PARAMETERS

data_N <- data[,1:ne]
col <- c(1:11,17)
for(i in 1:12){
	data_N[,col[i]]=(2/3)*((data[,col[i]]-Moy[,i])/Ecart[,i])
}

# Two hidden layers

data_N <- as.matrix(data_N)

rx <- dim(data_N)[1]
cx <- dim(data_N)[2]
a <- 1.715905*tanh((2./3)*(data_N %*% t(w1)+t(b1 %*% t(rep(1,rx)))))
b <- 1.715905*tanh((2./3)*(a %*% t(w3)+t(b3 %*% t(rep(1,rx))))) 
# two hidden layer output

y <- b %*% t(w2c2)+t(b2 %*% t(rep(1,rx)))

### Y is the output values of the neural network, 

### Denormalisation of the output of the NN for getting the true value of CHL
Estimated_MICRO <- 10.^(1.5*y[,1]*Ecart[,13]+Moy[,13])
Estimated_NANO <- 10.^(1.5*y[,2]*Ecart[,14]+Moy[,14])
Estimated_PICO <- 10.^(1.5*y[,3]*Ecart[,15]+Moy[,15])

#write.table(Estimated_MICRO, file="MICRO_mlp.dat", col.names=F, row.names=F)
#write.table(Estimated_NANO, file="NANO_mlp.dat", col.names=F, row.names=F)
#write.table(Estimated_PICO, file="PICO_mlp.dat", col.names=F, row.names=F)

## To check if everything is ok
#Desired_MICRO=10.^(data[,18])
#Desired_NANO=10.^(data[,19])
#Desired_PICO=10.^(data[,20])

#plot(Desired_MICRO,Estimated_MICRO,log="xy",pch=19,xlim=c(0.0001,20),ylim=c(0.0001,20))
#plot(Desired_NANO,Estimated_NANO,log="xy",pch=19,xlim=c(0.0001,20),ylim=c(0.0001,20))
#plot(Desired_PICO,Estimated_PICO,log="xy",pch=19,xlim=c(0.0001,20),ylim=c(0.0001,20))




