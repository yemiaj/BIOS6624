##########################################
# Function to generate data from the hdrm package using specified parameters
##########################################

library(hdrm) #Used to generate data

#Function to generate data
dat.gen <- function(n, rho){
  
  dat <- hdrm::gen_data(
    n = n,
    p = 20,
    p1 = 5,
    beta = c(0.5/3, 1/3, 1.5/3, 2.0/3, 2.5/3, rep(0,15)),
    family = "gaussian",
    SNR = 1,
    signal = "homogeneous",
    corr = "exchangeable",
    rho = rho
  )
  
  mydat <- data.frame(cbind(dat$y, dat$X))
  colnames(mydat) <- c("y",paste0("v",1:20))
  
  return(mydat)
}

#set.seed(1)
#test <- dat.gen(250,0.7)
