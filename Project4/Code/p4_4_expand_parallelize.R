##########################################
# Function to extend analysis to the 6 scenarios and perform simulation in parallel
##########################################

library(parallel)

#Expand to all 6 scenarios of interest
scens <- expand.grid(rho = c(0, 0.35, 0.7), n = c(250, 500))

#######################################
#Test run simulation for 100 iterations
#######################################
t_start <- Sys.time() #Define simulation start time

nsim <- 100   #Do 100 sims to check this iteration flow
res_list <- vector("list", nsim * nrow(scens)) #initiate vector to store the results of each iteration
iter <- 1 #Iteration counter

for (g in seq_len(nrow(scens))) {
  for (i in 1:nsim) {
    res_list[[iter]] <- analyze.data(n = scens$n[g], rho  = scens$rho[g], seed = i)
    iter <- iter + 1
  }
}
all.results <- do.call(rbind, res_list) #Extract and combine all results to this dataframe

t_end <- Sys.time() #End of simulation run
runtime <- t_end - t_start #How long simulation took
runtime
#100 iterations took about ~5.5 minutes on a single core of a PC with 13th Gen Intel Core i7-1355U processor, and 64GB RAM

#################################################################
#Perform simulation in parallel for 10K iterations
#This function was written with the help of Microsoft Copilot
################################################################
ncores <- detectCores() - 2  # Leave 2 cores for other computer activities
cl <- makeCluster(ncores)

clusterExport(cl, varlist = c("dat.gen", "compute.metrics","analyze.data"), envir = environment()) #Making sure all needed functions and packages are available in the clusters
clusterEvalQ(cl, {
  library(hdrm)
  library(olsrr)
  library(glmnet)
  library(stats)
  })

t_start <- Sys.time()

nsim <- 10000
res_list <- vector("list", nrow(scens))

for (g in seq_len(nrow(scens))) {
  
  n_g   <- scens$n[g]
  rho_g <- scens$rho[g]
  
  res_list[[g]] <- parLapply(
    cl,
    X = 1:nsim,
    fun = function(i, n , rho) {
      analyze.data(n = n, rho  = rho, seed = i)
    },
    n   = n_g, rho = rho_g
    )
}
full.results <- do.call(rbind, unlist(res_list, recursive = FALSE))

stopCluster(cl) #Stop cluster

#Export data to project directory
write.csv(full.results, "./Project4/DataProcessed/bios6624_p4_sim_results.csv")

t_end <- Sys.time() #End of simulation run
runtime <- t_end - t_start #How long simulation took
runtime
#Time difference of 3.21167 hours
#Running 10K simulations using 10 cores took 3.21 hours (see computer specs above), better spec'd PCs will achieve faster results