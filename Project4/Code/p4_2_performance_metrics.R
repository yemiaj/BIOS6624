##########################################
#Function to compute metrics of interest including bias estimate, significance indicator, coverage indicator,
    #indicator for true positive (TP) for each of the 5 main predictors (X1-X5), proportion of selection among X1-X5, and
    #proportion of selection for X6-X20 as false positive (FP)
#The function takes as input a matrix with 4 columns of beta estimate, p-value, LL, and UL of 95% CI with exact names
    # c(Estimate     Pr(>|t|)       2.5 %     97.5 %)

#This function was modified using Microsoft Copilot
##########################################

compute.metrics <- function(mat) {
  
  true_beta <- c(0.5/3, 1/3, 1.5/3, 2.0/3, 2.5/3, rep(0, 15)) #True beta
  names(true_beta) <- paste0("v", 1:20) #Name true beta so that it matches the names provided in the dat.gen() function
  
  res <- data.frame(matrix(ncol = 67, nrow = 0)) #The computed metrics will be stored in this data frame, it has ncol columns
  names(res) <- c(
    paste0("bias", 1:20),   #bias, first 20 columns stores bias for X1-X20
    paste0("p.sig", 1:20),  #significance indicator, next 20 stores significance indicator for X1-X20
    paste0("cov", 1:20),     #coverage indicator, same, coverage for X1-X20
    paste0("tp", 1:6), #variables related to TP for X1-X5 & their sum
    "fp1" #FP for X6-X20
    )
  
  new_row <- res[0, ][1, ] #A new row of data frame with same name as res
  
  for (j in 1:20) {   #Since the bias, significance, and coverage metrics comes in set of 20, this loops from 1 to 20
    
    vname <- paste0("v", j)   #Define names for the metrics
    bname <- paste0("bias", j)
    pname <- paste0("p.sig", j)
    cname <- paste0("cov", j)
    
    ## ---- define beta_hat ----
    if (vname %in% rownames(mat)) {   #Loop for variables selected by any of the procedures
      
      beta_hat <- mat[vname, "Estimate"] #Estimated betas
      new_row[[pname]] <- as.integer(mat[vname, "Pr(>|t|)"] <= 0.05)  #Indicator for p<=0.05
      new_row[[cname]] <- as.integer(mat[vname, "2.5 %"] <= true_beta[vname] && true_beta[vname] <= mat[vname, "97.5 %"])  #Indicator for coverage
      
    } else {  #Else statement for variables not selected
      
      beta_hat <- 0 
      new_row[[pname]] <- 0
      new_row[[cname]] <- 0
    }
    
    new_row[[bname]] <- beta_hat - true_beta[vname] #Estimate bias after defining the beta for unselected variables as 0
  }
  
  #Define TP and FP
  sel <- as.integer(paste0("v", 1:20) %in% rownames(mat)) #Indicator for which of X1-X20 were selected
  names(sel) <- paste0("v", 1:20) #Assign name to sel
  new_row[paste0("tp", 1:5)] <- sel[1:5]  #TP indicator for X1-X5
  new_row[["tp6"]] <- sum(sel[1:5]) / 5   #TP fraction for X1-X5
  new_row[["fp1"]] <- sum(sel[6:20]) / 15 #FP for X6-X20
  
  #Output all metrics
  res <- rbind(res, new_row) #Combine results into res and output res
  return(res)
}
