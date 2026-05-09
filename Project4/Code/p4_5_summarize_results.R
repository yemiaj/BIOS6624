##########################################
# Code/function to summarize results, create tables and figures
##########################################

library(knitr)
library(kableExtra)

#Import .csv file of all results
all.res <- read.csv("./Project4/DataProcessed/bios6624_p4_sim_results.csv")



###########################
#Summarize Bias
##########################
bias_cols <- paste0("bias", c(1:6,11)) #Define column names
all.res_bias <- all.res[, c("method", "n", "rho", bias_cols)] #Extract defined columns from larger dataset
means_bias <- aggregate(all.res_bias[, bias_cols], #Calculate mean bias by method, rho, and n
                        by = list(method = all.res_bias$method, rho = all.res_bias$rho, n= all.res_bias$n), 
                        FUN = function(x) mean(x, na.rm = TRUE)
                        )

names(means_bias)[names(means_bias) %in% paste0("bias", c(1:6,11))] <- paste0("X", c(1:6,11)) #rename variables from bias1-bias6 to X1-X6

means_bias2 <- reshape(means_bias, #Transform table from long to wide so it matches desired final table
                       idvar     = c("method", "rho"),
                       timevar   = "n",
                       direction = "wide"
                       )

means_bias2$X6.250[means_bias2$method=="1_Oracle (The Truth)"] <- NA
means_bias2$X6.500[means_bias2$method=="1_Oracle (The Truth)"] <- NA
means_bias2$X11.250[means_bias2$method=="1_Oracle (The Truth)"] <- NA
means_bias2$X11.500[means_bias2$method=="1_Oracle (The Truth)"] <- NA

means_bias2 <- means_bias2[order(means_bias2$method, means_bias2$rho), ]
cbind(means_bias2$method, round(means_bias2[,-1],3))



###########################
#Summarize Rejection
##########################
rej_cols <- paste0("p.sig", c(1:6,11))
all.res_rej <- all.res[, c("method", "n", "rho", rej_cols)]
means_rej <- aggregate(all.res_rej[, rej_cols],
                       by = list(method = all.res_rej$method, rho = all.res_rej$rho, n= all.res_rej$n),
                       FUN = function(x) mean(x, na.rm = TRUE)
                       )

names(means_rej)[names(means_rej) %in% paste0("p.sig", c(1:6,11))] <- paste0("X", c(1:6,11)) #rename variables from bias1-bias6 to X1-X6

means_rej2 <- reshape(means_rej, #Transform table from long to wide so it matches desired final table
                       idvar     = c("method", "rho"),
                       timevar   = "n",
                       direction = "wide")

means_rej2$X6.250[means_rej2$method=="1_Oracle (The Truth)"] <- NA
means_rej2$X6.500[means_rej2$method=="1_Oracle (The Truth)"] <- NA
means_rej2$X11.250[means_rej2$method=="1_Oracle (The Truth)"] <- NA
means_rej2$X11.500[means_rej2$method=="1_Oracle (The Truth)"] <- NA

means_rej2 <- means_rej2[order(means_rej2$method, means_rej2$rho), ]
cbind(means_rej2$method, round(means_rej2[,-1],3))



###########################
#Summarize TP and FP rates
##########################
tpf_cols <- c(paste0("tp", 1:6), "fp1")
all.res_tpf <- all.res[, c("method", "n", "rho", tpf_cols)]
means_tpf <- aggregate(all.res_tpf[, tpf_cols],
                       by = list(method = all.res_tpf$method, rho = all.res_tpf$rho, n= all.res_tpf$n),
                       FUN = function(x) mean(x, na.rm = TRUE)
                       )

#names(means_tpf)[names(means_tpf) %in% c(paste0("tp", 1:6), "fp1")] <- c(paste0("X", 1:5), "XX6", "FP6.20")
means_tpf2 <- reshape(means_tpf, 
                      idvar     = c("method", "rho"),
                      timevar   = "n",
                      direction = "wide")

means_tpf2 <- means_tpf2[order(means_tpf2$method, means_tpf2$rho), ]

means_tpf2$fp1.250[means_tpf2$method=="1_Oracle (The Truth)"] <- NA
means_tpf2$fp1.500[means_tpf2$method=="1_Oracle (The Truth)"] <- NA

cbind(means_tpf2$method, round(means_tpf2[,-1],3))

write.csv(cbind(means_tpf2$method, round(means_tpf2[,-1],3)), "./Project4/Reports/tab.tp.csv")


###########################
#Summarize and plot coverage
##########################









#Table output option 1
int.models <- flextable(int.models)
int.models <- autofit(int.models)
save_as_docx(int.models, path = "./Project3/Reports/table_figures/Supp. Table 2.docx")

#Table output option 2
kable_bias <- kable(bias_table, format  = "html", digits  = 3, caption = "Mean Bias by Method, Correlation (ρ), and Sample Size") %>% 
  kable_styling(bootstrap_options = c("condensed", "striped"), full_width = FALSE)
kable_bias

#Plot option 1
boxplot(means_bias$X1~means_bias$method + means_bias$rho + means_bias$n)


#Summarizing results
d1 <- aggregate(bias1 ~ method + n + rho, data = all.reuslts, FUN = mean, na.rm = TRUE)
d1<-d1[order(d1$method, d1$n, d1$rho),]







#Need to test that each seed result if reproducible; as well as aggrergation of mean is working fine
Table for bias + reject
Table for tp1-6, fp1
Plot for coverage




