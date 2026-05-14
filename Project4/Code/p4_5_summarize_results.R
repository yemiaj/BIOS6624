##########################################
# Code/function to summarize results, create tables and figures
##########################################

library(ggplot2) #For fancy  plots
library(flextable) #To output table to Word file

#Import .csv file of all results
all.res <- read.csv("./Project4/DataProcessed/bios6624_p4_sim_results.csv")

###########################
#Aggregate Bias
##########################
bias_cols <- paste0("bias", c(1:6,11)) #Define column names
all.res_bias <- all.res[, c("method", "n", "rho", bias_cols)] #Extract defined columns from larger dataset
means_bias <- aggregate(all.res_bias[, bias_cols], #Calculate mean bias by method, rho, and n
                        by = list(method = all.res_bias$method, rho = all.res_bias$rho, n= all.res_bias$n), 
                        FUN = function(x) mean(x, na.rm = TRUE)
                        )

means_bias$bias6[means_bias$method=="1_Oracle (The Truth)"] <- NA #X6 and X11 variables are not included in the Oracle model
means_bias$bias11[means_bias$method=="1_Oracle (The Truth)"] <- NA

names(means_bias)[names(means_bias) %in% paste0("bias", c(1:6,11))] <- paste0("X", c(1:6,11)) #rename variables from bias1-bias6,11 to X1-X6,11

means_bias2 <- reshape(means_bias, #Reshape from wide to long
                       varying   = c("X1", "X2", "X3", "X4", "X5", "X6", "X11"),
                       v.names   = "bias",
                       timevar   = "variable",
                       times     = c("X1", "X2", "X3", "X4", "X5", "X6", "X11"),
                       direction = "long")
means_bias2$id <- NULL

###########################
#Aggregate Rejection
##########################
rej_cols <- paste0("p.sig", c(1:6,11))
all.res_rej <- all.res[, c("method", "n", "rho", rej_cols)]
means_rej <- aggregate(all.res_rej[, rej_cols], by = list(method = all.res_rej$method, rho = all.res_rej$rho, n= all.res_rej$n), FUN = function(x) mean(x, na.rm = TRUE))
means_rej$p.sig6[means_rej$method=="1_Oracle (The Truth)"] <- NA
means_rej$p.sig11[means_rej$method=="1_Oracle (The Truth)"] <- NA
names(means_rej)[names(means_rej) %in% paste0("p.sig", c(1:6,11))] <- paste0("X", c(1:6,11)) #rename variables from bias1-bias6 to X1-X6
means_rej2 <- reshape(means_rej, varying = c("X1", "X2", "X3", "X4", "X5", "X6", "X11"), v.names = "reject", 
                      timevar = "variable", times = c("X1", "X2", "X3", "X4", "X5", "X6", "X11"), direction = "long")
means_rej2$id <- NULL

###########################
#Aggregate coverage
##########################
rej_covs <- paste0("cov", c(1:6,11))
all.res_cov <- all.res[, c("method", "n", "rho", rej_covs)]
means_cov <- aggregate(all.res_cov[, rej_covs], by = list(method = all.res_cov$method, rho = all.res_cov$rho, n = all.res_cov$n), FUN = function(x) mean(x, na.rm = TRUE))
means_cov$cov6[means_cov$method=="1_Oracle (The Truth)"] <- NA
means_cov$cov11[means_cov$method=="1_Oracle (The Truth)"] <- NA
names(means_cov)[names(means_cov) %in% paste0("cov", c(1:6,11))] <- paste0("X", c(1:6,11))
means_cov2 <- reshape(means_cov, varying = c("X1", "X2", "X3", "X4", "X5", "X6", "X11"), v.names = "coverage",
                      timevar = "variable", times = c("X1", "X2", "X3", "X4", "X5", "X6", "X11"), direction = "long")
means_cov2$id <- NULL

###########################
#Aggregate TP and FP rates
##########################
tpf_cols <- c(paste0("tp", 1:6), "fp1")
all.res_tpf <- all.res[, c("method", "n", "rho", tpf_cols)]
means_tpf <- aggregate(all.res_tpf[, tpf_cols], by = list(method = all.res_tpf$method, rho = all.res_tpf$rho, n= all.res_tpf$n), FUN = function(x) mean(x, na.rm = TRUE))
means_tpf$fp1[means_tpf$method=="1_Oracle (The Truth)"] <- NA
names(means_tpf)[names(means_tpf) %in% c(paste0("tp", 1:6), "fp1")] <- paste0("X", c(1:6,11)) #Note that tp1,...,tp6,fp1 is now mapped directly to X1,...,X6,X11
means_tpf2 <- reshape(means_tpf, varying = c("X1", "X2", "X3", "X4", "X5", "X6", "X11"), v.names = "tpfp", 
                      timevar = "variable", times = c("X1", "X2", "X3", "X4", "X5", "X6", "X11"), direction = "long")
means_tpf2$id <- NULL

###########################
#Merge all the aggregated tables, one at a time
##########################
agg.ests <- merge(means_bias2, means_rej2, by = c("method", "rho", "n", "variable"))
agg.ests <- merge(agg.ests, means_cov2, by = c("method", "rho", "n", "variable"))
agg.ests <- merge(agg.ests, means_tpf2, by = c("method", "rho", "n", "variable"))

old_labs <- c("1_Oracle (The Truth)", "2_Backwards p-value", "3_Backwards AIC", "4_Backwards BIC", "5_LASSO Lambda-min", "6_LASSO Lambda-1se", "7_ElasticNet Lambda-min", 
                "8_ElasticNet Lambda-1se", "9_ElasticNet (a.95) Lambda-min", "91_ElasticNet (a.95) Lambda-1se")

new_labs <- c("Oracle", "Backwards (p-value)", "Backwards (AIC)", "Backwards (BIC)", "LASSO (Lambda-min)", "LASSO (Lambda-1se)", "Elastic Net a=0.5 (Lambda-min)", "Elastic Net a=0.5 (Lambda-1se)", 
                "Elastic Net a=0.95 (Lambda-min)", "Elastic Net a=0.95 (Lambda-1se)")
agg.ests$method <- new_labs[match(agg.ests$method, old_labs)] #Relabel variable values

agg.ests$variable <- factor(agg.ests$variable, levels = c("X1", "X2", "X3", "X4", "X5", "X6", "X11"))
agg.ests$rho <- as.factor(agg.ests$rho)
agg.ests$n   <- as.factor(agg.ests$n)
agg.ests$method <- factor(agg.ests$method, 
                          levels = rev(c("Oracle", "Backwards (p-value)", "Backwards (AIC)", "Backwards (BIC)", "LASSO (Lambda-min)", "LASSO (Lambda-1se)", "Elastic Net a=0.5 (Lambda-min)",
                                         "Elastic Net a=0.5 (Lambda-1se)", "Elastic Net a=0.95 (Lambda-min)", "Elastic Net a=0.95 (Lambda-1se)")))

agg.ests <- agg.ests[order(agg.ests$method, agg.ests$rho, agg.ests$n), ]


###########################
#Plot of bias (tabulate too)
##########################
fig1 <- ggplot(agg.ests, aes(x = bias, y = method, color = n, shape = n)) + 
  geom_point(size = 4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_grid(rho ~ variable, scales = "fixed", labeller = labeller(rho = label_both)) +
  labs(title = "Figure 1: Mean Bias per Variable by Selection Method, Sample Size and Correlation", x = "Bias", y = "Method", color = "N", shape = "N") +
  theme_bw() +
  theme(axis.text.y  = element_text(size = 10),
        strip.text.x = element_text(face = "bold", size = 12),
        strip.text.y = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text  = element_text(size = 11)) + 
  scale_x_continuous(breaks = seq(-0.1, 0.03, by = 0.02), limits = c(-0.1, 0.03))
fig1 #the warning message is about the NAs for X6 and X11 for the Oracle method. As noted above, only X1-X5 variable are fitted for the Oracle model
ggsave("./Project4/Reports/table_figures/Figure1.pdf", plot = fig1, width = 36, height = 12, units = "in") #See comment above regarding the cause of this warning

#Tabulate bias
means_bias3 <- reshape(means_bias, #Transform table from long to wide so it matches desired final table
                       idvar     = c("method", "rho"),
                       timevar   = "n",
                       direction = "wide")
means_bias3 <- means_bias3[order(means_bias3$rho, means_bias3$method), ] #order by method and rho
means_bias3[, 3:ncol(means_bias3)] <- round(means_bias3[, 3:ncol(means_bias3)], 3) #round to 3 dp
means_bias3

tab1 <- flextable(means_bias3)
tab1 <- autofit(tab1)
save_as_docx(tab1, path = "./Project4/Reports/table_figures/Table1.docx")

###########################
#Plot of coverage (tabulate too)
##########################
fig2 <- ggplot(agg.ests, aes(x = method, y = coverage, color = n, shape = n)) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  facet_grid(rho ~ variable, scales = "fixed",
             labeller = labeller(rho = label_both)) +
  labs(title = "Figure 2: Mean Coverage per Variable by Selection Method, Sample Size and Correlation",
       x = "Method", y = "Coverage",
       color = "N", shape = "N") +
  theme_bw() +
  theme(axis.text.x  = element_text(size = 10, angle = 45, hjust = 1),
        strip.text.x = element_text(face = "bold", size = 12),
        strip.text.y = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text  = element_text(size = 11)) +
  scale_x_discrete(limits = rev(levels(agg.ests$method))) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1))
fig2 
ggsave("./Project4/Reports/table_figures/Figure2.pdf", plot = fig2, width = 32, height = 12, units = "in") #See comment above regarding the cause of this warning

#Tabulate coverage
means_cov3 <- reshape(means_cov, idvar = c("method", "rho"), timevar   = "n", direction = "wide")
means_cov3 <- means_cov3[order(means_cov3$rho, means_cov3$method), ]
means_cov3[, 3:ncol(means_cov3)] <- round(means_cov3[, 3:ncol(means_cov3)], 2) #to 2 dp
means_cov3

tab2 <- flextable(means_cov3)
tab2 <- autofit(tab2)
save_as_docx(tab2, path = "./Project4/Reports/table_figures/Table2.docx")

###########################
# Tabulate TP & FP
##########################
means_tpf_ <- means_tpf
names(means_tpf_)[names(means_tpf_) %in% paste0("X", c(1:6,11))] <- c(paste0("X", 1:5), "XX6", "FP6.20")
means_tpf3 <- reshape(means_tpf_, idvar = c("method", "rho"), timevar = "n", direction = "wide")
means_tpf3 <- means_tpf3[order(means_tpf3$rho, means_tpf3$method), ]
means_tpf3[, 3:ncol(means_tpf3)] <- round(means_tpf3[, 3:ncol(means_tpf3)], 2)
means_tpf3

tab4 <- flextable(means_tpf3)
tab4 <- autofit(tab4)
save_as_docx(tab4, path = "./Project4/Reports/table_figures/Table4.docx")

#Plot to see pattern
ggplot(agg.ests, aes(x = tpfp, y = method, color = n, shape = n)) +
  geom_point(size = 4) +
  geom_vline(xintercept = 0.95, linetype = "dashed", color = "black") +
  facet_grid(rho ~ variable, scales = "fixed",
             labeller = labeller(rho = label_both)) +
  labs(title = "TP & FP by Method, Variable, Sample Size and Correlation",
       x = "Bias", y = "Method",
       color = "N", shape = "N") +
  theme_bw() +
  theme(axis.text.y  = element_text(size = 10),
        strip.text.x = element_text(face = "bold", size = 12),
        strip.text.y = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text  = element_text(size = 11)) #See comment above regarding the cause of this warning


###########################
# Tabulate rejection
##########################
names(means_rej)[names(means_rej) %in% paste0("p.sig", c(1:6,11))] <- paste0("X", c(1:6,11))
means_rej3 <- reshape(means_rej, idvar = c("method", "rho"), timevar = "n", direction = "wide")
means_rej3 <- means_rej3[order(means_rej3$rho, means_rej3$method), ]
means_rej3[, 3:ncol(means_rej3)] <- round(means_rej3[, 3:ncol(means_rej3)], 2)
means_rej3

tab3 <- flextable(means_rej3)
tab3 <- autofit(tab3)
save_as_docx(tab3, path = "./Project4/Reports/table_figures/Table3.docx")

#Plot to see pattern
ggplot(agg.ests, aes(x = reject, y = method, color = n, shape = n)) +
  geom_point(size = 4) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "black") +
  facet_grid(rho ~ variable, scales = "fixed",
             labeller = labeller(rho = label_both)) +
  labs(title = "Rejection by Method, Variable, Sample Size and Correlation",
       x = "Bias", y = "Method",
       color = "N", shape = "N") +
  theme_bw() +
  theme(axis.text.y  = element_text(size = 10),
        strip.text.x = element_text(face = "bold", size = 12),
        strip.text.y = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text  = element_text(size = 11)) #See comment above regarding the cause of this warning
