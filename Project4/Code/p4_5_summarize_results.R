##########################################
# Code/function to summarize results, create tables and figures
##########################################

#Import .csv file of all results
#gtsummary?


#Summarizing results
d1 <- aggregate(bias1 ~ method + n + rho, data = all.reuslts, FUN = mean, na.rm = TRUE)
d1<-d1[order(d1$method, d1$n, d1$rho),]

d1 <- aggregate(p.sig7 ~ method + n + rho, data = all.reuslts, FUN = mean, na.rm = TRUE)
d1<-d1[order(d1$method, d1$n, d1$rho),]

d1[d1$method=="Backwards AIC",]
d1[d1$method=="Oracle (The Truth)",]
d1[d1$method=="ElasticNet Lambda-1se",]

