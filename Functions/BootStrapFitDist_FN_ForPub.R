########
# boostrap estimation of chosen distribution parameters
# to quantify uncertainty in estimated parameter values
########

bootstrapFits.fn<-function(data.file,evts,evt.col,start.col=13,min.plot=30,
                           write.file=FALSE,file.name="DistFitSummaryEVT",
                           include0=FALSE,add.val=0.1,verbose=FALSE,rankObj=NA,
                           n.iter=1000,removeOut=FALSE,out.mult=4) #rankObj is a distribution ranking object
{
  bootStrapFits<-list()
  cur.col<-start.col:ncol(data.file)
  for(i in 1:length(evts)) # for each EVT group
  {
# set up the data frame to record the estimate, the standard deviation, and the coefficient of variation for each estimated parameter,
# for the distribution designated as rank 1
    bootStrapFits[[i]]<-data.frame(fueltype=names(data.file)[cur.col],distr=NA,
                                   param1.est=NA,param1.bootSD=NA,param1.bootCV=NA,
                                   param2.est=NA,param2.bootSD=NA,param2.bootCV=NA)
    for(j in 1:length(cur.col))
    {
      if(!is.na(rankObj[[i]]$dist.LL[j])) # then a distribution was chosen
      {
        tmp.loads<-data.file[data.file[,evt.col]==evts[i],cur.col[j]] # isolate the relevant loadings
        cur.loads.vals0<-tmp.loads[!is.na(tmp.loads)&tmp.loads>=0] # include all values including zeroes
        cur.loads.vals<-tmp.loads[!is.na(tmp.loads)&tmp.loads>0] # only the non-zero values
        if(removeOut) # exclude outliers from distribution estimation
          cur.loads.vals<-cur.loads.vals[cur.loads.vals<(quantile(cur.loads.vals,probs = 0.75)+out.mult*IQR(cur.loads.vals))]
        
        bootStrapFits[[i]]$distr[j]<-rankObj[[i]]$dist1.fit[j]
        tmp.fit<-switch(bootStrapFits[[i]]$distr[j], # estimate the rank 1 distribution
                        normLL=fitdist(cur.loads.vals,distr="norm"),
                        lnormLL=fitdist(cur.loads.vals,distr="lnorm"),
                        gammaLL=fitdist(cur.loads.vals,distr="gamma"),
                        weibullLL=fitdist(cur.loads.vals,distr="weibull"))
        tmp.bs<-bootdist(tmp.fit,niter=n.iter) # perform the boostrap, part of the fitdistrplus package
        bootStrapFits[[i]]$param1.est[j]<-tmp.fit$estimate[1] # record the estimates
        bootStrapFits[[i]]$param2.est[j]<-tmp.fit$estimate[2]
        
        bootStrapFits[[i]]$param1.bootSD[j]<-sd(tmp.bs$estim[,1]) # calculate and record the bootstrap standard deviation
        bootStrapFits[[i]]$param2.bootSD[j]<-sd(tmp.bs$estim[,2])
        
        bootStrapFits[[i]]$param1.bootCV[j]<-bootStrapFits[[i]]$param1.bootSD[j]/bootStrapFits[[i]]$param1.est[j] # calculate and record the bootstrap coefficient of variation
        bootStrapFits[[i]]$param2.bootCV[j]<-bootStrapFits[[i]]$param2.bootSD[j]/bootStrapFits[[i]]$param2.est[j]
      }
    }
  }
# return the bootstrap list
  bootStrapFits
}