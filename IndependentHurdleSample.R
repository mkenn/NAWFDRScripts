#######
# sampling fuel loading distributions using
# fuels data base
########
# first read in the workspace with distribution fits
# the important objects are 
# distributionCustomRankingHurdleNOut, which gives the name of the best
# fit distribution (gamma or lognormal)
# and distributionCustomFittingHurdleNOut, which gives the paramter
# estimates and fit statistics for both distributions
# evt.vals, which gives the evt group numbers in the same order as
# the distribution fit objects
#######
load("d:/JFSPMap/FuelLoadDists/DistFittingRProj/Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData")
# source the hurdle sample function
source("d:/JFSPMap/FuelLoadDists/Functions/SimHurdleDist_FN.R")
# ##
# how many samples 
N.samp<-1000

# define which columns are being sampled
# this has to match the names in the R object
# the list of names can be found here:
distributionCustomRankingHurdleNOut[[1]]$fueltype

fuel.samp<-c("litter_loading_Mgha","shrub_loading_Mgha","duff_loading_Mgha")

# which evt group? this can be a single group or a vector of groups
# referred to by number
evt.num<-625
# first independent samples
fuel.samp.vals<-list()# to accommodate multiple EVT groups
for(k in 1:length(evt.num))
{
  # define which index for the distribution objects
  cur.evt<-which(evt.vals==evt.num[k])
  # created\s a separate data frame for each evt group
  # with a column for each fuel that is being sampled
  fuel.samp.vals[[k]]<-data.frame(matrix(NA,ncol=length(fuel.samp),nrow=N.samp))
  names(fuel.samp.vals[[k]])<-fuel.samp
  for(i in 1:length(fuel.samp)) # loop through the different fuels
  {
    # identify the row index for the current fuel
    cur.fuel.row<-which(distributionCustomRankingHurdleNOut[[cur.evt]]$fueltype==fuel.samp[i])
    # id the distribution. NOte, if this is NA that means there were
    # insufficient entries in the database to estimate the distribution
    # parameters. Should probably have a check here for that. 
    cur.distr<-distributionCustomRankingHurdleNOut[[cur.evt]]$dist1.fit[cur.fuel.row]
    if(!is.na(cur.distr))# then a function was fit
    # extract parameters
    {    
      curFit<-distributionCustomFittingHurdleNOut$HurdleFit[[cur.evt]]
    # rename the distribution to match R standards    
      tmp.distr<-switch(cur.distr,
                        lnormLL="lnorm",
                        gammaLL="gamma")
      # draw the random sample
      tmp.samp<-simHurdle.fn(distr=tmp.distr,prop0=curFit$prop0[cur.fuel.row],nsamp=N.samp,nrep=1,
                             param1=curFit[cur.fuel.row,paste(tmp.distr,".p1",sep="")],
                             param2=curFit[cur.fuel.row,paste(tmp.distr,".p2",sep="")],
                             upper.quantile=NA)
      # fill in the correct column
      fuel.samp.vals[[k]][,i]<-tmp.samp[,1]
      }  
    }
  
}
# these are Mg/ha
# you may do a matrix rearrangement if desired to preserve rank correlation coefficients
# among fuel types


