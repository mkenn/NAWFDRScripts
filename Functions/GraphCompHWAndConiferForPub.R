###########
# example groups for pub figures try 1
###########
# three EVT groups for each eastern woodland and conifer forest
# for each, 3-4 fuel loading types
# So either a 3x3 or 4x3 panel graph
###########
# consistent colors, that could be rendered in BW
# ColorBrewer
# lines ok, different colors
# add a key for the color-coding
###########

load("Workspaces/Current/HurdleCustomFitsNoZeroWNoOut.RData") # estimated distributions
library(fitdistrplus)
library(RColorBrewer)
source("../Functions/Dist_Fit_Graph_Pub_FN2.R")

fuel.names<-read.csv("../ConsumeSARProj/FCCS_DataBaseCategoryMapUpdatedWFOFEM.csv")

# evt group numbers
evt.ew<-c(655,682,666) # 
map.fuels.ew1<-data.frame(tree=rep(NA,3),cwd=c(7.73,8.41,4.48),
                          duff=c(16.14,17.75,1.79),litter=c(6.25,6.61,2.08))
map.fuels.ew2<-data.frame(tree=rep(NA,3),cwd=c(2.50,1.70,2.50),
                          duff=c(2.69,16.39,2.69),litter=c(5.60,4.19,5.60))
# duff.ew1<-c(16.14,17.75,1.79)
# duff.ew2<-c(2.69,16.39,2.69)
#evt.cf<-c(683,758,631) #peatland, black spruce woodland, pp
# or
evt.cf2<-c(683,631,625) # black spruce woodland, pp, df/pp/lp
map.fuels.cf1<-data.frame(tree=rep(NA,3),cwd=c(20.18,34.75,10.98),
                          duff=c(766.66,22.87,16.81),litter=c(2.04,3.36,4.21))
map.fuels.cf2<-data.frame(tree=rep(NA,3),cwd=c(3.61,3.41,2.11),
                          duff=c(224.15,16.39,7.5),litter=c(22.39,4.19,2.60))

evt.names<-read.csv("../Data/EVT_AbbrevNames.csv",stringsAsFactors=FALSE,allowEscapes=TRUE)

fuels.plot<-c("tree_loading_Mgha","cwd_loading_Mgha","duff_loading_Mgha","litter_loading_Mgha")

x.lims<-c(500,50,120,50)

fig.x1<-c(0,1/3,2/3)
fig.x2<-c(1/3,2/3,1)
fig.y1<-c(.72,.48,.24,0)
fig.y2<-c(1,.72,.48,.24)

#colors1<-brewer.pal(2,"Paired")
colors1<-brewer.pal(3,"Dark2")[2:3]


postscript(file="CompHardwoodTry1WithMappedNewColorUpdated.eps",horizontal = FALSE,height=12,width=10)
plot.new()
par(oma=c(0,2,0,0))
y.id<-0
x.id<-0
for(k in 1:length(fuels.plot))
{
  i<-fuels.plot[k]
  y.id<-y.id+1
  for(j in 1:length(evt.ew))
  {
    x.id<-x.id+1
    par()
    
    if(y.id==1)
      par(new=TRUE,fig=c(fig.x1[x.id],fig.x2[x.id],fig.y1[y.id],fig.y2[y.id]),
          mar=c(3.5,2.75,2.5,0.5),mgp=c(1.75,0.5,0),las=1)
    else
      par(new=TRUE,fig=c(fig.x1[x.id],fig.x2[x.id],fig.y1[y.id],fig.y2[y.id]),
          mar=c(3.5,2.75,0.5,0.5),mgp=c(1.75,0.5,0),las=1)
    tmp.id<-which(evt.vals==evt.ew[j])
    tmp.id2<-which(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype==i)
    tmp.which<-which(as.character(fuel.names$DataBaseInR)==as.character(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype[tmp.id2]))
    cur.distr<-switch(distributionCustomRankingHurdleNOut[[tmp.id]]$dist1.fit[tmp.id2],
                      gammaLL="gamma",
                      lnormLL="lnorm",
                      normLL="norm",
                      weibullLL="weibull")
    if(y.id==1)
    {
      main.txt<-as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.ew[j]])
    }
    else
      main.txt=""
    distFitForPubGraph2.fn(data.file=data.file,evts=evt.ew[j],cur.cols = tmp.id2+start.col-1,evt.col = EVTCol,
                          distr=cur.distr,
                          plot.qqAxes = TRUE,x.lab=as.character(fuel.names$graphLabelNoUnits[tmp.which]),#,x.lab = distributionCustomRankingHurdle[[i]]$fueltype[j],
                          main.txt=main.txt,removeOut=TRUE,x.lims = x.lims[y.id],prop0=round(distributionCustomFittingHurdleNOut$HurdleFit[[tmp.id]]$prop0[tmp.id2]*100,digits=1))
    
    # abline(v =c(map.fuels.ew1[j,k],map.fuels.ew2[j,k]),lty=1,lwd=2.5,
    #        col=c(colors1[1],colors1[2]))
    # segments(x0 =c(duff.ew1[j],duff.ew2[j]),y0 = c(0,0),x1 = c(duff.ew1[j],duff.ew2[j]),y1=c(0.02,0.02),
      #          lty=1,col="blue")
      # 
  }
  # if(y.id==2&x.id==3)
  #   legend(x="topright",legend=c("FB","FLM"),fill=colors1[1:2],
  #          cex=0.75)
  x.id<-0
  
}  
mtext(side=2,text="Density",line=0,outer=TRUE,las=0)

dev.off()

x2.lims<-c(600,150,250,50)
#c(400,100,250,50)
postscript(file="CompConiferTry1WithMappedNewColorUpdated.eps",horizontal = FALSE,height=12,width=10)
plot.new()
par(oma=c(0,2,0,0))

y.id<-0
x.id<-0
for(i in 1:length(fuels.plot))
{
  y.id<-y.id+1
  for(j in 1:length(evt.cf2))
  {
    x.id<-x.id+1
    par()
    
    if(y.id==1)
      par(new=TRUE,fig=c(fig.x1[x.id],fig.x2[x.id],fig.y1[y.id],fig.y2[y.id]),
          mar=c(3.5,2.75,2.5,0.5),mgp=c(2.25,0.5,0),las=1)
    else
      par(new=TRUE,fig=c(fig.x1[x.id],fig.x2[x.id],fig.y1[y.id],fig.y2[y.id]),
          mar=c(3.5,2.75,0.5,0.5),mgp=c(2.25,0.5,0),las=1)
    tmp.id<-which(evt.vals==evt.cf2[j])
    tmp.id2<-which(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype==fuels.plot[i])
    tmp.which<-which(as.character(fuel.names$DataBaseInR)==as.character(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype[tmp.id2]))
    cur.distr<-switch(distributionCustomRankingHurdleNOut[[tmp.id]]$dist1.fit[tmp.id2],
                      gammaLL="gamma",
                      lnormLL="lnorm",
                      normLL="norm",
                      weibullLL="weibull")
    if(y.id==1)
    {
      main.txt<-as.character(evt.names$EVT_Group_AbbreviatedName[evt.names$EVT_GP==evt.cf2[j]])
    }
    else
      main.txt=""
    distFitForPubGraph2.fn(data.file=data.file,evts=evt.cf2[j],cur.cols = tmp.id2+start.col-1,evt.col = EVTCol,
                           distr=cur.distr,
                           plot.qqAxes = TRUE,x.lab=as.character(fuel.names$graphLabelNoUnits[tmp.which]),#,x.lab = distributionCustomRankingHurdle[[i]]$fueltype[j],
                           main.txt=main.txt,removeOut=TRUE,x.lims = x2.lims[y.id],
                           prop0=round(distributionCustomFittingHurdleNOut$HurdleFit[[tmp.id]]$prop0[tmp.id2]*100,digits=1))
    # abline(v =c(map.fuels.cf1[j,i],map.fuels.cf2[j,i]),lty=1,lwd=2,
    #        col=c(colors1[1],colors1[2]))
    
    
  }
  # if(y.id==2&x.id==3)
  #   legend(x="topright",legend=c("FB","FLM"),fill=colors1[1:2],
  #          cex=0.75)
  
  x.id<-0
  
}  
mtext(side=2,text="Density",line=0,outer=TRUE,las=0)

dev.off()

#########
# try boxplots for direct comparison of EVT for each fuel type
box.y.lims<-c(600,150,800,50)
postscript(file="BoxplotEVTCompForM1Updated.eps",height=7,width=8,horizontal=FALSE)
par(mfrow=c(2,2),mar=c(2.5,3.5,1.5,0.25),mgp=c(2.5,0.5,0),las=1,
    oma=c(0,2,0,0))
y.id<-0
for(i in fuels.plot)
{
  tmp.loads<-list()
  y.id<-y.id+1
  x.id<-0
  for(j in evt.ew)
  {
    x.id<-x.id+1
    tmp.id<-which(evt.vals==j)
    tmp.id2<-which(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype==i)
    tmp.which<-which(as.character(fuel.names$DataBaseInR)==as.character(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype[tmp.id2]))
    tmp.df<-data.file[data.file[,EVTCol]==j,]
    tmp.loads[[x.id]]<-tmp.df[!is.na(tmp.df[,i]),i]
    tmp.loads[[x.id]]<-tmp.loads[[x.id]][tmp.loads[[x.id]]>0]
  }
  for(j in evt.cf2)
  {
    x.id<-x.id+1
    tmp.id<-which(evt.vals==j)
    tmp.id2<-which(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype==i)
    tmp.which<-which(as.character(fuel.names$DataBaseInR)==as.character(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype[tmp.id2]))
    tmp.df<-data.file[data.file[,EVTCol]==j,]
    tmp.loads[[x.id]]<-tmp.df[!is.na(tmp.df[,i]),i]
    tmp.loads[[x.id]]<-tmp.loads[[x.id]][tmp.loads[[x.id]]>0]
  }
  boxplot(tmp.loads[[1]],tmp.loads[[2]],tmp.loads[[3]],
          tmp.loads[[4]],tmp.loads[[5]],tmp.loads[[6]],
#          ylab=as.character(fuel.names$graphLabelNoUnits[tmp.which]),
          ylab="",main=as.character(fuel.names$graphLabelNoUnits[tmp.which]),
          names=c(evt.ew,evt.cf2),col=c(rep("white",3),rep("grey",3)),axes=FALSE,
          ylim=c(0,box.y.lims[y.id]))
  if(i!="tree_loading_Mgha")
  {
    points(1:6,c(map.fuels.ew1[,y.id],map.fuels.cf1[,y.id]),
           col=colors1[1],pch=16,cex=1.5)
    points(1:6,c(map.fuels.ew2[,y.id],map.fuels.cf2[,y.id]),
           col=colors1[2],pch=17,cex=1.5)
  }
    # boxplot(log(tmp.loads[[1]]),log(tmp.loads[[2]]),log(tmp.loads[[3]]),
  #         log(tmp.loads[[4]]),log(tmp.loads[[5]]),log(tmp.loads[[6]]),
  #         ylab=paste("log",as.character(fuel.names$graphLabelNoUnits[tmp.which])),
  #         names=c(evt.ew,evt.cf2),col=c(rep("white",3),rep("grey",3)),axes=FALSE)
  axis(2)
  axis(1,las=1,labels=c(evt.ew,evt.cf2),at=1:6,tick=FALSE)

  box()
}
  legend(x="topleft",legend=c("FCCS","FLM"),col=colors1[1:2],pch=16:17,cex=1.1)
mtext(side=2,outer=TRUE,text=expression(paste("Loading (Mg h",a^-1,")")),
      las=0,line=0,cex=1)
dev.off()

y.id<-0
for(i in fuels.plot)
{
  tmp.loads<-list()
  y.id<-y.id+1
  x.id<-0
  for(j in evt.ew)
  {
    x.id<-x.id+1
    tmp.id<-which(evt.vals==j)
    tmp.id2<-which(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype==i)
    tmp.which<-which(as.character(fuel.names$DataBaseInR)==as.character(distributionCustomRankingHurdleNOut[[tmp.id]]$fueltype[tmp.id2]))
    tmp.df<-data.file[data.file[,EVTCol]==j,]
    tmp.loads[[x.id]]<-tmp.df[!is.na(tmp.df[,i]),i]
    tmp.loads[[x.id]]<-tmp.loads[[x.id]][tmp.loads[[x.id]]>0]
  }
  boxplot(tmp.loads[[1]],tmp.loads[[2]],tmp.loads[[3]],ylab=as.character(fuel.names$graphLabelNoUnits[tmp.which]),
          names=evt.ew)
}
