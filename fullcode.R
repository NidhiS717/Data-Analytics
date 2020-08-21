fname=""
rawdata<-read.csv(file=fname,head=TRUE,sep=",",fileEncoding="UTF-8")
#rawdata = rawdata[c(2,12)]
x=as.numeric(rawdata$NoofRatings)
meanp=mean(x,na.rm=TRUE)
sdp=sd(x,na.rm=TRUE)
maxp=max(x,na.rm=TRUE)
minp=min(x,na.rm=TRUE)
rescale <- function(x) ((x-minp)/(maxp - minp))
y1=rescale(x)
rawdata$normNoOfRating=c(y1)
x=as.numeric(rawdata$Rating)
meanp=mean(x,na.rm=TRUE)
sdp=sd(x,na.rm=TRUE)
maxp=max(x,na.rm=TRUE)
minp=min(x,na.rm=TRUE)
rescale <- function(x) ((x-minp)/(maxp - minp))
y2=rescale(x)
rawdata$normRating=c(y2)
z=y2*y1
rawdata$PopScore=c(z)

arr <- array(0,dim=c(1,nrow(rawdata)))
j<-1
labeller_score<-function(z,arr)
{
  for (i in c(z))
  {
    if (is.na(i))
      a=5
    else if (i<=0.00189)#0.2851) #45)#0.1938)
      a=4
    else if (i<=0.0249)#0.4801)#82.5) #0.3973)
      a=3
    else if (i<=0.07)#0.5647)#131) #0.5975)
      a=2
    else
      a=1
    arr[j]<-a
    print(arr[j])
    j<-j+1
  }
  return(arr)
}
arr<-labeller_score(z,arr)
rawdata$discretePopScore=factor(c(arr))
arr <- array(0,dim=c(1,nrow(rawdata)))
j<-1
labeller_pages<-function(z,arr)
{
  for (i in c(z))
  {
    if (i<=313.8)
      a=4
    else if (i<=373)
      a=3
    else if (i<=461.2)
      a=2
    else
      a=1
    arr[j]<-a
    print(arr[j])
    j<-j+1
  }
  return(arr)
}  
arr<-labeller_pages(rawdata$Pages,arr)
rawdata$discretePages=c(arr)

library(arules)
rawdata$discretePopScore=factor(rawdata[["discretePopScore"]])
rawdata$discretePages=factor(rawdata[["discretePages"]])
dataf=data.frame(rawdata$Edition,rawdata$Author,rawdata$Bound,rawdata$discretePages,rawdata$Genre,rawdata$discretePopScore)
#dataf$rawdata.discretePopScore<-factor(dataf[[rawdata.discretePopScore]])
#write.csv(dataf,"/home/srinidhi/DAProject/DA/forapriorigoodreads.csv")
rules<-apriori(dataf,parameter=list(confidence=0.6,support=0.01))
a<-inspect(subset(rules,subset=rhs %pin% "discretePopScore="))
