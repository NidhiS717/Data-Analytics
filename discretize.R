fname=""
rawdata<-read.csv(file=fname,head=TRUE,sep=",",fileEncoding="UTF-8")
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
    if (i<=0.2789)
      a=4
    else if (i<=0.3316)
      a=3
    else if (i<=0.4100)
      a=2
    else
      a=1
    arr[j]<-a
    print(arr[j])
    j<-j+1
  }
  return(arr)
}  
arr<-labeller_pages(rawdata$Pages.1,arr)
rawdata$discretePages=c(arr)
write.csv(rawdata,"/home/srinidhi/DAProject/DA/goodreads6.csv")
