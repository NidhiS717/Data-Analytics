library(arules)
fname=""
rawdata<-read.csv(file=fname,head=TRUE,sep=",",fileEncoding="UTF-8",na.strings=c("NA",""))
rawdata$discretePopScore=factor(rawdata[["discretePopScore"]])
rawdata$discretePages=factor(rawdata[["discretePages"]])
dataf=data.frame(rawdata$Edition,rawdata$Author,rawdata$Bound,rawdata$discretePages,rawdata$Genre,rawdata$discretePopScore)
#dataf$rawdata.discretePopScore<-factor(dataf[[rawdata.discretePopScore]])
write.csv(dataf,"/home/srinidhi/DAProject/DA/forapriorigoodreads.csv")
rules<-apriori(dataf,parameter=list(confidence=0.6,support=0.01))
a<-inspect(subset(rules,subset=rhs %pin% "discretePopScore="))

