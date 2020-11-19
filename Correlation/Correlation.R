install.packages('devtools')
install.packages('ggplot2')
install.packages('openxlsx')
install.packages('hqreg')

library(devtools)
library(ggplot2)
library(openxlsx)
library(hqreg)


devtools::install_github("cmu-delphi/covidcast", ref = "main",
                         subdir = "R-packages/covidcast")
library(covidcast)


sevendaysaverage<- suppressMessages(
  covidcast_signal(data_source = "indicator-combination", signal = "confirmed_7dav_incidence_prop", 
                   start_day = "2020-07-2", end_day = "2020-10-31",
                   geo_type = "state")
)



Date <- seq.Date(from = as.Date("2020/07/02",format = "%Y/%m/%d"), by = "day", length.out = 122)

#Number of new confirmed COVID-19 cases per 100,000 population, daily 7 days average

A<-matrix(nrow=52,ncol=123)
dfsevenaverage<-(A)
colnames(dfsevenaverage)[2:123]<-as.character(Date)


colnames(dfsevenaverage)[1]<-'state'

dfsevenaverage[1:52,1]<-sevendaysaverage[1:52,3]


for (i in 1:52){
  for(j in 1:122){
    dfsevenaverage[i,j+1]=sevendaysaverage[i+(j-1)*52,7]
    
  }
  
}



dfseven50<-dfsevenaverage[-c(8,40),]

dfseven50<-data.frame(dfseven50)

#lable dfseven50



covidfeatures<-read.xlsx('/Users/guanchaotong/Desktop/Covid code/featuresprop.xlsx')


covidfeatures[,1]<-state.abb[match(covidfeatures[,1],state.name)]
covidfeatures<-covidfeatures[order(covidfeatures[,1]),]

#features covidfeatures
covidlableuse<-as.matrix(dfseven50[2:length(dfseven50)])
covidfeaturesuse<-as.matrix(covidfeatures[2:length(covidfeatures)])

#normalize
for(i in 1:dim(covidfeaturesuse)[2]){
  covidfeaturesuse[,i]=scale(covidfeaturesuse[,i],center=TRUE,scale=TRUE)
}




#as.numeric

covidlableuse2<-matrix(nrow=50,ncol=122)

for (i in 1:50){
  covidlableuse2[i,]<-as.numeric(covidlableuse[i,])
}


#featurename
featureneames<-colnames(covidfeaturesuse)





correlationvector<-c()



for(i in 1:122){

  
  correlationvector<-c(correlationvector,cor(covidlableuse2[,i],covidfeaturesuse[,2]))
  
}








df<-data.frame(Date=Date,Correlation=correlationvector)

shadow <- data.frame (xmin=Date[1], xmax=Date[13], ymin=-Inf, ymax=Inf)


ggplot(data = df, mapping = aes(x = Date, y = Correlation, group = 1)) + geom_line() + xlab('Date')+labs(title = "Correlation between the mask wearing rate and daily confirmed cases")+
  geom_rect(data=shadow, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)





