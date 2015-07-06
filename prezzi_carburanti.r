datasetprezziGE_filter <- read.csv("prezzi_20140807-20150613.csv")

library(ggplot2)
gg <- ggplot(datasetprezziGE_filter,aes(x=bnd,y=prezzo))
chart <- gg+ geom_boxplot(aes(fill=bnd))+
    facet_wrap(~carb)+
    scale_fill_discrete()+
    theme_bw()+
    theme(
        axis.text.x=(element_text(angle=90))
    );

chart

outlierEniBenzina<-subset(datasetprezziGE_filter,subset = (bnd == 'AgipEni' & prezzo < 1.3 & carb == 'Benzina'))
outlierEniBenzina_data<-subset(datasetprezziGE_filter,subset = (bnd == 'AgipEni' & id == 16210 & carb == 'Benzina' & dScrape > 20141025 & dScrape < 20141102 & isSelf == 1))

idGas <- unique(outlierEssoGasolio$id)
outlierEssoGasolio_data <- datasetprezziGE_filter[which(datasetprezziGE_filter$bnd=='Esso' & is.element(datasetprezziGE_filter$id,idGas)),]

View(outlierEssoGasolio_data[which(outlierEssoGasolio_data$id==16357 & outlierEssoGasolio_data$dScrape > 20150201 & outlierEssoGasolio_data$dScrape < 20150207 & outlierEssoGasolio_data$carb == 'Gasolio' & outlierEssoGasolio_data$isSelf==1),])

datasetprezziGE_filter[which(datasetprezziGE_filter$carb=='GPL' & datasetprezziGE_filter$bnd == 'TotalErg'),]

metanoData <- unique(datasetprezziGE_filter[which(datasetprezziGE_filter$carb=='Metano'),])
metanoDataU <-unique(metanoData[,c("dIns","prezzo")])

#Splitta il campo dIns nei due componenti
metanoDataU$data<-apply(metanoDataU,1,function(x){
x<-strsplit(as.character(x["dIns"])," ")[[1]][1]
x
})

 ggplot(metanoDataU,aes(data,prezzo,group=1))+geom_step()+labs(x="Data",y="Prezzo",title="Prezzo del Metano")+theme_bw()+ theme(axis.text.x=(element_text(angle=90)))
 
 ggplot(benzData,aes(data,prezzo,group=bnd,colour=bnd))+geom_step()+labs(x="Data",y="Prezzo",title="Prezzo della Benzina",colour="Marca")+theme_bw()+ theme(axis.text.x=(element_text(angle=90)))
 
 ggplot(benzData,aes(data,prezzo,group=bnd,colour=bnd))+labs(x="Data",y="Prezzo",title="Prezzo della Benzina")+theme_bw()+ theme(axis.text.x=(element_text(angle=90)))+stat_smooth(alpha=0.25)

#miglioramento sui tick mostrati 
unique(benzData$data)[seq(1,349,15)]
ggplot(benzData,aes(data,prezzo,group=bnd,colour=bnd))+labs(x="Data",y="Prezzo",title="Prezzo della Benzina",colour="Marca")+ scale_x_discrete(breaks=unique(benzData$data)[seq(1,349,15)]) +theme(axis.text.x=(element_text(angle=90)))+stat_smooth(alpha=0.15)

#esempio completo
gasolioData<-datasetprezziGE_filter[which(datasetprezziGE_filter$carb == 'Gasolio' & datasetprezziGE_filter$prezzo > 1.25),]
gasolioData<-unique(gasolioData[,c("id","bnd","dIns","prezzo")])
gasolioData<-aggregate(prezzo ~ id+bnd+dIns, FUN=mean,data = gasolioData)
gasolioData$data<-apply(gasolioData,1,function(x){
x<-strsplit(as.character(x["dIns"])," ")[[1]][1]
x
})
ggplot(gasolioData,aes(data,prezzo,group=bnd,colour=bnd))+labs(x="Data",y="Prezzo",title="Prezzo del Gasolio",colour="Marca")+ scale_x_discrete(breaks=unique(gasolioData$data)[seq(1,349,15)])+theme_bw() +theme(axis.text.x=(element_text(angle=90)))+stat_smooth(alpha=0.15)
