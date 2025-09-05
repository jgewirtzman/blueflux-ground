lgr2<-read.csv("/Users/jongewirtzman/Downloads/lgr2_data.csv")
lgr3<-read.csv("/Users/jongewirtzman/Downloads/lgr3_data.csv")

fluxes<-as.data.frame(rbind(lgr2, lgr3))
fluxes<-fluxes[which(fluxes$CO2_r2>0.9),]
fluxes<-fluxes[which(fluxes$component!="?"),]
fluxes$component[which(fluxes$component=="dead part")]<-"CWD"


library(ggplot2)

ggplot(
  aes(x=plot, y=CH4_flux, color=species), data=fluxes
)+
  geom_boxplot()

fluxes_main<-fluxes[which(fluxes$plot=="BL60"|
                            fluxes$plot=="CP40"|
                            fluxes$plot=="FLM30"|
                            fluxes$plot=="MI"|
                            fluxes$plot=="SE1"|
                            fluxes$plot=="SRS5"|
                            fluxes$plot=="SRS6"),]



ggplot(
  aes(x=plot, y=(CH4_flux*1000), color=component), data=fluxes_main
)+
  geom_boxplot()+ylim(0,1)+geom_jitter()

ggplot(
  aes(x=component, y=(CH4_flux*1000)), data=fluxes_main
)+
  geom_boxplot()+ylim(0,1)+geom_jitter()

ggplot(
  aes(x=component, y=(CH4_flux*1000)), data=fluxes_main
)+
  geom_boxplot()+geom_jitter()

ggplot(
  aes(x=component, y=log(CH4_flux*1000)), data=fluxes_main
)+
  geom_boxplot()+geom_jitter()


ggplot(
  aes(x=plot, y=(CH4_flux*1000/38), color=component), data=fluxes_main
)+
  geom_boxplot()+ylim(0,0.25)+geom_jitter()


stems<-fluxes_main[which(fluxes_main$component=="stem"),]

ggplot(aes(y=as.numeric(height), x=log(CH4_flux*1000)), data=stems)+
  geom_point()


ggplot(aes(y=as.numeric(height), x=log(CH4_flux*1000), color=species), data=stems)+
  geom_point()+
  facet_wrap(~plot)

ggplot(
  aes(x=plot, y=log(CH4_flux*1000)), data=stems
)+
  geom_boxplot()

ggplot(
  aes(x=plot, y=(CH4_flux*1000)), data=stems
)+
  geom_boxplot(outlier.shape = NA)+ylim(0,5)

ggplot(aes(as.factor(x=month(dates)), y=CH4_flux*1000, color=plot), data=stems)+
  geom_boxplot()+geom_jitter()

ggplot(aes(as.factor(x=month(dates)), y=log(CH4_flux*1000), color=plot), data=stems)+
  geom_boxplot()

stems$diameter<-as.numeric(stems$diameter)
stems$height<-as.numeric(stems$height)

summary(lm(CH4_flux~plot+height, data=stems))
