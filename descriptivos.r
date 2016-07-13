directorio<-"C:\\Users\\Androide1\\Documents\\Mathias\\Maestría\\Tópicos de Distribución del Ingreso\\Trabajo Empirico"
setwd(directorio)
load("cargadas1.RData")
base.0<-base.00
base.1<-base.01
base.2<-base.02
base.3<-base.03
base.4<-base.04
base.5<-base.05
base.6<-base.06
base.7<-base.07
base.8<-base.08
base.9<-base.09
rm(list=c("base.00","base.01","base.02","base.03","base.04","base.05","base.06","base.07","base.08","base.09"))
library(ineq)
library(IC2)
library(ggplot2)
#Los individuos son todos ocupados, de localidades mayores a 5000, no asisten en el momento de ser encuestados a educacion formal
#Filtrado a solo asalariados privados entre 18 y 65 que trabajaron al menos 1 hora en la semana anterior o lo hubieran hecho habitualmente.#
for(i in 1991:2014){
assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$pe3>=18 & get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$pe3<=65 & get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1>0 & get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$pf41a==1),])
}
#Renombro las variables pesoano#
base.6$peso2<-base.6$pesoano
base.7$peso2<-base.7$pesoano
base.8$peso2<-base.8$pesoano
base.9$peso2<-base.9$pesoano
base.10$peso2<-base.10$pesoano
base.11$peso2<-base.11$pesoano
base.12$peso2<-base.12$pesoano
base.13$peso2<-base.13$pesoano
base.14$peso2<-base.14$pesoano
######Indices de desigualdad del ingreso principal total estimados con pesoano para todos estos individuos, excluyendo los que tienen ingresos principales 0#######
gini<-list()
atkinson0<-list()
atkinson05<-list()
atkinson1<-list()
entropia0<-list()
entropia1<-list()
entropia2<-list()
for(i in 1991:2014){
if("principal"%in%colnames(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")))){
gini[[i]]<-calcSGini(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)])$ineq$index
atkinson0[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],epsilon=0)$ineq$index
atkinson05[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],epsilon=0.5)$ineq$index
atkinson1[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],epsilon=1)$ineq$index
entropia0[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],alpha=0)$ineq$index
entropia1[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],alpha=1)$ineq$index
entropia2[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],alpha=2)$ineq$index
}
if("principal_sf"%in%colnames(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")))){
gini[[i]]<-calcSGini(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2)$ineq$index
atkinson0[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],epsilon=0)$ineq$index
atkinson05[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],epsilon=0.5)$ineq$index
atkinson1[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],epsilon=1)$ineq$index
entropia0[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],alpha=0)$ineq$index
entropia1[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],alpha=1)$ineq$index
entropia2[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],alpha=2)$ineq$index
}
}
serie.indc<-as.data.frame(cbind("gini"=as.numeric(unlist(gini)),"atkinson0"=as.numeric(unlist(atkinson0)),"atkinson05"=as.numeric(unlist(atkinson05)),"atkinson1"=as.numeric(unlist(atkinson1)),"entropia0"=as.numeric(unlist(entropia0)),"entropia1"=as.numeric(unlist(entropia1)),"entropia2"=as.numeric(unlist(entropia2)),"año"=seq(1991,2014,1)))
#plot1
plot1<-ggplot(serie.indc)+geom_line(aes(x=año,y=gini),colour="#330000")+geom_line(aes(x=año,y=atkinson0),colour="#009933")+geom_line(aes(x=año,y=atkinson05),colour="#336600")+geom_line(aes(x=año,y=atkinson1),colour="#003300")+geom_line(aes(x=año,y=entropia0),colour="#66CCFF")+geom_line(aes(x=año,y=entropia1),colour="#003399")+geom_line(aes(x=año,y=entropia2),colour="#000099")+theme(legend.position="none")+ylim(0, 1.6)+xlim(1991,2014)
plot1
######Indices de desigualdad del ingreso principal horario estimados con pesoano para todos estos individuos, excluyendo los que tienen ingresos principales 0#######
gini<-list()
atkinson0<-list()
atkinson05<-list()
atkinson1<-list()
entropia0<-list()
entropia1<-list()
entropia2<-list()
for(i in 1991:2014){
if("principal"%in%colnames(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")))){
gini[[i]]<-calcSGini(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)])$ineq$index
atkinson0[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],epsilon=0)$ineq$index
atkinson05[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],epsilon=0.5)$ineq$index
atkinson1[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],epsilon=1)$ineq$index
entropia0[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],alpha=0)$ineq$index
entropia1[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],alpha=1)$ineq$index
entropia2[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)],alpha=2)$ineq$index
}
if("principal_sf"%in%colnames(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")))){
gini[[i]]<-calcSGini(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2)$ineq$index
atkinson0[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],epsilon=0)$ineq$index
atkinson05[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],epsilon=0.5)$ineq$index
atkinson1[[i]]<-calcAtkinson(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],epsilon=1)$ineq$index
entropia0[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],alpha=0)$ineq$index
entropia1[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],alpha=1)$ineq$index
entropia2[[i]]<-calcGEI(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)]/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0)]),w=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$peso2[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal_sf>0)],alpha=2)$ineq$index
}
}
serie.indc2<-as.data.frame(cbind("gini"=as.numeric(unlist(gini)),"atkinson0"=as.numeric(unlist(atkinson0)),"atkinson05"=as.numeric(unlist(atkinson05)),"atkinson1"=as.numeric(unlist(atkinson1)),"entropia0"=as.numeric(unlist(entropia0)),"entropia1"=as.numeric(unlist(entropia1)),"entropia2"=as.numeric(unlist(entropia2)),"año"=seq(1991,2014,1)))
#plot2
melted<-c(serie.indc2$gini,serie.indc2$atkinson05,serie.indc2$atkinson1,serie.indc2$entropia0,serie.indc2$entropia1)
melted<-as.data.frame(cbind("Año"=seq(1991,2014,1),melted,"estad"=as.factor(as.character(c(rep("gini",24),rep("atkinson05",24),rep("atkinson1",24),rep("entropia0",24),rep("entropia1",24))))))
plot2<-ggplot(melted)+geom_line(aes(x=Año,y=melted,colour=factor(estad),group=factor(estad)),size=1)+scale_colour_discrete(name  ="Indice",breaks=c(1,2,3,4,5),labels=c("Atkinson 0.5", "Atkinson 1","Entropía 0","Theil","Gini"))+scale_colour_manual(values=c("#336600","#003300","#66CCFF","#003399","#330000"),name  ="Indice",breaks=c(1,2,3,4,5),labels=c("Atkinson 0.5", "Atkinson 1","Entropía 0","Theil","Gini"))+ylab("%")+ggtitle("Desigualdad del Salario Horario (1991-2014)")+theme(plot.background=element_rect(fill='#4D6884'),axis.title.x = element_text(size=20,colour="black"),axis.title.y = element_text(size=20,colour="black"),axis.text.x  = element_text(angle=45,vjust=0.5,hjust=0.8, size=16,colour="black"),axis.text.y  = element_text(vjust=0.5, size=20,colour="black"),plot.title=element_text(size=20))+coord_cartesian(xlim = c(1991, 2014)) 
plot2+geom_vline(xintercept=c(2007), linetype="dotted")

plot(density(as.integer(base.t$edu2[which(base.t$anio==1991)])-1)$x,density(as.integer(base.t$edu2[which(base.t$anio==1991)])-1)$y,type="l",main="Distribución de trabajadores según años de educación (1991-2014)",xlab="# Años",ylab="Densidad Kernel",col="#7b9f71",lwd=2)
lines(density(as.integer(base.t$edu2[which(base.t$anio==1999)])-1)$x,density(as.integer(base.t$edu2[which(base.t$anio==1999)])-1)$y,type="l",col="#336600",lwd=2)
lines(density(as.integer(base.t$edu2[which(base.t$anio==2007)])-1)$x,density(as.integer(base.t$edu2[which(base.t$anio==2007)])-1)$y,type="l",col="#363c09",lwd=2)
lines(density(as.integer(base.t$edu2[which(base.t$anio==2014)])-1)$x,density(as.integer(base.t$edu2[which(base.t$anio==2014)])-1)$y,type="l",col="#927312",lwd=2)
box(lwd=2)
legend(x=17,y=0.27,c("1991","1999","2007","2014"),fill=c("#7b9f71","#336600","#363c09","#927312"))

plot(lista.retornos[[1991]][,1],type="l",main="Retornos a años de educación individuales (1991-2014)",xlab="# Años",ylab="%",col="#7b9f71",lwd=2,ylim=c(0,2))
lines(lista.retornos[[1999]][,1],,col="#336600",lwd=2)
lines(lista.retornos[[2007]][,1],,col="#363c09",lwd=2)
lines(lista.retornos[[2014]][,1],,col="#927312",lwd=2)
box(lwd=2)
legend(x=0.5,y=1.85,c("1991","1999","2007","2014"),fill=c("#7b9f71","#336600","#363c09","#927312"))
