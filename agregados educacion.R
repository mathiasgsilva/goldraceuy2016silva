directorio<-"C:\\Users\\Androide1\\Documents\\Mathias\\Maestría\\Tópicos de Distribución del Ingreso\\Trabajo Empirico"
setwd(directorio)
load("premincer.RData")
library(ineq)
library(ggplot2)
library(dummies)
####VERSION COMPATIBILIZADA EN ANCHO DE LAS BASES####
base.91<-base.91[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.92<-base.92[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.93<-base.93[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.94<-base.94[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.95<-base.95[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.96<-base.96[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.97<-base.97[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.98<-base.98[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.99<-base.99[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.0<-base.0[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.1<-base.1[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.2<-base.2[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.3<-base.3[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.4<-base.4[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.5<-base.5[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.6<-base.6[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.7<-base.7[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.8$principal<-base.8$principal_sf
base.8<-base.8[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.9$principal<-base.9$principal_sf
base.9<-base.9[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.10$principal<-base.10$principal_sf
base.10<-base.10[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.11$principal<-base.11$principal_sf
base.11<-base.11[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.12$principal<-base.12$principal_sf
base.12<-base.12[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.13$principal<-base.13$principal_sf
base.13<-base.13[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
base.14$principal<-base.14$principal_sf
base.14<-base.14[,c("dpto","peso2","principal","lnwh","edu2","mujer","interior","exppot","exppot2","fulltime","edu20","edu21","edu22","edu23","edu24","edu25","edu26","edu27","edu28","edu29","edu210","edu211","edu212","edu213","edu214","edu215","edu216","edu217","edu218","edu219","edu220","rama1","rama2","rama3","rama4","rama5","rama6","rama7","rama8")]
############BASE UNIFICADA################
base.t<-rbind(base.91,base.92,base.93,base.94,base.95,base.96,base.97,base.98,base.99,base.0,base.1,base.2,base.3,base.4,base.5,base.6,base.7,base.8,base.9,base.10,base.11,base.12,base.13,base.14)
base.t$anio<-c(rep(1991,nrow(base.91)),rep(1992,nrow(base.92)),rep(1993,nrow(base.93)),rep(1994,nrow(base.94)),rep(1995,nrow(base.95)),rep(1996,nrow(base.96)),rep(1997,nrow(base.97)),rep(1998,nrow(base.98)),rep(1999,nrow(base.99)),rep(2000,nrow(base.0)),rep(2001,nrow(base.1)),rep(2002,nrow(base.2)),rep(2003,nrow(base.3)),rep(2004,nrow(base.4)),rep(2005,nrow(base.5)),rep(2006,nrow(base.6)),rep(2007,nrow(base.7)),rep(2008,nrow(base.8)),rep(2009,nrow(base.9)),rep(2010,nrow(base.10)),rep(2011,nrow(base.11)),rep(2012,nrow(base.12)),rep(2013,nrow(base.13)),rep(2014,nrow(base.14)))
rm(list=c("base.91","base.92","base.93","base.94","base.95","base.96","base.97","base.98","base.99","base.0","base.1","base.2","base.3","base.4","base.5","base.6","base.7","base.8","base.9","base.10","base.11","base.12","base.13","base.14"))
####DEFINICION DE LOS GRUPOS EDUCATIVOS
base.t$hs.drops<-ifelse(as.integer(base.t$edu2)<=11,1,0)
base.t$hs<-ifelse(as.integer(base.t$edu2)==12,1,0)
base.t$coll<-ifelse(as.integer(base.t$edu2)>=16,1,0)
base.t$c.equiv<-ifelse(as.integer(base.t$edu2)>12 & as.integer(base.t$edu2)<16,1,0)
sorteo<-function(x){
set.seed(1234)
out<-rbinom(length(base.t$c.equiv[which(base.t$anio==x)]),size=1,prob=0.5)
return(out)
}
for(i in 1991:2014){
base.t$c.equiv[which(base.t$anio==i)]<-sorteo(i)*base.t$c.equiv[which(base.t$anio==i)]+base.t$coll[which(base.t$anio==i)]
}
#Pag. 34: HS EQUIVS
base.t$hs.equiv<-ifelse(as.integer(base.t$edu2)>12 & as.integer(base.t$edu2)<16 & base.t$c.equiv==0,1,0)
base.t$hs.equiv<-base.t$hs.equiv+base.t$hs+base.t$hs.drops
####PARTICIPACION DE CADA GRUPO EDUCATIVO EN EL TOTAL DE REMUNERACIONES
###Estan calculados sobre la muestra sin expandir###
partic.hs.drops<-function(x){
  sum(base.t$principal[which(base.t$anio==x & base.t$hs.drops==1)])/sum(base.t$principal[which(base.t$anio==x)])
}
partic.hs<-function(x){
  sum(base.t$principal[which(base.t$anio==x & base.t$hs==1)])/sum(base.t$principal[which(base.t$anio==x)])
}
partic.c.equiv<-function(x){
  sum(base.t$principal[which(base.t$anio==x & base.t$c.equiv==1)])/sum(base.t$principal[which(base.t$anio==x)])
}
partic.hs.equiv<-function(x){
  sum(base.t$principal[which(base.t$anio==x & base.t$hs.equiv==1)])/sum(base.t$principal[which(base.t$anio==x)])
  }
partic.vector<-function(x){
  c(x,partic.hs.drops(x),partic.hs(x),partic.c.equiv(x),partic.hs.equiv(x))
}
lista.aux<-list()
for(i in 1991:2014){
lista.aux[[i]]<-round(partic.vector(i)[c(2:5)],digits=3)
}
tabla.partic<-as.data.frame(cbind(seq(1991,2014,1),do.call(rbind,lista.aux)))
colnames(tabla.partic)<-c("Año","% Menos Secund.","% Secundaria","% Univ. Equiv.","% HS. Equiv.")
#tabla.partic
####MASA DE SALARIOS PARA CADA GRUPO DE EQUIVALENTES
masa.c.equiv<-function(x){
  sum(base.t$principal[which(base.t$anio==x & base.t$c.equiv==1)])
}
masa.hs.equiv<-function(x){
  sum(base.t$principal[which(base.t$anio==x & base.t$hs.equiv==1)])
}
masa.vector<-function(x){
  c(x,masa.c.equiv(x),masa.hs.equiv(x))
}
lista.aux2<-list()
for(i in 1991:2014){
  lista.aux2[[i]]<-round(masa.vector(i)[c(2:3)],digits=4)
}
tabla.masa<-as.data.frame(cbind(seq(1991,2014,1),do.call(rbind,lista.aux2)))
colnames(tabla.masa)<-c("Año","% Univ. Equiv.","% HS. Equiv.")

####PARTICIPACION DE CADA GRUPO EDUCATIVO EN EL TOTAL DE ASALARIADOS PRIVADOS
###Estan calculados sobre la muestra sin expandir###
l.weights<-function(x,e){
ifelse(is.na(table(as.integer(base.t$edu2[which(base.t$anio==x)])==e)[2])==F,table(as.integer(base.t$edu2[which(base.t$anio==x)])==e)[2]/nrow(base.t[which(base.t$anio==x),]),0)
}
l.pesos<-data.frame()
for(i in 1991:2014){
  for(j in 1:21){
l.pesos[i,j]<-(as.numeric(l.weights(i,j-1)))
}
}
for(i in 1991:2014){
l.pesos[i,1]<-1-sum(l.pesos[i,c(2:21)])
}
l.pesos<-l.pesos[c(1991:2014),]
#l.pesos
############EDUCATIONAL WAGE DIFFERENTIALS###
###TRAER LA LISTA DE RETORNOS DESDE mincers.R###
#LA DE UNI. EQ. ES PROMEDIO DE LA DISTANCIA AL RETORNO A 12 ANOS DE EDUCACION DEL RETORNO A CADA ANO DE EDUCACION MAYOR O IGUAL A 16 POR SU PARTICIPACION COMO GRUPO EN EL TOTAL DE INDIVIDUOS CONTEMPLADOS
#LA DE HS ES LA DISTANCIA AL RETORNO A 9 ANOS DE EDUCACION DEL RETORNO A 12 ANOS DE EDUCACION POR SUS PESOS.

for(i in 1991:2014){
  lista.retornos[[i]][,1]<-ifelse(lista.retornos[[i]][,4]<=0.05,lista.retornos[[i]][,1],0)
}
c.equiv_hs<-function(x){
  weighted.mean(lista.retornos[[x]][c(16:20),1]-lista.retornos[[x]][c(12),1],w=t(l.pesos[as.character(x),c(17:21)]))
}
hs_hs.drops<-function(x){
 (lista.retornos[[x]][c(12),1])-(lista.retornos[[x]][c(9),1])
}

serie.c.equiv_hs<-list()
serie.hs_hs.drops<-list()
for(i in 1991:2014){
  serie.c.equiv_hs[[i]]<-c.equiv_hs(i)
  serie.hs_hs.drops[[i]]<-hs_hs.drops(i)
}
serie.wagediff<-as.data.frame(cbind(seq(1991,2014,1),unlist(serie.c.equiv_hs),unlist(serie.hs_hs.drops)))
colnames(serie.wagediff)<-c("Año","CE/HS","HS/HSd")
############SERIE OFERTA DE TRABAJO RELATIVA POR GRUPO EDUCACION###
##LA OF DE TRABAJO RELATIVA DE LOS UNI. EQUIV. ES SU PARTICIPACION EN EL TOTAL DE LA BASE, CON LOS PESOANO.(todos los no univ. equiv son hs equiv. por pag 34)
##LA OF DE TRABAJO RELATIVA DE LOS HS EQUIV. ES LA PARTICIPACION DE LOS QUE TIENEN 12 ANOS DE EDUCACION RESPECTO A LOS QUE TIENEN DE 0 A 11 (pag 36)
base.t$hs_hsd_of<-ifelse(as.integer(base.t$edu2)<=13,1,0)
serie.c.equiv_hs_of<-list()
serie.hs_hs.drops_of<-list()
for(i in 1991:2014){
  serie.c.equiv_hs_of[[i]]<-weighted.mean(base.t$c.equiv[which(base.t$anio==i)],w=base.t$peso2[which(base.t$anio==i)])
  serie.hs_hs.drops_of[[i]]<-1-weighted.mean(base.t$hs_hsd_of[which(base.t$anio==i)]-base.t$hs[which(base.t$anio==i)],w=base.t$peso2[which(base.t$anio==i)])
}
serie.educsupply<-as.data.frame(cbind(seq(1991,2014,1),unlist(serie.c.equiv_hs_of),unlist(serie.hs_hs.drops_of)))
colnames(serie.educsupply)<-c("Año","CE/HS of","HS/HSd of")
##################BASE DE SERIES#######
#SERIE UNIV Y SECUNDARIA
serie.modelo1<-as.data.frame(cbind(seq(1991,2014,1),serie.educsupply[,2],serie.educsupply[,3],serie.wagediff[,2],serie.wagediff[,3]))
colnames(serie.modelo1)<-c("ano","unihsrelsup","bachdropssup","relwuni","relwbach")
write.csv(serie.modelo1,"series.csv")

