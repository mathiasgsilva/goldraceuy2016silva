directorio<-"C:\\Users\\Androide1\\Documents\\Mathias\\Maestría\\Tópicos de Distribución del Ingreso\\Trabajo Empirico"
setwd(directorio)
load("cargadas1.RData")
library(ineq)
library(ggplot2)
library(dummies)
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
#Los individuos son todos ocupados, de localidades mayores a 5000, no asisten en el momento de ser encuestados a educacion formal
#correccion nombre variables#
base.92$principal<-base.92$principal_sf
base.6$principal<-base.6$principal_sf
base.7$principal<-base.7$principal_sf
#Filtrado a solo asalariados privados entre 18 y 65 que trabajaron al menos 1 hora en la semana anterior o lo hubieran hecho habitualmente, y salario positivo de la ocupacion principal sin disse/fonasa, y se tiene el dato de anos educacion y rama.#
#aÃ±os educacion esta en todas, en la 2014 hay algunos errores de redondeo
base.14$edu<-floor(base.14$edu)
for(i in 1991:2014){
assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))[which(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$pe3>=18 & get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$pe3<=65 & is.na(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$edu)==F & is.na(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$rama)==F & get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1>0 & get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal>0 & get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$pf41a==1),])
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

#Log salario hora#
for(i in 1991:2014){
assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),"lnwh"=log(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$principal/(4*get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1))))
}

#Experiencia potencial y su cuadratica#
for(i in 1991:2014){
assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),"exppot"=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$pe3-6-get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$edu,"exppot2"=(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$pe3-6-get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$edu)^2))
}
#Compatibilizacion de la variable sexo, definicion de la variable mujer
func_sexo<-function(x){
  if(("mujer"%in%get(paste("base.",(x-1900)-100*((((x-1900)%/%100))),sep=""))$pe2) & (0%in%get(paste("base.",(x-1900)-100*((((x-1900)%/%100))),sep=""))$pe2==F)){
    as.integer(get(paste("base.",(x-1900)-100*((((x-1900)%/%100))),sep=""))$pe2)
  }
  if(0%in%get(paste("base.",(x-1900)-100*((((x-1900)%/%100))),sep=""))$pe2 & ("mujer"%in%get(paste("base.",(x-1900)-100*((((x-1900)%/%100))),sep=""))$pe2==F)){
    as.integer(get(paste("base.",(x-1900)-100*((((x-1900)%/%100))),sep=""))$pe2)+1
  }
  else{
    as.integer(get(paste("base.",(x-1900)-100*((((x-1900)%/%100))),sep=""))$pe2)
  }
}
for(i in 1991:2014){
  assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),"sexo"=func_sexo(i)))
}
for(i in 1991:2014){
  assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),"mujer"=get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$sexo-1))
}
#Compatibilizacion de ocupacion a 1 digito
#############CROSSWALK COTA70 A CIUO88 HASTA 1999
crosswalk<-read.csv("C:\\Users\\Androide1\\Documents\\Mathias\\Maestría\\Tópicos de Distribución del Ingreso\\Trabajo Empirico\\Crosswalks\\crosswalk_cota70_ciuo88.csv",row.names=NULL,sep=";")
crosswalk$COTA.70<-as.integer(crosswalk$COTA.70)
crosswalk$CIUO.88<-as.integer(crosswalk$CIUO.88)
crosw.cota.ciuo88<-function(x){
  out<-rep(0,nrow(get(paste("base.",x,sep=""))))
  for(j in 1:nrow(get(paste("base.",x,sep="")))){
    out[j]<-crosswalk$CIUO.88[which(crosswalk$COTA.70==get(paste("base.",x,sep=""))$pf39[j])[1]]
  }
  return(out)
}
for(i in 1991:1999){
assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),"ocup"=cut(crosw.cota.ciuo88((i-1900)-100*((((i-1900)%/%100)))),breaks=c(0,9999,19999,29999,39999,49999,59999,69999,79999,89999,99999),labels=c(0,1,2,3,4,5,6,7,8,9))))
}
##Compatibilizacion CIUO88 a 1 digito de 2000 en adelante
for(i in 2000:2004){
  assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),"ocup"=cut(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$pf39,breaks=c(0,99,199,299,399,499,599,699,799,899,999),labels=c(0,1,2,3,4,5,6,7,8,9))))
}
for(i in 2005:2014){
  assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),"ocup"=cut(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$pf39,breaks=c(0,999,1999,2999,3999,4999,5999,6999,7999,8999,9999),labels=c(0,1,2,3,4,5,6,7,8,9))))
}
###############Se da igualmente un salto importante entre 1999 y 2000 en la clasificacion a 1 digito, no se esta controlando del todo el error de medicion por cambio de sistema de clasificacion
###Compatibilizacion depto y variable interior
for(i in 1991:2014){
  assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),"interior"=cut(as.integer(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$dpto),breaks=c(0,1,20),labels=c(0,1))))
}
###Compatibilizacion de horas y creacion de part-time
for(i in 1991:2014){
  assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),"fulltime"=cut(as.integer(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))$horas_1),breaks=c(0,39,999),labels=c(0,1))))
}
#Verificacion que estan compatibilizadas
var.tester<-function(x){
for(i in 1991:2014){
print(x%in%colnames(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""))))
}
}
var.tester("lnwh")
var.tester("edu")
var.tester("exppot")
var.tester("exppot2")
var.tester("mujer")
var.tester("rama")
var.tester("ocup")
var.tester("interior")
var.tester("fulltime")
var.tester("peso2")
####Dummies por año de educacion
base.91$edu2<-cut(base.91$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.92$edu2<-cut(base.92$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.93$edu2<-cut(base.93$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.94$edu2<-cut(base.94$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.95$edu2<-cut(base.95$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.96$edu2<-cut(base.96$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.97$edu2<-cut(base.97$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.98$edu2<-cut(base.98$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.99$edu2<-cut(base.99$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.0$edu2<-cut(base.0$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.1$edu2<-cut(base.1$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.2$edu2<-cut(base.2$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.3$edu2<-cut(base.3$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.4$edu2<-cut(base.4$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.5$edu2<-cut(base.5$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.6$edu2<-cut(base.6$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.7$edu2<-cut(base.7$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.8$edu2<-cut(base.8$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.9$edu2<-cut(base.9$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.10$edu2<-cut(base.10$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.11$edu2<-cut(base.11$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.12$edu2<-cut(base.12$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.13$edu2<-cut(base.13$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
base.14$edu2<-cut(base.14$edu,include.lowest=T,right=F,breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,99),labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
for(i in 1991:2014){
  assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),dummy("edu2",get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")))))
  }
#####Dummies por rama
for(i in 1991:2014){
  assign(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep=""),cbind.data.frame(get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")),dummy("rama",get(paste("base.",(i-1900)-100*((((i-1900)%/%100))),sep="")))))
}

####HASTA ACA ESTA GUARDADO EN EL .RDATA####
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
