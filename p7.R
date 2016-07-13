directorio<-"C:\\Users\\Androide1\\Documents\\Mathias\\Maestría\\Tópicos de Distribución del Ingreso\\Trabajo Empirico\\ech\\ech\\personas\\bases compatibilizadas"
setwd(directorio)
library(foreign)
base.07<-read.dta("p7.dta",convert.factors=T)
#dpto. 1 es mvd, el resto es interior
#filtloc. 1 es localidad de mas de 5000 personas.
#mes. para ajustar por ipc
#peso2. peso anual
#principal. es ingresos laborales sin fonasa/disse de ocupacion principal
#esta todo ajustado por variable ipc a diciembre 2006=100

#pobp. categoria de actividad hasta 2006. luego de 2006 es pobpcoac
#pobp==2 es ocupados
#pf41a. categoria ocupacion. 1 es asal priv.
#rama. rama 1 digito. ver criterios
#pf39. ocupacion. COTA 70 a 3 digitos hasta 1999. 2000 hasta feb 2005 ciuo 88 a 3 digitos. mar 2005 a 2009 ciuo 88 a 4 digitos

#pe2. sexo. 1 es varon
#pe3. edad.
#horas_1. horas trabajadas efectivas en la ultima semana hasta 2000, horas habituales luego del 2000
#pe11. asiste actualmente a educacion formal. 1 es si.
#edu. anos de educacion formal
#nivel2. maximo nivel educativo cursado.
#finalizo. finalizo el nivel

####FORMALES?????#####

#A partir de 2006 se pregunta por posgrados por separado
#Educacion aca es distinta
#univ completa:como maximo finalizo nivel universitario o finalizo posgrado
base.07$finalizo_univ<-ifelse(base.07$nivel2==5 & (base.07$e54_5_2==1|base.07$e54_7_2==1),1,0)
#secundaria completa:como maximo finalizo secundaria, o curso utu con secundaria completa como exigencia previa, o curso universidad o magisterio/prof, o finalizo universidad
base.07$finalizo_sec<-ifelse((base.07$nivel2==2 & base.07$e54_2_2==1)|(base.07$nivel2==3 & base.07$e54_3_3==1)|(base.07$nivel2==4)|base.07$nivel2==5|base.07$finalizo_univ==1,1,0)


#restriccion de la base a poblacion de interes
#ocupados asalariados privados o publicos que no asistan a educacion formal, localidades mayores a 5 mil habs, que hayan trabajado al menos 1 hora la semana anterior o lo hubieran hecho habitualmente.
base.07<-base.07[which(base.07$pe11!=1 & base.07$pobpcoac==2 & base.07$filtloc==1),c("dpto","filtloc","mes","pesoano","principal_sf","pobpcoac","pf41a","rama","pf39","tipo_ocup","pe2","pe3","horas_1","pe11","edu","nivel2","finalizo_univ","finalizo_sec")]
