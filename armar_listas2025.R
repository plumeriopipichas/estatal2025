setwd("/home/FedericoYU/Documentos/Chamba/Olimpiada/Estatal 2025/estatal_2025_RProject")

library(dplyr)
library(stringr)
library(tools)

source("funciones_adhoc.R")

indep_formulario<-read.csv("../listas_crudas/independientes_formulario_posterior.csv")%>%
  select("Correo"=Dirección.de.correo.electrónico,Nombre,Primer_apellido,Segundo_apellido,Grado.escolar,
         "Escuela"=Escuela.de.procedencia,"Municipio"=Municipio.de.residencia,Sede)

indep_preregistro<-read.csv("../listas_crudas/preregistro_independiente.csv")%>%
  select(CURP,Correo)

independientes <- 
  merge(indep_preregistro,indep_formulario,  by = "Correo", all.y=TRUE)%>%unique()

# Asegura que CURP esté en mayúsculas
independientes <- independientes %>%
  mutate(CURP = toupper(CURP))

# Extrae las dos primeras letras del primer apellido y de la CURP
#curp_no_coincide <- independientes %>%
#  filter(!is.na(CURP)) %>%
#  mutate(
#    ape2 = str_sub(str_to_upper(Primer.apellido), 1, 2),
#    curp2 = str_sub(CURP, 1, 2)
#  ) %>%
#  filter(ape2 != curp2)

independientes$Escuela<-"Participante Independiente"
independientes$Clave_escuela <- NA
independientes$Correo_escuela <- NA

sedes_indy<-sort(unique(independientes$Sede))
print("lista de sedes con registro independiente")
print(sedes_indy)
#x<-which(sedes_indy>0)
#sedes_indy<-sedes_indy[x]

indy_por_sede<-list()    #lista para bases de datos de independientes por sede, completos
escuela_por_sede<-list() #lista para bases de datos de equipos de escuela por sede, completos
basica_sede <- list()    #lista para bases de datos por sede con variables basicas

archivos<-dir("../listas_crudas/csv_escuelas/")

#Se generan las tres listas arriba mencionadas, en primera version

for (i in 1:length(sedes_indy)){
  temp<-sedes_indy[[i]]
  x<-which(independientes$Sede==temp)
  indy_por_sede[[temp]] <- independientes[x, ]
  checksede <- paste(sedes_indy[i],"_",sep="")
  x <- grep(checksede,archivos)
  if (length(x)>1){
    print("Error: archivos excedentes.")
    break
  }
  path <- paste("../listas_crudas/csv_escuelas/",archivos[x],sep="")
  if (length(x)==1){
    escuela_por_sede[[temp]]<-read.csv(path,skip=2)%>%
      select(-Sede)
    x<-which(names(indy_por_sede[[i]])%in%names(escuela_por_sede[[i]]))
    aux <- indy_por_sede[[i]][x]
    x<-which(names(escuela_por_sede[[i]])%in%names(aux))
    aux2 <- escuela_por_sede[[i]][x]
    basica_sede[[temp]] <- rbind(aux,aux2)%>%
      arrange(CURP)
    basica_sede[[temp]]<-unique(basica_sede[[temp]])
  }
  else{
    escuela_por_sede[[temp]]<-data.frame()
    basica_sede[[temp]]<-data.frame()
  }
  if (i>9){
    basica_sede[[temp]]$numero_sede<-rep(i,nrow(basica_sede[[temp]]))
  }
  else{
    basica_sede[[temp]]$numero_sede<-rep(20+i,nrow(basica_sede[[temp]]))
  }
  if ("CURP"%in%names(basica_sede[[temp]])){
    basica_sede[[temp]]$CURP<-trimws(basica_sede[[temp]]$CURP)
    aux<-paste(as.character(basica_sede[[temp]]$numero_sede),
               substring(basica_sede[[temp]]$CURP,5,10),
               substring(basica_sede[[temp]]$CURP,18,18),sep="")
    pego<-paste(basica_sede[[temp]]$Primer_apellido,basica_sede[[temp]]$Nombre,sep="_")
    aux2<-nchar(pego)%%10
    rm(pego)
    basica_sede[[temp]]$clave<-paste(aux,aux2,sep="")
    basica_sede[[temp]]$sede<-temp
  basica_sede[[i]]<-remplazo_clave(basica_sede[[i]])
  }
}

lista_general_registro <- juntar_bases(basica_sede)
lista_general_registro$Escuela<-subte(lista_general_registro$Escuela)
write.csv(lista_general_registro,
          "../listas_generadas/lista_general_registro.csv",row.names = FALSE)
sedes<-names(basica_sede)

anyDuplicated(lista_general_registro$clave)

#-------------------------------

# hacer las listas de asistencia desde las listas basicas


asistencia<-list()

for (i in sedes){
  if (length ( names ( basica_sede [[i]] ) ) > 1) {
    basica_sede[[i]] <- arrange(basica_sede[[i]],Nombre)
    print(c('aseo',i))
    for (nombre in c('Nombre','Primer_apellido','Segundo_apellido'))
    {basica_sede[[i]][[nombre]]<-toTitleCase(tolower
                                             (basica_sede[[i]][[nombre]]))
    basica_sede[[i]][[nombre]]<-limpiar(basica_sede[[i]][[nombre]])
    }    
    asistencia[[i]] <-select ( basica_sede[[i]], Nombre, Primer_apellido, Segundo_apellido,
                               Escuela, clave,sede)
    orden<-1:nrow(asistencia[[i]])
    asistencia[[i]]<-cbind(orden,asistencia[[i]])
  }
}

#exportar algunas listas  de cada sede a un csv

for (sede in sedes){
  if ("Nombre"%in%names(basica_sede[[sede]])){
    print(c("exportar listas de",sede))
  }
  path1 <- paste("../listas_generadas/listas_basicas/lista_basica_",sede,".csv",sep="")
  path2 <- paste("../listas_generadas/listas_asistencia/lista_asistencia_",sede,".csv",sep="")
  
  print(dim(basica_sede[[sede]]))
  write.csv(basica_sede[[sede]],file = path1,row.names = FALSE)
  write.csv(asistencia[[sede]],file = path2,row.names = FALSE)
}

resumen <- tibble(
  sede = names(basica_sede),
  alumnos = sapply(basica_sede, nrow)
)

write.csv(resumen,"../listas_generadas/participacion_sedes.csv",row.names = FALSE)


#-----------extraer lista de correos y mandarla a un csv

#correos<-select(lista_general_registro,Correo)%>%
 #         unique()
#correosesc<-select(lista_general_registro,Correo_escuela)%>%
 # unique()
#names(correosesc)<-"Correo"
#correos<-rbind(correos,correosesc)

#write.csv(x = correos,"../listas_generadas/correos.csv",row.names = FALSE)

#rm(correos,correosesc)
#--------------------------------



rm(temp,x)