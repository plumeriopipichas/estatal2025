setwd("/home/FedericoYU/Documentos/Chamba/Olimpiada/Estatal 2025/estatal_2025_RProject/")

library(dplyr)
library(readr)
library(stringr)
library(tools)

source("funciones_adhoc.R")


#---------- Agregar los registros tardios independientes y de escuela 

porescuela_extra<-
  read.csv("../listas_crudas/csv_escuelas/extra_escuelas.csv",
           encoding = "UTF-8")

print(names(porescuela_extra))

#indep1<-read.csv("../listas_crudas/independientes_formulario.csv")%>%
#        select("Correo"=Dirección.de.correo.electrónico,Nombre,Primer_apellido,Segundo_apellido,Grado.de.estudios,
#                    "Escuela"=Escuela.de.procedencia,"Municipio"=Municipio.de.residencia,Sede)
#indep2<-read.csv("../listas_crudas/independientes_formulario_posterior.csv")%>%
#          select("Correo"=Dirección.de.correo.electrónico,Nombre,Primer_apellido,Segundo_apellido,Grado.de.estudios,
#                "Escuela"=Escuela.de.procedencia,"Municipio"=Municipio.de.residencia,Sede)
#x<-which(!indep2$Correo%in%indep1$Correo)
#write.csv(indep2[x, ],"../listas_generadas/extras_independientes.csv",row.names = FALSE)

#independientes_extra<-
#  read.csv("../listas_generadas/extras_independientes.csv",encoding = "UTF-8")
#indep_preregistro<-read.csv("../listas_crudas/preregistro_independiente.csv")%>%
#  select(CURP,Correo)

#independientes_extra <- 
#  merge(indep_preregistro,independientes_extra,  by = "Correo", all.y=TRUE)%>%unique()

# Asegura que CURP esté en mayúsculas
#independientes_extra <- independientes_extra %>%
#  mutate(CURP = toupper(CURP))

#write.csv(independientes_extra,"../listas_generadas/indep_extra2.csv")

#independientes_extra$Escuela<-"Participante Independiente"
#independientes_extra$Clave_escuela <- NA
#independientes_extra$Correo_escuela <- NA


agregar_independientes<-read.csv("../listas_crudas/agregar_independientes.csv",encoding="UTF-8")
agregar_independientes$Clave_escuela<-NA


aux <- intersect(names(porescuela_extra),names(agregar_independientes))

lista_registros_tardios <- 
  rbind(select(porescuela_extra,all_of(aux)),select(agregar_independientes,all_of(aux)))
lista_registros_tardios$Escuela <- subte(lista_registros_tardios$Escuela)
       

lista_previa <- read.csv("../listas_generadas/lista_general_registro.csv")
lista_nueva <- read.csv("../listas_generadas/lista_general_registro_.csv")
lista_general<-merge(select(lista_previa,clave,Grado.escolar),lista_nueva,by="clave",all=TRUE)
aux <- intersect(names(lista_general),names(lista_registros_tardios))


lista_general_puntuaciones <- filter(lista_general,nchar(clave)>0) #TEMPORAL
print(summary(nchar(lista_general_puntuaciones$clave)))
lista_general_puntuaciones$clave <- gsub("[A-Z]", " ", lista_general_puntuaciones$clave)
print(summary(nchar(lista_general_puntuaciones$clave)))

lista_general_puntuaciones <- rbind(select(lista_general,all_of(aux)),select(lista_registros_tardios,all_of(aux)))

for (nombre in c('Nombre','Primer_apellido','Segundo_apellido')){
  lista_general_puntuaciones[[nombre]]<-
    toTitleCase(tolower(lista_general_puntuaciones[[nombre]]))
}



#---------- Limpiar listas de respuestas por sede 

respuestas_sedes <- list()

# Variables esperadas
vars <- c("clave", "Aciertos", paste0("Resp_", 1:12))

for (sede in sedes) {
  path <- paste0("../listas_crudas/respuestas_examen/", sede, "2025.csv")
  print(c("Leyendo:", sede))
  
  if (file.exists(path)) {
    # Intenta leer y seleccionar columnas
    tryCatch({
      df <- read.csv(path,fileEncoding = "latin1")
      
      # Verifica que estén todas las columnas necesarias
      columnas_faltantes <- setdiff(vars, names(df))
      if (length(columnas_faltantes) > 0) {
        warning(paste("Faltan columnas en", sede, ":", paste(columnas_faltantes, collapse = ", ")))
      }
      
      df <- df %>% select(any_of(vars)) %>% unique()
      df$sede <- sede
      respuestas_sedes[[sede]] <- df
      
    }, error = function(e) {
      warning(paste("Error al leer el archivo de la sede", sede, ":", e$message))
    })
  } else {
    warning(paste("Archivo no encontrado para la sede:", sede))
  }
}

####Poner puntuaciones en la lista de respuestas

lista_respuestas <- juntar_bases(respuestas_sedes)

# Respuestas correctas
correctas <- c("C","B","D","D","A","B","D","C","C","B","B","A")

# Ponderación por bloques
valores <- c(rep(3, 4), rep(4, 4), rep(5, 4))

# Inicializa columna de puntos
lista_respuestas$Puntos <- 0

# Recorre fila por fila y suma puntos correctos
for (i in 1:nrow(lista_respuestas)) {
  respuestas_alumno <- as.character(lista_respuestas[i, paste0("Resp_", 1:12)])
  aciertos <- respuestas_alumno == correctas
  puntaje <- sum(valores[aciertos]) + 5  # Bonificación final
  lista_respuestas$Puntos[i] <- puntaje
}

# Reordenar columnas: dejar 'clave', 'Aciertos', 'Puntos' al inicio
orden_col <- c("clave", "Aciertos", "Puntos",
               setdiff(names(lista_respuestas), c("clave", "Aciertos", "Puntos")))
lista_respuestas <- lista_respuestas[, orden_col]

#----------hacer que las claves ausentes en lista respuestas no se asocien (temporal, hay que checar las hojitas)

x<-which(nchar(lista_respuestas$clave)==0)
lista_respuestas$clave[x]<-1:length(x)

#------------- Crear lista para revisar examanes con claves no registradas

x<-which(!lista_respuestas$clave%in%lista_general_puntuaciones$clave)

claves_revisar <- lista_respuestas[x, c("clave","Aciertos","Puntos","sede")]



#--------------la lista completa y la lista depurada

x<-which(names(lista_respuestas)=="Identificacion")
names(lista_respuestas)[x]<-"clave"

print(dim(lista_general_puntuaciones))
lista_general_puntuaciones <- merge(lista_general_puntuaciones,lista_respuestas,by ="clave",all.y=TRUE)
print(dim(lista_general_puntuaciones))

#lista_depurada <- filter(lista_general_puntuaciones,!is.na(Nombre))

#--------------agregar nivel de estudios (habra que completar a mano) y ordenar por puntos y respuestas-------------

lista_general_puntuaciones$nivel<-""
x<-grep("reparat",lista_general_puntuaciones$Grado.escolar,ignore.case = TRUE)
x<-c(x,grep("semestre",lista_general_puntuaciones$Grado.escolar,ignore.case = TRUE))
x<-c(x,grep("preparatoria",lista_general_puntuaciones$Escuela,ignore.case = TRUE))
x<-c(x,grep("achiller",lista_general_puntuaciones$Escuela,ignore.case = TRUE))
x<-c(x,grep("cobaeh",lista_general_puntuaciones$Escuela,ignore.case = TRUE))
x<-c(x,grep("cemsad",lista_general_puntuaciones$Escuela,ignore.case = TRUE))
x<-c(x,grep("cecyt",lista_general_puntuaciones$Escuela,ignore.case = TRUE))
x<-c(x,grep("cbt",lista_general_puntuaciones$Escuela,ignore.case = TRUE))
x<-c(x,grep("cuarto",lista_general_puntuaciones$Grado.escolar,ignore.case = TRUE))
x<-c(x,grep("4",lista_general_puntuaciones$Grado.escolar,ignore.case = TRUE))
x<-c(x,grep("@UAEH",lista_general_puntuaciones$Correo,ignore.case = TRUE))
x<-c(x,grep("UAEH",lista_general_puntuaciones$Escuela,ignore.case = TRUE))
x<-c(x,grep("13DCT",lista_general_puntuaciones$Clave_escuela,ignore.case = TRUE))
x<-c(x,grep("@cecyteh",lista_general_puntuaciones$Correo,ignore.case = TRUE))
x<-c(x,grep("@cobaeh",lista_general_puntuaciones$Correo,ignore.case = TRUE))


y<-grep("secundaria",lista_general_puntuaciones$Grado.escolar,ignore.case = TRUE)
y<-c(y,grep("est",lista_general_puntuaciones$Escuela))
y<-c(y,grep("secundaria",lista_general_puntuaciones$Escuela,ignore.case = TRUE))
y<-c(y,grep("13PES",lista_general_puntuaciones$Clave_escuela,ignore.case = TRUE))
y<-c(y,grep("Sec.",lista_general_puntuaciones$Escuela,ignore.case = TRUE))
y<-c(y,grep("13DST",lista_general_puntuaciones$Clave_escuela,ignore.case = TRUE))

lista_general_puntuaciones$nivel[x]<-"media superior"
lista_general_puntuaciones$nivel[y]<-"secundaria"


lista_general_puntuaciones <- lista_general_puntuaciones %>%
  arrange(
    desc(Puntos),           # Primero por puntuación (mayor primero)
    desc(Aciertos),         # Luego por aciertos (mayor primero)
    Resp_1, Resp_2, Resp_3, Resp_4, Resp_5, Resp_6,
    Resp_7, Resp_8, Resp_9, Resp_10, Resp_11, Resp_12
  )

print("A")

lista_general_puntuaciones<-lista_general_puntuaciones%>%
  mutate(Nombre_completo=paste(Nombre,Primer_apellido,Segundo_apellido))

print(names(lista_general_puntuaciones))
write.csv(lista_general_puntuaciones,"../listas_generadas/revisar_falta_nivel.csv")

#-------------- varias listas para entregar: comite de examen, constancias de participacion

lista_secundaria_local<-filter(lista_general_puntuaciones,!nivel=="media superior")%>%
  select(clave,Aciertos,Puntos,Nombre_completo,Grado.escolar)%>%
  unique()
lista_preparatoria_local<-filter(lista_general_puntuaciones,!nivel=="secundaria")%>%
  select(clave,Aciertos,Puntos,Nombre_completo)%>%
  unique()%>%
  !is.na(Nombre_completo)
  
  print("B")
comite_examen_secundaria <- filter(lista_secundaria_local,!is.na(Nombre_completo))%>%
                            select(clave,Aciertos,Puntos)%>%
                            unique()%>%
                            !is.na(Nombre_completo)
                                 
comite_examen_preparatoria <- filter(lista_preparatoria_local,!is.na(Nombre_completo))%>%
                             select(clave,Aciertos,Puntos)%>%
                              unique()%>%
                              !is.na(Nombre_completo)


write.csv(lista_secundaria_local,"../listas_generadas/local_examen_secu.csv")
write.csv(lista_preparatoria_local,"../listas_generadas/local_examen_prepa.csv")
write.csv(comite_examen_secundaria,"../listas_generadas/comite_examen_secu.csv")
write.csv(comite_examen_preparatoria,"../listas_generadas/comite_examen_prepa.csv")


