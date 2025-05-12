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

agregar_independientes<-read.csv("../listas_crudas/agregar_independientes.csv",encoding="UTF-8")
agregar_independientes$Clave_escuela<-NA
agregar_independientes$Correo_escuela<-NA

aux <- intersect(names(porescuela_extra),names(agregar_independientes))

lista_registros_tardios <- 
  rbind(select(porescuela_extra,all_of(aux)),select(agregar_independientes,all_of(aux)))
lista_registros_tardios$Escuela <- subte(lista_registros_tardios$Escuela)


lista_previa <- read.csv("../listas_generadas/lista_general_registro_anclado.csv")
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
  puntaje <- sum(valores[aciertos]) 
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


lista_general_puntuaciones<-lista_general_puntuaciones%>%
  mutate(Nombre_completo=paste(Nombre,Primer_apellido,Segundo_apellido))

print(names(lista_general_puntuaciones))
write.csv(lista_general_puntuaciones,"../listas_generadas/revisar_falta_nivel.csv")

#-------------- varias listas para entregar: comite de examen, constancias de participacion

lista_secundaria_local<-filter(lista_general_puntuaciones,!nivel=="media superior")%>%
  select(clave,Aciertos,Puntos,Nombre_completo,Grado.escolar,Escuela,nivel,
         Correo,Correo_escuela)%>%
  unique()

lista_preparatoria_local<-filter(lista_general_puntuaciones,!nivel=="secundaria")%>%
  select(clave,Aciertos,Puntos,Nombre_completo,Grado.escolar,Escuela,nivel,
         Correo,Correo_escuela)%>%
  unique()
  
comite_examen_secundaria <- filter(lista_secundaria_local,nchar(nivel)>0)%>%
                            select(clave,Aciertos,Puntos)%>%
                            unique()
                                 
comite_examen_preparatoria <- filter(lista_preparatoria_local,nchar(nivel)>0)%>%
                             select(clave,Aciertos,Puntos)%>%
                              unique()

write.csv(lista_secundaria_local,"../listas_generadas/local_examen_secu.csv")
write.csv(lista_preparatoria_local,"../listas_generadas/local_examen_prepa.csv")
write.csv(comite_examen_secundaria,"../listas_generadas/comite_examen_secu.csv")
write.csv(comite_examen_preparatoria,"../listas_generadas/comite_examen_prepa.csv")

#---------------- la lista para la segunda etapa

corte_sec<-26
corte_bach<-29

segunda_etapa_sec <- filter(lista_secundaria_local,Puntos>corte_sec)%>%
  select(Nombre_completo,Escuela,nivel,Correo,Correo_escuela,Puntos)

segunda_etapa_ms <- filter(lista_preparatoria_local,Puntos>corte_bach)%>%
  select(Nombre_completo,Escuela,nivel,Correo,Correo_escuela,Puntos)


segunda_etapa<-rbind(segunda_etapa_sec,segunda_etapa_ms)%>%
  select(Nombre_completo,Equipo=Escuela,Correo,Correo_escuela,Puntos,nivel)


segunda_etapa$Equipo <- subte(segunda_etapa$Equipo)

#directos <- read.csv("../listas_crudas/directos_segunda.csv")

#segunda_etapa_todos <- rbind(select(segunda_etapa,all_of(names(directos))),
 #                            directos)

correos_segunda_etapa<-unique(c(segunda_etapa$Correo,segunda_etapa$Correo_escuela))
publicar_segunda_etapa<-filter(segunda_etapa,nchar(nivel)>0)%>%
                        select(Nombre_completo,Equipo,nivel)%>%
                        arrange(Nombre_completo)

write.csv(segunda_etapa,"../listas_generadas/segunda_etapa_general.csv",row.names = FALSE)
write.csv(publicar_segunda_etapa,"../listas_generadas/segunda_etapa_publica.csv",row.names = FALSE)
write.csv(correos_segunda_etapa,"../listas_generadas/correos_2e.csv",row.names = FALSE)

#----------para sacar puntuaciones por escuela

revision_escuelas <-select(lista_general_puntuaciones,-c(CURP,clave,Grado.escolar,
                        Correo_escuela,Correo,sede))%>%
                    filter(!is.na(Clave_escuela))

revision_escuelas<-unique(revision_escuelas)
revision_escuelas<-group_by(revision_escuelas,Clave_escuela)

revisar_escuelas<-summarise(revision_escuelas,cuantos=n(),total_aciertos=sum(Aciertos),
                          suma_puntos=sum(Puntos),varianza=var(Puntos),)%>%
                          arrange(desc(suma_puntos),varianza)

revision_escuelas<-ungroup(revision_escuelas)
#--------------------------------------------
# para hacer las constancias

para_constancias <- select(lista_general_puntuaciones,Nombre,Primer_apellido,Segundo_apellido,
                           Escuela,Correo,Correo_escuela)


x<-grep("@",para_constancias$Correo)
y<-grep("@",para_constancias$Correo_escuela)
para_constancias<-para_constancias[union(x,y), ]
para_constancias$Escuela<-subte(para_constancias$Escuela)
para_constancias$Nombre_completo<-paste(para_constancias$Nombre,
                                        para_constancias$Primer_apellido,para_constancias$Segundo_apellido)

para_constancias<-select(para_constancias,Nombre_completo,Escuela,Correo,Correo_escuela)
para_constancias$Correo<-subte_correo(para_constancias$Correo)
para_constancias$Correo_escuela<-subte_correo(para_constancias$Correo_escuela)
para_constancias<-unique(para_constancias)%>%
                  arrange(Nombre_completo)

inicio <- 409
para_constancias$folio <- inicio:(inicio+nrow(para_constancias)-1)  

write.csv(para_constancias,"../listas_generadas/lista_constancias_participacion.csv",
          row.names = FALSE)


segunda_etapa_todos<-rbind(lista_secundaria_local,lista_preparatoria_local)%>%
                  select(Nombre_completo,Escuela,Correo,Correo_escuela)


constancias_segundo <- select(segunda_etapa_todos,Nombre_completo,Escuela,Correo,Correo_escuela)

x<-grep("@", constancias_segundo$Correo)
y<-grep("@", constancias_segundo$Correo_escuela)
constancias_segundo<-constancias_segundo[union(x,y), ]


constancias_segundo<-unique(constancias_segundo)%>%
            arrange(Nombre_completo)
constancias_segundo$Correo<-subte_correo(constancias_segundo$Correo)
constancias_segundo$Correo_escuela<-subte_correo(constancias_segundo$Correo_escuela)            
            
inicio <- nrow(para_constancias)+1
constancias_segundo$folio <- inicio:(inicio+nrow(constancias_segundo)-1)  

write.csv(constancias_segundo,"../listas_generadas/lista_constancias_segundo.csv",
          row.names = FALSE)


