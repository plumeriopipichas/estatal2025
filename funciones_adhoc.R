abreviar <- function(x,long=8){
  substr(gsub(" ","",x),1,long)
}


agradar<-function(nivel,grado){
  fg<-""
  f<-""
  g<-""
  if (nivel=="s"){
    g<-"año de Secundaria"
  }
  if (nivel=="m"){
    g<-"semestre de Bachillerato"
  }
  if (grado==1){
    f<-"Primer"
  }
  if (grado==2){
    f<-"Segundo"
  }
  if (grado==3){
    f<-"Tercer"
  }
  if (grado==4){
    f<-"Cuarto"
  }
  if (grado==5){
    f<-"Quinto"
  }
  fg<-paste(f,g)
  return(fg)
}


#encontrar los alumnos de una clave de escuela en la lista de puntos

detective <- function(m){
  x<-which(revision_escuelas$Clave_escuela==revisar_escuelas$Clave_escuela[m])              
  chequeo <- revision_escuelas[x, ]
  chequeo <- mutate(chequeo, nombre_completo=paste(Nombre,Primer_apellido,Segundo_apellido))%>%
    select(Clave_escuela,nombre_completo,Escuela,Aciertos,Puntos)%>%
    arrange(Escuela)
  return(chequeo)
}

juntar_bases<-function(lista_bases){
  juntos <- as.data.frame(matrix(nrow=0,ncol = ncol(lista_bases[[1]])))
  for (base in names(lista_bases)){
    if (nrow(lista_bases[[base]]>0)){
      print(c("juntando base",base))
      juntos <- rbind(juntos,lista_bases[[base]])  
    }
    print(c("dimension de bases juntas",dim(juntos)))
  }
  return(juntos)
}


#reportar resultados de uno o varios alumnos

reportar <- function(x,nombre_archivo="vacio"){
      reporte <- lista_completa[x, ]
      reporte<- mutate(reporte,Nombre_completo=
                                 paste(Nombre,Primer_apellido,Segundo_apellido))%>%
                select(clave,Nombre_completo,Aciertos,Puntos,all_of(respes))
      write.csv(reporte,file=paste("../listas_generadas/reportes/",nombre_archivo,".csv",sep=""),
                row.names = FALSE)
      print("sd")
      return(reporte)
}

#respuestas de una clave dada, con nombre si hay

respuestas <- function(clave){
      temp <- respuestas_completas
      x <- which(temp$Folio==clave)
      temp <- temp[x, ]
      temp <- select(temp,-Folio,-Aciertos)
      if (nrow(temp)<1){
        print("Clave no existente")
        break
      }
      resp <- as.character(temp[1, ])
      veredicto <- 1:12
      x<-which(resp==correctas)
      veredicto[x]<-"Correcto"
      veredicto[-x]<-"Incorrecto"
      base<-data.frame(numero_pregunta=1:12,
                       respuesta=resp,resultado=veredicto)
      return(base) 
}


limpiar <- function(texto){
  texto <- trimws(texto)
  texto <- str_to_title(texto)
  texto<-gsub(" De "," de ",texto)
  texto<-gsub(" Del "," del ",texto)
  texto<-gsub(" La "," la ",texto)
  texto<-gsub("à","á",texto)
  texto<-gsub("À","Á",texto)
}


notitle <- function(texto){
  texto<-gsub("Ing.","",texto,ignore.case = TRUE)
  texto<-gsub("Mtro.","",texto,ignore.case = TRUE)
  texto<-gsub("MTRA.","",texto,ignore.case = TRUE)
  texto<-gsub("Maestra ","",texto,ignore.case = TRUE)
  texto<-gsub("M.C.","",texto,ignore.case = TRUE)
  texto<-gsub("Lic.","",texto,ignore.case = TRUE)
  texto<-gsub("Profra.","",texto,ignore.case = TRUE)
  texto<-gsub("Profr.","",texto,ignore.case = TRUE)
  texto<-gsub("Profa.","",texto,ignore.case = TRUE)
  texto<-gsub("Prof.","",texto,ignore.case = TRUE)
  texto<-gsub("Arq.","",texto,ignore.case = TRUE)
  texto<-gsub("Arquitecto ","",texto,ignore.case = TRUE)
}

#---para revisar si estan los examenes donde hubo algo raro en 2023

revisar<-function(clave,nombre="Camelio"){
  x<-which(comite_examen$clave==clave)
  if (length(x)>0){
    print(comite_examen[x, ])  
  }
  else{
    print("no esta en la lista con puntos.")
  }
  x<-grep(nombre,lista_general_participacion$Nombre)
  con_nombre_dado<-select(lista_general_participacion[x,],clave,
                          Nombre,Primer_apellido,Segundo_apellido,Correo,Sede)
  View(con_nombre_dado)
}

subte <- function(x){
  x<-gsub("°","",x)
  x<-toTitleCase(tolower(x))
  x<-gsub("cndaria", "cundaria",x,ignore.case = TRUE)
  x<-gsub("Escuela Preparatoria", "Preparatoria",x,ignore.case = TRUE)
  x<-gsub("COBAEH CEMSAD","CEMSAD",x,ignore.case = TRUE)
  x<-gsub("CECYTE HIDALGO,","CECYTEH",x,ignore.case = TRUE)
  x<-gsub("CECYTE HIDALGO","CECYTEH",x,ignore.case = TRUE)
  x<-gsub("COBAEH","COBAEH",x,ignore.case = TRUE)
  x<-gsub("CECYTEH","CECYTEH",x,ignore.case = TRUE)
  x<-gsub("CECYTE-","CECYTE ",x,ignore.case = TRUE)
  x<-gsub("UAEH","UAEH",x,ignore.case = TRUE)
  x<-gsub("IPN","IPN",x,ignore.case = TRUE)
  x<-gsub("cbtys","CBTIS",x,ignore.case = TRUE)
  x<-gsub("CEMSAD","CEMSAD",x,ignore.case = TRUE)
  x<-gsub("CONALEP","CONALEP",x,ignore.case = TRUE)
  x<-gsub("CBTIS","CBTIS",x,ignore.case = TRUE)
  x<-gsub("CBTA","CBTA",x,ignore.case = TRUE)
  x<-gsub("CETIS","CETIS",x,ignore.case = TRUE)
  x<-gsub("CECYT","CECyT",x,ignore.case = TRUE)
  x<-gsub("C.E.C.Y.T. ","CECyT ",x,ignore.case = TRUE)
  x<-gsub("Plantel","",x,ignore.case = TRUE)
  x<-gsub("ES de Cd. Sahagún t/v","ES Ciudad Sahagun UAEH",x,ignore.case = TRUE)
  x<-gsub("ES de Cd. Sahagún t/v","ES Ciudad Sahagun UAEH",x,ignore.case = TRUE)
  x<-gsub("Núnero","",x,ignore.case = TRUE)
  x<-gsub("Número","",x,ignore.case = TRUE)
  x<-gsub("Numero","",x,ignore.case = TRUE)
  x<-gsub("Escuela Superior", "ES",x,ignore.case = TRUE)
  x<-gsub("EscuelaSuperior", "ES",x,ignore.case = TRUE)
  x<-gsub("Escuela Secundaria","Secundaria",x,ignore.case = TRUE)
  x<-gsub("COLEGIO DE BACHILLERES DEL ESTADO DE HIDALGO","COBAEH",x,ignore.case = TRUE)
  x<-gsub("Bachillerato General","",x,ignore.case = TRUE)
  x<-gsub("Turno Vespertino","",x,ignore.case = TRUE)
  x<-gsub("Turno Matutino","",x,ignore.case = TRUE)
  x<-gsub("de Hgo.","",x)
  x<-gsub("de Hgo","",x)
  x<-gsub("Hgo.","",x)
  x<-gsub('"\"',"",x)
  x<-gsub("Siete","7",x)
  x<-gsub("Cinco","5",x)
  x<-gsub("Hujeutla","Huejutla",x)
  x<-gsub("No. ","",x,ignore.case = TRUE)
  x<-gsub(" No ","",x,ignore.case = TRUE)
  x<-gsub("particular","",x,ignore.case = TRUE)
  x<-gsub("Escuela ","",x,ignore.case = TRUE)
  x<-gsub(" De "," de ",x,ignore.case = TRUE)
  x<-gsub("Actòpan","Actopan",x,ignore.case = TRUE)
  x<-gsub(", "," de ",x,ignore.case = TRUE)
  x<-gsub("a2","a 2",x,ignore.case = TRUE)
  x<-gsub("Sec.tecnica","Secundaria Técnica",x,ignore.case = TRUE)
  x<-gsub("federal por cooperacion","",x,ignore.case = TRUE)
  x<-gsub("Centro de Bachillerato Tecnológico Industrial y de Servicios","CBTIS",x,ignore.case = TRUE)
  return(x)
}

subte_correo <- function(x){
  x<-gsub("gmal", "gmail",x,ignore.case = TRUE)
 return(x)
}

susti_ord <- function(x){
  gap<-max(nchar(x))-min(nchar(x))
  print(gap)
  if (gap > 9){
    print("demasiado grande el gap")
    break
  }
  minimo<-min(nchar(x))
  for (i in 1:length(x)){
    largo <- nchar(x[[i]])-minimo
    x[[i]]<-paste(as.character(largo),"_",x[[i]],sep="")
    print(i)
    print(x[[i]])
  }
  return(x)
}

trimba<-function(x){
  a<-str_replace_all(x, pattern=" ", repl="")
  a<-str_replace_all(a, pattern="ú", repl="u")
  return(a)
}


#remplazar claves en participantes sin CURP y otros donde se requiere


remplazo_clave<-function(lista){
    sincurp<-which(is.na(lista$CURP))
    sincurp<-c(sincurp,which(nchar(lista$CURP)<8))
    remp_clave <- sprintf("%04d", 1:length(sincurp)) 
    c=1
  
    for (i in sincurp){
        lista$clave[i]<-
          gsub("NANA",remp_clave[c],lista$clave[i])
        if (nchar(lista$clave[i])<5){
            lista$clave[i]<-
              paste(lista$clave[i],remp_clave[c],"000",sep="")
        }
    c<-c+1
    }
    x<-which(nchar(lista$clave)<10)
    for (i in x){
        y<-10-nchar(lista$clave[i])
        lista$clave[i]<-
          paste(lista$clave[i],strrep("0",y),sep="")
    }
    return(lista)
}  




