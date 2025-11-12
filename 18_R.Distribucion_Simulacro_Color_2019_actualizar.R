# Cargar librerías necesarias
library(dplyr)
library(openxlsx)

# Suppress dplyr warnings for cleaner output and faster execution
options(dplyr.summarise.inform = FALSE)

# Lectura de la tabla

dist2 <- read.table ("distribucion_machos_ad_2020.txt", header=TRUE, sep="\t", dec=',')
trat.original <- read.table ("trat.txt", header=TRUE, sep="\t", dec=',')
trat <- trat.original
trat <- trat %>%
  dplyr::mutate(Lag19 = as.factor(Lag19)) %>%
  dplyr::select(-Encl19)

# Nota: las columnas auxiliares de `dist2` se inicializan más abajo cuando ya existe `dist2`.

# Datos del experimento

C.18 <- 8             #La interacci?n Lag18:Variabilidad18 tiene 8 niveles (0.5L,0.5M,1L,1M,2L,2M,4L,4M)
C.19 <- 8             #La interacci?n Lag19:Variabilidad19 tiene 8 niveles (1D,4D,1L,1M,2L,2M,4L,4M)
Encl.18 <- 16         #El n?mero de cercados de 2018 (C1, C2, C3 [...], C16)  
Encl.19  <- 16        #El n?mero de cercados de 2019 (C1, C2, C3 [...], C16)
Lag.19 <- 3           #El tratamiento Lag19 tiene tres niveles (1,2 y 4)
Variabilidad.19 <- 3  #El tratamiento Variabilidad19 tiene tres niveles (D, L y M)

# Crear las variables necesarias, y comprobar su formato

dist2 <- dist2 %>%
  dplyr::mutate(
    Lag18 = as.factor(Lag18),
    Comb18 = paste(Lag18, Variabilidad18, sep = "_"),
    SVL = as.numeric(SVL),
    BodyConditionR = resid(lm(Weight.at.release ~ SVL, data = .)),
    TailLength = as.numeric(TailLength)
  )

R.Comb18 <- dplyr::count (dist2, Comb18) %>%
  dplyr::mutate(R5.1 = ceiling(n / C.19)) %>%
  dplyr::select(-n)
dist2 <- merge (dist2, R.Comb18, by ="Comb18")
ind.Encl <- dplyr::count (dist2, Encl18, Comb18) %>%
  dplyr::select(-Comb18) %>%
  dplyr::rename(ind.Encl18 = n)
dist2 <- merge (dist2, ind.Encl, by="Encl18")

# Indice por combinación Encl18-Comb18: número de individuos por (Encl18, Comb18)
ind.Comb.Encl <- dplyr::count(dist2, Encl18, Comb18)

cer18 <- dplyr::count (dist2, Encl18, Comb18) %>%
  dplyr::select(-n)
cer19 <- dplyr::select (trat.original, Encl19, Comb19)
cer18.R1 <- cer18 %>%
  dplyr::rename(Encl = Encl18)
cer19.R1 <- cer19 %>%
  dplyr::rename(Encl = Encl19)
R1.C18.C19 <- merge (cer18.R1, cer19.R1, by="Encl") %>%
  dplyr::mutate(Comb18_Comb19 = paste(Comb18, Comb19, sep = "_")) %>%
  dplyr::select(Encl, Comb18_Comb19)
R1.n.caben <- dplyr::count (R1.C18.C19, Comb18_Comb19) %>%
  dplyr::rename(n.caben = n) %>%
  as.data.frame()

R1.1 <- seq_len(nrow(R1.n.caben))
R1.2 <- seq_len(nrow(R1.n.caben))
for(i in seq_len(nrow(R1.n.caben))){
  if (R1.n.caben$n.caben[i]==1){   
    R1.1[i] <- R1.n.caben$Comb18_Comb19[i]
    R1.2[i] <- NA
  } else if (R1.n.caben$n.caben[i]==2){   
    R1.1[i] <- NA
    R1.2[i] <- R1.n.caben$Comb18_Comb19[i]
  }  
}
R1.1 <- R1.1[is.na(R1.1) == F] 
R1.2 <- R1.2[is.na(R1.2) == F] 

# Variables de R5 y columnas auxiliares: crear con dplyr::mutate() y vectorizar el mapeo de Color2
dist2 <- dist2 %>%
  dplyr::mutate(
    R5 = 0,
    R5.2 = 0,
    R5.5 = 0,
    n.Encl18 = 0,
    caben = 0,
    n.caben = 0,
    caben.bueno = 0,
    R5.3 = 0,
    Encl18_Encl19 = "NA",
    n.C19 = "NA",
    n.C18.C19 = "NA",
    n.R2 = "NA",
    R2 = "NA",
    n.R4 = "NA",
    R4 = "NA",
    n.grapadora = "NA",
    n.celo = "NA",
    R4_Comb18_Comb19 = "NA",
    n.rest2 = "NA",
    Comb19 = "NA",
    Comb18_Comb19 = "NA",
    WW = ifelse(Color2 == "WW", 1, 0),
    WO = ifelse(Color2 == "WO", 1, 0),
    YO = ifelse(Color2 == "YO", 1, 0),
    WY = ifelse(Color2 == "WY", 1, 0),
    OO = ifelse(Color2 == "OO", 1, 0),
    YY = ifelse(Color2 == "YY", 1, 0)
  )

#????????????????????????????????????????????????????????????????????



  ##### CONTADOR DE PRUEBA #####

#contariamos <- 0
#n <- 0
#repeat{
#  if(contariamos ==20) {
#    break
#  }
#  else {
#    contariamos = contariamos+1
  #n <- n+1
  #message(paste0("At ", Sys.time()," iteration number ", n, " finished"))


##### PASO 2: Distribuci?n preliminar de tratamientos #####

# Convertir dist2 en dist, para evitar que en cada vuelta del bucle tenga que
# crear todas las columnas nuevamente.
    
dist <- dist2

Comb.19 <- c ("1_D", "4_D", "1_L", "1_M", "2_L", "2_M", "4_L", "4_M")

# Vectorized: Replace NA values in Comb19 with random samples
na_mask <- dist$Comb19 == "NA"
if (any(na_mask)) {
  dist$Comb19[na_mask] <- sample(Comb.19, sum(na_mask), replace = TRUE)
}

#????????????????????????????????????????????????????????????????????

##### PASO 3: N?mero m?ximo de individuos en cada tratamiento #####

# R5: Debe haber el mismo n?mero de individuos que van a cada tratamiento
#     desde cada tratamiento

#Antes para hacer este paso ten?a las dos partes dentro del mismo bucle
#ahora he hecho un bucle anidado, pero no s? que es m?s eficiente, si as?
# o al rev?s


repeat{
  
  # Ajustar el n?mero de individuos que va a cada Comb19 desde cada Comb18
  
  # Ir cambiando los que est?n mal
  for(i in seq_len(nrow(dist))){
    if (dist$R5[i]>dist$R5.1[i]){
      dist$R5.2[i] <- sample (c (0,0,1), 1) 
    } else {
      dist$R5.2[i] <- 0  
    }
  }
  
  # VECTORIZED REPLACEMENT
  dist$R5.2 <- ifelse(dist$R5 > dist$R5.1, 
                       sample(c(0, 0, 1), nrow(dist), replace = TRUE), 
                       0)
  
  for (i in seq_len(nrow(dist))) {
    if (dist$R5.2[i]==1) {
      dist$Comb19[i] <- sample (Comb.19, 1) 
      }
  }
  
  # VECTORIZED REPLACEMENT
  mask_to_update <- dist$R5.2 == 1
  if (any(mask_to_update)) {
    dist$Comb19[mask_to_update] <- sample(Comb.19, sum(mask_to_update), replace = TRUE)
  }
  
  # Eliminar todas las columnas, para que pueda calcularlas nuevamente en cada
  # vuelta del bucle
  
  dist <- dplyr::select (dist, -Comb18_Comb19, -R5, -R5.2)
  
  #Calcular R5 y R5.1
  
  R5 <- dplyr::count (dist, Comb18, Comb19) %>%
    dplyr::mutate(Comb18_Comb19 = paste(Comb18, Comb19, sep = "_")) %>%
    dplyr::select(Comb18_Comb19, n) %>%
    dplyr::rename(R5 = n) %>%
    dplyr::mutate(R5 = as.numeric(R5))
  
  dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
  dist <- merge (dist, R5, by="Comb18_Comb19")
  
  if(
    #R
  all(dist$R5[seq_len(nrow(dist))] <= dist$R5.1[seq_len(nrow(dist))])
  )
    
  {break}

R5
if (nrow(R5)>58){break}

R5
#????????????????????????????????????????????????????????????????????

##### Paso 5 ####

repeat {
#Mismo n?mero de individuos en todos los tratamientos
repeat {
  
  dist <- dplyr::select (dist, -n.C19, -n.C18.C19)
  dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
  
  tabla.R3 <- dplyr::count (dist, Comb19) %>%
    dplyr::mutate(n = as.numeric(n)) %>%
    dplyr::rename(n.C19 = n)

  R3 <- dplyr::count (dist, Comb18, Comb19) %>%
    dplyr::mutate(
      Comb18 = as.character(Comb18),
      n.C18.C19 = as.numeric(n)
    ) %>%
    dplyr::select(-n) %>%
    dplyr::left_join(tabla.R3, by = "Comb19") %>%
    dplyr::mutate(Comb18_Comb19 = paste(Comb18, Comb19, sep = "_"))

  C.C18 <- rep(NA, nrow(R3))
  
  for(i in seq_len(nrow(R3))){
    if ((R3$n.C19[i]>(nrow (dist)/C.19))&&
        (R3$n.C18.C19 [i]>=(ceiling (nrow(dist)/(C.18*C.19))))){   #Los que hay que cambiar
      C.C18[i] <-  R3$Comb18[i]
    } else {C.C18[i]<- NA
    }
  }
  
  C.C18 <- C.C18[is.na(C.C18) == F] 
  C.C18 <- unique (C.C18) #Los que hay que cambiar
  
  D.C18 <- 1:(nrow(R3))
  
  for(i in seq_len(nrow(R3))){
    if ((R3$n.C19[i]<(nrow (dist)/C.19))&&(R3$n.C18.C19 [i]>=(ceiling (nrow(dist)/(C.18*C.19))))){   #Los que hay que cambiar
      D.C18[i] <-  R3$Comb18[i]
    } else {D.C18[i]<- NA
    }
  }
  
  D.C18 <- D.C18[is.na(D.C18) == F] 
  D.C18 <- unique (D.C18) #Los que no se pueden cambiar
  
  D.C19 <- 1:(nrow(R3))
  
  for(i in seq_len(nrow(R3))){
    if ((R3$n.C19[i]<(nrow (dist)/C.19))&&(R3$n.C18.C19 [i]<=(floor (nrow(dist)/(C.18*C.19))))){   #Los que hay que cambiar
      D.C19[i] <-  R3$Comb19[i]
    } else {D.C19[i]<- NA
    }
  }
  
  D.C19 <- D.C19[is.na(D.C19) == F] #Por los que hay que cambiarlos
  D.C19 <- unique (D.C19)
  
  cambiables <- C.C18[!C.C18%in%D.C18]
  
  if ((length(cambiables)==0)){   
    cambiables <-  C.C18
  } 
  
  R3 <- dplyr::select (R3, -Comb18, -Comb19)
  
  dist <- merge (dist, R3, by="Comb18_Comb19")
  
  dist$ordenar <- sample(seq_len(nrow(dist)), replace=FALSE)
  dist <- dist[with(dist, order(dist$ordenar)), ] 
  
  cont =0
  for (i in seq_len(nrow(dist))){
    if(cont ==1) {
      break
    }
    else if((dist$n.C19[i]>ceiling(nrow (dist)/C.19))&&
            (dist$n.C18.C19[i]>=(ceiling (nrow(dist)/(C.18*C.19))))&&
            dist$Comb18[i] %in% cambiables){
      dist$Comb19[i] <- D.C19[1]
      cont = cont + 1
    }
  }
  
  rest <- dplyr::count (dist, Comb19)
  rest 
  
  if(
  (all(rest$n[seq_len(nrow(rest))] == (nrow(dist)/C.19)))
  )
    
  {break}
}
#Interacci?n Comb18_Comb19 tiene el mismo n?mero de individuos
repeat{
  
  dist <- dist %>% dplyr::select(-n.rest2) %>%
    dplyr::mutate(Comb18_Comb19 = paste(Comb18, Comb19, sep = "_"))
  rest2 <- dist %>% dplyr::count (Comb18_Comb19) %>% dplyr::rename(n.rest2 = n)
  dist <- dist %>% dplyr::left_join(rest2, by = "Comb18_Comb19") %>%
    dplyr::mutate(Comb18 = as.character(Comb18))
  
  # VECTORIZED: Replace conditional loop with ifelse
  mask_needed <- dist$n.rest2 < (dist$R5.1 - 1)
  necesitados <- ifelse(mask_needed, dist$Comb19, NA)
  nec <- ifelse(mask_needed, dist$Comb18, NA)
  
  necesitados <- na.omit(necesitados)
  necesitados <- unique (necesitados)
  necesitados
  nec <- na.omit (nec)
  nec <- unique (nec)
  nec
  
  cont =0
  for (i in seq_len(nrow(dist))){
    if(cont ==1) {
      break
    } 
    else if((dist$n.rest2[i]>=dist$R5.1[i])&&
            (length(necesitados)!=0)&&
            (dist$Comb18[i]==nec[1])){
      dist$Comb19[i] <- sample (necesitados, 1)
      cont = cont + 1
    }
    else if((dist$n.rest2[i]>dist$R5.1[i])&&
            (length(necesitados)==0)){
      dist$Comb19[i] <- sample (Comb.19, 1)
      cont = cont + 1
    }
  }
  
  dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")
  
  R5.3 <- dplyr::count (dist, Comb18, Comb19) %>%
    dplyr::rename(R5.3 = n) %>%
    dplyr::mutate(R5.3 = as.numeric(R5.3))
  
  R.Comb18 <- dplyr::count (dist, Comb18) %>%
    dplyr::mutate(balance = ceiling(n / C.19)) %>%
    dplyr::select(-n)
  
  R5.3 <- merge (R5.3, R.Comb18, by = "Comb18")
  R5.3
  
  if (
  all(R5.3$R5.3[seq_len(nrow(R5.3))] >= (R5.3$balance-1)) &&
  all(R5.3$R5.3[seq_len(nrow(R5.3))] <= R5.3$balance)
  )
    
  {break}
}
disco <- dplyr::count (dist, Comb19)

if (all(disco$n==8)){break}}

##### PASO 6: Permutaciones tratamientos de 2019 #####

dist <- dist %>%
  dplyr::arrange(Comb18) %>%
  dplyr::mutate(fila = row_number())

#dist <- dplyr::select (dist, -R5)

dist <- dist %>% dplyr::mutate(Comb18 = as.factor(Comb18))
guante <- levels (dist$Comb18)
guante

contar <- 0
contar.hasta <- length(guante)

repeat {
contar <- contar+1
  
R5 <- dplyr::count (dist, Encl18, Comb18, Comb19)

R5$Encl18 <- as.character (R5$Encl18)
R5$Comb18 <- as.character (R5$Comb18)

dist3 <- dplyr::select (dist, fila, Encl18, Comb18, Comb19)
frio <- dist3[dist3$Comb18 == guante[1], ] 
frio

guante <- guante [-c(1)]
guante 

n.dos <- dplyr::count (frio, Encl18)
n.max <- max (n.dos$n)
n.min <- min (n.dos$n)
min.max <- n.max+n.min

# Add iteration counter to prevent infinite loops
iter_count <- 0
iter_max <- 50  # Maximum iterations to prevent hanging (reduced for testing)

repeat{
  iter_count <- iter_count + 1
  if (iter_count > iter_max) {
    cat("\nINFO: Reached maximum iterations (", iter_max, "). Breaking out of repeat loop.\n")
    break
  }
  
  # Recalculate ind.Comb.Encl within the loop as dist changes
  ind.Comb.Encl <- dplyr::count(dist, Encl18, Comb18)
  
  frio <- dplyr::mutate (frio, Comb19.2 = sample(Comb19))
  #frio$Comb19.2 <- frio$Comb19 
  #frio <- frio[with(frio, order(frio$Comb19.2)), ]
  #R6.1 <- dplyr::count (frio, Encl18, Comb19)
  R6.1 <- dplyr::count (frio, Encl18, Comb19.2)
  R6.1$Comb18_Comb19 <- paste (frio$Comb18[1],  R6.1$Comb19.2, sep="_")
  R6 <- dplyr::count (R6.1, Encl18, n)
  colnames(R6)[colnames(R6)=="nn"] <- "duplicado"
  R6$gordi <- "NA"
  colnames(n.dos)[colnames(n.dos)=="n"] <- "n.ind"
  R6 <- merge (R6, n.dos, by="Encl18")
  
  for(i in seq_len(nrow(R6))){
    if ((R6$n[i]==1)&&(R6$n.ind[i]<=C.19)){   
      R6$gordi[i] <-  R6$n.ind[i]}
    else if ((R6$n[i]>=2)&&(R6$n.ind[i]<=C.19)){   
      R6$gordi[i] <-  0}
    
    else if ((R6$n[i]<floor(R6$n.ind[i]/C.19))&&(R6$n.ind[i]>C.19)){   
      R6$gordi[i] <-  0}
    else if ((R6$n[i]==floor(R6$n.ind[i]/C.19))&&(R6$n.ind[i]>C.19)){   
      R6$gordi[i] <-  C.19-(R6$n.ind[i]-(C.19*(floor (R6$n.ind[i]/C.19))))}
    else if ((R6$n[i]==ceiling(R6$n.ind[i]/C.19))&&(R6$n.ind[i]>C.19)){   
      R6$gordi[i] <- R6$n.ind[i]-(C.19*(floor (R6$n.ind[i]/C.19)))}
    else if ((R6$n[i]>floor(R6$n.ind[i]/C.19))&&(R6$n.ind[i]>C.19)){   
      R6$gordi[i] <-  0}
  }
  
  R6$ok <- "NA"
  
  for(i in seq_len(nrow(R6))){
    if (R6$duplicado[i]==R6$gordi[i]){   
      R6$ok[i] <-  0
    } else {   
      R6$ok[i] <-  1}
  }
  R6$ok <- as.numeric (R6$ok)
  R6
  R6.1$ok <- 0
  
  for(i in seq_len(nrow(R6.1))){
    if (R6.1$Comb18_Comb19[i] %in% R1.2){   
      R6.1$ok[i] <-  1
    } else {   
      R6.1$ok[i] <-  0}
  }
  R6.1$ok <- as.numeric (R6.1$ok)
  
  R6.1
  
  for(i in seq_len(nrow(R6.1))){
    if ((R6.1$ok[i]==1)&&
        (R6.1$n[i]>1)&&
        ((n.min<Encl.19-1)|
         (n.max<Encl.19-1))){   
      R6.1$ok[i] <-  1
    } else {   
      R6.1$ok[i] <-  0}
  }
  R6.1$ok <- as.numeric (R6.1$ok)
  
  frio$Comb18_Comb19 <- paste (frio$Comb18,  frio$Comb19, sep="_")
  R6.1.prueba <- dplyr::count (frio, Comb18_Comb19)
  colnames(R6.1.prueba)[colnames(R6.1.prueba)=="n"] <- "n.max"
  R6.1 <- merge (R6.1, R6.1.prueba, by="Comb18_Comb19")
  
  for(i in seq_len(nrow(R6.1))){
    if ((R6.1$ok[i]==1)&&
        (R6.1$n[i]<=R6.1$n.max[i]-1)){   
      R6.1$ok[i] <-  0  #### ANTES PON?A ELSE IF 0, ES DECIR TODO CEROS
    } 
  }
  
  R6.1 <- dplyr::select (R6.1, -n.max)
  R6.1$ok <- as.numeric (R6.1$ok)
  
  R6.1.1 <- dplyr::mutate (R6.1, fila=row_number())
  
  for (i in seq_len(nrow(R6.1.1))) {
    if     ((R6.1.1$Comb18_Comb19[i] %in% R1.1)&&
            (R6.1.1$n[i]==2))
    {R6.1.1$ok[i] <- 1}
    else 
    {R6.1.1$ok[i] <- 0}
  }
  
  #-
  papel <- dplyr::count (dist, Comb18, Comb19)
  papel$Comb18_Comb19 <- paste (papel$Comb18,  papel$Comb19, sep="_")
  
  merge3 <- merge (R1.C18.C19, R6.1.1, by="Comb18_Comb19")
  if (nrow(merge3)==0){
    merge3 <- R6.1.1
    merge3$ok2 <- 0
  }
  
  for(i in seq_len(nrow(merge3))){
  for (j in seq_len(nrow(ind.Comb.Encl))){
       if ((merge3$Encl18[i]==ind.Comb.Encl$Encl18[j])){   
        merge3$nuevo[i] <-  ind.Comb.Encl$n[j]/C.19
      }
    }
  }
  for(i in seq_len(nrow(merge3))){
  for (j in seq_len(nrow(papel))){
      if ((merge3$Comb18_Comb19[i]==papel$Comb18_Comb19[j])){   
        merge3$nuevo2[i] <-  floor(papel$n[j]/(Encl.19/C.19))
      }
    }
  }
  
  
  for (i in seq_len(nrow(merge3))) {
    if (
      (merge3$ok[i] == 1)&&
      (merge3$Encl[i] == merge3$Encl18[i])&&
      (((merge3$n[i] > merge3$nuevo[i])&&
        (merge3$n[i] < merge3$nuevo2[i]))|
      ((merge3$n[i] > merge3$nuevo[i])&&
      (merge3$n[i] != merge3$nuevo2[i])))  ### POSIBLE FUENTE DE FALLOS
      )
    {merge3$ok2[i] <- 1}
    else 
    {merge3$ok2[i] <- 0}
  }
  merge3
  merge4 <- dplyr::select (merge3, ok2, fila)
  merge5 <- merge (R6.1.1, merge4, by="fila", all=TRUE)
  merge5$ok2[is.na(merge5$ok2)] <- 0  
  
  R6
  R6.1
  merge5
  #-
  
  if ((sum (R6$ok)==0)&&
    (sum (R6.1$ok)==0)&&
    (sum (merge5$ok2)==0)
    )
  {break}
}



correcto <- dplyr::select (frio, fila, Comb19.2)
dist <- merge (dist, correcto, by ="fila", all=TRUE)
dist$Comb19.2[is.na(dist$Comb19.2)] <- 0

for(i in seq_len(nrow(dist))){
  if ((dist$Comb19.2[i]=="0")){   
    dist$Comb19.2[i] <-  dist$Comb19[i]
  }
}

dist <- dplyr::select (dist, -Comb19)
colnames (dist) [colnames(dist)=="Comb19.2"] <- "Comb19"
dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")


R6
R6.1
merge5
contar

if (contar==contar.hasta)

{break}
}



R5 <- dplyr::count (dist, Encl18, Comb18, Comb19)
R7 <- dplyr::count (R5, n)
R7






  ##### CONTADOR repeticiones 1: cambia distribuci?n tratamientos 2018 #####

#contador = 0
#repeat{
#  if(contador ==1) {
#    break
#  }
#  else {
#    contador = contador+1


##### Paso 8: Permutaciones individuos entre tratamientos #####


dist <- dist %>% dplyr::arrange(Encl18)

#dist <- dplyr::select (dist, -R5)

#repeat{

dist <- dist %>% dplyr::mutate(Encl18 = as.factor(Encl18))
levels.Encl18 <- levels(dist$Encl18)
levels.Encl18

contar <- 0
contar.hasta <- length(levels.Encl18)

repeat {
  contar <- contar+1
  
  dist <- dist %>% dplyr::arrange(fila)
  dist4 <- dplyr::select (dist, fila, Encl18, Comb19)
  
  nose <- dist4[dist4$Encl18 == levels.Encl18[1], ]  
  
  nose <- nose %>% dplyr::mutate(Encl18 = as.character(Encl18),
                                 Comb19 = as.character(Comb19))
  
  levels.Encl18 <- levels.Encl18 [-c(1)]
  levels.Encl18
  
  nose <- nose %>% dplyr::mutate(Comb19.2 = sample(Comb19))
  nose
  
  si <- dplyr::select (nose, fila, Comb19.2)
  dist <- merge (dist, si, by ="fila", all=TRUE)
  dist$Comb19.2[is.na(dist$Comb19.2)] <- 0
  
  for(i in seq_len(nrow(dist))){
    if ((dist$Comb19.2[i]=="0")){   
      dist$Comb19.2[i] <-  dist$Comb19[i]
    }
  }
  
  dist <- dplyr::select (dist, -Comb19)
  colnames (dist) [colnames(dist)=="Comb19.2"] <- "Comb19"
  dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
  
  if (contar==contar.hasta)
    
  {break}
}

##### Color y morfos #####

  ##### Arreglar los morfos #####


repeat {

print ("W")  
vector.morfos <- c("W")
vector.morfos.original <- vector.morfos

cascos <- 100

repeat{
  
  morfos <- dist %>%
    dplyr::group_by (Comb19) %>%
    dplyr::summarise (sum(O), sum(Y), sum(W))
  colnames.morfos <- c ("Comb19", "O", "Y", "W")
  colnames(morfos) <- colnames.morfos
  morfos
  
  morfos1 <- morfos
  morfos1 <- dplyr::select (morfos1, -Comb19) 
  ok <- morfos1
  
  for (i in seq_len(nrow(morfos1))){
  for (j in seq_len(ncol(morfos1))){
      if ((morfos1[i,j]>=floor(sum(morfos1[j])/C.19))&
          (morfos1[i,j]<=ceiling(sum(morfos1[j])/C.19))){
        ok[i,j] <- 0
      } else {
        ok[i,j] <- 1
      }
    }
  }
  
  ok$suma <- rowSums(ok)
  
  mas <- seq_len(nrow(morfos))
  mas[seq_len(nrow(morfos))] <- NA
  for(i in seq_len(nrow(morfos))){
    if (any(morfos[ ,vector.morfos[1]]>ceiling(sum(dist[vector.morfos[1]])/C.19))&
        (morfos[i,vector.morfos[1]]>floor(sum(dist[vector.morfos[1]])/C.19))){
      mas[i] <- morfos$Comb19[i]
    } else if (all(morfos[ ,vector.morfos[1]]<=ceiling(sum(dist[vector.morfos[1]])/C.19))&
               (morfos[i,vector.morfos[1]]>floor(sum(dist[vector.morfos[1]])/C.19))){
      mas[i] <- morfos$Comb19[i]
    }
  }
  
  mas <- na.omit (mas)
  mas <- sample (mas)
  morfos
  mas
  
  menos <- seq_len(nrow(morfos))
  for(i in seq_len(nrow(morfos))){
    if (any(morfos[, vector.morfos[1]]<floor(sum(dist[vector.morfos[1]])/C.19))&
        (morfos[i, vector.morfos[1]]<floor(sum(dist[vector.morfos[1]])/C.19))){
      menos[i] <- morfos$Comb19[i]
    } else if (all(morfos[, vector.morfos[1]]>=floor(sum(dist[vector.morfos[1]])/C.19))&
               (morfos[i, vector.morfos[1]]==floor(sum(dist[vector.morfos[1]])/C.19))){
      menos[i] <- morfos$Comb19[i]
    } else {
      menos[i] <- NA
    }
  }
  
  menos <- na.omit(menos)
  menos <- sample (menos, replace=FALSE)
  menos
  
  okis2 <- as.vector (rbind (mas[1], menos[1]))
  okis2
  
  luz <- morfos$Comb19
  luz <- luz[!luz%in%okis2]
  okis <- luz

  
  dist.morfos <- dist
  repeat{
    if (length(okis)==0){
      break
    } else {
      dist.morfos <- dist.morfos[dist.morfos$Comb19 != okis [1], ] 
      okis <- okis[-c(1)]
    }
  }
  dplyr::count (dist.morfos, Comb19)
  
  ok.original <- seq_len(nrow(morfos))
  for (i in seq_len(nrow(morfos))){
  for (j in seq_len(ncol(morfos))){
      if ((morfos[i,vector.morfos[1]]>=floor(sum(morfos[vector.morfos[1]])/C.19))&
          (morfos[i,vector.morfos[1]]<=ceiling(sum(morfos[vector.morfos[1]])/C.19))){
        ok.original[i] <- 0
      } else {
        ok.original[i] <- 1
      }
    }
  }
  ok.original
  
  dist.morfos.original <- dist.morfos
  
  mando <- 0  
  repeat {
    
  dist.morfos4 <- dplyr::select (dist.morfos, fila, Encl18, Comb19)
  
  dist.morfos4 <- dist.morfos4 %>%
    dplyr::group_by (Encl18)
  dist.morfos4 <- dplyr::mutate (dist.morfos4, Comb19.2 = sample(Comb19))
  
  dist.morfos$Comb19.2 <- dist.morfos4$Comb19.2

  dist.morfos <- dplyr::select (dist.morfos, -Comb19)
  colnames (dist.morfos) [colnames(dist.morfos)=="Comb19.2"] <- "Comb19"
      
  colnames (dist.morfos) [colnames(dist.morfos)=="Comb19"] <- "Comb19.3"
  Comb19.3 <- dplyr::select (dist.morfos, fila, Comb19.3)
  dist <- merge (dist, Comb19.3, by="fila", all=TRUE)
  dist$Comb19.3[is.na(dist$Comb19.3)] <- 0  
  colnames (dist.morfos) [colnames(dist.morfos)=="Comb19.3"] <- "Comb19"
      
  for(i in seq_len(nrow(dist))){
    if ((dist$Comb19.3[i]=="0")){   
      dist$Comb19.3[i] <-  dist$Comb19[i]
    }
  }
  dist <- dplyr::select (dist, -Comb19)
  colnames (dist) [colnames(dist)=="Comb19.3"] <- "Comb19"
  dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
      
  color <- dist %>%
    dplyr::group_by (Comb19) %>%
    dplyr::summarise (sum(WY), sum(WO), sum(WW), sum(YY), sum(YO), sum(OO))
  colnames.color <- c ("Comb19", "WY", "WO", "WW", "YY", "YO", "OO")
  colnames(color) <- colnames.color
  color
    
  color1 <- color
  color1 <- dplyr::select (color1, -Comb19) 
  ok.color <- color1
    
  for (i in seq_len(nrow(color1))){
  for (j in seq_len(ncol(color1))){
        if (color1[i,j]<floor(sum(color1[j])/C.19)){
          ok.color[i,j] <- 1
        } else if (color1[i,j]<(floor(sum(color1[j])/C.19)-1)){
          ok.color[i,j] <- 2
        } else if (color1[i,j]>ceiling(sum(color1[j])/C.19)){
          ok.color[i,j] <- 2
        } else {
          ok.color[i,j] <- 0
        }
      }
    }
    ok.color
    
    if (sum(ok.color)>30) {
      
      dist.morfos <- dist.morfos.original
      colnames (dist.morfos) [colnames(dist.morfos)=="Comb19"] <- "Comb19.3"
      Comb19.3 <- dplyr::select (dist.morfos, fila, Comb19.3)
      dist <- merge (dist, Comb19.3, by="fila", all=TRUE)
      dist$Comb19.3[is.na(dist$Comb19.3)] <- 0  
      colnames (dist.morfos) [colnames(dist.morfos)=="Comb19.3"] <- "Comb19"
      
  for(i in seq_len(nrow(dist))){
        if ((dist$Comb19.3[i]=="0")){   
          dist$Comb19.3[i] <-  dist$Comb19[i]
        }
      }
      
      dist <- dplyr::select (dist, -Comb19)
      colnames (dist) [colnames(dist)=="Comb19.3"] <- "Comb19"
      dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
      
    }
    
    morfos <- dist %>%
      dplyr::group_by (Comb19) %>%
      dplyr::summarise (sum(O), sum(Y), sum(W))
    colnames.morfos <- c ("Comb19", "O", "Y", "W")
    colnames(morfos) <- colnames.morfos
    morfos
    
  ok <- seq_len(nrow(morfos))
  for (i in seq_len(nrow(morfos))){
  for (j in seq_len(ncol(morfos))){
        if ((morfos[i,vector.morfos[1]]>=floor(sum(morfos[vector.morfos[1]])/C.19))&
            (morfos[i,vector.morfos[1]]<=ceiling(sum(morfos[vector.morfos[1]])/C.19))){
          ok[i] <- 0
        } else {
          ok[i] <- 1
        }
      }
    }
    ok
    
    mando  <- mando+1
    print (mando)
    
    if ((sum(ok)<sum(ok.original))|
        (mando==10))
    {break}
  }
  
  cascos <- cascos+1
  print (cascos)
  
  if ((sum(ok)==0)|
      (cascos==110))
  {break}
}

morfos1 <- morfos
morfos1 <- dplyr::select (morfos1, -Comb19) 
ok.morfos <- morfos1

for (i in seq_len(nrow(morfos1))){
  for (j in seq_len(ncol(morfos1))){
    if (morfos1[i,j]<floor(sum(morfos1[j])/C.19)){
      ok.morfos[i,j] <- 1
    } else if (morfos1[i,j]>ceiling(sum(morfos1[j])/C.19)){
      ok.morfos[i,j] <- 2
    } else {
      ok.morfos[i,j] <- 0
    }
  }
}
ok.morfos

if (sum(ok.morfos)>0){

print ("O")
vector.morfos <- c("O")
vector.morfos.original <- vector.morfos

cascos <- 100

repeat{
  
  morfos <- dist %>%
    dplyr::group_by (Comb19) %>%
    dplyr::summarise (sum(O), sum(Y), sum(W))
  colnames.morfos <- c ("Comb19", "O", "Y", "W")
  colnames(morfos) <- colnames.morfos
  morfos
  
  morfos1 <- morfos
  morfos1 <- dplyr::select (morfos1, -Comb19) 
  ok <- morfos1
  
  for (i in seq_len(nrow(morfos1))){
  for (j in seq_len(ncol(morfos1))){
      if ((morfos1[i,j]>=floor(sum(morfos1[j])/C.19))&
          (morfos1[i,j]<=ceiling(sum(morfos1[j])/C.19))){
        ok[i,j] <- 0
      } else {
        ok[i,j] <- 1
      }
    }
  }
  
  ok$suma <- rowSums(ok)
  
  mas <- seq_len(nrow(morfos))
  mas[seq_len(nrow(morfos))] <- NA
  for(i in seq_len(nrow(morfos))){
    if (any(morfos[ ,vector.morfos[1]]>ceiling(sum(dist[vector.morfos[1]])/C.19))&
        (morfos[i,vector.morfos[1]]>floor(sum(dist[vector.morfos[1]])/C.19))){
      mas[i] <- morfos$Comb19[i]
    } else if (all(morfos[ ,vector.morfos[1]]<=ceiling(sum(dist[vector.morfos[1]])/C.19))&
               (morfos[i,vector.morfos[1]]>floor(sum(dist[vector.morfos[1]])/C.19))){
      mas[i] <- morfos$Comb19[i]
    }
  }
  
  mas <- na.omit (mas)
  mas <- sample (mas)
  morfos
  mas
  
  menos <- seq_len(nrow(morfos))
  for(i in seq_len(nrow(morfos))){
    if (any(morfos[, vector.morfos[1]]<floor(sum(dist[vector.morfos[1]])/C.19))&
        (morfos[i, vector.morfos[1]]<floor(sum(dist[vector.morfos[1]])/C.19))){
      menos[i] <- morfos$Comb19[i]
    } else if (all(morfos[, vector.morfos[1]]>=floor(sum(dist[vector.morfos[1]])/C.19))&
               (morfos[i, vector.morfos[1]]==floor(sum(dist[vector.morfos[1]])/C.19))){
      menos[i] <- morfos$Comb19[i]
    } else {
      menos[i] <- NA
    }
  }
  
  menos <- na.omit(menos)
  menos <- sample (menos, replace=FALSE)
  menos
  
  okis2 <- as.vector (rbind (mas[1], menos[1]))
  okis2
  
  luz <- morfos$Comb19
  luz <- luz[!luz%in%okis2]
  okis <- luz
  
  
  dist.morfos <- dist
  repeat{
    if (length(okis)==0){
      break
    } else {
      dist.morfos <- dist.morfos[dist.morfos$Comb19 != okis [1], ] 
      okis <- okis[-c(1)]
    }
  }
  dplyr::count (dist.morfos, Comb19)
  
  ok.original <- seq_len(nrow(morfos))
  for (i in seq_len(nrow(morfos))){
  for (j in seq_len(ncol(morfos))){
      if ((morfos[i,vector.morfos[1]]>=floor(sum(morfos[vector.morfos[1]])/C.19))&
          (morfos[i,vector.morfos[1]]<=ceiling(sum(morfos[vector.morfos[1]])/C.19))){
        ok.original[i] <- 0
      } else {
        ok.original[i] <- 1
      }
    }
  }
  ok.original
  
  dist.morfos.original <- dist.morfos
  
  mando <- 0  
  
  repeat {
    
    dist.morfos4 <- dplyr::select (dist.morfos, fila, Encl18, Comb19)
    
    dist.morfos4 <- dist.morfos4 %>%
      dplyr::group_by (Encl18)
    dist.morfos4 <- dplyr::mutate (dist.morfos4, Comb19.2 = sample(Comb19))
    
    dist.morfos$Comb19.2 <- dist.morfos4$Comb19.2
    
    dist.morfos <- dplyr::select (dist.morfos, -Comb19)
    colnames (dist.morfos) [colnames(dist.morfos)=="Comb19.2"] <- "Comb19"
    
    colnames (dist.morfos) [colnames(dist.morfos)=="Comb19"] <- "Comb19.3"
    Comb19.3 <- dplyr::select (dist.morfos, fila, Comb19.3)
    dist <- merge (dist, Comb19.3, by="fila", all=TRUE)
    dist$Comb19.3[is.na(dist$Comb19.3)] <- 0  
    colnames (dist.morfos) [colnames(dist.morfos)=="Comb19.3"] <- "Comb19"
    
  for(i in seq_len(nrow(dist))){
      if ((dist$Comb19.3[i]=="0")){   
        dist$Comb19.3[i] <-  dist$Comb19[i]
      }
    }
    dist <- dplyr::select (dist, -Comb19)
    colnames (dist) [colnames(dist)=="Comb19.3"] <- "Comb19"
    dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
    
    color <- dist %>%
      dplyr::group_by (Comb19) %>%
      dplyr::summarise (sum(WY), sum(WO), sum(WW), sum(YY), sum(YO), sum(OO))
    colnames.color <- c ("Comb19", "WY", "WO", "WW", "YY", "YO", "OO")
    colnames(color) <- colnames.color
    color
    
    color1 <- color
    color1 <- dplyr::select (color1, -Comb19) 
    ok.color <- color1
    
  for (i in seq_len(nrow(color1))){
  for (j in seq_len(ncol(color1))){
        if (color1[i,j]<floor(sum(color1[j])/C.19)){
          ok.color[i,j] <- 1
        } else if (color1[i,j]>ceiling(sum(color1[j])/C.19)){
          ok.color[i,j] <- 2
        } else {
          ok.color[i,j] <- 0
        }
      }
    }
    ok.color
    
    if (sum(ok.color)>30) {
      
      dist.morfos <- dist.morfos.original
      colnames (dist.morfos) [colnames(dist.morfos)=="Comb19"] <- "Comb19.3"
      Comb19.3 <- dplyr::select (dist.morfos, fila, Comb19.3)
      dist <- merge (dist, Comb19.3, by="fila", all=TRUE)
      dist$Comb19.3[is.na(dist$Comb19.3)] <- 0  
      colnames (dist.morfos) [colnames(dist.morfos)=="Comb19.3"] <- "Comb19"
      
  for(i in seq_len(nrow(dist))){
        if ((dist$Comb19.3[i]=="0")){   
          dist$Comb19.3[i] <-  dist$Comb19[i]
        }
      }
      
      dist <- dplyr::select (dist, -Comb19)
      colnames (dist) [colnames(dist)=="Comb19.3"] <- "Comb19"
      dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
      
    }
    
    morfos <- dist %>%
      dplyr::group_by (Comb19) %>%
      dplyr::summarise (sum(O), sum(Y), sum(W))
    colnames.morfos <- c ("Comb19", "O", "Y", "W")
    colnames(morfos) <- colnames.morfos
    morfos
    
  ok <- seq_len(nrow(morfos))
  for (i in seq_len(nrow(morfos))){
  for (j in seq_len(ncol(morfos))){
        if ((morfos[i,vector.morfos[1]]>=floor(sum(morfos[vector.morfos[1]])/C.19))&
            (morfos[i,vector.morfos[1]]<=ceiling(sum(morfos[vector.morfos[1]])/C.19))){
          ok[i] <- 0
        } else {
          ok[i] <- 1
        }
      }
    }
    ok
    
    mando  <- mando+1
    print (mando)
    
    if ((sum(ok)<sum(ok.original))|
        (mando==10))
    {break}
  }
  
  cascos <- cascos+1
  print (cascos)
  
  if ((sum(ok)==0)|
      (cascos==110))
  {break}
}

}

morfos1 <- morfos
morfos1 <- dplyr::select (morfos1, -Comb19) 
ok.morfos <- morfos1

for (i in seq_len(nrow(morfos1))){
  for (j in seq_len(ncol(morfos1))){
    if (morfos1[i,j]<floor(sum(morfos1[j])/C.19)){
      ok.morfos[i,j] <- 1
    } else if (morfos1[i,j]>ceiling(sum(morfos1[j])/C.19)){
      ok.morfos[i,j] <- 2
    } else {
      ok.morfos[i,j] <- 0
    }
  }
}
ok.morfos

if (sum(ok.morfos)>0){

print ("Y")
vector.morfos <- c("Y")
vector.morfos.original <- vector.morfos

cascos <- 100

repeat{
  
  morfos <- dist %>%
    dplyr::group_by (Comb19) %>%
    dplyr::summarise (sum(O), sum(Y), sum(W))
  colnames.morfos <- c ("Comb19", "O", "Y", "W")
  colnames(morfos) <- colnames.morfos
  morfos
  
  morfos1 <- morfos
  morfos1 <- dplyr::select (morfos1, -Comb19) 
  ok <- morfos1
  
  for (i in seq_len(nrow(morfos1))){
  for (j in seq_len(ncol(morfos1))){
      if ((morfos1[i,j]>=floor(sum(morfos1[j])/C.19))&
          (morfos1[i,j]<=ceiling(sum(morfos1[j])/C.19))){
        ok[i,j] <- 0
      } else {
        ok[i,j] <- 1
      }
    }
  }
  
  ok$suma <- rowSums(ok)
  
  mas <- seq_len(nrow(morfos))
  mas[seq_len(nrow(morfos))] <- NA
  for(i in seq_len(nrow(morfos))){
    if (any(morfos[ ,vector.morfos[1]]>ceiling(sum(dist[vector.morfos[1]])/C.19))&
        (morfos[i,vector.morfos[1]]>floor(sum(dist[vector.morfos[1]])/C.19))){
      mas[i] <- morfos$Comb19[i]
    } else if (all(morfos[ ,vector.morfos[1]]<=ceiling(sum(dist[vector.morfos[1]])/C.19))&
               (morfos[i,vector.morfos[1]]>floor(sum(dist[vector.morfos[1]])/C.19))){
      mas[i] <- morfos$Comb19[i]
    }
  }
  
  mas <- na.omit (mas)
  mas <- sample (mas)
  morfos
  mas
  
  menos <- seq_len(nrow(morfos))
  for(i in seq_len(nrow(morfos))){
    if (any(morfos[, vector.morfos[1]]<floor(sum(dist[vector.morfos[1]])/C.19))&
        (morfos[i, vector.morfos[1]]<floor(sum(dist[vector.morfos[1]])/C.19))){
      menos[i] <- morfos$Comb19[i]
    } else if (all(morfos[, vector.morfos[1]]>=floor(sum(dist[vector.morfos[1]])/C.19))&
               (morfos[i, vector.morfos[1]]==floor(sum(dist[vector.morfos[1]])/C.19))){
      menos[i] <- morfos$Comb19[i]
    } else {
      menos[i] <- NA
    }
  }
  
  menos <- na.omit(menos)
  menos <- sample (menos, replace=FALSE)
  menos
  
  okis2 <- as.vector (rbind (mas[1], menos[1]))
  okis2
  
  luz <- morfos$Comb19
  luz <- luz[!luz%in%okis2]
  okis <- luz
  
  
  dist.morfos <- dist
  repeat{
    if (length(okis)==0){
      break
    } else {
      dist.morfos <- dist.morfos[dist.morfos$Comb19 != okis [1], ] 
      okis <- okis[-c(1)]
    }
  }
  dplyr::count (dist.morfos, Comb19)
  
  ok.original <- seq_len(nrow(morfos))
  for (i in seq_len(nrow(morfos))){
  for (j in seq_len(ncol(morfos))){
      if ((morfos[i,vector.morfos[1]]>=floor(sum(morfos[vector.morfos[1]])/C.19))&
          (morfos[i,vector.morfos[1]]<=ceiling(sum(morfos[vector.morfos[1]])/C.19))){
        ok.original[i] <- 0
      } else {
        ok.original[i] <- 1
      }
    }
  }
  ok.original
  
  dist.morfos.original <- dist.morfos
  
  mando <- 0  
  
  repeat {
    
    dist.morfos4 <- dplyr::select (dist.morfos, fila, Encl18, Comb19)
    
    dist.morfos4 <- dist.morfos4 %>%
      dplyr::group_by (Encl18)
    dist.morfos4 <- dplyr::mutate (dist.morfos4, Comb19.2 = sample(Comb19))
    
    dist.morfos$Comb19.2 <- dist.morfos4$Comb19.2
    
    dist.morfos <- dplyr::select (dist.morfos, -Comb19)
    colnames (dist.morfos) [colnames(dist.morfos)=="Comb19.2"] <- "Comb19"
    
    colnames (dist.morfos) [colnames(dist.morfos)=="Comb19"] <- "Comb19.3"
    Comb19.3 <- dplyr::select (dist.morfos, fila, Comb19.3)
    dist <- merge (dist, Comb19.3, by="fila", all=TRUE)
    dist$Comb19.3[is.na(dist$Comb19.3)] <- 0  
    colnames (dist.morfos) [colnames(dist.morfos)=="Comb19.3"] <- "Comb19"
    
    for(i in seq_len(nrow(dist))){
      if ((dist$Comb19.3[i]=="0")){   
        dist$Comb19.3[i] <-  dist$Comb19[i]
      }
    }
    dist <- dplyr::select (dist, -Comb19)
    colnames (dist) [colnames(dist)=="Comb19.3"] <- "Comb19"
    dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
    
    color <- dist %>%
      dplyr::group_by (Comb19) %>%
      dplyr::summarise (sum(WY), sum(WO), sum(WW), sum(YY), sum(YO), sum(OO))
    colnames.color <- c ("Comb19", "WY", "WO", "WW", "YY", "YO", "OO")
    colnames(color) <- colnames.color
    color
    
    color1 <- color
    color1 <- dplyr::select (color1, -Comb19) 
    ok.color <- color1
    
    for (i in 1:nrow(color1)){
  for (j in seq_len(ncol(color1))){
        if (color1[i,j]<floor(sum(color1[j])/C.19)){
          ok.color[i,j] <- 1
        } else if (color1[i,j]>ceiling(sum(color1[j])/C.19)){
          ok.color[i,j] <- 2
        } else {
          ok.color[i,j] <- 0
        }
      }
    }
    ok.color
    
    if (sum(ok.color)>30) {
      
      dist.morfos <- dist.morfos.original
      colnames (dist.morfos) [colnames(dist.morfos)=="Comb19"] <- "Comb19.3"
      Comb19.3 <- dplyr::select (dist.morfos, fila, Comb19.3)
      dist <- merge (dist, Comb19.3, by="fila", all=TRUE)
      dist$Comb19.3[is.na(dist$Comb19.3)] <- 0  
      colnames (dist.morfos) [colnames(dist.morfos)=="Comb19.3"] <- "Comb19"
      
      for(i in seq_len(nrow(dist))){
        if ((dist$Comb19.3[i]=="0")){   
          dist$Comb19.3[i] <-  dist$Comb19[i]
        }
      }
      
      dist <- dplyr::select (dist, -Comb19)
      colnames (dist) [colnames(dist)=="Comb19.3"] <- "Comb19"
      dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
      
    }
    
    morfos <- dist %>%
      dplyr::group_by (Comb19) %>%
      dplyr::summarise (sum(O), sum(Y), sum(W))
    colnames.morfos <- c ("Comb19", "O", "Y", "W")
    colnames(morfos) <- colnames.morfos
    morfos
    
    ok <- seq_len(nrow(morfos))
    for (i in seq_len(nrow(morfos))){
      for (j in seq_len(ncol(morfos))){
        if ((morfos[i,vector.morfos[1]]>=floor(sum(morfos[vector.morfos[1]])/C.19))&
            (morfos[i,vector.morfos[1]]<=ceiling(sum(morfos[vector.morfos[1]])/C.19))){
          ok[i] <- 0
        } else {
          ok[i] <- 1
        }
      }
    }
    ok
    
    mando  <- mando+1
    print (mando)
    
    if ((sum(ok)<sum(ok.original))|
        (mando==10))
    {break}
  }
  
  cascos <- cascos+1
  print (cascos)
  
  if ((sum(ok)==0)|
      (cascos==110))
  {break}
}

}

morfos1 <- morfos
morfos1 <- dplyr::select (morfos1, -Comb19) 
ok.morfos <- morfos1

for (i in 1:nrow(morfos1)){
  for (j in 1:ncol (morfos1)){
    if (morfos1[i,j]<floor(sum(morfos1[j])/C.19)){
      ok.morfos[i,j] <- 1
    } else if (morfos1[i,j]>ceiling(sum(morfos1[j])/C.19)){
      ok.morfos[i,j] <- 2
    } else {
      ok.morfos[i,j] <- 0
    }
  }
}
ok.morfos


if (sum(ok.morfos)==0) 
{break}
}

morfos <- dist %>%
  dplyr::group_by (Comb19) %>%
  dplyr::summarise (sum(O), sum(Y), sum(W))
colnames.morfos <- c ("Comb19", "O", "Y", "W")
colnames(morfos) <- colnames.morfos
morfos

color <- dist %>%
  dplyr::group_by (Comb19) %>%
  dplyr::summarise (sum(WY), sum(WO), sum(WW), sum(YY), sum(YO), sum(OO))
colnames.color <- c ("Comb19", "WY", "WO", "WW", "YY", "YO", "OO")
colnames(color) <- colnames.color
color








chorlito <- dist

##### MEDIA ####

media <- dist %>%
  dplyr::group_by (Comb19) %>%
  dplyr::summarise (mean.SVL = mean(SVL), 
                    var.SVL = var(SVL),
                    mean.BC = mean(BodyConditionR), 
                    var.BC = var(BodyConditionR),
                    mean.TL = mean(TailLength), 
                    var.TL = var(TailLength)) %>%
  dplyr::mutate(dplyr::across(mean.SVL:var.TL, ~ round(., 2)))
media
plot (vector)


##### p.valor #####

p.valor <- 0.15

cont <- 0  

repeat{
  
  dist.2 <- dist
  
  vuelta.svl.1 <- 100
  
  repeat{
    
    dist <- dist.2
    
    yuyu <- sample ((unique (dist$Comb19)), 1)
    yuyu
    
    repeat {
      yaya <- sample ((unique (dist$Comb19)), 1)
      yaya
      if (yuyu[1]!=yaya[1]){
        break}
    }
    yaya
    
    dist.prueba <- subset (dist, Comb19 == yuyu[1])
    dist.prueba1 <- subset (dist, Comb19 == yaya[1] )
    dist.prueba <- rbind (dist.prueba, dist.prueba1)
    
    ejemplo <- dist.prueba %>%
      dplyr::group_by (Encl18, Color2, add=TRUE)
    ejemplo
    
    vuelta.svl <- 0
    
    repeat{
      
      dist <- dist.2
      
      media <- dist %>%
        dplyr::group_by (Comb19) %>%
        dplyr::summarise (mean.SVL = mean(SVL), 
                          var.SVL = var(SVL),
                          mean.BC = mean(BodyConditionR), 
                          var.BC = var(BodyConditionR),
                          mean.TL = mean(TailLength), 
                          var.TL = var(TailLength)) %>%
        dplyr::mutate(dplyr::across(mean.SVL:var.TL, ~ round(., 2)))
      media
      
      ejemplo <- dplyr::mutate (ejemplo, Comb19.2 = sample(Comb19))
      
      dist.prueba$Comb19.2 <- ejemplo$Comb19.2
      si <- dplyr::select (dist.prueba, fila, Comb19.2)
      
      dist <- merge (dist, si, by ="fila", all=TRUE)
      dist$Comb19.2[is.na(dist$Comb19.2)] <- 0
      
      for(i in 1:nrow(dist)){
        if ((dist$Comb19.2[i]=="0")){   
          dist$Comb19.2[i] <-  dist$Comb19[i]
        }
      }
      
      dist <- dplyr::select (dist, -Comb19)
      colnames (dist) [colnames(dist)=="Comb19.2"] <- "Comb19"
      
      media2 <- dist %>%
        dplyr::group_by (Comb19) %>%
        dplyr::summarise (mean.SVL = mean(SVL), 
                          var.SVL = var(SVL),
                          mean.BC = mean(BodyConditionR), 
                          var.BC = var(BodyConditionR),
                          mean.TL = mean(TailLength), 
                          var.TL = var(TailLength)) %>%
        dplyr::mutate(dplyr::across(mean.SVL:var.TL, ~ round(., 2)))
      
      media2[,-1] <- round (media2[, -1], 2)
      media2    
      
      ##### P.valor. dist (cambiado)
      
      dist <- merge (dist, trat, by="Comb19")
      dist <- dist[!duplicated(dist), ]
      
      
      dist.exp.1 <- dist %>%
        dplyr::filter(Comb19 != "1_D", Comb19 != "4_D") %>%
        dplyr::mutate(
          Encl19 = as.factor(Encl19),
          Lag19 = as.factor(Lag19),
          Variabilidad19 = as.factor(Variabilidad19),
          SVL = as.numeric(SVL),
          TailLength = as.numeric(TailLength)
        )

      dist.exp.2 <- dist %>%
        dplyr::filter(Comb19 != "2_L", Comb19 != "2_M") %>%
        dplyr::mutate(
          Encl19 = as.factor(Encl19),
          Lag19 = as.factor(Lag19),
          Variabilidad19 = as.factor(Variabilidad19),
          SVL = as.numeric(SVL),
          TailLength = as.numeric(TailLength)
        )
      
      lm.exp1.SVL <-lm (SVL^5~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.1)
      lm.exp2.SVL <-lm (SVL^5~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.2)
      lm.exp1.BC <-lm (BodyConditionR~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.1)
      lm.exp2.BC <-lm (BodyConditionR~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.2)
      lm.exp1.TL <-lm (TailLength~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.1)
      lm.exp2.TL <-lm (TailLength~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.2)
      
      nuevo.a.exp1.SVL <- anova (lm.exp1.SVL)
      nuevo.a.exp2.SVL <- anova (lm.exp2.SVL)
      nuevo.a.exp1.BC <- anova (lm.exp1.BC)
      nuevo.a.exp2.BC <- anova (lm.exp2.BC)
      nuevo.a.exp1.TL <- anova (lm.exp1.TL)
      nuevo.a.exp2.TL <- anova (lm.exp2.TL)
      
      dist <- dplyr::select (dist, -Lag19, -Variabilidad19)
      
      ##### P.valor. dist.2 (anterior_estable)
      
      dist.2 <- dist.2 %>%
        dplyr::left_join(trat, by = "Comb19") %>%
        dplyr::distinct()

      dist.exp.1 <- dist.2 %>%
        dplyr::filter(Comb19 != "1_D", Comb19 != "4_D") %>%
        dplyr::mutate(
          Encl19 = as.factor(Encl19),
          Lag19 = as.factor(Lag19),
          Variabilidad19 = as.factor(Variabilidad19),
          SVL = as.numeric(SVL),
          TailLength = as.numeric(TailLength)
        )

      dist.exp.2 <- dist.2 %>%
        dplyr::filter(Comb19 != "2_L", Comb19 != "2_M") %>%
        dplyr::mutate(
          Encl19 = as.factor(Encl19),
          Lag19 = as.factor(Lag19),
          Variabilidad19 = as.factor(Variabilidad19),
          SVL = as.numeric(SVL),
          TailLength = as.numeric(TailLength)
        )
      
      lm.exp1.SVL <-lm (SVL^5~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.1)
      lm.exp2.SVL <-lm (SVL^5~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.2)
      lm.exp1.BC <-lm (BodyConditionR~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.1)
      lm.exp2.BC <-lm (BodyConditionR~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.2)
      lm.exp1.TL <-lm (TailLength~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.1)
      lm.exp2.TL <-lm (TailLength~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.2)
      
      a.exp1.SVL <- anova (lm.exp1.SVL)
      a.exp2.SVL <- anova (lm.exp2.SVL)
      a.exp1.BC <- anova (lm.exp1.BC)
      a.exp2.BC <- anova (lm.exp2.BC)
      a.exp1.TL <- anova (lm.exp1.TL)
      a.exp2.TL <- anova (lm.exp2.TL)
      
      dist.2 <- dplyr::select (dist.2, -Lag19, -Variabilidad19)
      
      if(
        ((nuevo.a.exp1.SVL$`Pr(>F)`[1]>=a.exp1.SVL$`Pr(>F)`[1])|
         (nuevo.a.exp1.SVL$`Pr(>F)`[1]>=p.valor))&
        ((nuevo.a.exp1.SVL$`Pr(>F)`[2]>=a.exp1.SVL$`Pr(>F)`[2])|
         (nuevo.a.exp1.SVL$`Pr(>F)`[2]>=p.valor))&
        ((nuevo.a.exp1.SVL$`Pr(>F)`[3]>=a.exp1.SVL$`Pr(>F)`[3])|
         (nuevo.a.exp1.SVL$`Pr(>F)`[3]>=p.valor))&
        ((nuevo.a.exp2.SVL$`Pr(>F)`[1]>=a.exp2.SVL$`Pr(>F)`[1])|
         (nuevo.a.exp2.SVL$`Pr(>F)`[1]>=p.valor))&
        ((nuevo.a.exp2.SVL$`Pr(>F)`[2]>=a.exp2.SVL$`Pr(>F)`[2])|
         (nuevo.a.exp2.SVL$`Pr(>F)`[2]>=p.valor))&
        ((nuevo.a.exp2.SVL$`Pr(>F)`[3]>=a.exp2.SVL$`Pr(>F)`[3])|
         (nuevo.a.exp2.SVL$`Pr(>F)`[3]>=p.valor))&
        ((nuevo.a.exp1.BC$`Pr(>F)`[1]>=a.exp1.BC$`Pr(>F)`[1])|
         (nuevo.a.exp1.BC$`Pr(>F)`[1]>=p.valor))&
        ((nuevo.a.exp1.BC$`Pr(>F)`[2]>=a.exp1.BC$`Pr(>F)`[2])|
         (nuevo.a.exp1.BC$`Pr(>F)`[2]>=p.valor))&
        ((nuevo.a.exp1.BC$`Pr(>F)`[3]>=a.exp1.BC$`Pr(>F)`[3])|
         (nuevo.a.exp1.BC$`Pr(>F)`[3]>=p.valor))&
        ((nuevo.a.exp2.BC$`Pr(>F)`[1]>=a.exp2.BC$`Pr(>F)`[1])|
         (nuevo.a.exp2.BC$`Pr(>F)`[1]>=p.valor))&
        ((nuevo.a.exp2.BC$`Pr(>F)`[2]>=a.exp2.BC$`Pr(>F)`[2])|
         (nuevo.a.exp2.BC$`Pr(>F)`[2]>=p.valor))&
        ((nuevo.a.exp2.BC$`Pr(>F)`[3]>=a.exp2.BC$`Pr(>F)`[3])|
         (nuevo.a.exp2.BC$`Pr(>F)`[3]>=p.valor))&
        ((nuevo.a.exp1.TL$`Pr(>F)`[1]>=a.exp1.TL$`Pr(>F)`[1])|
         (nuevo.a.exp1.TL$`Pr(>F)`[1]>=p.valor))&
        ((nuevo.a.exp1.TL$`Pr(>F)`[2]>=a.exp1.TL$`Pr(>F)`[2])|
         (nuevo.a.exp1.TL$`Pr(>F)`[2]>=p.valor))&
        ((nuevo.a.exp1.TL$`Pr(>F)`[3]>=a.exp1.TL$`Pr(>F)`[3])|
         (nuevo.a.exp1.TL$`Pr(>F)`[3]>=p.valor))&
        ((nuevo.a.exp2.TL$`Pr(>F)`[1]>=a.exp2.TL$`Pr(>F)`[1])|
         (nuevo.a.exp2.TL$`Pr(>F)`[1]>=p.valor))&
        ((nuevo.a.exp2.TL$`Pr(>F)`[2]>=a.exp2.TL$`Pr(>F)`[2])|
         (nuevo.a.exp2.TL$`Pr(>F)`[2]>=p.valor))&
        ((nuevo.a.exp2.TL$`Pr(>F)`[3]>=a.exp2.TL$`Pr(>F)`[3])|
         (nuevo.a.exp2.TL$`Pr(>F)`[3]>=p.valor))
      )
      {
        dist.2 <- dist
        #print (((sum(abs(media2$mean.SVL-mean(dist$SVL))))#*10+
        #(sum(abs(media2$var.SVL-var(dist$SVL)))))+
        #((sum(abs(media2$mean.BC-mean(dist$BodyConditionR))))*20+
        #(sum(abs(media2$var.BC-var(dist$BodyConditionR)))))+
        #((sum(abs(media2$mean.TL-mean(dist$TailLength))))/2+
        #(sum(abs(media2$var.TL-var(dist$TailLength))))/400
        #))
        
      } else{
        #print (((sum(abs(media$mean.SVL-mean(dist$SVL))))#*10+
        #(sum(abs(media$var.SVL-var(dist$SVL)))))+
        #((sum(abs(media$mean.BC-mean(dist$BodyConditionR))))*20+
        #(sum(abs(media$var.BC-var(dist$BodyConditionR)))))+
        #((sum(abs(media$mean.TL-mean(dist$TailLength))))/2+
        #(sum(abs(media$var.TL-var(dist$TailLength))))/400
        #))
      }
      
      vuelta.svl <- vuelta.svl+1
      #print(vuelta.svl)
      
      
      
      if(
        (vuelta.svl == 10)|
        ((nuevo.a.exp1.SVL$`Pr(>F)`[1]>p.valor+0.1)&
         (nuevo.a.exp1.SVL$`Pr(>F)`[2]>p.valor+0.1)&
         (nuevo.a.exp1.SVL$`Pr(>F)`[3]>p.valor+0.1)&
         (nuevo.a.exp2.SVL$`Pr(>F)`[1]>p.valor+0.1)&
         (nuevo.a.exp2.SVL$`Pr(>F)`[2]>p.valor+0.1)&
         (nuevo.a.exp2.SVL$`Pr(>F)`[3]>p.valor+0.1)&
         (nuevo.a.exp1.BC$`Pr(>F)`[1]>p.valor)&
         (nuevo.a.exp1.BC$`Pr(>F)`[2]>p.valor)&
         (nuevo.a.exp1.BC$`Pr(>F)`[3]>p.valor)&
         (nuevo.a.exp2.BC$`Pr(>F)`[1]>p.valor)&
         (nuevo.a.exp2.BC$`Pr(>F)`[2]>p.valor)&
         (nuevo.a.exp2.BC$`Pr(>F)`[3]>p.valor)&
         (nuevo.a.exp1.TL$`Pr(>F)`[1]>p.valor)&
         (nuevo.a.exp1.TL$`Pr(>F)`[2]>p.valor)&
         (nuevo.a.exp1.TL$`Pr(>F)`[3]>p.valor)&
         (nuevo.a.exp2.TL$`Pr(>F)`[1]>p.valor)&
         (nuevo.a.exp2.TL$`Pr(>F)`[2]>p.valor)&
         (nuevo.a.exp2.TL$`Pr(>F)`[3]>p.valor)
        )
      ){break}
    }
    
    vuelta.svl.1 <- vuelta.svl.1+1
    print(vuelta.svl.1)
    
    
    if (
      (vuelta.svl.1 == 200)|
      ((nuevo.a.exp1.SVL$`Pr(>F)`[1]>p.valor+0.1)&
       (nuevo.a.exp1.SVL$`Pr(>F)`[2]>p.valor+0.1)&
       (nuevo.a.exp1.SVL$`Pr(>F)`[3]>p.valor+0.1)&
       (nuevo.a.exp2.SVL$`Pr(>F)`[1]>p.valor+0.1)&
       (nuevo.a.exp2.SVL$`Pr(>F)`[2]>p.valor+0.1)&
       (nuevo.a.exp2.SVL$`Pr(>F)`[3]>p.valor+0.1)&
       (nuevo.a.exp1.BC$`Pr(>F)`[1]>p.valor)&
       (nuevo.a.exp1.BC$`Pr(>F)`[2]>p.valor)&
       (nuevo.a.exp1.BC$`Pr(>F)`[3]>p.valor)&
       (nuevo.a.exp2.BC$`Pr(>F)`[1]>p.valor)&
       (nuevo.a.exp2.BC$`Pr(>F)`[2]>p.valor)&
       (nuevo.a.exp2.BC$`Pr(>F)`[3]>p.valor)&
       (nuevo.a.exp1.TL$`Pr(>F)`[1]>p.valor)&
       (nuevo.a.exp1.TL$`Pr(>F)`[2]>p.valor)&
       (nuevo.a.exp1.TL$`Pr(>F)`[3]>p.valor)&
       (nuevo.a.exp2.TL$`Pr(>F)`[1]>p.valor)&
       (nuevo.a.exp2.TL$`Pr(>F)`[2]>p.valor)&
       (nuevo.a.exp2.TL$`Pr(>F)`[3]>p.valor)
      )
    ){break}
  }
  
  dist <- dist.2
  
  if (vuelta.svl.1 == 200) {
    p.valor <- p.valor-0.2
  } else if (vuelta.svl.1 < 200){
    p.valor <- p.valor+0.05
  }
  
  if (vuelta.svl.1 == 200) {cont <- cont+1}
  
  print (p.valor)
  
  if ((p.valor>0.95)|(cont==3)){break}
}

media <- dist %>%
  dplyr::group_by (Comb19) %>%
  dplyr::summarise (var(SVL), var(BodyConditionR), var(TailLength))


##### PASO 7: Asignar LAG y Variabilidad #####


dist <- dist %>% dplyr::left_join(trat, by = "Comb19") %>% dplyr::distinct()

dist.exp.1 <- dist %>%
  dplyr::filter(Comb19 != "1_D", Comb19 != "4_D") %>%
  dplyr::mutate(
    Encl19 = as.factor(Encl19),
    Lag19 = as.factor(Lag19),
    Variabilidad19 = as.factor(Variabilidad19),
    SVL = as.numeric(SVL),
    TailLength = as.numeric(TailLength)
  )

dist.exp.2 <- dist %>%
  dplyr::filter(Comb19 != "2_L", Comb19 != "2_M") %>%
  dplyr::mutate(
    Encl19 = as.factor(Encl19),
    Lag19 = as.factor(Lag19),
    Variabilidad19 = as.factor(Variabilidad19),
    SVL = as.numeric(SVL),
    TailLength = as.numeric(TailLength)
  )

lm.exp1.SVL <-lm (SVL^5~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.1)
a.exp1.SVL <- anova (lm.exp1.SVL)

lm.exp2.SVL <-lm (SVL^5~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.2)
exp2.SVL <- anova (lm.exp2.SVL)

lm.exp1.BC <-lm (BodyConditionR~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.1)
a.exp1.BC <- anova (lm.exp1.BC)

lm.exp2.BC <-lm (BodyConditionR~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.2)
a.exp2.BC <- anova (lm.exp2.BC)

lm.exp1.TL <-lm (TailLength^2~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.1)
a.exp1.TL <- anova (lm.exp1.TL)

lm.exp2.TL <-lm (TailLength^2~Lag19 + Variabilidad19 + Lag19:Variabilidad19, data=dist.exp.2)
a.exp2.TL <- anova (lm.exp2.TL)

dist <- dplyr::select (dist, -Lag19, -Variabilidad19)

a.exp1.SVL
a.exp2.SVL
a.exp1.BC
a.exp2.BC
a.exp1.TL
a.exp2.TL

##### PASO 7: Asignar LAG y Variabilidad #####

# Para asignar a la combinaci?n su lag y variabilidad

dist <- merge (dist, trat, by="Comb19")
dist <- dist[!duplicated(dist), ]

  ##### CONTADOR repeticiones 2: cambia distribuci?n cercados 2019 #####

#contemos = 0
#repeat{
#if(contemos ==1) {
#    break
#  }
#  else {
#    contemos = contemos+1
#    n <- n+1
#    message(paste0("At ", Sys.time()," iteration number ", n, " finished"))


##### PASO 9: Asignar Cercados 2019 #####

# Este c?digo es para asignar un cercado a su correspondiente tratamiento

trat.original$Comb19 <- as.character(trat.original$Comb19)
trat.original$Encl19 <- as.character(trat.original$Encl19)
tratamientos19 <- trat.original$Comb19
tratamientos19 <- unique (tratamientos19)
ya <- 0
parar <- length(tratamientos19)

repeat{
  
ya <- ya+1

tratamiento <- tratamientos19[1]
encl <- 1:nrow(trat.original)

for (i in 1:nrow(trat.original)) {
if (trat.original$Comb19[i]==tratamiento){
  encl[i] <- trat.original$Encl19[i]
}else{
  encl[i] <- NA}
}
encl <- na.omit (encl)
tratamiento
encl

for (i in 1:nrow(dist)) {
  if     (dist$Comb19[i]==tratamiento){
    dist$Encl19[i] <- (sample(c(encl), 1))
    }
}

tratamientos19 <- tratamientos19[-c(1)]

if(ya==parar)
  {break}
  
}

#????????????????????????????????????????????????????????????????????

##### PASO 10: REPEAT R1, R2, R4 #####

  R9 <- dplyr::count (dist, Encl18)
  colnames(R9)[colnames(R9)=="n"] <- "n.Encl18"
  dist <- dplyr::select (dist, -n.Encl18)
  dist <- merge (dist, R9, by="Encl18")
  
  
  enchufe <-dplyr::count (dist, Comb19, Encl18)
  enchufe <- dplyr::count (enchufe, Encl18, n)
  enchufe1 <- enchufe[enchufe$n==1,]
  enchufe1 <- dplyr::select (enchufe1, -n)
  colnames(enchufe1)[colnames(enchufe1)=="nn"] <- "restar"
  
  hola <- R1.C18.C19
  colnames(hola)[colnames(hola)=="Encl"] <- "Encl18"
  colnames(hola)[colnames(hola)=="Comb18_Comb19"] <- "R4_Comb18_Comb19"
  dist <- dist %>% dplyr::select(-R4_Comb18_Comb19) %>%
    dplyr::left_join(hola, by = "Encl18") %>%
    dplyr::mutate(Comb18_Comb19 = paste(Comb18, Comb19, sep = "_"))
  
repeat{
  
  #R4
  repeat{
  
  # Eliminar todas las columnas, para que pueda calcularlas nuevamente en cada
  # vuelta del bucle
  
 # if     (all(R9$n[1:nrow(R9)]<=Encl.19-1)) {
  
  dist <- dplyr::select (dist, -Encl18_Encl19, -n.R4, -R4, -n.Encl18)
  
  dist <- merge (dist, R9, by="Encl18")
 
  # Reasignar cercados que no est?n bien
  # R4: No pueden ir juntos dos individuos que provengan del mismo cercado 
  
  # Igual que R2, pero comprobando el n?mero de individuos de cada cercado
  # que va a cada cercado.
  
  tabla.R4 <- dplyr::count (dist, Encl19, Encl18) %>%
    dplyr::mutate(n = as.numeric(n)) %>%
    dplyr::rename(n.R4 = n) %>%
    dplyr::mutate(Encl18_Encl19 = paste(Encl18, Encl19, sep = "_")) %>%
    dplyr::select(Encl18_Encl19, n.R4)
  
  prueba <- dplyr::count (tabla.R4, Encl18)
  prueba <- merge (prueba, R9, by="Encl18")
  
  dist$Encl18_Encl19 <- paste(dist$Encl18, dist$Encl19, sep="_")
  
  dist <- merge(dist, tabla.R4, by="Encl18_Encl19")

  for(i in 1:nrow(prueba)){
    if (prueba$n.Encl18[i]>(Encl.19-1)){
      prueba$deberia[i] <- 15
    } else {
      prueba$deberia[i] <- prueba$n.Encl18[i] 
    }
  }
  

  prueba <- merge (prueba, enchufe1, by="Encl18", all=TRUE)
  prueba$restar[is.na(prueba$restar)] <- 0  
  
  
  for (i in 1:nrow(prueba)){
    if (prueba$deberia[i] == (Encl.19-1)){
    prueba$resultado[i] <- prueba$deberia[i]-prueba$restar[i]
    } else {
      prueba$resultado[i] <- prueba$n.Encl18[i]
    }
  }
  
  prueba<-dplyr::select (prueba, -restar)
  
  for (i in 1:nrow(dist)){
    if ((dist$Comb18_Comb19[i]==dist$R4_Comb18_Comb19[i])
    ){
      dist$verdadero[i] <- "VERDADERO" 
    }else{
      dist$verdadero[i] <- "FALSO" 
    }
  }
  
  desesperacion <- dplyr::count (dist, Encl18, n.R4, verdadero)
  desesperacion <- desesperacion[desesperacion$verdadero=="VERDADERO", ]
  desesperacion <- desesperacion[desesperacion$n.R4>1, ]
  
  if (nrow(desesperacion)>1){  #A?ADIDO NUEVO (ESTA LINEA)
  
  desesperacion$cambiar <- 1
  desesperacion <- dplyr::select (desesperacion, Encl18, cambiar)
  
  prueba <- merge (prueba, desesperacion, by="Encl18", all=TRUE)
  prueba$cambiar[is.na(prueba$cambiar)] <- 0  
  
  
  for (i in 1:nrow(prueba)){
    if ((prueba$n.Encl18[i]<Encl.19)
    ){
      prueba$resultado[i] <- (prueba$resultado[i]-prueba$cambiar[i])
    }else{
      prueba$resultado[i] <- (prueba$resultado[i])
    }
  }
  
  } #A?ADIDO NUEVO (ESTA LINEA)
  
  for (i in 1:nrow(dist)){
    if ((dist$Comb18_Comb19[i]!=dist$R4_Comb18_Comb19[i])&&
      (dist$n.R4[i]>1)
      ){
      dist$R4[i] <- sample (c (0,1), 1)  
    }else{
      dist$R4[i] <- 0
    }
  }

  # Reordenar la tabla 
  
  for (i in 1:nrow(dist)) {
    if     ((dist$R4[i]==1)&&(dist$Comb19[i]=="1_D")) 
    {dist$Encl19[i] <- (sample(c("C03", "C13"), 1))}
    
    else if((dist$R4[i]==1)&&(dist$Comb19[i]=="4_D")) 
    {dist$Encl19[i] <- (sample(c("C08", "C10"), 1))}
    
    else if((dist$R4[i]==1)&&(dist$Comb19[i]=="1_L")) 
    {dist$Encl19[i] <- (sample(c("C06", "C15"), 1))}
    
    else if((dist$R4[i]==1)&&(dist$Comb19[i]=="1_M"))
    {dist$Encl19[i] <- (sample(c("C01", "C12"), 1))}
    
    else if((dist$R4[i]==1)&&(dist$Comb19[i]=="2_L"))
    {dist$Encl19[i] <- (sample(c("C04", "C09"), 1))}
    
    else if((dist$R4[i]==1)&&(dist$Comb19[i]=="2_M"))
    {dist$Encl19[i] <- (sample(c("C07", "C14"), 1))}
  
    else if((dist$R4[i]==1)&&(dist$Comb19[i]=="4_L"))
    {dist$Encl19[i] <- (sample(c("C02", "C11"), 1))}
    
    else if((dist$R4[i]==1)&&(dist$Comb19[i]=="4_M"))
    {dist$Encl19[i] <- (sample(c("C05", "C16"), 1))}
  }
  
  
  tabla.R4.2 <- dplyr::count (dist, Encl19, Encl18) %>%
    dplyr::mutate(n = as.numeric(n))
  
  prueba <- dplyr::count (tabla.R4.2, Encl18)
  prueba <- merge (prueba, R9, by="Encl18")
  
  
  for(i in 1:nrow(prueba)){
    if (prueba$n.Encl18[i]>(Encl.19-1)){
      prueba$deberia[i] <- 15
    } else {
      prueba$deberia[i] <- prueba$n.Encl18[i] 
    }
  }
  
  
  prueba <- merge (prueba, enchufe1, by="Encl18", all=TRUE)
  prueba$restar[is.na(prueba$restar)] <- 0  
  
  
  for (i in 1:nrow(prueba)){
    if (prueba$deberia[i] == (Encl.19-1)){
      prueba$resultado[i] <- prueba$deberia[i]-prueba$restar[i]
    } else {
      prueba$resultado[i] <- prueba$n.Encl18[i]
    }
  }
  
  prueba<-dplyr::select (prueba, -restar)
  
  for (i in 1:nrow(dist)){
    if ((dist$Comb18_Comb19[i]==dist$R4_Comb18_Comb19[i])
    ){
      dist$verdadero[i] <- "VERDADERO" 
    }else{
      dist$verdadero[i] <- "FALSO" 
    }
  }
  
  desesperacion <- dplyr::count (dist, Encl18, n.R4, verdadero)
  desesperacion <- desesperacion[desesperacion$verdadero=="VERDADERO", ]
  desesperacion <- desesperacion[desesperacion$n.R4>1, ]
  
  
  if (nrow(desesperacion)>1){  #A?ADIDO NUEVO (ESTA LINEA)
    
  desesperacion$cambiar <- 1
  desesperacion <- dplyr::select (desesperacion, Encl18, cambiar)
  
  prueba <- merge (prueba, desesperacion, by="Encl18", all=TRUE)
  prueba$cambiar[is.na(prueba$cambiar)] <- 0  
  
  
  for (i in 1:nrow(prueba)){
    if ((prueba$n.Encl18[i]<Encl.19)
    ){
      prueba$resultado[i] <- (prueba$resultado[i]-prueba$cambiar[i])
    }else{
      prueba$resultado[i] <- (prueba$resultado[i])
    }
  }
  
  } #A?ADIDO NUEVO (ESTA LINEA)
  
  prueba$ok <- 0
  
  for (i in 1:nrow(prueba)) {
    if (prueba$n[i]>=(prueba$resultado[i])){
        prueba$ok[i] <- 0}
    else
    {prueba$ok[i] <- 1}
  }
  prueba
  
if(
  sum (prueba$ok) == 0
  )

{break}}
  #R2
  repeat{
    
    # R2: mismo n?mero de invididuos en todos los cercados
    
    # Crear la columna R2 en "dist": 
    # 0 <- n.R2 (n?ind.por Encl19) < media n?ind para cada cercado
    # Elige entre seis 0 y un 1 <- n.R2 (n?ind.por Encl19) > media n?ind para cada cercado
    # (esto lo hago para que no cambie todos los ind. en cercados que sobran bichos, 
    # sino uno de cada seis)
    
    dist <- dplyr::select (dist, -n.R2, -R2, -n.grapadora, -n.celo)
    
    tabla.R2 <-  dplyr::count (dist, Encl19)
    tabla.R2$n <- as.numeric (tabla.R2$n)
    colnames(tabla.R2)[colnames(tabla.R2)=="n"] <- "n.R2"
    
    dist<-merge (dist, tabla.R2, by="Encl19")
    dist <- dist[with(dist, order(dist$Animal_number1)), ] # Reordenar la tabla 
    dist$Encl18_Encl19 <- paste(dist$Encl18, dist$Encl19, sep="_")
    dist$Encl18_Comb19 <- paste(dist$Encl18, dist$Comb19, sep="_")
    
    gafas <- dplyr::count (dist, Encl18, Encl19) %>%
      dplyr::mutate(Encl18_Encl19 = paste(Encl18, Encl19, sep = "_")) %>%
      dplyr::select(Encl18_Encl19, n) %>%
      dplyr::rename(n.grapadora = n)
    
    lentillas <- dplyr::count (dist, Encl18, Comb19) %>%
      dplyr::mutate(Encl18_Comb19 = paste(Encl18, Comb19, sep = "_")) %>%
      dplyr::select(Encl18_Comb19, n) %>%
      dplyr::rename(n.celo = n)
    
    dist <- merge (dist, gafas, by="Encl18_Encl19")
    dist <- merge (dist, lentillas, by="Encl18_Comb19")
    
    R2  <- 1:(nrow(dist))
    
    for(i in 1:nrow(dist)){
      if (
        ((dist$n.grapadora[i]>(sum(dist$n.grapadora/nrow(dist))))&&
        (dist$n.R2[i]>(nrow(dist)/Encl.19)))|
        ((dist$n.celo[i]==(min(dist$n.celo)))&&
         (dist$n.R2[i]>(nrow(dist)/Encl.19)))
        ){
        R2[i] <- sample (c (0,0,0,0,0,0,0,1), 1) 
      } else {
        R2[i] <- 0  
      }
    }
    
    dist$R2 <- R2
    R2
    
    #R2
    #for(i in 1:nrow(dist)){
    #  if (dist$R2[i]==1) dist$Encl19[i] <- sample (cercados.R2, 1) 
    #}
    
    
    dist <- dist[with(dist, order(dist$Animal_number1)), ] 
    
    for (i in 1:nrow(dist)) {
      if     ((dist$R2[i]==1)&&(dist$Comb19[i]=="1_D")) 
      {dist$Encl19[i] <- (sample(c("C03", "C13"), 1))}
      
      else if((dist$R2[i]==1)&&(dist$Comb19[i]=="4_D")) 
      {dist$Encl19[i] <- (sample(c("C08", "C10"), 1))}
      
      else if((dist$R2[i]==1)&&(dist$Comb19[i]=="1_L")) 
      {dist$Encl19[i] <- (sample(c("C06", "C15"), 1))}
      
      else if((dist$R2[i]==1)&&(dist$Comb19[i]=="1_M"))
      {dist$Encl19[i] <- (sample(c("C01", "C12"), 1))}
      
      else if((dist$R2[i]==1)&&(dist$Comb19[i]=="2_L"))
      {dist$Encl19[i] <- (sample(c("C04", "C09"), 1))}
      
      else if((dist$R2[i]==1)&&(dist$Comb19[i]=="2_M"))
      {dist$Encl19[i] <- (sample(c("C07", "C14"), 1))}
      
      else if((dist$R2[i]==1)&&(dist$Comb19[i]=="4_L"))
      {dist$Encl19[i] <- (sample(c("C02", "C11"), 1))}
      
      else if((dist$R2[i]==1)&&(dist$Comb19[i]=="4_M"))
      {dist$Encl19[i] <- (sample(c("C05", "C16"), 1))}
    }
    
    

    
    
    
    rest.R2 <- dplyr::count (dist, Encl19)
    rest.R2
    
    #R2
    if(
      all((rest.R2$n[1:nrow(rest.R2)])==(nrow(dist)/Encl.19))
    )
    {break}}
  #R1
  repeat{
    
    #R1
    
    dist <- dist[with(dist, order(dist$Animal_number1)), ] 
    
    for (i in 1:nrow(dist)) {
      if     ((dist$Encl19[i]==dist$Encl18[i])&&(dist$Comb19[i]=="1_D")) 
      {dist$Encl19[i] <- (sample(c("C03", "C13"), 1))}
      
      else if((dist$Encl19[i]==dist$Encl18[i])&&(dist$Comb19[i]=="4_D")) 
      {dist$Encl19[i] <- (sample(c("C08", "C10"), 1))}
      
      else if((dist$Encl19[i]==dist$Encl18[i])&&(dist$Comb19[i]=="1_L")) 
      {dist$Encl19[i] <- (sample(c("C06", "C15"), 1))}
      
      else if((dist$Encl19[i]==dist$Encl18[i])&&(dist$Comb19[i]=="1_M"))
      {dist$Encl19[i] <- (sample(c("C01", "C12"), 1))}
      
      else if((dist$Encl19[i]==dist$Encl18[i])&&(dist$Comb19[i]=="2_L"))
      {dist$Encl19[i] <- (sample(c("C04", "C09"), 1))}
      
      else if((dist$Encl19[i]==dist$Encl18[i])&&(dist$Comb19[i]=="2_M"))
      {dist$Encl19[i] <- (sample(c("C07", "C14"), 1))}
      
      else if((dist$Encl19[i]==dist$Encl18[i])&&(dist$Comb19[i]=="4_L"))
      {dist$Encl19[i] <- (sample(c("C02", "C11"), 1))}
      
      else if((dist$Encl19[i]==dist$Encl18[i])&&(dist$Comb19[i]=="4_M"))
      {dist$Encl19[i] <- (sample(c("C05", "C16"), 1))}
    }
    if(
      #R1
      all(dist$Encl19[1:nrow(dist)]!=dist$Encl18[1:nrow(dist)])
    )
      
    {break}
  }

  tabla.R4.2 <-  dplyr::count (dist, Encl19, Encl18)
  tabla.R4.2$n <- as.numeric (tabla.R4.2$n)
  
  prueba <- dplyr::count (tabla.R4.2, Encl18)
  prueba <- merge (prueba, R9, by="Encl18")
  
  
  for(i in 1:nrow(prueba)){
    if (prueba$n.Encl18[i]>(Encl.19-1)){
      prueba$deberia[i] <- 15
    } else {
      prueba$deberia[i] <- prueba$n.Encl18[i] 
    }
  }
  
  
  prueba <- merge (prueba, enchufe1, by="Encl18", all=TRUE)
  prueba$restar[is.na(prueba$restar)] <- 0  
  
  
  for (i in 1:nrow(prueba)){
    if (prueba$deberia[i] == (Encl.19-1)){
      prueba$resultado[i] <- prueba$deberia[i]-prueba$restar[i]
    } else {
      prueba$resultado[i] <- prueba$n.Encl18[i]
    }
  }
  
  prueba<-dplyr::select (prueba, -restar)
  
  for (i in 1:nrow(dist)){
    if ((dist$Comb18_Comb19[i]==dist$R4_Comb18_Comb19[i])
    ){
      dist$verdadero[i] <- "VERDADERO" 
    }else{
      dist$verdadero[i] <- "FALSO" 
    }
  }
  
  desesperacion <- dplyr::count (dist, Encl18, n.R4, verdadero)
  desesperacion <- desesperacion[desesperacion$verdadero=="VERDADERO", ]
  desesperacion <- desesperacion[desesperacion$n.R4>1, ]
  
  if (nrow(desesperacion)>1){  #A?ADIDO NUEVO (ESTA LINEA)
    
  desesperacion$cambiar <- 1
  desesperacion <- dplyr::select (desesperacion, Encl18, cambiar)
  
  prueba <- merge (prueba, desesperacion, by="Encl18", all=TRUE)
  prueba$cambiar[is.na(prueba$cambiar)] <- 0  

  
  for (i in 1:nrow(prueba)){
    if ((prueba$n.Encl18[i]<Encl.19)
    ){
      prueba$resultado[i] <- (prueba$resultado[i]-prueba$cambiar[i])
    }else{
      prueba$resultado[i] <- (prueba$resultado[i])
    }
  }
   
   }#A?ADIDO NUEVO (ESTA LINEA)
  
  prueba$ok <- 0
  
  for (i in 1:nrow(prueba)) {
    if (prueba$n[i]>=(prueba$resultado[i])){
      prueba$ok[i] <- 0}
    else
    {prueba$ok[i] <- 1}
  }
  prueba
  
  rest.R2 <- dplyr::count (dist, Encl19)
  rest.R2
  
  if(
    (all(dist$Encl19[1:nrow(dist)]!=dist$Encl18[1:nrow(dist)]))&&
    (all((rest.R2$n[1:nrow(rest.R2)])==(nrow(dist)/Encl.19)))&&
    sum (prueba$ok) == 0
  )
  {break}
  }

#setwd("F:/Tesis/2019/06_R.Distribuciones/Simulacro/Resultados")
#dir ("F:/Tesis/2019/06_R.Distribuciones/Simulacro/Resultados")
#write.xlsx(dist, paste0("Distribucion.", n, ".xlsx"))

#Par?ntesis contador 1

  
 
  
  

##### Tabla borrar #####
write.xlsx(dist, "dist155.xlsx")
#poner codigo para guardar el workspace

  
##### MODELOS #####

library(nlme)  #Paquete nlme, funci?n lme
library(lme4)  #Paquete lme4, funci?n lmer


dist <- dist %>%
  dplyr::mutate(
    Encl19 = as.factor(Encl19),
    Lag19 = as.factor(Lag19),
    Variabilidad19 = as.factor(Variabilidad19),
    SVL = as.numeric(SVL),
    TailLength = as.numeric(TailLength)
  )


dist.1 <- dist[dist$Comb19 != "1_D", ] 
dist.1 <- dist.1[dist.1$Comb19 != "4_D", ] 

dist.2 <- dist[dist$Comb19 != "2_L", ] 
dist.2 <- dist.2[dist.2$Comb19 != "2_M", ] 

str (dist.1)
str (dist.2)

# MEDIA SVL#

  # Experimento 1
lm.exp1.SVL <-lme (SVL^5~Lag19 + Variabilidad19 + Lag19:Variabilidad19, random=~1|Encl19, data=dist.1)
anova (lm.exp1.SVL)

shapiro.test(residuals(lm.exp1.SVL))   		      #Normalidad mal
bartlett.test(residuals(lm.exp1.SVL)~Lag19, data=dist.1)		  	#Homocedasticidad ok
bartlett.test(residuals(lm.exp1.SVL)~Variabilidad19, data=dist.1)	#Homocedasticidad ok
bartlett.test(residuals(lm.exp1.SVL)~interaction(Lag19,Variabilidad19), data=dist.1)		  	#Homocedasticidad ok

  # Experimento 2

lm.exp2.SVL <-lme (SVL^5~Lag19 + Variabilidad19 + Lag19:Variabilidad19, random=~1|Encl19, data=dist.2)
anova (lm.exp2.SVL)

shapiro.test(residuals(lm.exp2.SVL))   		      #Normalidad mal
bartlett.test(residuals(lm.exp2.SVL)~Lag19, data=dist.2)		  	#Homocedasticidad ok
bartlett.test(residuals(lm.exp2.SVL)~Variabilidad19, data=dist.2)	#Homocedasticidad ok
bartlett.test(residuals(lm.exp2.SVL)~interaction(Lag19,Variabilidad19), data=dist.2)		  	#Homocedasticidad ok

# MEDIA BC #

# Experimento 1

lm.exp1.BC <-lme (BodyConditionR~Lag19 + Variabilidad19 + Lag19:Variabilidad19, random=~1|Encl19, data=dist.1)
anova (lm.exp1.BC)

shapiro.test(residuals(lm.exp1.BC))   		      #Normalidad mal
bartlett.test(residuals(lm.exp1.BC)~Lag19, data=dist.1)		  	#Homocedasticidad ok
bartlett.test(residuals(lm.exp1.BC)~Variabilidad19, data=dist.1)	#Homocedasticidad ok
bartlett.test(residuals(lm.exp1.BC)~interaction(Lag19,Variabilidad19), data=dist.1)		  	#Homocedasticidad ok

# Experimento 2

lm.exp2.BC <-lme (BodyConditionR~Lag19 + Variabilidad19 + Lag19:Variabilidad19, random=~1|Encl19, data=dist.2)
anova (lm.exp2.BC)

shapiro.test(residuals(lm.exp2.BC))   		      #Normalidad mal
bartlett.test(residuals(lm.exp2.BC)~Lag19, data=dist.2)		  	#Homocedasticidad ok
bartlett.test(residuals(lm.exp2.BC)~Variabilidad19, data=dist.2)	#Homocedasticidad ok
bartlett.test(residuals(lm.exp2.BC)~interaction(Lag19,Variabilidad19), data=dist.2)		  	#Homocedasticidad ok

# MEDIA TL #

# Experimento 1

lm.exp1.TL <-lme (TailLength~Lag19 + Variabilidad19 + Lag19:Variabilidad19, random=~1|Encl19, data=dist.1)
anova (lm.exp1.TL)

shapiro.test(residuals(lm.exp1.TL))   		      #Normalidad mal
bartlett.test(residuals(lm.exp1.TL)~Lag19, data=dist.1)		  	#Homocedasticidad ok
bartlett.test(residuals(lm.exp1.TL)~Variabilidad19, data=dist.1)	#Homocedasticidad ok
bartlett.test(residuals(lm.exp1.TL)~interaction(Lag19,Variabilidad19), data=dist.1)		  	#Homocedasticidad ok

# Experimento 2

lm.exp2.TL <-lme (TailLength~Lag19 + Variabilidad19 + Lag19:Variabilidad19, random=~1|Encl19, data=dist.2)
anova (lm.exp2.TL)

shapiro.test(residuals(lm.exp2.TL))   		      #Normalidad mal
bartlett.test(residuals(lm.exp2.TL)~Lag19, data=dist.2)		  	#Homocedasticidad ok
bartlett.test(residuals(lm.exp2.TL)~Variabilidad19, data=dist.2)	#Homocedasticidad ok
bartlett.test(residuals(lm.exp2.TL)~interaction(Lag19,Variabilidad19), data=dist.2)		  	#Homocedasticidad ok

# VARIANZA #

#EXPERIMENTO 1

comb.encl.1 <- data.frame(dist.1$Encl19, dist.1$Comb19, dist.1$Lag19, dist.1$Variabilidad19)
comb.encl.1 <- comb.encl.1[!duplicated(comb.encl.1), ]
colnames(comb.encl.1)[colnames(comb.encl.1)=="dist.1.Encl19"] <- "Encl19"
colnames(comb.encl.1)[colnames(comb.encl.1)=="dist.1.Comb19"] <- "Comb19"
colnames(comb.encl.1)[colnames(comb.encl.1)=="dist.1.Lag19"] <- "Lag19"
colnames(comb.encl.1)[colnames(comb.encl.1)=="dist.1.Variabilidad19"] <- "Variabilidad19"

#??????????????#

# EXPERIMENTO 1

varSVL <- tapply(dist.1$SVL, dist.1$Encl19, var)
varSVL <- data.frame(varSVL)
varSVL$Encl19 <- as.factor(levels(dist.1$Encl19))
varSVL <- na.omit (varSVL)

var.SVL<-merge (varSVL,comb.encl.1, by="Encl19")

lm.var.SVL<- lm(varSVL~Lag19+Variabilidad19+Lag19:Variabilidad19, data=var.SVL)
anova(lm.var.SVL)

shapiro.test(residuals(lm.var.SVL))   		    #Normalidad ok
bartlett.test(residuals(lm.var.SVL)~Lag19, data=var.SVL)		  	#Homocedasticidad ok
bartlett.test(residuals(lm.var.SVL)~Variabilidad19, data=var.SVL)	#Homocedasticidad ok
bartlett.test(residuals(lm.var.SVL)~interaction(Lag19,Variabilidad19), data=var.SVL)		  	#Homocedasticidad ok

# EXPERIMENTO 2

varSVL <- tapply(dist.2$SVL, dist.2$Encl19, var)
varSVL <- data.frame(varSVL)
varSVL$Encl19 <- as.factor(levels(dist.1$Encl19))
varSVL <- na.omit (varSVL)

var.SVL<-merge (varSVL,comb.encl.1, by="Encl19")

lm.var.SVL<- lm(varSVL~Lag19+Variabilidad19+Lag19:Variabilidad19, data=var.SVL)
anova(lm.var.SVL)

shapiro.test(residuals(lm.var.SVL))   		    #Normalidad ok
bartlett.test(residuals(lm.var.SVL)~Lag19, data=var.SVL)		  	#Homocedasticidad ok
bartlett.test(residuals(lm.var.SVL)~Variabilidad19, data=var.SVL)	#Homocedasticidad ok
bartlett.test(residuals(lm.var.SVL)~interaction(Lag19,Variabilidad19), data=var.SVL)		  	#Homocedasticidad ok



##### PARENTESIS CONTADORES #####  

#Paréntesis contador 2

#Paréntesis contador prueba


##### SE HA ACABADO DE VERDAD #####

###############################################
##### PASO 4: Tabla te?rica de interacci?n Comb18_Comb19 #####

#trat <- dplyr::select (trat, -Encl19)
trat <- trat [!duplicated (trat), ]
año2018 <-  dplyr::select (dist, Lag18, Variabilidad18)
año2018 <- año2018[!duplicated(año2018), ] 
tabla1 <- merge (año2018, trat)
tabla1$Comb18 <- paste(tabla1$Lag18, tabla1$Variabilidad18, sep="_")
tabla1$Comb18_Comb19 <- paste(tabla1$Comb18, tabla1$Comb19, sep="_")

tabla2 <- dplyr::select (tabla1, Comb18_Comb19, Comb18, Comb19)
tabla2$Comb19 <- as.character(tabla2$Comb19)

dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")  

repeat {
  
  R5.ideal <- merge (R5, tabla2, by ="Comb18_Comb19", all=TRUE)
  R5.ideal$R5[is.na(R5.ideal$R5)] <- 0  
  #N?mero de combinaciones que existe 
  #actualmente, pero que deber?a ser 64
  R5.falta <- R5.ideal[R5.ideal$R5 < 1, ]  # Se ven las combinaciones que faltan, 
  # Hay que arreglarlo para que esas Comb18 que sobren de alguna Comb19, vayan a 
  # las Comb19 que est? tabla indica que faltan
  
  R5.Comb18 <- dist$Comb18[1:nrow(dist)] == R5.falta$Comb18[1] #Aparecen como "True"
  # aquellos casos en los que R5.falta$Comb18 = a la primera fila de R5.falta
  
  dist <- dplyr::select (dist, -R5)
  dist <- merge (dist, R5, by="Comb18_Comb19")
  
  # HAY QUE SEGUIR ARREGLANDO A PARTIR DE AQU?
  
  R5.Comb18.dup <- (dist$R5[1:nrow(dist)] > 1) #Aparecen como true aquellos casos en los 
  # que se han asignado m?s de un Comb19 al comb18
  
  R5.modificar <- R5.Comb18+R5.Comb18.dup  #Los que pone 2 son los casos en 
  # los que las dos anteriores condiciones son True, de modo que son aquellas
  # Comb18 en las que hay m?s de un Comb19. Alguna de ellas habr? que cambiarla
  # de comb19 para poner las que faltan seg?n R5.falta
  
  
  for(i in 1:length(R5.modificar)){
    if (is.na(R5.modificar[i])){
      R5.modificar[i]  <- 0  
    }
  }
  
  dist$R5.modificar <- R5.modificar
  
  # Con esto, vamos a cambiar 1 de ellos
  
  cont =0
  for (i in 1:nrow(dist)){
    if(cont ==1) {
      break
    }
    else if((dist$R5.modificar[i]==2)){
      dist$Comb19[i] <- R5.falta$Comb19[1]
      cont = cont + 1
    }
  }
  cont
  
  dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")
  R5 <- dplyr::count (dist, Comb18, Comb19)
  R5$Comb18_Comb19 <- paste (R5$Comb18, R5$Comb19, sep="_")
  colnames (R5) [colnames(R5)=="n"] <- "R5"
  R5$R5 <- as.numeric (R5$R5)
  R5 <- dplyr::select (R5, -Comb18, -Comb19)
  
  if(
    (nrow(R5)==(C.18*C.19))
  )
    
  {break}
  
}

#????????????????????????????????????????????????????????????????????


##### PASO 5: Corregir errores del paso 4 #####

# R3: mismo n?mero de invididuos en todas las combinaciones





repeat{
  #Mismo n?mero de individuos en todos los tratamientos
  repeat {
    
    dist <- dplyr::select (dist, -n.C19, -n.C18.C19)
    dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
    
    tabla.R3 <-  dplyr::count (dist, Comb19)
    tabla.R3$n <- as.numeric (tabla.R3$n)
    colnames(tabla.R3)[colnames(tabla.R3)=="n"] <- "n.C19"
    
    R3 <-dplyr::count (dist, Comb18, Comb19)
    R3$Comb18 <- as.character (R3$Comb18)
    colnames(R3)[colnames(R3)=="n"] <- "n.C18.C19"
    R3 <- merge (R3, tabla.R3, by="Comb19")
    R3$Comb18_Comb19 <- paste(R3$Comb18, R3$Comb19, sep="_")
    
    C.C18 <- 1:(nrow(R3))
    
    for(i in 1:nrow(R3)){
      if ((R3$n.C19[i]>(nrow (dist)/C.19))&&
          (R3$n.C18.C19 [i]>=(ceiling (nrow(dist)/(C.18*C.19))))){   #Los que hay que cambiar
        C.C18[i] <-  R3$Comb18[i]
      } else {C.C18[i]<- NA
      }
    }
    
    C.C18 <- C.C18[is.na(C.C18) == F] 
    C.C18 <- unique (C.C18) #Los que hay que cambiar
    
    D.C18 <- 1:(nrow(R3))
    
    for(i in 1:nrow(R3)){
      if ((R3$n.C19[i]<(nrow (dist)/C.19))&&(R3$n.C18.C19 [i]>=(ceiling (nrow(dist)/(C.18*C.19))))){   #Los que hay que cambiar
        D.C18[i] <-  R3$Comb18[i]
      } else {D.C18[i]<- NA
      }
    }
    
    D.C18 <- D.C18[is.na(D.C18) == F] 
    D.C18 <- unique (D.C18) #Los que no se pueden cambiar
    
    D.C19 <- 1:(nrow(R3))
    
    for(i in 1:nrow(R3)){
      if ((R3$n.C19[i]<(nrow (dist)/C.19))&&(R3$n.C18.C19 [i]<=(floor (nrow(dist)/(C.18*C.19))))){   #Los que hay que cambiar
        D.C19[i] <-  R3$Comb19[i]
      } else {D.C19[i]<- NA
      }
    }
    
    D.C19 <- D.C19[is.na(D.C19) == F] #Por los que hay que cambiarlos
    D.C19 <- unique (D.C19)
    
    cambiables <- C.C18[!C.C18%in%D.C18]
    
    if ((length(cambiables)==0)){   
      cambiables <-  C.C18
    } 
    
    R3 <- dplyr::select (R3, -Comb18, -Comb19)
    
    dist <- merge (dist, R3, by="Comb18_Comb19")
    
    dist$ordenar <- sample (1:nrow(dist), replace=FALSE)
    dist <- dist[with(dist, order(dist$ordenar)), ] 
    
    cont =0
    for (i in 1:nrow(dist)){
      if(cont ==1) {
        break
      }
      else if((dist$n.C19[i]>ceiling(nrow (dist)/C.19))&&
              (dist$n.C18.C19[i]>=(ceiling (nrow(dist)/(C.18*C.19))))&&
              dist$Comb18[i] %in% cambiables){
        dist$Comb19[i] <- D.C19[1]
        cont = cont + 1
      }
    }
    
    rest <- dplyr::count (dist, Comb19)
    rest 
    
    if(
      (all(rest$n[1:nrow(rest)]==(nrow (dist)/C.19)))
    )
      
    {break}
  }
  #Interacci?n Comb18_Comb19 tiene el mismo n?mero de individuos
  repeat{
    
    dist <- dplyr::select (dist, -n.rest2)
    dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")
    rest2 <- dplyr::count (dist, Comb18_Comb19)
    colnames(rest2)[colnames(rest2)=="n"] <- "n.rest2"
    dist <- merge (dist, rest2, by="Comb18_Comb19")
    dist$Comb18 <- as.character(dist$Comb18)
    
    necesitados <- 1:nrow(dist)
    nec <- 1:nrow(dist)
    for (i in 1:nrow(dist)){
      if(dist$n.rest2[i]<(dist$R5.1[i]-1)){
        necesitados[i] <- dist$Comb19[i]
        nec [i] <- dist$Comb18[i]
      }else{
        necesitados[i] <- NA
        nec[i] <- NA
      }
    }
    
    necesitados <- na.omit (necesitados)
    necesitados <- unique (necesitados)
    necesitados
    nec <- na.omit (nec)
    nec <- unique (nec)
    nec
    
    cont =0
    for (i in 1:nrow(dist)){
      if(cont ==1) {
        break
      } 
      else if((dist$n.rest2[i]>=dist$R5.1[i])&&
              (length(necesitados)!=0)&&
              (dist$Comb18[i]==nec[1])){
        dist$Comb19[i] <- sample (necesitados, 1)
        cont = cont + 1
      }
      else if((dist$n.rest2[i]>dist$R5.1[i])&&
              (length(necesitados)==0)){
        dist$Comb19[i] <- sample (Comb.19, 1)
        cont = cont + 1
      }
    }
    
    dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")
    
    R5.3 <-  dplyr::count (dist, Comb18, Comb19)
    R5.3$n <- as.numeric (R5.3$n)
    colnames(R5.3)[colnames(R5.3)=="n"] <- "R5.3"
    R.Comb18 <- dplyr::count (dist, Comb18)
    R.Comb18 <- dplyr::mutate (R.Comb18, balance = ceiling (R.Comb18$n/C.19))
    R.Comb18 <- dplyr::select (R.Comb18, -n)
    R5.3 <- merge (R5.3, R.Comb18, by ="Comb18")
    R5.3
    
    if (
      all(R5.3$R5.3[1:nrow(R5.3)]>=(R5.3$balance-1))&&
      all(R5.3$R5.3[1:nrow(R5.3)]<=R5.3$balance)
    )
      
    {break}
  }
  dplyr::count (dist, Comb19)
  
  dist <- dplyr::select (dist, -n.C18.C19, -caben, -n.caben, -caben.bueno)
  dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")
  
  dist <- merge (dist, R1.n.caben, by="Comb18_Comb19", all=TRUE)
  dist$n.caben[is.na(dist$n.caben)] <- 0  
  dist$caben <- Encl.18/C.18*Encl.19/C.19
  
  ############################ METIDO NUEVO
  for (i in 1:nrow(dist)) {
    if     (dist$caben[i]<(dist$R5.1[i]))
    {dist$caben.bueno[i] <- (dist$R5.1[i])}
    else{
      dist$caben.bueno[i] <- dist$caben[i]
    }
  }  
  
  for(i in 1:nrow(dist)){
    dist$caben.bueno[i] <- dist$caben.bueno[i]-dist$n.caben[i]
  }
  
  for (i in 1:nrow(dist)) {
    if     (dist$caben.bueno[i]<(dist$R5.1[i]-1))
    {dist$caben.bueno[i] <- (dist$R5.1[i]-1)}
  }
  ###TROZO NUEVO DE C?DIGO TUMOROSO
  for (i in 1:nrow(dist)) {
    if     (dist$caben.bueno[i]>(dist$R5.1[i]))
    {dist$caben.bueno[i] <- (dist$R5.1[i])}
  }
  ###################################
  
  R3 <-dplyr::count (dist, Comb18, Comb19)
  R3$Comb18 <- as.character (R3$Comb18)
  R3$Comb18_Comb19 <- paste(R3$Comb18, R3$Comb19, sep="_")
  R3 <- dplyr::select (R3, -Comb19, -Comb18)
  colnames(R3)[colnames(R3)=="n"] <- "n.C18.C19"
  dist <- merge (dist, R3, by="Comb18_Comb19")
  
  n.C19 <- dplyr::count (dist, Comb19)
  colnames(n.C19)[colnames(n.C19)=="n"] <- "n.C19"
  dist <- dplyr::select (dist, -n.C19)
  dist <- merge (dist, n.C19, by="Comb19")
  
  repeat{
    
    manta <- 1:nrow(dist)
    for (i in 1:nrow(dist)) {
      if((dist$n.C18.C19[i]>dist$caben.bueno[i]))
      {manta[i] <- dist$Comb18[i]}
      else
      {manta[i] <- NA}
    }
    
    manta <- na.omit(manta)
    manta <-unique (manta)
    
    if (length(manta)==0){
      manta <- 1:nrow(dist)
      for (i in 1:nrow(dist)) {
        if((dist$n.C18.C19[i]>=dist$caben.bueno[i]))
        {manta[i] <- dist$Comb18[i]}
        else
        {manta[i] <- NA}
      }
      
      manta <- na.omit(manta)
      manta <-unique (manta)
    }
    
    repeat{
      
      manta2 <- 1:nrow(dist)
      for (i in 1:nrow(dist)) {
        if((dist$Comb18[i]==manta[1])&&
           (dist$n.C18.C19[i]<dist$caben.bueno[i]))
        {manta2[i] <- dist$Comb19[i]}
        else
        {manta2[i] <- NA}
      }
      
      manta2 <- na.omit(manta2)
      manta2 <-unique (manta2)
      
      if (length(manta2)==0){
        manta <- manta [-c(1)] 
      }
      
      manta
      manta2
      
      if ((((length(manta)==0)&&
            (length (manta2)==0)))|
          (((length(manta)!=0)&&
            (length (manta2)!=0))))
      {break}
    }
    
    cascos <- dist$n.C18.C19>dist$caben.bueno
    cascos <- sum(cascos == "TRUE")
    
    if (length (manta)>0){
      
      dist$ordenar <- sample (1:nrow(dist), replace=FALSE)
      dist <- dist[with(dist, order(dist$ordenar)), ] 
      
      cont =0
      for (i in 1:nrow(dist)){
        if(cont ==1) {
          break
        }
        else if(
          (dist$Comb18[i]==manta[1])&&
          (dist$n.C18.C19[i]>dist$caben.bueno[i])
        ){
          dist$Comb19[i] <- sample (manta2, 1)
          cont = cont + 1
        }
      }
      cont
    }
    
    dist <- dplyr::select (dist, -n.C18.C19, -caben, -n.caben, -caben.bueno)
    dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")
    
    dist <- merge (dist, R1.n.caben, by="Comb18_Comb19", all=TRUE)
    dist$n.caben[is.na(dist$n.caben)] <- 0  
    dist$caben <- Encl.18/C.18*Encl.19/C.19
    
    ############################ METIDO NUEVO
    for (i in 1:nrow(dist)) {
      if     (dist$caben[i]<(dist$R5.1[i]))
      {dist$caben.bueno[i] <- (dist$R5.1[i])}
      else{
        dist$caben.bueno[i] <- dist$caben[i]
      }
    }  
    
    for(i in 1:nrow(dist)){
      dist$caben.bueno[i] <- dist$caben.bueno[i]-dist$n.caben[i]
    }
    
    for (i in 1:nrow(dist)) {
      if     (dist$caben.bueno[i]<(dist$R5.1[i]-1))
      {dist$caben.bueno[i] <- (dist$R5.1[i]-1)}
    }
    ###TROZO NUEVO DE C?DIGO TUMOROSO
    for (i in 1:nrow(dist)) {
      if     (dist$caben.bueno[i]>(dist$R5.1[i]))
      {dist$caben.bueno[i] <- (dist$R5.1[i])}
    }
    ###################################
    
    R3 <-dplyr::count (dist, Comb18, Comb19)
    R3$Comb18 <- as.character (R3$Comb18)
    R3$Comb18_Comb19 <- paste(R3$Comb18, R3$Comb19, sep="_")
    R3 <- dplyr::select (R3, -Comb19, -Comb18)
    colnames(R3)[colnames(R3)=="n"] <- "n.C18.C19"
    dist <- merge (dist, R3, by="Comb18_Comb19")
    
    n.C19 <- dplyr::count (dist, Comb19)
    colnames(n.C19)[colnames(n.C19)=="n"] <- "n.C19"
    dist <- dplyr::select (dist, -n.C19)
    dist <- merge (dist, n.C19, by="Comb19")
    
    cascos <- dist$n.C18.C19>dist$caben.bueno
    cascos <- sum(cascos == "TRUE")
    
    rest4 <- dplyr::count (dist, Comb19)  
    rest4
    
    if(
      all(dist$n.C18.C19[1:nrow(dist)]<=dist$caben.bueno[1:nrow(dist)])|
      ((length(manta) ==0)&&
       (length(manta2) ==0))
    )
    {break}}
  
  #si el numero ya es correcto  
  
  if (all(rest4$n[1:nrow(rest4)]==(nrow(dist)/C.19))){
    repeat{
      
      abrigo <- 1:nrow(dist)
      for (i in 1:nrow(dist)) {
        if(
          (dist$n.C18.C19[i]==dist$caben.bueno[i])&&
          (dist$n.caben[i]==0)
        )
        {abrigo[i] <- dist$Comb18[i]}
        else
        {abrigo[i] <- NA}
      }
      
      abrigo <- na.omit(abrigo)
      abrigo <-unique (abrigo)
      
      repeat{
        
        abrigo2 <- 1:nrow(dist)
        for (i in 1:nrow(dist)) {
          if(
            (dist$Comb18[i]==abrigo[1])&&
            (dist$n.C18.C19[i]<dist$caben.bueno[i])
          )
          {abrigo2[i] <- dist$Comb19[i]}
          else
          {abrigo2[i] <- NA}
        }
        
        abrigo2 <- na.omit(abrigo2)
        abrigo2 <-unique (abrigo2)
        
        if (length(abrigo2)==0){
          abrigo <- abrigo [-c(1)]
        }
        
        if ((length(abrigo)==0)&&
            (length (abrigo2)==0)){
          
          abrigo <- 1:nrow(dist)
          for (i in 1:nrow(dist)) {
            if(
              (dist$n.C18.C19[i]==dist$caben.bueno[i])&&
              (dist$n.caben[i]==0)
            )
            {abrigo[i] <- dist$Comb18[i]}
            else
            {abrigo[i] <- NA}
          }
          
          abrigo <- na.omit(abrigo)
          abrigo <-unique (abrigo)
          
          
          abrigo2 <- 1:nrow(dist)
          for (i in 1:nrow(dist)) {
            if((dist$Comb18[i]==abrigo[1])&&
               (dist$n.C18.C19[i]<dist$caben.bueno[i]))
            {abrigo2[i] <- dist$Comb19[i]}
            else
            {abrigo2[i] <- NA}
          }
          
          abrigo2 <- na.omit(abrigo2)
          abrigo2 <-unique (abrigo2)
        }
        
        abrigo
        abrigo2
        
        if(length(abrigo2)>0)
        {break}
      }
      
      
      
      cont =0
      for (i in 1:nrow(dist)){
        if(cont ==1) {
          break
        }
        else if(
          (dist$Comb18[i]==abrigo[1])&&
          (dist$n.C18.C19[i]==dist$caben.bueno[i])&&
          (dist$n.caben[i]==0)
        ){
          dist$Comb19[i] <- sample (abrigo2, 1)
          cont = cont + 1
        }
      }
      cont
      
      dist <- dplyr::select (dist, -n.C18.C19, -caben, -n.caben, -caben.bueno)
      dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")
      
      dist <- merge (dist, R1.n.caben, by="Comb18_Comb19", all=TRUE)
      dist$n.caben[is.na(dist$n.caben)] <- 0  
      dist$caben <- Encl.18/C.18*Encl.19/C.19
      
      ############################ METIDO NUEVO
      for (i in 1:nrow(dist)) {
        if     (dist$caben[i]<(dist$R5.1[i]))
        {dist$caben.bueno[i] <- (dist$R5.1[i])}
        else{
          dist$caben.bueno[i] <- dist$caben[i]
        }
      }  
      
      for(i in 1:nrow(dist)){
        dist$caben.bueno[i] <- dist$caben.bueno[i]-dist$n.caben[i]
      }
      
      for (i in 1:nrow(dist)) {
        if     (dist$caben.bueno[i]<(dist$R5.1[i]-1))
        {dist$caben.bueno[i] <- (dist$R5.1[i]-1)}
      }
      ###TROZO NUEVO DE C?DIGO TUMOROSO
      for (i in 1:nrow(dist)) {
        if     (dist$caben.bueno[i]>(dist$R5.1[i]))
        {dist$caben.bueno[i] <- (dist$R5.1[i])}
      }
      ###################################
      
      R3 <-dplyr::count (dist, Comb18, Comb19)
      R3$Comb18 <- as.character (R3$Comb18)
      R3$Comb18_Comb19 <- paste(R3$Comb18, R3$Comb19, sep="_")
      R3 <- dplyr::select (R3, -Comb19, -Comb18)
      colnames(R3)[colnames(R3)=="n"] <- "n.C18.C19"
      dist <- merge (dist, R3, by="Comb18_Comb19")
      
      n.C19 <- dplyr::count (dist, Comb19)
      colnames(n.C19)[colnames(n.C19)=="n"] <- "n.C19"
      dist <- dplyr::select (dist, -n.C19)
      dist <- merge (dist, n.C19, by="Comb19")
      
      
      rest4 <- dplyr::count (dist, Comb19)
      rest4
      
      if(
        (any(rest4$n[1:nrow(rest)]!=(nrow (dist)/C.19)))
      )
        
      {break}
    }
  }
  
  if (any(rest4$n[1:nrow(rest4)]!=(nrow(dist)/C.19))){
    
    repeat{
      
      abrigo <- 1:nrow(dist)
      for (i in 1:nrow(dist)) {
        if(
          (dist$n.C18.C19[i]==dist$caben.bueno[i])&&
          (dist$n.caben[i]==0)&&
          (dist$n.C19[i]>(nrow(dist)/C.19))
        )
        {abrigo[i] <- dist$Comb18[i]}
        else
        {abrigo[i] <- NA}
      }
      
      abrigo <- na.omit(abrigo)
      abrigo <-unique (abrigo)
      
      repeat{
        
        abrigo2 <- 1:nrow(dist)
        for (i in 1:nrow(dist)) {
          if(
            (dist$Comb18[i]==abrigo[1])&&
            (dist$n.C18.C19[i]<dist$caben.bueno[i])&&
            (dist$n.C19[i]<(nrow(dist)/C.19))
          )
          {abrigo2[i] <- dist$Comb19[i]}
          else
          {abrigo2[i] <- NA}
        }
        
        abrigo2 <- na.omit(abrigo2)
        abrigo2 <-unique (abrigo2)
        
        if (length(abrigo2)==0){
          abrigo <- abrigo [-c(1)]
        }
        
        if ((length(abrigo)==0)&&
            (length (abrigo2)==0)){
          
          abrigo <- 1:nrow(dist)
          for (i in 1:nrow(dist)) {
            if(
              (dist$n.C18.C19[i]==dist$caben.bueno[i])&&
              (dist$n.caben[i]==0)&&
              (dist$n.C19[i]>(nrow(dist)/C.19))
            )
            {abrigo[i] <- dist$Comb18[i]}
            else
            {abrigo[i] <- NA}
          }
          
          abrigo <- na.omit(abrigo)
          abrigo <-unique (abrigo)
          
          
          abrigo2 <- 1:nrow(dist)
          for (i in 1:nrow(dist)) {
            if((dist$Comb18[i]==abrigo[1])&&
               (dist$n.C18.C19[i]<dist$caben.bueno[i]))
            {abrigo2[i] <- dist$Comb19[i]}
            else
            {abrigo2[i] <- NA}
          }
          
          abrigo2 <- na.omit(abrigo2)
          abrigo2 <-unique (abrigo2)
        }
        
        abrigo
        abrigo2
        
        if(length(abrigo2)>0)
        {break}
      }
      
      
      
      cont =0
      for (i in 1:nrow(dist)){
        if(cont ==1) {
          break
        }
        else if(
          (dist$Comb18[i]==abrigo[1])&&
          (dist$n.C18.C19[i]==dist$caben.bueno[i])&&
          (dist$n.caben[i]==0)&&
          (dist$n.C19[i]>(nrow(dist)/C.19))
        ){
          dist$Comb19[i] <- sample (abrigo2, 1)
          cont = cont + 1
        }
      }
      cont
      
      dist <- dplyr::select (dist, -n.C18.C19, -caben, -n.caben, -caben.bueno)
      dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")
      
      dist <- merge (dist, R1.n.caben, by="Comb18_Comb19", all=TRUE)
      dist$n.caben[is.na(dist$n.caben)] <- 0  
      dist$caben <- Encl.18/C.18*Encl.19/C.19
      
      ############################ METIDO NUEVO
      for (i in 1:nrow(dist)) {
        if     (dist$caben[i]<(dist$R5.1[i]))
        {dist$caben.bueno[i] <- (dist$R5.1[i])}
        else{
          dist$caben.bueno[i] <- dist$caben[i]
        }
      }  
      
      for(i in 1:nrow(dist)){
        dist$caben.bueno[i] <- dist$caben.bueno[i]-dist$n.caben[i]
      }
      
      for (i in 1:nrow(dist)) {
        if     (dist$caben.bueno[i]<(dist$R5.1[i]-1))
        {dist$caben.bueno[i] <- (dist$R5.1[i]-1)}
      }
      ###TROZO NUEVO DE C?DIGO TUMOROSO
      for (i in 1:nrow(dist)) {
        if     (dist$caben.bueno[i]>(dist$R5.1[i]))
        {dist$caben.bueno[i] <- (dist$R5.1[i])}
      }
      ###################################
      
      R3 <-dplyr::count (dist, Comb18, Comb19)
      R3$Comb18 <- as.character (R3$Comb18)
      R3$Comb18_Comb19 <- paste(R3$Comb18, R3$Comb19, sep="_")
      R3 <- dplyr::select (R3, -Comb19, -Comb18)
      colnames(R3)[colnames(R3)=="n"] <- "n.C18.C19"
      dist <- merge (dist, R3, by="Comb18_Comb19")
      
      n.C19 <- dplyr::count (dist, Comb19)
      colnames(n.C19)[colnames(n.C19)=="n"] <- "n.C19"
      dist <- dplyr::select (dist, -n.C19)
      dist <- merge (dist, n.C19, by="Comb19")
      
      
      rest <- dplyr::count (dist, Comb19)
      
      if(
        (all(rest$n[1:nrow(rest)]==(nrow (dist)/C.19)))
      )
        
      {break}
    }
    
    R5.3 <-  dplyr::count (dist, Comb18, Comb19)
    R5.3$n <- as.numeric (R5.3$n)
    colnames(R5.3)[colnames(R5.3)=="n"] <- "R5.3"
    R.Comb18 <- dplyr::count (dist, Comb18)
    R.Comb18 <- dplyr::mutate (R.Comb18, balance = ceiling (R.Comb18$n/C.19))
    R.Comb18 <- dplyr::select (R.Comb18, -n)
    R5.3 <- merge (R5.3, R.Comb18, by ="Comb18")
    R5.3
    rest4 <- dplyr::count (dist, Comb19)  
    rest4
    
  }
  
  repeat{
    
    dist$Comb18 <- as.character(dist$Comb18)
    dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")
    
    
    R5.3.1 <- dplyr::count (dist, Comb18, Comb19)
    colnames(R5.3.1)[colnames(R5.3.1)=="n"] <- "R5.3"
    R5.3.1$Comb18_Comb19 <- paste(R5.3$Comb18, R5.3$Comb19, sep="_")
    R5.3.1 <- dplyr::select (R5.3.1, Comb18_Comb19, R5.3)
    dist <- dplyr::select (dist, -R5.3)
    dist <- merge (dist, R5.3.1, by="Comb18_Comb19")
    
    cargador <- 1:nrow(ind.Comb.Encl)
    for(i in 1:nrow(ind.Comb.Encl)){  
      if (ind.Comb.Encl$n[i]%%C.19==0){   
        cargador[i] <- ind.Comb.Encl$Comb18[i]
      } else {   
        cargador[i] <-  NA
      }
    }
    
    cargador <- na.omit (cargador)
    cargador <- unique (cargador)
    cargador
    
    dolar <- 1:nrow(dist)
    for(i in 1:nrow(dist)){  
      if ((dist$R5.3[i]==dist$caben.bueno[i])&&
          (dist$Comb18[i]%in%cargador)&&
          (dist$Comb18_Comb19[i] %in% R1.1)){   
        dist$botella[i] <-  1
        dolar[i] <- dist$Comb18[i]
      } else {   
        dist$botella[i] <-  0
        dolar[i] <- NA}
    }
    
    dolar <- na.omit (dolar)
    dolar <- unique (dolar)
    
    sum (dist$botella)
    
    ponibles <- 1:nrow (dist)
    for(i in 1:nrow(dist)){  
      if ((dist$R5.3[i]<= (floor (nrow(dist)/(C.18*C.19))))&&
          (dist$Comb18[i] %in% dolar)){   
        ponibles[i] <-  dist$Comb19[i]
      } else {   
        ponibles[i] <-  NA}
    }
    ponibles <- na.omit (ponibles)
    ponibles <- unique (ponibles)
    ponibles
    
    sum (dist$botella)
    
    contando <- 0
    for (i in 1:nrow(dist)){
      if((contando == 1)|
         sum (dist$botella)<2){
        break
      }
      else if((dist$botella[i]==1)){
        dist$Comb19[i] <-  sample (ponibles, 1)
        contando <- contando + 1 
      }
    }
    contando
    
    
    
    
    dist <- dplyr::select (dist, -n.C18.C19, -caben, -n.caben, -caben.bueno)
    dist$Comb18_Comb19 <- paste(dist$Comb18, dist$Comb19, sep="_")
    
    dist <- merge (dist, R1.n.caben, by="Comb18_Comb19", all=TRUE)
    dist$n.caben[is.na(dist$n.caben)] <- 0  
    dist$caben <- Encl.18/C.18*Encl.19/C.19
    
    ############################ METIDO NUEVO
    for (i in 1:nrow(dist)) {
      if     (dist$caben[i]<(dist$R5.1[i]))
      {dist$caben.bueno[i] <- (dist$R5.1[i])}
      else{
        dist$caben.bueno[i] <- dist$caben[i]
      }
    }  
    
    for(i in 1:nrow(dist)){
      dist$caben.bueno[i] <- dist$caben.bueno[i]-dist$n.caben[i]
    }
    
    for (i in 1:nrow(dist)) {
      if     (dist$caben.bueno[i]<(dist$R5.1[i]-1))
      {dist$caben.bueno[i] <- (dist$R5.1[i]-1)}
    }
    ###TROZO NUEVO DE C?DIGO TUMOROSO
    for (i in 1:nrow(dist)) {
      if     (dist$caben.bueno[i]>(dist$R5.1[i]))
      {dist$caben.bueno[i] <- (dist$R5.1[i])}
    }
    ###################################
    
    R3 <-dplyr::count (dist, Comb18, Comb19)
    R3$Comb18 <- as.character (R3$Comb18)
    R3$Comb18_Comb19 <- paste(R3$Comb18, R3$Comb19, sep="_")
    R3 <- dplyr::select (R3, -Comb19, -Comb18)
    colnames(R3)[colnames(R3)=="n"] <- "n.C18.C19"
    dist <- merge (dist, R3, by="Comb18_Comb19")
    
    n.C19 <- dplyr::count (dist, Comb19)
    colnames(n.C19)[colnames(n.C19)=="n"] <- "n.C19"
    dist <- dplyr::select (dist, -n.C19)
    dist <- merge (dist, n.C19, by="Comb19")
    
    
    R5.3 <-  dplyr::count (dist, Comb18, Comb19)
    R5.3$n <- as.numeric (R5.3$n)
    colnames(R5.3)[colnames(R5.3)=="n"] <- "R5.3"
    R.Comb18 <- dplyr::count (dist, Comb18)
    R.Comb18 <- dplyr::mutate (R.Comb18, balance = ceiling (R.Comb18$n/C.19))
    R.Comb18 <- dplyr::select (R.Comb18, -n)
    R5.3 <- merge (R5.3, R.Comb18, by ="Comb18")
    R5.3
    rest4 <- dplyr::count (dist, Comb19)  
    rest4
    
    
    if(sum(dist$botella)<= 1)
      
    {break}
  }
  
  
  if(
    all(R5.3$R5.3[1:nrow(R5.3)]>=(R5.3$balance-1))&&
    all(R5.3$R5.3[1:nrow(R5.3)]<=R5.3$balance)&&
    all(rest4$n[1:nrow(rest4)]==(nrow (dist)/C.19))&&
    (sum(dist$botella)<= 1)
  )
    
  {break}
}

###############################################
################################################
##### Arreglar el color #####

repeat{
  ##### Arreglar los color- al derecho #####
  
  dist.prueba <- dist
  vector.color <- dplyr::count (dist.prueba, Color2)
  vector.color <- vector.color[with(vector.color, order(vector.color$n)), ] 
  
  vector.color <- vector.color$Color2
  vector.color <- as.character(vector.color)
  vector.color <- vector.color[1:(length(vector.color)-1)]
  
  mando <- 0
  
  repeat{
    
    color <- dist %>%
      dplyr::group_by (Comb19) %>%
      dplyr::summarise (sum(WY), sum(WO), sum(WW), sum(YY), sum(YO), sum(OO))
    colnames.color <- c ("Comb19", "WY", "WO", "WW", "YY", "YO", "OO")
    colnames(color) <- colnames.color
    color
    
    
    ok <- 1:nrow(color)
    for(i in 1:nrow(color)){
      if ((color[i, vector.color[1]]>=floor(sum(dist[vector.color[1]])/C.19))&
          (color[i, vector.color[1]]<=ceiling(sum(dist[vector.color[1]])/C.19))){
        ok[i] <- 0
      } else {
        ok[i] <- 1
      }
    }
    
    color
    ok
    
    if (sum(ok)>0){
      
      color$ok <- 0
      leche <- 1:nrow(color)
      leche[1:nrow(color)] <- NA
      for(i in 1:nrow(color)){
        if        (all(color[, vector.color[1]]!=floor(sum(dist[vector.color[1]])/C.19))&
                   (color[i, vector.color[1]]!=floor(sum(dist[vector.color[1]])/C.19))){
          color$ok[i] <- 1
          leche[i] <- color$Comb19[i]
        } else if (any(color[, vector.color[1]]>ceiling(sum(dist[vector.color[1]])/C.19))&
                   (color[i, vector.color[1]]>ceiling(sum(dist[vector.color[1]])/C.19))){
          color$ok[i] <- 1
          leche[i] <- color$Comb19[i]
        } else if (all(color[, vector.color[1]]<=ceiling(sum(dist[vector.color[1]])/C.19))&
                   (color[i, vector.color[1]]==ceiling(sum(dist[vector.color[1]])/C.19))){
          color$ok[i] <- 1
          leche[i] <- color$Comb19[i]
        }
      }
      color
      
      leche <- na.omit (leche)
      leche <- sample (leche)
      leche
      
      cafe <- 1:nrow(color)
      for(i in 1:nrow(color)){
        if (any(color[, vector.color[1]]<floor(sum(dist[vector.color[1]])/C.19))&
            (color[i, vector.color[1]]<floor(sum(dist[vector.color[1]])/C.19))){
          cafe[i] <- color$Comb19[i]
        } else if (all(color[, vector.color[1]]>=floor(sum(dist[vector.color[1]])/C.19))&
                   (color[i, vector.color[1]]==floor(sum(dist[vector.color[1]])/C.19))){
          cafe[i] <- color$Comb19[i]
        } else {
          cafe[i] <- NA
        }
      }
      
      cafe <- na.omit(cafe)
      cafe <- sample (cafe, replace=FALSE)
      cafe
      
      okis <- as.vector (rbind (cafe[1], leche[1]))
      okis
      
      luz <- color$Comb19
      luz <- luz[!luz%in%okis]
      okis <- luz
      
      dist.color <- dist
      repeat{
        if (length(okis)==0){
          break
        } else {
          dist.color <- dist.color[dist.color$Comb19 != okis [1], ] 
          okis <- okis[-c(1)]
        }
      }
      dist.color <- dplyr::filter (dist.color, Y>0)
      dplyr::count (dist.color, Comb19)
      
      hartura <-  100   
      
      repeat {
        
        dist.color4 <- dplyr::select (dist.color, fila, Encl18, Comb19)
        
        dist.color4 <- dist.color4 %>%
          dplyr::group_by (Encl18)
        dist.color4 <- dplyr::mutate (dist.color4, Comb19.2 = sample(Comb19))
        
        dist.color$Comb19.2 <- dist.color4$Comb19.2
        
        dist.color <- dplyr::select (dist.color, -Comb19)
        colnames (dist.color) [colnames(dist.color)=="Comb19.2"] <- "Comb19"
        
        colnames (dist.color) [colnames(dist.color)=="Comb19"] <- "Comb19.3"
        Comb19.3 <- dplyr::select (dist.color, fila, Comb19.3)
        dist <- merge (dist, Comb19.3, by="fila", all=TRUE)
        dist$Comb19.3[is.na(dist$Comb19.3)] <- 0  
        colnames (dist.color) [colnames(dist.color)=="Comb19.3"] <- "Comb19"
        
        for(i in 1:nrow(dist)){
          if ((dist$Comb19.3[i]=="0")){   
            dist$Comb19.3[i] <-  dist$Comb19[i]
          }
        }
        dist <- dplyr::select (dist, -Comb19)
        colnames (dist) [colnames(dist)=="Comb19.3"] <- "Comb19"
        dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
        
        
        ### NO FALTAR?A AQU? CREAR LOS COLORES???? ###
        
        color <- dist %>%
          dplyr::group_by (Comb19) %>%
          dplyr::summarise (sum(WY), sum(WO), sum(WW), sum(YY), sum(YO), sum(OO))
        colnames.color <- c ("Comb19", "WY", "WO", "WW", "YY", "YO", "OO")
        colnames(color) <- colnames.color
        color
        
        ok <- 1:nrow(color)
        for(i in 1:nrow(color)){
          if ((color[i, vector.color[1]]>=floor(sum(dist[vector.color[1]])/C.19))&
              (color[i, vector.color[1]]<=ceiling(sum(dist[vector.color[1]])/C.19))){
            ok[i] <- 0
          } else {
            ok[i] <- 1
          }
        }
        
        color
        ok
        
        
        if (sum(ok)>=1){
          
          alex <- 1:nrow(color)
          for(i in 1:nrow(color)){
            if ((any(color[, vector.color[1]]<floor(sum(dist[vector.color[1]])/C.19)))&
                (color[i, vector.color[1]]<floor(sum(dist[vector.color[1]])/C.19))){
              alex[i] <- color$Comb19[i]
            } else if ((all(color[, vector.color[1]]>=floor(sum(dist[vector.color[1]])/C.19)))&
                       (color[i, vector.color[1]]<=floor(sum(dist[vector.color[1]])/C.19))){
              alex[i] <- color$Comb19[i]
            } else {
              alex[i] <- NA
            }
          }
          alex <- na.omit (alex)
          alex <- sample (alex, replace=FALSE)
          alex
          
          rebe <- 1:nrow(color)
          for(i in 1:nrow(color)){
            if ((any(color[, vector.color[1]]<floor(sum(dist[vector.color[1]])/C.19)))&
                (color[i, vector.color[1]]>=ceiling(sum(dist[vector.color[1]])/C.19))){
              rebe[i] <- color$Comb19[i]
            } else if ((all(color[, vector.color[1]]>=floor(sum(dist[vector.color[1]])/C.19)))&
                       (color[i, vector.color[1]]>ceiling(sum(dist[vector.color[1]])/C.19))){
              rebe[i] <- color$Comb19[i]
            }else {
              rebe[i] <- NA
            }
          }
          rebe <- na.omit (rebe)
          rebe <- sample (rebe, replace=FALSE)
          rebe
          
          okis <- as.vector (rbind (alex[1], rebe[1]))
          okis
          
          luz <- color$Comb19
          luz <- luz[!luz%in%okis]
          okis <- luz
          
          dist.color <- dist
          repeat{
            if(length(okis)==0){
              break
            } else {
              dist.color <- dist.color[dist.color$Comb19 != okis [1], ] 
              okis <- okis[-c(1)]
            }
          }
          dplyr::count (dist.color, Comb19)
          
          
          
        }
        
        color
        ok
        dplyr::count (dist.color, Comb19)
        
        hartura <- hartura + 1 
        print (hartura)
        
        if ((sum(ok)==0)|
            (hartura ==110))
        {break}
      }
    }  
    
    vector.color[1]
    color
    
    dist.menos <- dist 
    dist.menos <- dplyr::select (dist.menos, fila, Comb19)
    
    colnames (dist.menos) [colnames(dist.menos)=="Comb19"] <- "Comb19.2"
    
    dist.prueba <- merge (dist.prueba, dist.menos, by="fila", all=TRUE)
    dist.prueba$Comb19.2[is.na(dist.prueba$Comb19.2)] <- 0  
    
    for(i in 1:nrow(dist.prueba)){
      if ((dist.prueba$Comb19.2[i]=="0")){   
        dist.prueba$Comb19.2[i] <-  dist.prueba$Comb19[i]
      }
    }
    
    dist.prueba <- dplyr::select (dist.prueba, -Comb19)
    colnames (dist.prueba) [colnames(dist.prueba)=="Comb19.2"] <- "Comb19"
    dist.prueba$Comb18_Comb19 <- paste (dist.prueba$Comb18, dist.prueba$Comb19, sep="_")
    
    dist <- dist[dist$Color2 != vector.color[1], ]  
    
    vector.color <- vector.color[-c(1)]
    vector.color
    
    mando <- mando+1
    print (mando)
    
    if (length(vector.color)==0)
    {break}
  }
  
  
  dist <- dist.prueba
  
  color <- dist %>%
    dplyr::group_by (Comb19) %>%
    dplyr::summarise (sum(WY), sum(WO), sum(WW), sum(YY), sum(YO), sum(OO))
  colnames.color <- c ("Comb19", "WY", "WO", "WW", "YY", "YO", "OO")
  colnames(color) <- colnames.color
  color
  
  
  ##### Arreglar los color - a la inversa #####
  
  dist.prueba <- dist
  vector.color <- dplyr::count (dist.prueba, Color2)
  vector.color <- vector.color[with(vector.color, order(-vector.color$n)), ] 
  
  vector.color <- vector.color$Color2
  vector.color <- as.character(vector.color)
  vector.color <- vector.color[1:(length(vector.color)-1)]
  
  mando <- 0
  
  repeat{
    
    color <- dist %>%
      dplyr::group_by (Comb19) %>%
      dplyr::summarise (sum(WY), sum(WO), sum(WW), sum(YY), sum(YO), sum(OO))
    colnames.color <- c ("Comb19", "WY", "WO", "WW", "YY", "YO", "OO")
    colnames(color) <- colnames.color
    color
    
    ok <- 1:nrow(color)
    for(i in 1:nrow(color)){
      if ((color[i, vector.color[1]]>=floor(sum(dist[vector.color[1]])/C.19))&
          (color[i, vector.color[1]]<=ceiling(sum(dist[vector.color[1]])/C.19))){
        ok[i] <- 0
      } else {
        ok[i] <- 1
      }
    }
    
    color
    ok
    
    if (sum(ok)>0){
      
      color$ok <- 0
      leche <- 1:nrow(color)
      leche[1:nrow(color)] <- NA
      for(i in 1:nrow(color)){
        if        (all(color[, vector.color[1]]!=floor(sum(dist[vector.color[1]])/C.19))&
                   (color[i, vector.color[1]]!=floor(sum(dist[vector.color[1]])/C.19))){
          color$ok[i] <- 1
          leche[i] <- color$Comb19[i]
        } else if (any(color[, vector.color[1]]>ceiling(sum(dist[vector.color[1]])/C.19))&
                   (color[i, vector.color[1]]>ceiling(sum(dist[vector.color[1]])/C.19))){
          color$ok[i] <- 1
          leche[i] <- color$Comb19[i]
        } else if (all(color[, vector.color[1]]<=ceiling(sum(dist[vector.color[1]])/C.19))&
                   (color[i, vector.color[1]]==ceiling(sum(dist[vector.color[1]])/C.19))){
          color$ok[i] <- 1
          leche[i] <- color$Comb19[i]
        }
      }
      color
      
      leche <- na.omit (leche)
      leche <- sample (leche)
      leche
      
      cafe <- 1:nrow(color)
      for(i in 1:nrow(color)){
        if (any(color[, vector.color[1]]<floor(sum(dist[vector.color[1]])/C.19))&
            (color[i, vector.color[1]]<floor(sum(dist[vector.color[1]])/C.19))){
          cafe[i] <- color$Comb19[i]
        } else if (all(color[, vector.color[1]]>=floor(sum(dist[vector.color[1]])/C.19))&
                   (color[i, vector.color[1]]==floor(sum(dist[vector.color[1]])/C.19))){
          cafe[i] <- color$Comb19[i]
        } else {
          cafe[i] <- NA
        }
      }
      
      cafe <- na.omit(cafe)
      cafe <- sample (cafe, replace=FALSE)
      cafe
      
      okis <- as.vector (rbind (cafe[1], leche[1]))
      okis
      
      luz <- color$Comb19
      luz <- luz[!luz%in%okis]
      okis <- luz
      
      dist.color <- dist
      repeat{
        if (length(okis)==0){
          break
        } else {
          dist.color <- dist.color[dist.color$Comb19 != okis [1], ] 
          okis <- okis[-c(1)]
        }
      }
      dplyr::count (dist.color, Comb19)
      
      hartura <-  100   
      hartura <-  100   
      
      repeat {
        
        dist.color4 <- dplyr::select (dist.color, fila, Encl18, Comb19)
        
        dist.color4 <- dist.color4 %>%
          dplyr::group_by (Encl18)
        dist.color4 <- dplyr::mutate (dist.color4, Comb19.2 = sample(Comb19))
        
        dist.color$Comb19.2 <- dist.color4$Comb19.2
        
        dist.color <- dplyr::select (dist.color, -Comb19)
        colnames (dist.color) [colnames(dist.color)=="Comb19.2"] <- "Comb19"
        
        colnames (dist.color) [colnames(dist.color)=="Comb19"] <- "Comb19.3"
        Comb19.3 <- dplyr::select (dist.color, fila, Comb19.3)
        dist <- merge (dist, Comb19.3, by="fila", all=TRUE)
        dist$Comb19.3[is.na(dist$Comb19.3)] <- 0  
        colnames (dist.color) [colnames(dist.color)=="Comb19.3"] <- "Comb19"
        
        for(i in 1:nrow(dist)){
          if ((dist$Comb19.3[i]=="0")){   
            dist$Comb19.3[i] <-  dist$Comb19[i]
          }
        }
        dist <- dplyr::select (dist, -Comb19)
        colnames (dist) [colnames(dist)=="Comb19.3"] <- "Comb19"
        dist$Comb18_Comb19 <- paste (dist$Comb18, dist$Comb19, sep="_")
        
        
        ### NO FALTAR?A AQU? CREAR LOS COLORES???? ###
        
        color <- dist %>%
          dplyr::group_by (Comb19) %>%
          dplyr::summarise (sum(WY), sum(WO), sum(WW), sum(YY), sum(YO), sum(OO))
        colnames.color <- c ("Comb19", "WY", "WO", "WW", "YY", "YO", "OO")
        colnames(color) <- colnames.color
        color
        
        ok <- 1:nrow(color)
        for(i in 1:nrow(color)){
          if ((color[i, vector.color[1]]>=floor(sum(dist[vector.color[1]])/C.19))&
              (color[i, vector.color[1]]<=ceiling(sum(dist[vector.color[1]])/C.19))){
            ok[i] <- 0
          } else {
            ok[i] <- 1
          }
        }
        
        color
        ok
        
        
        if (sum(ok)>=1){
          
          alex <- 1:nrow(color)
          for(i in 1:nrow(color)){
            if ((any(color[, vector.color[1]]<floor(sum(dist[vector.color[1]])/C.19)))&
                (color[i, vector.color[1]]<floor(sum(dist[vector.color[1]])/C.19))){
              alex[i] <- color$Comb19[i]
            } else if ((all(color[, vector.color[1]]>=floor(sum(dist[vector.color[1]])/C.19)))&
                       (color[i, vector.color[1]]<=floor(sum(dist[vector.color[1]])/C.19))){
              alex[i] <- color$Comb19[i]
            } else {
              alex[i] <- NA
            }
          }
          alex <- na.omit (alex)
          alex <- sample (alex, replace=FALSE)
          alex
          
          rebe <- 1:nrow(color)
          for(i in 1:nrow(color)){
            if ((any(color[, vector.color[1]]<floor(sum(dist[vector.color[1]])/C.19)))&
                (color[i, vector.color[1]]>=ceiling(sum(dist[vector.color[1]])/C.19))){
              rebe[i] <- color$Comb19[i]
            } else if ((all(color[, vector.color[1]]>=floor(sum(dist[vector.color[1]])/C.19)))&
                       (color[i, vector.color[1]]>ceiling(sum(dist[vector.color[1]])/C.19))){
              rebe[i] <- color$Comb19[i]
            }else {
              rebe[i] <- NA
            }
          }
          rebe <- na.omit (rebe)
          rebe <- sample (rebe, replace=FALSE)
          rebe
          
          okis <- as.vector (rbind (alex[1], rebe[1]))
          okis
          
          luz <- color$Comb19
          luz <- luz[!luz%in%okis]
          okis <- luz
          
          dist.color <- dist
          repeat{
            if(length(okis)==0){
              break
            } else {
              dist.color <- dist.color[dist.color$Comb19 != okis [1], ] 
              okis <- okis[-c(1)]
            }
          }
          dplyr::count (dist.color, Comb19)
          
          
          
        }
        
        color
        ok
        dplyr::count (dist.color, Comb19)
        
        hartura <- hartura + 1 
        print (hartura)
        
        if ((sum(ok)==0)|
            (hartura ==110))
        {break}
      }
    }  
    
    vector.color[1]
    color
    
    dist.menos <- dist 
    dist.menos <- dplyr::select (dist.menos, fila, Comb19)
    
    colnames (dist.menos) [colnames(dist.menos)=="Comb19"] <- "Comb19.2"
    
    dist.prueba <- merge (dist.prueba, dist.menos, by="fila", all=TRUE)
    dist.prueba$Comb19.2[is.na(dist.prueba$Comb19.2)] <- 0  
    
    for(i in 1:nrow(dist.prueba)){
      if ((dist.prueba$Comb19.2[i]=="0")){   
        dist.prueba$Comb19.2[i] <-  dist.prueba$Comb19[i]
      }
    }
    
    dist.prueba <- dplyr::select (dist.prueba, -Comb19)
    colnames (dist.prueba) [colnames(dist.prueba)=="Comb19.2"] <- "Comb19"
    dist.prueba$Comb18_Comb19 <- paste (dist.prueba$Comb18, dist.prueba$Comb19, sep="_")
    
    dist <- dist[dist$Color2 != vector.color[1], ]  
    
    vector.color <- vector.color[-c(1)]
    vector.color
    
    mando <- mando+1
    print (mando)
    
    if (length(vector.color)==0)
    {break}
  }
  
  
  dist <- dist.prueba
  
  color <- dist %>%
    dplyr::group_by (Comb19) %>%
    dplyr::summarise (sum(WY), sum(WO), sum(WW), sum(YY), sum(YO), sum(OO))
  colnames.color <- c ("Comb19", "WY", "WO", "WW", "YY", "YO", "OO")
  colnames(color) <- colnames.color
  color
  
  ###### hasta aqu? #####
  
  color1 <- color
  color1 <- dplyr::select (color1, -Comb19) 
  ok <- color1
  
  for (i in 1:nrow(color1)){
    for (j in 1:ncol (color1)){
      if ((color1[i, j]>=floor(sum(color1[j])/C.19))&
          (color1[i, j]<=ceiling(sum(color1[j])/C.19))){
        ok[i, j] <- 0
      } else {
        ok[i, j] <- 1
      }
    }
  }
  
  if (sum(ok)==0)
  {break}
}

color
ok

# Script completed successfully
cat("\n=== SCRIPT COMPLETED SUCCESSFULLY ===\n", file = stderr())

################################################