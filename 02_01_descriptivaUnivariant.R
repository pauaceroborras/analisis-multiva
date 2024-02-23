# ==============================================================================
# [AMD]       01_lecturaDades.R
# 
# Autor:      , (c) 22.02.2024
# Llençament: 
#     source(".../01_lecturaDades.R")
# ==============================================================================
# Carreguem les llibreries necesaries


# ==============================================================================
# Llegim la bbdd
path <- "C:/Users/ignasi.juncadella/Downloads/"
fitxer <- "datos.csv"
dades <- read.csv(paste0(path, fitxer)); cat("S'han llegit les dades./n")

# ==============================================================================
# cambiem el nom de la bbdd
colnames(dades) <- c("", "", "", "")

# ==============================================================================
# Guardem la bbdd
fitxerFin <- "airlines.RData"
save(dades, file = paste0(path, fitxerFin)); cat("S'ha guardat les dades en", paste0(path, fitxerFin), "/n")

# ==============================================================================

# ==============================================================================
# [AMD]       02_01_descriptivaUnivariant.R
# 
# Autor:      Ignasi Juncadella, (c) 22.02.2024
# Llençament: 
#     source(".../02_01_descriptivaUnivariant.R")
# ==============================================================================
# Carreguem les llibreries necesaries
library(psych)
library(ggplot2)  

# ==============================================================================
# Llegim la bbdd
path <- "C:/Users/ignasi.juncadella/Downloads/"
fitxer <- "airlines.RData"
dades <- get(load(paste0(path, fitxer)))

# ==============================================================================
# Transformar 3 variables categòriques a numèriques

food <- dades[,12]
dec <- runif(length(food), min = 0, max = 0.99)
food <- food + dec

dades$food_and_drink <- food

online <- dades[,13]
dec <- runif(length(food), min = 0, max = 0.99)
online <- online + dec

dades$online_boarding <- online

seat <- dades[,14]
dec <- runif(length(food), min = 0, max = 0.99)
seat <- seat + dec

dades$seat_comfort <- seat

# ==============================================================================
#Fiquem bé el format de les variables

dades<-dades[,-1]
dades$departure_delay_in_minutes<-as.numeric(dades$departure_delay_in_minutes)
dades$inflight_wifi_service<-as.factor(dades$inflight_wifi_service)
dades$departure_arrival_time_convenient<-as.factor(dades$departure_arrival_time_convenient)
dades$ease_of_online_booking<-as.factor(dades$ease_of_online_booking)
dades$age<-as.numeric(dades$age)
dades$flight_distance<-as.numeric(dades$flight_distance)
dades$age<-as.numeric(dades$age)

dades$satisfaction<-as.factor(dades$satisfaction)


# ------------------------------------------------------------------------------
# Detectem el format de cada variable
format <- sapply(dades, class)

varNum <- names(format)[which(format %in% c("numeric"))]
varCat <- colnames(dades)[which(!colnames(dades) %in% varNum)]

# ==============================================================================
# Comencem amb la descriptiva de les variables categoriques

for(vC in varCat) {
  # Creem la taula 
  tabla <- table(dades[, vC])
  cat(vC, "\n")
  cat(tabla, "\n")
  
  # creamos el grásfico correspondiente
  tabla <- data.frame(tabla)
  
  grafic <- ggplot(data= tabla, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=Var1), vjust=-0.3, size=3.5)+
    theme_minimal()
  print(grafic)
}


# ==============================================================================
#Seguim amb la descriptiva de les variables numèriques
#???

for(vn in varNum) {
  # Creem la taula 
  tabla1 <- table(dades[, vn])
  cat(vn, "\n")
  cat(tabla1, "\n")
  
  # creamos el grásfico correspondiente
  tabla1 <- data.frame(tabla)
  
  grafic <- ggplot(data= tabla, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=Var1), vjust=-0.3, size=3.5)+
    theme_minimal()
  print(grafic)
}
# ==============================================================================
