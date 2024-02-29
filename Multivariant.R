# ==============================================================================
# [AMD]       01_lecturaDades.R
# 
# Autor:      , (c) 22.02.2024
# Llençament: 
#     source(".../01_lecturaDades.R")
# ==============================================================================
# Carreguem les llibreries necesaries
library(psych)
library(ggplot2)  

# ==============================================================================
# Llegim la bbdd
path <- "C:/Users/ignasi.juncadella/Downloads/"
fitxer <- "datos.csv"
dades <- read.csv(paste0(path, fitxer)); cat("S'han llegit les dades./n")
# ==============================================================================
# cambiem el nom de la bbdd
#colnames(dades) <- c("", "", "", "")
# ==============================================================================
# Guardem la bbdd
fitxerFin <- "airlines.RData"
save(dades, file = paste0(path, fitxerFin)); cat("S'ha guardat les dades en", paste0(path, fitxerFin), "/n")

# ==============================================================================
# Eliminació de la columna ID

dades <- dades[,-1]
# ==============================================================================
# Transformació de 3 variables discretes a contínues

food <- dades[,12]
dec1 <- runif(length(food), min = 0, max = 0.99)
food <- food + dec1

dades$food_and_drink <- food

online <- dades[,13]
dec2 <- runif(length(food), min = 0, max = 0.99)
online <- online + dec2

dades$online_boarding <- online

seat <- dades[,14]
dec3 <- runif(length(food), min = 0, max = 0.99)
seat <- seat + dec3
dades$seat_comfort <- seat

# ==============================================================================
# Passar a numèrica una variable llegida com a caràcter

dades$arrival_delay_in_minutes <- as.numeric(dades$arrival_delay_in_minutes)

# ==============================================================================
# Declaració d'una nova variable

dades$flight_time_difference <- dades$departure_delay_in_minutes -
  dades$arrival_delay_in_minutes
dades <- dades[, -22] #eliminem la vartiable arrival_delay_in_minutes
# ==============================================================================
# Identificar NAs

# Inicializar el vector NAs con ceros
NAs <- rep(0, ncol(dades))

# Iterar sobre las columnas del dataframe
for (i in 1:ncol(dades)) {
  for (j in 1:nrow(dades)) {
    if (is.na(dades[j, i])) {
      NAs[i] <- NAs[i] + 1
    }
  }
  cat("\n La variable", names(dades)[i], "contiene", NAs[i], "missing(s).\n")
}
# ==============================================================================
# Imputació NAs

#dades <- 
  
  # ==============================================================================
# Passar a factor les variables categòriques

dades$Gender <- as.factor(dades$Gender)
dades$customer_type <- as.factor(dades$customer_type)
dades$type_of_travel <- as.factor(dades$type_of_travel)
dades$customer_class <- as.factor(dades$customer_class)
dades$inflight_wifi_service <- as.factor(dades$inflight_wifi_service)
dades$departure_arrival_time_convenient <- as.factor(dades$departure_arrival_time_convenient)
dades$ease_of_online_booking <- as.factor(dades$ease_of_online_booking)
dades$gate_location <- as.factor(dades$gate_location)
dades$inflight_entertainment <- as.factor(dades$inflight_entertainment)
dades$onboard_service <- as.factor(dades$onboard_service)
dades$leg_room_service <- as.factor(dades$leg_room_service)
dades$baggage_handling <- as.factor(dades$baggage_handling)
dades$checkin_service <- as.factor(dades$checkin_service)
dades$inflight_service <- as.factor(dades$inflight_service)
dades$cleanliness <- as.factor(dades$cleanliness)
dades$satisfaction <- as.factor(dades$satisfaction)


# ==============================================================================
# Crear vectors amb els noms de les variables numèriques i categòriques

format <- sapply(dades, class)

varNum <- names(format)[which(format %in% c("numeric","integer"))]
varCat <- colnames(dades)[which(!colnames(dades) %in% varNum)]

# ==============================================================================
# Descriptiva univariant de les variables categòriques

for(vC in varCat) {
  # Creem la taula 
  tabla <- table(dades[, vC])
  cat(vC, "\n")
  cat(tabla, "\n")
  
  # creamos el gráfico correspondiente
  tabla <- data.frame(tabla)
  
  grafic <- ggplot(data= tabla, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=Var1), vjust=-0.3, size=3.5)+
    theme_minimal()
  print(grafic)
}

# ==============================================================================
# Descriptiva univariant de les variables numèriques

for(vC in varNum){
  cat("Resum estadístic de la variable", vC, "\n")
  print(summary(dades[, vC]))
  cat("\n")
  
  hist(dades[, vC], main = paste0("Histograma de la variable ", vC), col = "skyblue")
}

# Univariante: 
## numerica
### histograma (si es normal boxplot)
### summary de los valores (describe paquete psych)
## categorico
### barplot
### tabla de frecuencias (absoluta o relativo)

# Bivariante: 
## num vs numerico
### scatterplot 
### correlaciones
## num vs categorico
### histograma multiple o boxplot multiple (si es normal)
### summary de los valores (describeBy paquete psych)
## cat vs cat
### barplot multiple
### tabla de contigencia (table de dos variables) y test chi quadrado 


# ==============================================================================
# IMPUTACIO 
library(VIM)
aggr(dades, numbers = T, sortVar = T)

mcar(dades)

imputed_data1 <- mice(dades,m = 5,
                      maxit = 20, method = "pmm",seed = 2018)
complete.data1 <- mice::complete(imputed_data1)

plot(density(complete.data1$flight_time_difference))
plot(density(dades$flight_time_difference, na.rm = T))
