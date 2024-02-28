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
# Eliminació de la columna ID

dades <- dades[,-1]

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

# Passar a numèrica una variable llegida com a caràcter

dades$arrival_delay_in_minutes <- as.numeric(dades$arrival_delay_in_minutes)

# Declaració d'una nova variable

dades$flight_time_difference <- dades$departure_delay_in_minutes -
  dades$arrival_delay_in_minutes

dades <- dades[, -22]

# Eliminar NAs

dades <- dades[-(which(is.na(dades$flight_time_difference))), ]

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
# Descriptiva univariant de les variables numèriques

for(vN in varNum){
  cat("Resum estadístic de la variable", vN, "\n")
  print(summary(dades[, vN]))
  cat("\n")
  
  hist(dades[, vN], main = paste0("Histograma de la variable ", vN), col = "skyblue")
}
# ==============================================================================
