
# Lectura de la base de dades

fitxer <- "datos.csv"
dades <- read.csv2(fitxer, sep = ",")

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

# Crear vectors amb els noms de les variables numèriques i categòriques

format <- sapply(dades, class)

varNum <- names(format)[which(format %in% c("numeric","integer"))]
varCat <- colnames(dades)[which(!colnames(dades) %in% varNum)]

# Descriptiva univariant de les variables categòriques

for(vC in varCat) {
  taula <- table(dades[, vC])
  cat("Taula de freqüències de la variable", vC, "\n")
  print(taula)
  cat("\n")
  
  barplot(taula, main = paste0("Gràfic de barres de la variable ", vC), 
          ylab = "Freqüència", col = "skyblue")
}

# Descriptiva univariant de les variables numèriques

for(vC in varNum){
  cat("Resum estadístic de la variable", vC, "\n")
  print(summary(dades[, vC]))
  cat("\n")
  
  hist(dades[, vC], main = paste0("Histograma de la variable ", vC), col = "skyblue")
}