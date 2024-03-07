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
# ==============================================================================
# Descriptiva bivariant, categorica vs categorica, numerica vs numerica, categorica vs numerica y numerica vs categorica

library(RColorBrewer)
var <- colnames(dades)
for(i in var){
  
  for(j in var){
    nombre_archivo <- paste0("grafico_", i,j, ".png")
    if(i != j && which(var == i) < which(var == j)){
      if(i %in% varCat && j %in% varCat){
        if (i %in% varCat && j %in% varCat) {
          cat("Taula bivariant de la variable", i, "i la variable", j, "\n")
          taula <- table(dades[[i]], dades[[j]])
          print(taula)
          cat("\n")
          
          pvalor <- chisq.test(dades[[i]], dades[[j]])$p.value
          cat("El p-valor del test chi-quadrat és: ", pvalor, "\n")
          
          png(nombre_archivo, width = 800, height = 600)
          nombres <- levels(dades[[i]])
          colores_barras <- brewer.pal(length(nombres), "Blues") 
          barras <- barplot(taula, xlab = i, ylab = j, col = colores_barras)
          legend("topright", legend = nombres, fill = colores_barras, title = "Grupos")
          
          dev.off()
        }
        
      }
      if(i %in% varNum && j %in% varNum && i != j){
        correlacion <- cor(dades[[i]],dades[[j]], use = "complete.obs")
        png(nombre_archivo, width = 800, height = 600)
        plot(dades[[i]], dades[[j]], xlab = i, ylab = j, col = "skyblue")
        text(x = max(dades[[i]])*0.8, y = max(dades[[j]])*0.9, 
             labels = paste("Correlación:", round(correlacion, 4)), 
             pos = 4, col = "red", cex = 1.2)
        dev.off()
        cat("Gráfico", i, "guardado como", nombre_archivo, "\n")
      }
      if(i %in% varNum && j %in% varCat && i != j){
        #cat(i," ");cat(j,"\n")
      }
      if(i %in% varCat && j %in%varNum){
        #cat(i," ");cat(j,"\n")
      }
    }
  }
}

# ==============================================================================
# Descriptiva bivariant categorica vs numèrica:

var <- colnames(dades)

for(i in var){
  for(j in var){
    nombre_archivo <- paste0("grafico_", i,j, ".png")
    if(i != j && which(var == i) < which(var == j)){
      if(i %in% varNum && j %in% varCat && i != j){
          
         cat("Anàlisi bivariant de la variable", i, "i la variable", j, "\n")
         taula <- describeBy(dades[[i]],group=dades[[j]])
         print(taula)
         
          cat("\n")
          
          grafic <- ggplot(dades, aes(x = dades[[i]], fill =  dades[[j]])) +
            geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
            labs(title = "Histograma Múltiple de Variable Numérica por Categoría",
                 x = i,
                 y = "Freqüència",
                 fill=j)
          print(grafic)
      }
    }
  }
}
# ==============================================================================
# SELECCIÓ DE COLUMNES
dadesfinals <- dades[,-c(7,8,10,14,16,17,18,19,20)]
colnames(dadesfinals) <- c("Genere","Tipus_client","Edat","Tipus_viatge",
                           "Classe_client","Distancia","Booking","Menjar",
                           "Embarcament","Comoditat","Servei","Retard_sortida",
                           "Satsifaccio","Diferencia_durada")


# ==============================================================================
# SELECCIÓ DE FILES
keep <- which(is.na(dadesfinals$Diferencia_durada))
dadesnoNAs <- dadesfinals[-keep,]
dadesnoNAs <- dadesnoNAs[sample(nrow(dadesnoNAs),2700),]
dadesNAs <- dadesfinals[keep,]
dadesfinals <- rbind(dadesnoNAs,dadesNAs)
dadesfinals <- dadesfinals[sample(nrow(dadesfinals)),]


# ==============================================================================
# IMPUTACIÓ 
aggr(dadesfinals, numbers = T, sortVar = T)

mcar(dadesfinals)

imputed_data1 <- mice(dadesfinals,m = 5,
                      maxit = 20, method = "pmm",seed = 2018)
complete.data1 <- mice::complete(imputed_data1)

plot(density(complete.data1$Diferencia_durada))
plot(density(dadesfinals$Diferencia_durada, na.rm = T))

dadesfinals <- complete.data1

# ==============================================================================
# Gràfics categòriques vs numèriques

ggplot(dadesfinals, aes(x = age, fill =  customer_type)) +
          geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
          labs(title = "Histograma Múltiple de Variable Numérica por Categoría",
                     x = "Edat",
                     y = "Freqüència") 
          +     theme_minimal()

# ==============================================================================
# Crear vectors amb els noms de les variables numèriques i categòriques

format <- sapply(dadesfinals, class)

varNum <- names(format)[which(format %in% c("numeric","integer"))]
varCat <- colnames(dadesfinals)[which(!colnames(dadesfinals) %in% varNum)]

# ==============================================================================
# Descriptiva univariant de les variables categòriques

for(vC in varCat) {
  # Creem la taula 
  tabla <- table(dadesfinals[, vC])
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
  print(summary(dadesfinals[, vC]))
  cat("\n")
  
  hist(dadesfinals[, vC], main = paste0("Histograma de la variable ", vC), col = "skyblue")
}
# ==============================================================================
# Descriptiva bivariant, categorica vs categorica, numerica vs numerica, categorica vs numerica y numerica vs categorica

library(RColorBrewer)
var <- colnames(dadesfinals)
for(i in var){
  
  for(j in var){
    nombre_archivo <- paste0("grafico_", i,j, ".png")
    if(i != j && which(var == i) < which(var == j)){
      if(i %in% varCat && j %in% varCat){
        if (i %in% varCat && j %in% varCat) {
          cat("Taula bivariant de la variable", i, "i la variable", j, "\n")
          taula <- table(dadesfinals[[i]], dadesfinals[[j]])
          print(taula)
          cat("\n")
          
          pvalor <- chisq.test(dadesfinals[[i]], dadesfinals[[j]])$p.value
          cat("El p-valor del test chi-quadrat és: ", pvalor, "\n")
          
          png(nombre_archivo, width = 800, height = 600)
          nombres <- levels(dadesfinals[[i]])
          colores_barras <- brewer.pal(length(nombres), "Blues") 
          barras <- barplot(taula, xlab = i, ylab = j, col = colores_barras)
          legend("topright", legend = nombres, fill = colores_barras, title = "Grupos")
          
          dev.off()
        }
        
      }
      if(i %in% varNum && j %in% varNum && i != j){
        correlacion <- cor(dadesfinals[[i]],dadesfinals[[j]], use = "complete.obs")
        png(nombre_archivo, width = 800, height = 600)
        plot(dadesfinals[[i]], dadesfinals[[j]], xlab = i, ylab = j, col = "skyblue")
        text(x = max(dadesfinals[[i]])*0.8, y = max(dadesfinals[[j]])*0.9, 
             labels = paste("Correlación:", round(correlacion, 4)), 
             pos = 4, col = "red", cex = 1.2)
        dev.off()
        cat("Gráfico", i, "guardado como", nombre_archivo, "\n")
      }
      if(i %in% varNum && j %in% varCat && i != j){
        #cat(i," ");cat(j,"\n")
      }
      if(i %in% varCat && j %in%varNum){
        #cat(i," ");cat(j,"\n")
      }
    }
  }
}

# ==============================================================================
var <- colnames(dades)

for(i in var){
  for(j in var){
    nombre_archivo <- paste0("grafico_", i,j, ".png")
    if(i != j && which(var == i) < which(var == j)){
      if(i %in% varNum && j %in% varCat && i != j){
          
         cat("Anàlisi bivariant de la variable", i, "i la variable", j, "\n")
         taula <- describeBy(dadesfinals[[i]],group=dadesfinals[[j]])
         print(taula)
         
          cat("\n")
          
          grafic <- ggplot(dadesfinals, aes(x = dadesfinals[[i]], fill =  dadesfinals[[j]])) +
            geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
            labs(title = "Histograma Múltiple de Variable Numérica por Categoría",
                 x = i,
                 y = "Freqüència",
                 fill=j)
          print(grafic)
      }
    }
  }
}
