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
dades <- get(load(paste0(pah, fitxer)))

# ------------------------------------------------------------------------------
# Detectem el format de cada variable
format <- sapply(dades, class)

varNum <- names(format)[which(format %in% c("factor", "integer"))]
varCat <- colnames(dades)[which(!colnames(dades) %in% varCat)]

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
