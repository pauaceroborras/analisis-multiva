# ==============================================================================
# [AMD]       01_lecturaDades.R
# 
# Autor:      Ignasi Juncadella, (c) 22.02.2024
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
