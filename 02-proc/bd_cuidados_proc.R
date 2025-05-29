# Procesamiento BD Encuesta Cuidados y Corresponsabilidad----
# Instalar 'pacman' si no lo estuviera
if(!require("pacman")) install.packages("pacman")

# Instalar resto de paquetes----
pacman::p_load(here, readxl, tidyverse)

# Cargar datos del excel original----
cuidados <- read_excel(here("01-input", "data-orig", "BD_CUIDADOS_REC.xlsx"))

# Guardar
save(cuidados, file = "01-input/data-proc/bd_cuidados.RData")
