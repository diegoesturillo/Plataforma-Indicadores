# Procesamiento BD Encuesta EMA----
# Instalar 'pacman' si no lo estuviera
if(!require("pacman")) install.packages("pacman")

# Instalar resto de paquetes----
pacman::p_load(here, readxl, tidyverse)

# Cargar distintas hojas del excel consolidado de la Encuesta Mujeres en la Academia----
archivo <- "01-input/data-orig/CONSOLIDADO EMA 2018-2024_USACH.xlsx"
hojas <- excel_sheets("01-input/data-orig/CONSOLIDADO EMA 2018-2024_USACH.xlsx")
datos <- map(hojas, ~ read_excel(archivo, sheet = .x)) %>% 
  set_names(hojas)

# Guardar
save(datos, file = "01-input/data-proc/datos_ema.RData")

