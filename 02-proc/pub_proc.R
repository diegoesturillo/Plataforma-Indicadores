# Procesamiento nueva BD Publicaciones USACH----
# Instalar 'pacman' si no lo estuviera
if(!require("pacman")) install.packages("pacman")

# Instalar resto de paquetes----
pacman::p_load(here, janitor, readxl, tidyverse)

# Cargar base de datos----
pub <- read_excel(here("01-input", "data-orig", "pubs_rut_15_07_2025.xlsx"))

# Limpieza inicial----
pub <- pub %>%
  janitor::clean_names() %>%
  select(-c(rut, dv)) %>%
  rename(año = ano) %>%
  filter(año >= 2018)

# Guardar
save(pub, file = "01-input/data-proc/publicaciones.RData")
