# Ajuste BD titulados----
# Cargar paquetes
pacman::p_load(here, readxl, tidyverse)

# Cargar BD----
titulados <- read_excel(here("01-input", "data-orig", "TITULADO_2007-2024_web_19_05_2025_E.xlsx"))

titulacion <- titulados %>%
  janitor::clean_names() %>%
  rename(año = ano, 
         titulacion_total = total_titulaciones,
         titulacion_mujeres = titulaciones_mujeres_por_programa,
         titulacion_hombres = titulaciones_hombres_por_programa
  ) %>%
  mutate(año = recode(año,
                      "TIT_2024" = 2024,
                      "TIT_2023" = 2023,
                      "TIT_2022" = 2022,
                      "TIT_2021" = 2021,
                      "TIT_2020" = 2020,
                      "TIT_2019" = 2019,
                      "TIT_2018" = 2018,
                      "TIT_2017" = 2017,
                      "TIT_2016" = 2016,
                      "TIT_2015" = 2015,
                      "TIT_2014" = 2014,
                      "TIT_2013" = 2013,
                      "TIT_2012" = 2012,
                      "TIT_2011" = 2011,
                      "TIT_2010" = 2010,
                      "TIT_2009" = 2009,
                      "TIT_2008" = 2008,
                      "TIT_2007" = 2007)) %>%
  filter(!str_detect(nombre_institucion, "CONVENIO"),
         codigo_institucion == 71,
         nivel_global == "Pregrado",
         tipo_de_plan_de_la_carrera == "Plan Regular",
         año >= 2018)

# Dataframes agrupados
# Por año
titulados_general_pre <- titulacion %>%
  group_by(año) %>%
  summarise(
    total_titulados = sum(titulacion_total, na.rm = T),
    titulados_mujeres = sum(titulacion_mujeres, na.rm = T),
    titulados_hombres = sum(titulacion_hombres, na.rm = T),
    prop_fem = round((titulados_mujeres/total_titulados)*100, 0),
    prop_mas = round((titulados_hombres/total_titulados)*100, 0),
    .groups = 'drop'
  )

# Por año y área del conocimiento
titulados_ac_pre <- titulacion %>%
  group_by(año, area_del_conocimiento) %>%
  summarise(
    total_titulados = sum(titulacion_total, na.rm = T),
    titulados_mujeres = sum(titulacion_mujeres, na.rm = T),
    titulados_hombres = sum(titulacion_hombres, na.rm = T),
    prop_fem = round((titulados_mujeres/total_titulados)*100, 0),
    prop_mas = round((titulados_hombres/total_titulados)*100, 0),
    .groups = 'drop'
  )

# Guardar
save(titulados_general_pre, titulados_ac_pre,
     file = "01-input/data-proc/titulados.RData")
