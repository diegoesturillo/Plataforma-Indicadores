# Procesamiento BD Matrícula USACH----
# Carga de paquetes
pacman::p_load(here, readxl, tidyverse)

# Cargar datos----
bd <- read_excel(here("01-input", "data-orig", "Matricula_2007_2025_WEB_15_07_2025.xlsx"))

# Limpieza----
matricula <- bd %>%
  janitor::clean_names() %>%
  rename(año = ano, 
         matricula_total = total_matricula,
         matricula_mujer_total = total_matricula_mujeres,
         matricula_hombre_total = total_matricula_hombres,
         matricula_total_1año = total_matricula_primer_ano,
         matricula_mujer_1año = total_matricula_mujeres_primer_ano,
         matricula_hombre_1año = total_matricula_hombres_primer_ano
  ) %>%
  mutate(año = recode(año,
                      "MAT_2025" = 2025,
                      "MAT_2024" = 2024,
                      "MAT_2023" = 2023,
                      "MAT_2022" = 2022,
                      "MAT_2021" = 2021,
                      "MAT_2020" = 2020,
                      "MAT_2019" = 2019,
                      "MAT_2018" = 2018,
                      "MAT_2017" = 2017,
                      "MAT_2016" = 2016,
                      "MAT_2015" = 2015,
                      "MAT_2014" = 2014,
                      "MAT_2013" = 2013,
                      "MAT_2012" = 2012,
                      "MAT_2011" = 2011,
                      "MAT_2010" = 2010,
                      "MAT_2009" = 2009,
                      "MAT_2008" = 2008,
                      "MAT_2007" = 2007)) %>%
  filter(!str_detect(nombre_institucion, "CONVENIO"),
         codigo_de_institucion == 71,
         nivel_global == "Pregrado",
         tipo_de_plan_de_la_carrera == "Plan Regular",
         año >= 2018)

# Dataframes agrupados
# Por año
matricula_año <- matricula %>%
  group_by(año) %>%
  summarise(
    mt = sum(matricula_total, na.rm = T),
    mt_f = sum(matricula_mujer_total, na.rm = T),
    mt_m = sum(matricula_hombre_total, na.rm = T),
    m1 = sum(matricula_total_1año, na.rm = T),
    m1_f = sum(matricula_mujer_1año, na.rm = T),
    m1_m = sum(matricula_hombre_1año, na.rm = T),
    .groups = 'drop'
  )

# Por área del conocimiento genérica
matricula_ac <- matricula %>%
  group_by(año, area_del_conocimiento) %>%
  summarise(
    mt = sum(matricula_total, na.rm = T),
    mt_f = sum(matricula_mujer_total, na.rm = T),
    mt_m = sum(matricula_hombre_total, na.rm = T),
    m1 = sum(matricula_total_1año, na.rm = T),
    m1_f = sum(matricula_mujer_1año, na.rm = T),
    m1_m = sum(matricula_hombre_1año, na.rm = T),
    .groups = 'drop'
  )

# Guardar
save(matricula_año, matricula_ac,
     file = "01-input/data-proc/matricula usach 2018-2025.RData")
