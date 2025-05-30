# Procesamiento BD Encuesta LGBTQIA+----
# Instalar 'pacman' si no lo estuviera
if(!require("pacman")) install.packages("pacman")

# Instalar resto de paquetes----
pacman::p_load(here, janitor, labelled, readxl, tidyverse)

# Cargar base de datos----
bd_lgbt <- read_excel(here("01-input", "data-orig", "BD_LGBT.xlsx"))

# Limpieza BD----
# Versión alfanumérica----
bd_lgbt_1 <- bd_lgbt %>%
  janitor::clean_names() %>%
  select(-c(otro_ident_gen, otro_orient_sex)) %>%
  mutate(ident_gen = recode(ident_gen,
                            "Trans masculino" = "Transmasculino",
                            "Trans femenino" = "Transfemenino",
                            "Género diverso (no binario, género fluido)" = "Género diverso",
                            "Prefiero no decir mi género" = "Prefiero no decir")) %>% 
  set_variable_labels(
    ident_gen = "Género",
    orient_sex = "Orientación sexual",
    pert_grup = "Estamento de pertenencia",
    pert_um = "Unidad mayor de pertenencia",
    x5_1 = "Usach entrega apoyo psicosocial a personas LGBT",
    x5_2 = "Usach promueve la salud sexual y reproductiva",
    x5_3 = "Infraestructura acorde para personas LGBT",
    x5_4 = "Usach aborda temáticas de interés LGBT",
    x5_5 = "Prevención y sensibilización considera personas LGBT",
    x5_6 = "Cursos específicos de formación LGBT",
    x5_7 = "Usach espacio seguro personas LGBT",
    x6_1 = "Usach respetan derechos personas LGBT",
    x6_2 = "Discriminación Usach por ser LGBT",
    x6_3 = "Discriminación en el 2023 en la Usach por ser LGBT",
    x7_1 = "Experimentar insultos por ser LGBT",
    x7_2 = "Experimentar gritos por ser LGBT",
    x7_3 = "Experimentar hostigamiento por ser LGBT",
    x7_4 = "Le ridiculizaron por ser LGTB",
    x7_5 = "Le obligaron a cambiar su apariencia por ser LGBT",
    x7_6 = "Le trataron sin respetar el género",
    x7_7 = "Le sancionaron por su identidad de género",
    x7_8 = "Le negaron o dificultaron el derecho a registrar su identidad",
    x7_9 = "Le acosaron sexualmente",
    x7_10 = "Le agredieron físicamente",
    x8 = "Me siento conforme con la vida universitaria en la Usach",
    x9 = "Comentarios"
  )

# Versión procesada----
bd_lgbt_2 <- bd_lgbt %>%
  janitor::clean_names() %>%
  select(-c(otro_ident_gen, otro_orient_sex)) %>%
  mutate(ident_gen = recode(ident_gen,
                            "Trans masculino" = "Transmasculino",
                            "Trans femenino" = "Transfemenino",
                            "Género diverso (no binario, género fluido)" = "Género diverso",
                            "Prefiero no decir mi género" = "Prefiero no decir")) %>%
  mutate(across(c(x5_1, x5_2, x5_3, x5_4, x5_5, x5_6, x5_7), 
                ~ case_when(
                  . == 0 ~ "No sabe/No contesta",
                  . == 1 ~ "Muy en desacuerdo",
                  . == 2 ~ "En desacuerdo",
                  . == 3 ~ "De acuerdo",
                  . == 4 ~ "Muy de acuerdo",
                  T ~ NA_character_
                ))) %>%
  mutate(across(c(x6_1, x6_2, x6_3, x7_1, x7_2, x7_3, x7_4, x7_5, x7_6, x7_7, x7_8, x7_9, x7_10),
                ~ case_when(
                  . == 0 ~ "No aplica",
                  . == 1 ~ "Sí",
                  . == 2 ~ "No",
                  T ~ NA_character_
                ))) %>%
  mutate(x8 = case_when(
    x8 == 0 ~ "No aplica",
    x8 == 1 ~ "Muy disconforme",
    x8 == 2 ~ "Disconforme",
    x8 == 3 ~ "Ni conforme ni disconforme",
    x8 == 4 ~ "Conforme",
    x8 == 5 ~ "Muy conforme",
    T ~ NA_character_
  )) %>%
    set_variable_labels(
    ident_gen = "Género",
    orient_sex = "Orientación sexual",
    pert_grup = "Estamento de pertenencia",
    pert_um = "Unidad mayor de pertenencia",
    x5_1 = "Usach entrega apoyo psicosocial a personas LGBT",
    x5_2 = "Usach promueve la salud sexual y reproductiva",
    x5_3 = "Infraestructura acorde para personas LGBT",
    x5_4 = "Usach aborda temáticas de interés LGBT",
    x5_5 = "Prevención y sensibilización considera personas LGBT",
    x5_6 = "Cursos específicos de formación LGBT",
    x5_7 = "Usach espacio seguro personas LGBT",
    x6_1 = "Usach respetan derechos personas LGBT",
    x6_2 = "Discriminación Usach por ser LGBT",
    x6_3 = "Discriminación en el 2023 en la Usach por ser LGBT",
    x7_1 = "Experimentar insultos por ser LGBT",
    x7_2 = "Experimentar gritos por ser LGBT",
    x7_3 = "Experimentar hostigamiento por ser LGBT",
    x7_4 = "Le ridiculizaron por ser LGTB",
    x7_5 = "Le obligaron a cambiar su apariencia por ser LGBT",
    x7_6 = "Le trataron sin respetar el género",
    x7_7 = "Le sancionaron por su identidad de género",
    x7_8 = "Le negaron o dificultaron el derecho a registrar su identidad",
    x7_9 = "Le acosaron sexualmente",
    x7_10 = "Le agredieron físicamente",
    x8 = "Me siento conforme con la vida universitaria en la Usach",
    x9 = "Comentarios"
  )

# Guardar como .RData----
save(bd_lgbt_1, bd_lgbt_2,
     file = "01-input/data-proc/bd_lgbt.RData")
