# Gráficos en formato plotly
# Carga de paquetes
pacman::p_load(here, plotly, RColorBrewer, readxl, scales, tidyverse)

# Cargar datos----
# Cargar lista original
load(here("01-input", "data-proc", "datos_ema.RData"))

# Para género y grado académico
ema <- datos$ACAD_GRADO
acad_grados <- datos$ACAD_GRADO

# Para jerarquías académicas
ema_jq <- datos$ACAD_JQ

# Para cantidad de publicaciones académicas y por área de conocimiento
ema_pub <- datos$ACAD_PUB
ema_pub_ac <- datos$ACAD_PUB_AC

# Para puestos directivos
ema_puestos <- datos$ACAD_DIRECCION

# Para matrícula de pregrado por género
ema_mat <- datos$MAT_PRE_GENERAL

# Para matrícula de pregrado por género y área de conocimiento
ema_mat_area <- datos$MAT_PRE_AC

# Para titulación por género y área de conocimiento
load(here("01-input", "data-proc", "titulados.RData"))

# Para cuidados y corresponsabilidad
load(here("01-input", "data-proc", "bd_cuidados.RData"))


# Gráfico. Cantidad de académicos por género en la USACH (2018-2024)----
# Calcular proporciones
academicos_usach <- ema %>%
  select(año, n_acad_total, n_acad_f, n_acad_m) %>%
  mutate(
    prop_f = round((n_acad_f/n_acad_total)*100, 0),
    prop_m = round((n_acad_m/n_acad_total)*100, 0)
  ) %>%
  pivot_longer(
    cols = c(n_acad_f, n_acad_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero, "n_acad_f" = "Femenino", "n_acad_m" = "Masculino"),
    texto_tooltip = paste0(
      "<b>USACH</b><br>Año: ", año,
      "<br>Género: ", genero,
      "<br>Cantidad: ", format(cantidad, big.mark = ".", decimal.mark = ","),
      "<br>Proporción: ", ifelse(genero == "Femenino", prop_f, prop_m), "%"
    )
  )

# Graficar
p1 <- plot_ly() %>%
  # Trazo USACH (Mujeres)
  add_trace(
    data = filter(academicos_usach, genero == "Femenino"),
    x = ~año,
    y = ~cantidad,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(color = "#8D69F3", width = 3),
    marker = list(color = "#8D69F3", size = 8),
    name = "Femenino",
    text = ~texto_tooltip,
    hoverinfo = 'text'
  ) %>%
  # Trazo USACH (Hombres)
  add_trace(
    data = filter(academicos_usach, genero == "Masculino"),
    x = ~año,
    y = ~cantidad,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(color = "#41776E", width = 3),
    marker = list(color = "#41776E", size = 8),
    name = "Masculino",
    text = ~texto_tooltip,
    hoverinfo = 'text'
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 1. Estamento académico por género (2018-2024)</b>",
      font = list(size = 12.5),
      x = 0.5
    ),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = "", tickformat = ".,0"),
    legend = list(
      title = list(text = "<b>Género</b>"),
      orientation = "v",
      x = 1,
      y = 0.5,
      xanchor = "left"
    ),
    margin = list(t = 80),
    hovermode = "closest"
  )

p1

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Cantidad de académicos por género y grado académico----
# Preparar datos - grado doctorado----
acad_grado_doc <- acad_grados %>%
  pivot_longer(
    cols = c(n_acad_doc_f, n_acad_doc_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "n_acad_doc_f" = "Femenino",
                    "n_acad_doc_m" = "Masculino")) %>%
  group_by(año) %>%
  mutate(total_anual = sum(cantidad, na.rm = T),
         proporcion = round((cantidad/total_anual)*100, 0)) %>%
  ungroup()

# Graficar
p2 <- plot_ly(
  data = acad_grado_doc,
  x = ~año,
  y = ~cantidad,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
  type = 'scatter',
  mode = 'lines+markers',
  text = ~paste("<b>USACH</b><br>",
  "<b>Año:</b> ", año, "<br>",
  "<b>Género:</b> ", genero, "<br>",
  "<b>Cantidad:</b> ", cantidad, "<br>",
  "<b>Proporción:</b> ", proporcion, "%"),
  hoverinfo = 'text',
  name = ~genero
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 2. Estamento académico con doctorado (2018-2024)</b>",
      font = list(size = 12.5),
      x = 0.5
    ),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = "", tickformat = ".,0"),
    legend = list(
      title = list(text = "<b>Género</b>"),
      orientation = "v",
      x = 1,
      y = 0.5,
      xanchor = "left"
    ),
    margin = list(t = 80),
    hovermode = "closest"
  )

p2

# Preparar datos - grado magíster----
acad_grado_mg <- acad_grados %>%
  select(año, n_acad_mg_f, n_acad_mg_m)%>%
  pivot_longer(
    cols = c(n_acad_mg_f, n_acad_mg_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "n_acad_mg_f" = "Femenino",
                    "n_acad_mg_m" = "Masculino")) %>%
  group_by(año) %>%
  mutate(total_anual = sum(cantidad, na.rm = T),
         proporcion = round((cantidad/total_anual)*100, 0)) %>%
  ungroup()

# Graficar
p3 <- plot_ly(
  data = acad_grado_mg,
  x = ~año,
  y = ~cantidad,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
  type = 'scatter',
  mode = 'lines+markers',
  text = ~paste("<b>USACH</b><br>",
                "<b>Año:</b> ", año, "<br>",
                "<b>Género:</b> ", genero, "<br>",
                "<b>Cantidad:</b> ", cantidad, "<br>",
                "<b>Proporción:</b> ", proporcion, "%"),
  hoverinfo = 'text',
  name = ~genero
  
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 3. Estamento académico con magíster (2018-2024)</b>",
      font = list(size = 12.5),
      x = 0.5
    ),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = "", tickformat = ".,0"),
    legend = list(
      title = list(text = "<b>Género</b>"),
      orientation = "v",
      x = 1,
      y = 0.5,
      xanchor = "left"
    ),
    margin = list(t = 80),
    hovermode = "closest"
  )

p3

# Preparar datos - grado licenciado----
acad_grado_lic <- acad_grados %>%
  select(año, n_acad_lic_f, n_acad_lic_m)%>%
  pivot_longer(
    cols = c(n_acad_lic_f, n_acad_lic_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "n_acad_lic_f" = "Femenino",
                    "n_acad_lic_m" = "Masculino")) %>%
  group_by(año) %>%
  mutate(total_anual = sum(cantidad, na.rm = T),
         proporcion = round((cantidad/total_anual)*100, 0)) %>%
  ungroup()

# Graficar
p4 <- plot_ly(
  data = acad_grado_lic,
  x = ~año,
  y = ~cantidad,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
  type = 'scatter',
  mode = 'lines+markers',
  text = ~paste("<b>USACH</b><br>",
                "<b>Año:</b> ", año, "<br>",
                "<b>Género:</b> ", genero, "<br>",
                "<b>Cantidad:</b> ", cantidad, "<br>",
                "<b>Proporción:</b> ", proporcion, "%"),
  hoverinfo = 'text',
  name = ~genero
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 4. Estamento académico con título profesional y/o licenciatura (2018-2024)</b>",
      font = list(size = 12.5),
      x = 0.5
    ),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = "", tickformat = ".,0"),
    legend = list(
      title = list(text = "<b>Género</b>"),
      orientation = "v",
      x = 1,
      y = 0.5,
      xanchor = "left"
    ),
    margin = list(t = 80),
    hovermode = "closest"
  )

p4

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Académicos por género y jerarquía - 3 jerarquías----
# Preparar datos
academicos_jerarquia <- ema_jq %>%
  pivot_longer(
    cols = starts_with("n_acad_"),
    names_to = "categoria",
    values_to = "valor") %>%
  mutate(
    genero = case_when(
      str_ends(categoria, "_f") ~ "Mujeres",
      str_ends(categoria, "_m") ~ "Hombres",
      T ~ NA_character_),
    jerarquia = case_when(
      str_detect(categoria, "titular") ~ "Titular",
      str_detect(categoria, "asoc") ~ "Asociado/a",
      str_detect(categoria, "asis") ~ "Asistente",
      str_detect(categoria, "inst") ~ "Instructor/a",
      str_detect(categoria, "adj") ~ "Adjunto/a",
      str_detect(categoria, "sj") ~ "Sin jerarquía",
      T ~ NA_character_),
    tipo = case_when(
      str_detect(categoria, "_f|_m") ~ "detalle",
      T ~ "total")) %>%
  group_by(año, jerarquia) %>%
  mutate(
    total_jerarquia = sum(valor[tipo == "detalle"], na.rm = T),
    proporcion = round((valor/total_jerarquia)*100, 0)) %>%
  ungroup() %>%
  filter(tipo == "detalle") %>%
  rename(cantidad = valor) %>%
  select(año, genero, jerarquia, cantidad, proporcion) %>%
  mutate(
    grupo = interaction(jerarquia, genero, sep = " ", lex.order = T),
    jerarquia = factor(jerarquia, levels = c("Titular", "Asociado/a", "Asistente", "Instructor/a", "Adjunto/a", "Sin jerarquía")),
    genero = factor(genero, levels = c("Mujeres", "Hombres"))) %>%
  filter(jerarquia %in% c("Titular", "Asociado/a", "Asistente")) %>%
  mutate(
    grupo = factor(
      interaction(jerarquia, genero, sep = " "),
      levels = c(
        "Titular Mujeres", "Titular Hombres",
        "Asociado/a Mujeres", "Asociado/a Hombres",
        "Asistente Mujeres", "Asistente Hombres")))

# Graficar
p5 <- academicos_jerarquia %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,  
    color = ~grupo,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c(
      "Titular Mujeres" = "#6A5ACD",
      "Titular Hombres" = "#41776E",
      "Asociado/a Mujeres" = "#A07DF5",
      "Asociado/a Hombres" = "#5C918A",
      "Asistente Mujeres" = "#B598F7",
      "Asistente Hombres" = "#76ACA4"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Jerarquía:</b> ", jerarquia, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Cantidad:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(
    title = list(
      text = "<b>Gráfico 5. Estamento académico por género y jerarquía (2018-2024)</b>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Jerarquía y género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

p5

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Académicos por género y jerarquía - 3 jerarquías----
# Preparar datos
academicos_jerarquia_2 <- ema_jq %>%
  pivot_longer(
    cols = starts_with("n_acad_"),
    names_to = "categoria",
    values_to = "valor"
  ) %>%
  mutate(
    genero = case_when(
      str_ends(categoria, "_f") ~ "Mujeres",
      str_ends(categoria, "_m") ~ "Hombres",
      T ~ NA_character_
    ),
    jerarquia = case_when(
      str_detect(categoria, "titular") ~ "Titular",
      str_detect(categoria, "asoc") ~ "Asociado/a",
      str_detect(categoria, "asis") ~ "Asistente",
      str_detect(categoria, "inst") ~ "Instructor/a",
      str_detect(categoria, "adj") ~ "Adjunto/a",
      str_detect(categoria, "sj") ~ "Sin jerarquía",
      T ~ NA_character_
    ),
    tipo = case_when(
      str_detect(categoria, "_f|_m") ~ "detalle",
      T ~ "total"
    )
  ) %>%
  group_by(año, jerarquia) %>%
  mutate(
    total_jerarquia = sum(valor[tipo=="detalle"], na.rm = T),
    proporcion = round((valor/total_jerarquia)*100, 0)
  ) %>%
  ungroup() %>%
  filter(tipo=="detalle") %>%
  rename(cantidad = valor) %>%
  select(año, genero, jerarquia, cantidad, proporcion) %>%
  mutate(
    grupo = interaction(jerarquia, genero, sep = " ", lex.order = T),
    jerarquia = factor(jerarquia, levels = c("Titular", "Asociado/a", "Asistente", "Instructor/a", "Adjunto/a", "Sin jerarquía")),
    genero = factor(genero, levels = c("Mujeres", "Hombres"))) %>%
  filter(jerarquia %in% c("Instructor/a", "Adjunto/a", "Sin jerarquía")) %>%
  filter(año!=2024) %>%
  mutate(
    grupo = factor(
      interaction(jerarquia, genero, sep = " "),
      levels = c(
        "Instructor/a Mujeres", "Instructor/a Hombres",
        "Adjunto/a Mujeres", "Adjunto/a Hombres",
        "Sin jerarquía Mujeres", "Sin jerarquía Hombres")))

# Graficar
p6 <- academicos_jerarquia_2 %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~grupo,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c(
      "Instructor/a Mujeres" = "#6A5ACD",
      "Instructor/a Hombres" = "#41776E",
      "Adjunto/a Mujeres" = "#A07DF5",
      "Adjunto/a Hombres" = "#5C918A",
      "Sin jerarquía Mujeres" = "#B598F7",
      "Sin jerarquía Hombres" = "#76ACA4"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Jerarquía:</b> ", jerarquia, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Cantidad:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(
    title = list(
      text = "<b>Gráfico 6. Estamento académico por género y jerarquía (2018-2023)</b>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Jerarquía y género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

p6

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Cantidad de publicaciones en revistas indexadas por género----
# Preparar datos
acad_pub <- ema_pub %>%
  mutate(
    prop_f = round(n_pub_f/n_pub*100,0),
    prop_m = round(n_pub_m/n_pub*100,0)) %>%
  pivot_longer(
    cols = c(n_pub_f, n_pub_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "n_pub_f" = "Femenino",
                    "n_pub_m" = "Masculino"),
    proporcion = case_when(
      genero == "Femenino" ~ prop_f,
      genero == "Masculino" ~ prop_m))

# Graficar
p7 <- acad_pub %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = text ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Publicaciones:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(
    title = list(
      text = "<b> Gráfico 7. Publicaciones en revistas indexadas por género (WOS, Scopus, Scielo y Otras) (2018-2024)</b>",
      font = list(size = 12),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

p7

# Preparar datos - indexación en WOS----
wos_pub <- ema_pub %>%
  filter(año!=2018) %>%
  pivot_longer(
    cols = c(n_pub_wos_f, n_pub_wos_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "n_pub_wos_f" = "Femenino",
                    "n_pub_wos_m" = "Masculino")) %>%
  group_by(año) %>%
  mutate(total_anual = sum(cantidad, na.rm = T),
         proporcion = round((cantidad/total_anual)*100, 0)) %>%
  ungroup()
# Graficar
p8 <- wos_pub %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = text ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Publicaciones:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(
    title = list(
      text = "<b>Gráfico 9. Publicaciones en revistas indexadas por género en WOS (2019-2024)</b>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

p8

# Preparar datos - indexación en Scopus----
scopus_pub <- ema_pub %>%
  filter(año!=2018) %>%
  mutate(
    prop_f = round(n_pub_scopus_f/n_pub_scopus*100,0),
    prop_m = round(n_pub_scopus_m/n_pub_scopus*100,0)) %>%
  pivot_longer(
    cols = c(n_pub_scopus_f, n_pub_scopus_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "n_pub_scopus_f" = "Femenino",
                    "n_pub_scopus_m" = "Masculino"),
    proporcion = case_when(
      genero == "Femenino" ~ prop_f,
      genero == "Masculino" ~ prop_m))

# Graficar
p9 <- scopus_pub %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = text ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Publicaciones:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(
    title = list(
      text = "<b>Gráfico 9. Publicaciones en revistas indexadas por género en Scopus (2019-2024)</b>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

p9

# Preparar datos - indexación en Scielo----
scielo_pub <- ema_pub %>%
  filter(año > 2018 & año < 2024) %>%
  mutate(
    prop_f = round(n_pub_scielo_f/n_pub_scielo*100,0),
    prop_m = round(n_pub_scielo_m/n_pub_scielo*100,0)) %>%
  pivot_longer(
    cols = c(n_pub_scielo_f, n_pub_scielo_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "n_pub_scielo_f" = "Femenino",
                    "n_pub_scielo_m" = "Masculino"),
    proporcion = case_when(
      genero == "Femenino" ~ prop_f,
      genero == "Masculino" ~ prop_m))

# Graficar
p10 <- scielo_pub %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = text ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Publicaciones:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(
    title = list(
      text = "<b>Gráfico 10. Publicaciones en revistas indexadas por género en Scielo (2019-2023)</b>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

p10

# Preparar datos - indexación en otras bases----
otras_pub <- ema_pub %>%
  filter(año!=2018) %>%
  mutate(
    prop_f = round(n_pub_otras_f/n_pub_otras*100,0),
    prop_m = round(n_pub_otras_m/n_pub_otras*100,0)) %>%
  pivot_longer(
    cols = c(n_pub_otras_f, n_pub_otras_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "n_pub_otras_f" = "Femenino",
                    "n_pub_otras_m" = "Masculino"),
    proporcion = case_when(
      genero == "Femenino" ~ prop_f,
      genero == "Masculino" ~ prop_m))

# Graficar
p11 <- otras_pub %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = text ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Publicaciones:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(
    title = list(
      text = "<b>Gráfico 11. Publicaciones en revistas indexadas por género en otras bases de datos (2019-2024)</b>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

p11

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Publicaciones por área de conocimiento (y género)----
# Transformación de los datos
ema_pub_long <- ema_pub_ac %>%
  pivot_longer(
    cols = -año,
    names_to = c("base", "area", "genero"),
    names_sep = "_",
    values_to = "publicaciones"
  ) %>%
  group_by(año, area) %>%
  summarise(
    total_publicaciones = sum(publicaciones, na.rm = T),
    femenino = sum(publicaciones[genero == "f"], na.rm = T),
    masculino = sum(publicaciones[genero == "m"], na.rm = T),
    proporcion_femenina = femenino/total_publicaciones,
    proporcion_masculina = masculino/total_publicaciones
  ) %>%
  ungroup()

# Diccionario para nombres completos de áreas
nombres_areas <- c(
  "ayc" = "Administración y Comercio",
  "agro" = "Agropecuaria",
  "aya" = "Arte y Arquitectura",
  "csh" = "Ciencias Sociales y Humanidades",
  "der" = "Derecho",
  "edu" = "Educación",
  "salud" = "Salud",
  "tec" = "Tecnología"
)

# Aplicar nombres completos
ema_pub_long <- ema_pub_long %>%
  mutate(area_nombre = nombres_areas[area])

# 2. Crear el gráfico interactivo con Plotly
p12 <- plot_ly(
  data = ema_pub_long,
  x = ~año,
  y = ~total_publicaciones,
  color = ~area_nombre,
  type = 'scatter',
  mode = 'lines+markers',
  hoverinfo = 'text',
  text = ~paste(
    "Año: ", año, "<br>",
    "Área: ", area_nombre, "<br>",
    "Total publicaciones: ", total_publicaciones, "<br>",
    ifelse(total_publicaciones > 0,
           paste("Proporción femenina:", round(proporcion_femenina * 100, 0), "%<br>",
                 "Proporción masculina:", round(proporcion_masculina * 100, 0), "%"),
           "No hay publicaciones registradas")
  )) %>%
  layout(
    title = list(
      text = "<b>Gráfico 12. Publicaciones en revistas indexadas (WOS, Scopus, Scielo y otras) por área de conocimiento (2022-2024)</b>",
      font = list(size = 12),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2022:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    margin = list(t = 80),
    hovermode = "closest")

p12

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Puestos directivos por género - 2 posiciones----
# Filtrar puestos
ema_puestos_1 <- ema_puestos %>%
  filter(puesto %in% c("Junta Directiva", "Consejo Académico"))

# Calcular proporciones
ema_puestos_1 <- ema_puestos_1 %>%
  group_by(año, puesto) %>%
  mutate(
    total_puesto = sum(cantidad),
    prop = round((cantidad/total_puesto)*100)) %>%
  ungroup()

# Crear gráfico
plot_puestos_1 <- ema_puestos_1 %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Puesto:</b> ", puesto, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Cantidad:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", prop, "%"),
    hoverinfo = 'text')

plot_puestos_1

# Separar en dos subplots, uno por puesto
puestos_split_1 <- split(ema_puestos_1, ema_puestos_1$puesto)
subplots_1 <- list(
  "Junta Directiva" = puestos_split_1[["Junta Directiva"]],
  "Consejo Académico" = puestos_split_1[["Consejo Académico"]]
) %>%
  lapply(function(df) {
    plot_ly(
      data = df,
      x = ~año,
      y = ~cantidad,
      color = ~genero,
      type = 'scatter',
      mode = 'lines+markers',
      colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
      text = ~paste0(
        "<b>Año:</b> ", año, "<br>",
        "<b>Puesto:</b> ", puesto, "<br>",
        "<b>Género:</b> ", genero, "<br>",
        "<b>Cantidad:</b> ", cantidad, "<br>",
        "<b>Proporción:</b> ", prop, "%"),
      hoverinfo = 'text',
      showlegend = F)
  })

# Combinar subplots
p13 <- subplot(subplots_1, nrows = 2, shareX = T, shareY = T, titleY = T) %>%
  layout(
    title = list(
      text = "<b>Gráfico 13. Puestos directivos por género - Junta Directiva y Consejo Académico</b>",
      font = list(size = 13),
      x = 0.5),
    legend = list(title = list(text = "Género")),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    yaxis2 = list(title = ""),
    margin = list(t = 80),
    annotations = list(
      list(x = 0, y = 1.02, text = "<b>Junta Directiva</b>", showarrow = F, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11)),
      list(x = 0, y = 0.47, text = "<b>Consejo Académico</b>", showarrow = F, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11))))

p13

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Puestos directivos por género - Vicerrectorías----
ema_puestos_x <- ema_puestos %>%
  filter(puesto=="Vicerrectoría")

# Calcular proporciones
ema_puestos_x <- ema_puestos_x %>%
  group_by(año, puesto) %>%
  mutate(
    total_puesto = sum(cantidad),
    prop = round((cantidad/total_puesto)*100)) %>%
  ungroup()

# Graficar
p14 <- ema_puestos_x %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Puesto:</b> ", puesto, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Cantidad:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", prop, "%"),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b> Gráfico 14. Puestos directivos por género - Vicerrectorías</b>",
    font = list(size = 13),
    x = 0.5),
    title = "",
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = "", tickvals = 1:6, range = c(1,5)),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 20),
    hovermode = "closest")

p14

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Puestos directivos por género - 2 posiciones----
# Filtrar puestos
ema_puestos_2 <- ema_puestos %>%
  filter(puesto %in% c("Decanato", "Vicedecanato"))

# Calcular proporciones
ema_puestos_2 <- ema_puestos_2 %>%
  group_by(año, puesto) %>%
  mutate(
    total_puesto = sum(cantidad),
    prop = round((cantidad/total_puesto)*100)) %>%
  ungroup()

# Crear gráfico
plot_puestos_2 <- ema_puestos_2 %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Puesto:</b> ", puesto, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Cantidad:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", prop, "%"),
    hoverinfo = 'text')

plot_puestos_2

# Separar en dos subplots, uno por puesto
puestos_split_2 <- split(ema_puestos_2, ema_puestos_2$puesto)

subplots_2 <- lapply(puestos_split_2, function(df) {
  plot_ly(
    data = df,
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Puesto:</b> ", puesto, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Cantidad:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", prop, "%"),
    hoverinfo = 'text',
    showlegend = F)})

# Combinar subplots
p15 <- subplot(subplots_2, nrows = 2, shareX = T, shareY = T, titleY = T) %>%
  layout(title = list(
    text = "<b>Gráfico 15. Puestos directivos por género - Decanato y Vicedecanato</b>",
    font = list(size = 13),
    x = 0.75),
    legend = list(title = list(text = "Género")),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    yaxis2 = list(title = ""),
    margin = list(t = 80),
    annotations = list(
      list(x = 0, y = 1.02, text = "<b>Decanato</b>", showarrow = F, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11)),
      list(x = 0, y = 0.47, text = "<b>Vicedecanato</b>", showarrow = F, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11))))

p15

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Puestos directivos por género - Dirección o jefatura----
# Filtrar puestos
ema_puestos_3 <- ema_puestos %>%
  filter(puesto=="Dirección o jefatura")

# Calcular proporciones
ema_puestos_3 <- ema_puestos_3 %>%
  group_by(año, puesto) %>%
  mutate(
    total_puesto = sum(cantidad),
    prop = round((cantidad/total_puesto)*100)) %>%
  ungroup()

# Graficar
p16 <- ema_puestos_3 %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Puesto:</b> ", puesto, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Cantidad:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", prop, "%"),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b> Gráfico 16. Puestos directivos por género - Dirección o jefatura</b>",
    font = list(size = 13),
    x = 0.5),
    title = "",
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 20),
    hovermode = "closest")

p16

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Matrícula pregrado por género - Matrícula total 2018-2024----
# Calcular proporciones y transoformar datos
ema_mat_total <- ema_mat %>%
  select(año, mt_pre_f, mt_pre_m) %>%
  pivot_longer(
    cols = c(mt_pre_f, mt_pre_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "mt_pre_f" = "Femenino",
                    "mt_pre_m" = "Masculino")) %>%
  group_by(año) %>%
  mutate(
    total_anual = sum(cantidad),
    proporcion = round((cantidad/total_anual)*100)) %>%
  ungroup()

# Graficar
p17 <- ema_mat_total %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Matrícula:</b> ", format(cantidad, big.mark = ".", decimal.mark = ","), "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text'
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 17. Matrícula total de pregrado por género (2018-2024)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = "",
      tickformat = "digits",  # Desactiva abreviaciones como "5k"
      separatethousands = T  # Opcional: añade separadores de miles (1.000)
    ),
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

p17

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Matrícula pregrado por género - Matrícula 1er año 2018-2024----
# Calcular proporciones y transformar datos
ema_mat_1 <- ema_mat %>%
  select(año, m1_pre_f, m1_pre_m) %>%
  pivot_longer(
    cols = c(m1_pre_f, m1_pre_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "m1_pre_f" = "Femenino",
                    "m1_pre_m" = "Masculino")) %>%
  group_by(año) %>%
  mutate(
    total_anual = sum(cantidad),
    proporcion = round((cantidad/total_anual)*100)) %>%
  ungroup()

# Graficar
p18 <- ema_mat_1 %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Matrícula:</b> ", label_comma(big.mark = ".", decimal.mark = ",")(cantidad), "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b>Gráfico 18. Matrícula de 1er año de pregrado por género (2018-2024)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest")

p18

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Tasa de retención por género 2018-2023----
# Calcular proporciones
ema_tr <- ema_mat %>%
  select(año, tasa_ret_f, tasa_ret_m) %>%
  pivot_longer(
    cols = c(tasa_ret_f, tasa_ret_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "tasa_ret_f" = "Femenino",
                    "tasa_ret_m" = "Masculino")) 

# Graficar
p19 <- ema_tr %>%
  plot_ly(
    x = ~año,
    y = ~cantidad/100,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Tasa de retención:</b> ", cantidad, "%"),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b>Gráfico 19. Tasa de retención de 1er año por género (2018-2023)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = "",
      tickformat = ".0%"),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest")

p19

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Matrícula total por área de conocimiento genérica 2018-2024----
# Preparar datos
ema_mat_area <- ema_mat_area %>%
  mutate(
    prop_f_mt = mt_f/mt,
    prop_m_mt = mt_m/mt,
    prop_f_m1 = m1_f/m1,
    prop_m_m1 = m1_m/m1)

# Graficar
p20 <- ema_mat_area %>%
  plot_ly(
    x = ~año,
    y = ~mt,
    color = ~area_del_conocimiento,
    colors = rainbow(length(unique(ema_mat_area$area_del_conocimiento))),
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Área:</b> ", area_del_conocimiento, "<br>",
      "<b>Matrícula total:</b> ", comma(mt, big.mark = ".", decimal.mark = ","), "<br>",
      "<b>Proporción mujeres:</b> ", percent(prop_f_mt, accuracy = 1, decimal.mark = ",")),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b>Gráfico 20. Matrícula total de pregrado por área de conocimiento (2018-2024)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024, tickangle = 0),
    yaxis = list(
      title = "",
      tickformat = ".0f",
      separatethousands = T),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50))

p20

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Matrícula 1er año por área de conocimiento genérica 2018-2024----
# Graficar
p21 <- ema_mat_area %>%
  plot_ly(
    x = ~año,
    y = ~m1,
    color = ~area_del_conocimiento,
    colors = colorRampPalette(brewer.pal(9, "Set1"))(9),
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Área:</b> ", area_del_conocimiento, "<br>",
      "<b>Matrícula 1er año:</b> ", comma(m1, big.mark = ".", decimal.mark = ","), "<br>",
      "<b>Proporción mujeres:</b> ", percent(prop_f_m1, accuracy = 1, decimal.mark = ",")),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b>Gráfico 21. Matrícula de 1er año de pregrado por área de conocimiento (2018-2024)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024, tickangle = 0),
    yaxis = list(
      title = "",
      tickformat = ".0f",
      separatethousands = T),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50))

p21

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Tasa de retención por área de conocimiento genérica 2018-2023----
# Preparar datos
ema_tr_area <- ema_mat_area %>%
  pivot_longer(cols = c(tasa_ret_f, tasa_ret_m),
               names_to = "genero",
               values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "tasa_ret_f" = "Femenino",
                    "tasa_ret_m" = "Masculino"),
    porcentaje = paste0(round(cantidad, 1), "%"),
    tooltip = paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Área:</b> ", area_del_conocimiento, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Tasa de retención:</b> ", porcentaje)) %>%
  filter(!is.na(cantidad))

# Dividir por área para hacer facets
area_split <- split(ema_tr_area, ema_tr_area$area_del_conocimiento)

# Graficar por área
plots_area <- lapply(names(area_split), function(area_name) {
  df <- area_split[[area_name]]
  
  plot_ly(
    data = df,
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~tooltip,
    hoverinfo = 'text',
    showlegend = F) %>%
    layout(
      yaxis = list(
        title = "",
        tickvals = seq(50, 100, 10),
        range = c(50, 100),
        ticksuffix = "%"),
      xaxis = list(title = "", tickvals = 2018:2024))})

# Crear subplots
n_cols <- 3
n_rows <- ceiling(length(plots_area)/n_cols)

p22 <- subplot(plots_area, nrows = n_rows, shareX = T, shareY = T, margin = 0.04) %>%
  layout(title = list(
    text = "<b>Gráfico 22. Tasa de retención de 1er año por área de conocimiento y género (2018-2023)</b>",
    font = list(size = 13),
    x = 0.5),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 50),
    hovermode = "closest")

# Agregar anotaciones
annotations <- lapply(seq_along(names(area_split)), function(i) {
  col_i <- (i-1) %% n_cols
  row_i <- floor((i-1)/n_cols)
  x_pos <- (col_i+0.5)/n_cols
  y_pos <- 1 - (row_i/n_rows)
  
  list(
    x = x_pos,
    y = y_pos,
    text = paste0("<b>", names(area_split)[i], "</b>"),
    showarrow = F,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    font = list(size = 13))})

# Añadir anotaciones al layout
p22 <- p22 %>%
  layout(annotations = annotations)

p22

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados por género 2018-2024----
titulados_g <- titulados_general_pre %>%
  filter(año >= 2018) %>%
  select(año, titulados_mujeres, titulados_hombres, prop_fem, prop_mas) %>%
  pivot_longer(
    cols = c(titulados_mujeres, titulados_hombres),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "titulados_mujeres" = "Femenino",
                    "titulados_hombres" = "Masculino")) %>%
  group_by(año) %>%
  mutate(
    total_anual = sum(cantidad),
    proporcion = round((cantidad/total_anual)*100, 0)) %>%
  ungroup()

# Graficar
p23 <- titulados_g %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Titulados:</b> ", label_comma(big.mark = ".", decimal.mark = ",")(cantidad), "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b>Gráfico 23. Titulación de pregrado por género (2018-2024)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = "",
    separators = "."), 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest")

p23

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados por género y área de conocimiento genérica 2018-2024----
p24<- titulados_ac_pre %>%
  filter(año >= 2018) %>%
  plot_ly(
    x = ~año,
    y = ~total_titulados,
    color = ~area_del_conocimiento,
    colors = colorRampPalette(brewer.pal(8, "Accent"))(8),
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Área:</b> ", area_del_conocimiento, "<br>",
      "<b>Titulados total:</b> ", comma(total_titulados, big.mark = ".", decimal.mark = ","), "<br>",
      "<b>Proporción mujeres:</b> ", percent(prop_fem/100, accuracy = 1, decimal.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  layout(title = list(
    text = "<b>Gráfico 24. Titulación de pregrado por área de conocimiento (2018-2024)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2023, tickangle = 0),
    yaxis = list(
      title = ""),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50))

p24

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados administración por género y total 2018-2024----
# Filtar datos
titulados_ac_1 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Administración y Comercio",
         año >= 2018)

p25 <- plot_ly(titulados_ac_1, x = ~año) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", año, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", año, "<br>Masculino:", titulados_hombres,
                       "<br>Proporción:", prop_mas, "%"),
    hoverinfo = "text"
  ) %>%
  add_trace(
    y = ~total_titulados,
    name = "Total",
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", width = 3),
    marker = list(color = "black", size = 6),
    hovertext = ~paste("Año:", año, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 25. Titulación de pregrado por género - Administración y Comercio (2018-2024)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

p25

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados arte y arquitectura por género y total 2018-2024----
titulados_ac_2 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Arte y Arquitectura",
         año >= 2018)

p26 <- plot_ly(titulados_ac_2, x = ~año) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", año, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", año, "<br>Masculino:", titulados_hombres,
                       "<br>Proporción:", prop_mas, "%"),
    hoverinfo = "text"
  ) %>%
  add_trace(
    y = ~total_titulados,
    name = "Total",
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", width = 3),
    marker = list(color = "black", size = 6),
    hovertext = ~paste("Año:", año, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 26. Titulación de pregrado por género - Arte y Arquitectura (2018-2024)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

p26

#///////////////////////////////////////////////////////////////////////////////
#Gráfico. Titulados ciencias básicas por género y total 2018-2024----
titulados_ac_3 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Ciencias Básicas",
         año >= 2018)

p27 <- plot_ly(titulados_ac_3, x = ~año) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", año, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", año, "<br>Masculino:", titulados_hombres,
                       "<br>Proporción:", prop_mas, "%"),
    hoverinfo = "text"
  ) %>%
  add_trace(
    y = ~total_titulados,
    name = "Total",
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", width = 3),
    marker = list(color = "black", size = 6),
    hovertext = ~paste("Año:", año, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 27. Titulación de pregrado por género - Ciencias Básicas (2018-2024)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

p27

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados ciencias sociales por género y total 2018-2024----
titulados_ac_4 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Ciencias Sociales",
         año >= 2018)

p28 <- plot_ly(titulados_ac_4, x = ~año) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", año, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", año, "<br>Masculino:", titulados_hombres,
                       "<br>Proporción:", prop_mas, "%"),
    hoverinfo = "text"
  ) %>%
  add_trace(
    y = ~total_titulados,
    name = "Total",
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", width = 3),
    marker = list(color = "black", size = 6),
    hovertext = ~paste("Año:", año, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 28. Titulación de pregrado por género - Ciencias Sociales (2018-2024)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

p28

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados derecho por género y total 2018-2024----
titulados_ac_5 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Derecho",
         año >= 2018)

p29 <- plot_ly(titulados_ac_5, x = ~año) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", año, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", año, "<br>Masculino:", titulados_hombres,
                       "<br>Proporción:", prop_mas, "%"),
    hoverinfo = "text"
  ) %>%
  add_trace(
    y = ~total_titulados,
    name = "Total",
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", width = 3),
    marker = list(color = "black", size = 6),
    hovertext = ~paste("Año:", año, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 29. Titulación de pregrado por género - Derecho (2023-2024)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2023:2024),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

p29

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados educación por género y total 2018-2024----
titulados_ac_6 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Educación",
         año >= 2018)

p30 <- plot_ly(titulados_ac_6, x = ~año) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", año, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", año, "<br>Masculino:", titulados_hombres,
                       "<br>Proporción:", prop_mas, "%"),
    hoverinfo = "text"
  ) %>%
  add_trace(
    y = ~total_titulados,
    name = "Total",
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", width = 3),
    marker = list(color = "black", size = 6),
    hovertext = ~paste("Año:", año, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 30. Titulación de pregrado por género - Educación (2018-2024)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

p30

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados humanidades por género y total 2018-2024----
titulados_ac_7 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Humanidades",
         año >= 2018)

p31 <- plot_ly(titulados_ac_7, x = ~año) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", año, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", año, "<br>Masculino:", titulados_hombres,
                       "<br>Proporción:", prop_mas, "%"),
    hoverinfo = "text"
  ) %>%
  add_trace(
    y = ~total_titulados,
    name = "Total",
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", width = 3),
    marker = list(color = "black", size = 6),
    hovertext = ~paste("Año:", año, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 31. Titulación de pregrado por género - Humanidades (2018-2024)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

p31

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados salud por género y total 2018-2024----
titulados_ac_8 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Salud",
         año >= 2018)

p32 <- plot_ly(titulados_ac_8, x = ~año) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", año, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", año, "<br>Masculino:", titulados_hombres,
                       "<br>Proporción:", prop_mas, "%"),
    hoverinfo = "text"
  ) %>%
  add_trace(
    y = ~total_titulados,
    name = "Total",
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", width = 3),
    marker = list(color = "black", size = 6),
    hovertext = ~paste("Año:", año, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 32. Titulación de pregrado por género - Salud (2018-2024)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

p32

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados tecnología por género y total 2018-2024----
titulados_ac_9 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Tecnología",
         año >= 2018)

p33 <- plot_ly(titulados_ac_9, x = ~año) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", año, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", año, "<br>Masculino:", titulados_hombres,
                       "<br>Proporción:", prop_mas, "%"),
    hoverinfo = "text"
  ) %>%
  add_trace(
    y = ~total_titulados,
    name = "Total",
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "black", width = 3),
    marker = list(color = "black", size = 6),
    hovertext = ~paste("Año:", año, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 33. Titulación de pregrado por género - Tecnología (2018-2024)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

p33

#///////////////////////////////////////////////////////////////////////////////
# Gráficos Encuesta Cuidados y Corresponsabilidad

# Gráfico. Cuidado de NNA y personas adultas por género----
# Preparar datos
# Calcular porcentajes por tipo de cuidado y género
porcentajes <- cuidados %>%
  filter(genero != "Transmasculino" & genero != "Prefiero no decir" & genero != "Transfemenino") %>%
  pivot_longer(cols = c(cuidado_menor, cuidado_mayores), 
               names_to = "tipo_cuidado", 
               values_to = "cuida") %>%
  group_by(genero, tipo_cuidado) %>%
  summarise(
    total = n(),
    cuidan = sum(cuida == "Sí", na.rm = T),
    .groups = "drop") %>%
  mutate(porcentaje = round((cuidan/total)*100, 0)) %>%
  mutate(tipo_cuidado = recode(tipo_cuidado,
    "cuidado_menor" = "Cuida NNA",
    "cuidado_mayores" = "Cuida personas adultas"
  )) %>%
  mutate(
    hovertext_barras = paste0(
      "Género: ", genero, "<br>",
      "Tipo de cuidado: ", ifelse(tipo_cuidado == "Cuida NNA", "Cuidado de menores", "Cuidado de mayores"), "<br>",
      "Personas que cuidan:", cuidan, "/", total ,"<br>",
      "Porcentaje: ", round(porcentaje, 0), "%"
    )
  )

p34 <- plot_ly(
  data = porcentajes,
  x = ~genero,
  y = ~porcentaje,
  color = ~tipo_cuidado,
  colors = c("Cuida NNA" = "steelblue2", "Cuida personas adultas" = "grey80"),
  type = "bar",
  text = ~hovertext_barras,
  hoverinfo = "text",
  textposition = "none"
) %>%
  layout(
    barmode = "group",
    title = list(text = "<b>Gráfico 34. Distribución del trabajo de cuidado por género</b>", 
                 x = 0.5, font = list(size = 13)),
    xaxis = list(title = ""),
    yaxis = list(
      title = "Porcentaje (%)"  
    ),
    # Leyenda a la derecha
    legend = list(
      orientation = "v",  
      x = 1,             
      y = 0.5,           
      xanchor = "left",   
      title = list(text = "<b>Tipo de cuidado</b>")  
    ),
    margin = list(r = 120)  
  )
p34

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Cantidad de horas dedicadas al cuidado----
# Preparar datos
horas_cuidado <- cuidados %>%
  filter(!is.na(horas_cuidado), genero %in% c("Femenino", "Masculino")) %>%
  count(genero, horas_cuidado) %>%
  group_by(genero) %>%
  mutate(
    porcentaje = round(n/sum(n)*100, 0),
    hovertext = paste0(
      "Género: ", genero, "<br>",
      "Rango horario: ", horas_cuidado, "<br>",
      "Personas: ", n, "<br>",
      "Porcentaje: ", porcentaje, "%"
    )
  ) %>%
  # Ordenar factores para mejor visualización
  mutate(
    horas_cuidado = factor(horas_cuidado,
                           levels = c("Menos de 1 hora al día",
                                      "1-2 horas diarias",
                                      "3-5 horas diarias",
                                      "5-8 horas diarias",
                                      "9-14 horas diarias",
                                      "15-20 horas diarias",
                                      "21-24 horas diarias",
                                      "No sabe / No contesta"),
                           ordered = T)
  )

# Graficar
p35 <- plot_ly(
  data = horas_cuidado,
  x = ~horas_cuidado,
  y = ~porcentaje,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
  type = "bar",
  text = ~hovertext,
  hoverinfo = "text",
  textposition = "none"
) %>%
  layout(
    barmode = "group",
    title = list(
      text = "<b> Gráfico 35. Distribución de horas diarias de cuidado por género</b>",
      x = 0.5,
      font = list(size = 13)
    ),
    xaxis = list(title = ""),
    yaxis = list(
      title = "Porcentaje (%)"
    ),
    legend = list(
      title = list(text = "<b>Género</b>"),
      orientation = "v",    
      x = 1,                
      y = 0.5,                
      xanchor = "left",       
      yanchor = "middle",     
      font = list(size = 12)  
    ),
    margin = list(r = 150), 
    xaxis = list(title = "Género"),
    yaxis = list(title = "Porcentaje (%)", range = c(0, 100))
  )

p35

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Días de cuidado por género
# Preparar datos
dias_cuidado <- cuidados %>%
  filter(!is.na(dias_cuidado), genero %in% c("Femenino", "Masculino")) %>%
  count(genero, dias_cuidado) %>%
  group_by(genero) %>%
  mutate(
    porcentaje = round(n/sum(n)*100, 0),
    hovertext = paste0(
      "Género: ", genero, "<br>",
      "Días de cuidado a la semana: ", dias_cuidado, "<br>",
      "Personas: ", n, "<br>",
      "Porcentaje: ", porcentaje, "%"
    )
  ) %>%
  mutate(
    dias_cuidado = factor(dias_cuidado,
                           levels = c("1 día",
                                      "2 días",
                                      "3 días",
                                      "4 días",
                                      "5 días",
                                      "6 días",
                                      "7 días"),
                           ordered = T)
  )

# Gráfico de barras agrupadas 
p36 <- plot_ly(
  data = dias_cuidado,
  x = ~dias_cuidado,
  y = ~porcentaje,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
  type = "bar",
  text = ~hovertext,
  hoverinfo = "text",
  textposition = "none"
) %>%
  layout(
    barmode = "group",
    title = list(
      text = "<b> Gráfico 36. Distribución de días semanales de cuidado por género</b>",
      x = 0.5,
      font = list(size = 13)
    ),
    xaxis = list(title = ""),
    yaxis = list(
      title = "Porcentaje (%)"
    ),
    # Ajustes de leyenda (posición derecha)
    legend = list(
      title = list(text = "<b>Género</b>"),
      orientation = "v",       
      x = 1,                
      y = 0.5,             
      xanchor = "left",       
      yanchor = "middle",     
      font = list(size = 13)  
    ),
    margin = list(r = 150),   
    xaxis = list(title = "Género"),
    yaxis = list(title = "Porcentaje (%)")
  )

p36
