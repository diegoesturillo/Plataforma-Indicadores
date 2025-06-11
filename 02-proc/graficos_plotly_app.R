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

# Para Encuesta LGBTQIA+
load(here("01-input", "data-proc", "bd_lgbt.RData"))


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
    texto_tooltip = paste0("Año: ", año,
      "<br>Género: ", genero,
      "<br>Cantidad: ", format(cantidad, big.mark = ".", decimal.mark = ","),
      "<br>Proporción: ", ifelse(genero == "Femenino", prop_f, prop_m), "%"
    )
  )

# Graficar
p1 <- plot_ly() %>%
  add_trace(
    data = filter(academicos_usach, genero == "Femenino"),
    x = ~año,
    y = ~cantidad,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(color = "#8D69F3"),
    marker = list(color = "#8D69F3"),
    name = "Femenino",
    text = ~texto_tooltip,
    hoverinfo = 'text'
  ) %>%
  add_trace(
    data = filter(academicos_usach, genero == "Masculino"),
    x = ~año,
    y = ~cantidad,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(color = "#41776E"),
    marker = list(color = "#41776E"),
    name = "Masculino",
    text = ~texto_tooltip,
    hoverinfo = 'text'
  ) %>%
  layout(
    title = list(
      text = "<b>Gráfico 1. Estamento académico por género (2018-2024)</b>",
      font = list(size = 13),
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
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
  text = ~paste(
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
      font = list(size = 13),
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
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
  text = ~paste(
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
      font = list(size = 13),
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
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
  text = ~paste(
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
      font = list(size = 13),
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
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
      "Asociado/a Mujeres" = "#6600FF",
      "Asociado/a Hombres" = "#009900",
      "Asistente Mujeres" = "#9933FF",
      "Asistente Hombres" = "#33CC33"),
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
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
      "Adjunto/a Mujeres" = "#6600FF",
      "Adjunto/a Hombres" = "#009900",
      "Sin jerarquía Mujeres" = "#9933FF",
      "Sin jerarquía Hombres" = "#33CC33"),
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
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Jerarquía y género</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
      text = "<b> Gráfico 7. Publicaciones en revistas indexadas* por género (2018-2024)</b>",
      font = list(size = 12),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>*WOS, Scopus, Scielo y otras</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
      text = "<b>Gráfico 8. Publicaciones en revistas indexadas por género en WOS</b><br><span style='font-size:13px'>WOS (2019-2024)</span>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
      text = "<b>Gráfico 9. Publicaciones en revistas indexadas por género</b><br><span style='font-size:13px'>Scopus (2019-2024)</span>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
      text = "<b>Gráfico 10. Publicaciones en revistas indexadas por género </b><br><span style='font-size:13px'>Scielo (2019-2023)</span>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2023),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
      text = "<b>Gráfico 11. Publicaciones en revistas indexadas por género </b><br><span style='font-size:13px'>Otras bases de datos (2019-2024)</span>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
      text = "<b>Gráfico 12. Publicaciones en revistas indexadas por área de conocimiento (2022-2024)</b>",
      font = list(size = 13),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2022:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
      text = "<b>Gráfico 13. Puestos directivos por género</b><br><span style='font-size:12px'>Junta Directiva y Consejo Académico</span>",
      font = list(size = 13),
      x = 0.5),
    legend = list(title = list(text = "Género")),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    yaxis2 = list(title = ""),
    margin = list(t = 80, b = 100),  
    annotations = list(
      list(x = 0, y = 1.02, text = "<b>Junta Directiva</b>", showarrow = F, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11)),
      list(x = 0, y = 0.47, text = "<b>Consejo Académico</b>", showarrow = F, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11)),
      list(
        x = 0.5,   
        y = -0.15, 
        xref = "paper",
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

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
    text = "<b> Gráfico 14. Puestos directivos por género</b><br><span style='font-size:12px'>Vicerrectorías</span>",
    font = list(size = 13),
    x = 0.5),
    title = "",
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = "", tickvals = 1:6, range = c(1,5)),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 50, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
  layout(
    title = list(
      text = "<b>Gráfico 15. Puestos directivos por género</b><br><span style='font-size:12px'>Decanato y Vicedecanato</span>",
      font = list(size = 13),
      x = 0.5),
    legend = list(title = list(text = "Género")),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    yaxis2 = list(title = ""),
    margin = list(t = 80, b = 100),  
    annotations = list(
      list(x = 0, y = 1.02, text = "<b>Decanato</b>", showarrow = F, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11)),
      list(x = 0, y = 0.47, text = "<b>Vicedecanato</b>", showarrow = F, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11)),
      list(
        x = 0.5,   
        y = -0.15, 
        xref = "paper",
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

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
    text = "<b> Gráfico 16. Puestos directivos por género</b><br><span style='font-size:12px'>Dirección o Jefatura</span>",
    font = list(size = 13),
    x = 0.5),
    title = "",
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 50, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
      tickformat = "digits",  
      separatethousands = T  
    ),
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
    margin = list(t = 50, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
      tickformat = ".0%",
      range = c(0.7,0.9)),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

p19

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Matrícula total por área del conocimiento genérica 2018-2024----
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
    text = "<b>Gráfico 20. Matrícula total de pregrado por área del conocimiento (2018-2024)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024, tickangle = 0),
    yaxis = list(
      title = "",
      tickformat = ".0f",
      separatethousands = T),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50, b = 100),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
    text = "<b>Gráfico 21. Matrícula de 1er año de pregrado por área del conocimiento (2018-2024)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024, tickangle = 0),
    yaxis = list(
      title = "",
      tickformat = ".0f",
      separatethousands = T),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50, b = 100),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
    text = "<b>Gráfico 22. Tasa de retención de 1er año por área del conocimiento y género (2018-2023)</b>",
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
    margin = list(t = 50, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

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
    text = "<b>Gráfico 24. Titulación de pregrado por área del conocimiento (2018-2024)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2023, tickangle = 0),
    yaxis = list(
      title = ""),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50, b = 100),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
    )

p24

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados administración por género y total 2018-2024----
# Filtar datos
titulados_ac_1 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Administración y Comercio",
         año >= 2018)

p25 <- plot_ly(
  titulados_ac_1, x = ~año) %>%
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
      text = "<b>Gráfico 25. Titulación de pregrado por género</b><br><span style='font-size:13px'>Administración y Comercio (2018-2024)</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 70, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
      text = "<b>Gráfico 26. Titulación de pregrado por género</b><br><span style='font-size:13px'>Arte y Arquitectura (2018-2024)</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 70, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
      text = "<b>Gráfico 27. Titulación de pregrado por género</b><br><span style='font-size:13px'>Ciencias Básicas (2018-2024)</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 70, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
      text = "<b>Gráfico 28. Titulación de pregrado por género</b><br><span style='font-size:13px'>Ciencias Sociales (2018-2024)</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 70, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
      text = "<b>Gráfico 29. Titulación de pregrado por género</b><br><span style='font-size:13px'>Derecho (2023-2024)</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 70, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
      text = "<b>Gráfico 30. Titulación de pregrado por género</b><br><span style='font-size:13px'>Educación (2018-2024)</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 70, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
      text = "<b>Gráfico 31. Titulación de pregrado por género</b><br><span style='font-size:13px'>Humanidades (2018-2024)</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 70, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
      text = "<b>Gráfico 32. Titulación de pregrado por género</b><br><span style='font-size:13px'>Salud (2018-2024)</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 70, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
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
      text = "<b>Gráfico 33. Titulación de pregrado por género</b><br><span style='font-size:13px'>Tecnología (2018-2024)</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 70, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p33

#///////////////////////////////////////////////////////////////////////////////
# Gráficos Encuesta Cuidados y Corresponsabilidad
# Gráfico. Proporción de personas cuidadas según grupo etario
# Preparar datos
tipo_cuidado <- cuidados %>%
  filter(!is.na(menores_mayores)) %>%
  count(menores_mayores, name = "conteo") %>%
  ungroup() %>%
  mutate(
    porcentaje = conteo/sum(conteo)*100
  ) %>%
  mutate(menores_mayores = recode(menores_mayores,
                                  "Solo menores" = "Sólo NNA",
                                  "Solo mayores" = "Sólo personas adultas",
                                  "Menores y mayores" = "Ambos")) %>%
  mutate(menores_mayores = factor(menores_mayores,
                                  levels = c("Sólo NNA", 
                                             "Sólo personas adultas", 
                                             "Ambos"),
                                  ordered = T)) %>%
  mutate(hovertext = paste0(
    "<b>Tipo de cuidado:</b> ", menores_mayores, "<br>",
    "<b>Cantidad:</b> ", conteo, "<br>",
    "<b>Porcentaje:</b> ", round(porcentaje, 0), "%"
  ))

# Graficar
p34 <- plot_ly(
  data = tipo_cuidado,
  x = ~menores_mayores,
  y = ~porcentaje,
  color = ~menores_mayores,  
  colors = c("Sólo NNA" = "red3", 
             "Sólo personas adultas" = "steelblue3", 
             "Ambos" = "grey80"),
  type = "bar",
  text = ~hovertext,
  hoverinfo = "text",
  textposition = "none",
  marker = list(line = list(color = "white", width = 1))  
) %>%
  layout(
    barmode = "group",
    title = list(text = "<b>Gráfico 34. Proporción de personas cuidadas según grupo etario</b>", 
                 x = 0.5, 
                 font = list(size = 13)),
    xaxis = list(title = "",
                 categoryorder = "array",  
                 categoryarray = c("Sólo NNA", "Sólo personas adultas", "Ambos"),
                 tickangle = 0),
    yaxis = list(title = "",
                 ticksuffix = "%"),  
    legend = list(
      orientation = "v",
      x = 1.05,  
      y = 0.5,
      xanchor = "left",
      title = list(text = "<b>Persona(s) cuidada(s)</b>")
    ),
    margin = list(r = 150, b = 100, t = 50),
    hoverlabel = list(
      bgcolor = "white",
      font = list(color = "black")
    ),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p34

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Cuidado de NNA y personas adultas por género----
# Preparar datos
# Calcular porcentajes por tipo de cuidado y género
porcentajes <- cuidados %>%
  filter(genero %in% c("Masculino", "Femenino", "Género diverso")) %>%
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
    "cuidado_menor" = "NNA",
    "cuidado_mayores" = "Persona adulta"
  )) %>%
  mutate(
    hovertext_barras = paste0(
      "Género: ", genero, "<br>",
      "Tipo de cuidado: ", ifelse(tipo_cuidado == "NNA", "Cuidado de menores", "Cuidado de mayores"), "<br>",
      "Personas que cuidan:", cuidan, "/", total ,"<br>",
      "Porcentaje: ", round(porcentaje, 0), "%"
    )
  )

p35 <- plot_ly(
  data = porcentajes,
  x = ~genero,
  y = ~porcentaje,
  color = ~tipo_cuidado,
  colors = c("NNA" = "steelblue2", "Persona adulta" = "grey80"),
  type = "bar",
  text = ~hovertext_barras,
  hoverinfo = "text",
  textposition = "none"
) %>%
  layout(
    barmode = "group",
    title = list(text = "<b>Gráfico 35. Distribución del trabajo de cuidado por género</b>", 
                 x = 0.5, font = list(size = 13)),
    xaxis = list(title = ""),
    yaxis = list(
      title = "", ticksuffix = "%"  
    ),
    legend = list(
      orientation = "v",  
      x = 1,             
      y = 0.5,           
      xanchor = "left",   
      title = list(text = "<b>Persona cuidada</b>")  
    ),
    margin = list(r = 120, b = 100),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p35

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
p36 <- plot_ly(
  data = horas_cuidado,
  x = ~porcentaje,
  y = ~horas_cuidado,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
  orientation = 'h',
  type = "bar",
  text = ~hovertext,
  hoverinfo = "text",
  textposition = "none"
) %>%
  layout(
    barmode = "group",
    title = list(
      text = "<b> Gráfico 36. Distribución de horas diarias de cuidado por género</b>",
      x = 0.5,
      font = list(size = 13)
    ),
    xaxis = list(title = "", ticksuffix = "%", range = c(0,40)),
    yaxis = list(
      title = "", categoryorder = "array",
      autorange = "reversed"
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
    margin = list(r = 150, b = 100), 
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p36

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
p37 <- plot_ly(
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
      text = "<b> Gráfico 37. Distribución de días semanales de cuidado por género</b>",
      x = 0.5,
      font = list(size = 13)
    ),
    xaxis = list(title = ""),
    yaxis = list(
      title = "", ticksuffix = "%", range = c(0,70)
    ),
    legend = list(
      title = list(text = "<b>Género</b>"),
      orientation = "v",       
      x = 1,                
      y = 0.5,             
      xanchor = "left",       
      yanchor = "middle",     
      font = list(size = 13)  
    ),
    margin = list(r = 150, b = 100),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p37

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Estrategias de conciliación individual----
# Preparar los datos
organizar <- cuidados %>%
  select(starts_with("organizar_")) %>%
  pivot_longer(cols = everything(), 
               names_to = "tipo_organizacion", 
               values_to = "respuesta") %>%
  filter(!is.na(respuesta)) %>%
  mutate(
    tipo_organizacion = recode(
      tipo_organizacion, 
      "organizar_1" = "Requiere apoyo de integrante del núcleo familiar",
      "organizar_2" = "Requiere apoyo de familiares o amistades",
      "organizar_3" = "Requiere apoyo de una persona contratada",
      "organizar_4" = "Requiere apoyo de un servicio contratado",
      "organizar_5" = "Lo hace solo/a/e"
    ),
    tipo_organizacion = factor(tipo_organizacion,
                               levels = c("Requiere apoyo de integrante del núcleo familiar",
                                          "Requiere apoyo de familiares o amistades",
                                          "Requiere apoyo de una persona contratada",
                                          "Requiere apoyo de un servicio contratado",
                                          "Lo hace solo/a/e"),  
                               ordered = T),
    respuesta = factor(respuesta, 
                       levels = c("Nunca", "A veces", "Frecuentemente", "Siempre", "No aplica"),
                       ordered = T)
  ) %>%
  group_by(tipo_organizacion) %>%
  summarise(
    total_respuestas = n(),
    positivas = sum(respuesta %in% c("Siempre", "Frecuentemente"), na.rm = T),
    porcentaje = positivas/total_respuestas*100,
    .groups = 'drop'
  ) %>%
  mutate(
    texto_tooltip = paste0(
      "<b>Porcentaje: ", round(porcentaje, 0), "%</b>"
    )
  )

# Graficar
p38 <- plot_ly(
  data = organizar,
  x = ~porcentaje,
  y = ~tipo_organizacion,
  type = "bar",
  orientation = "h",
  hoverinfo = "text",
  text = ~texto_tooltip,
  textposition = 'none',
  marker = list(
    color = "#8D69F3",
    opacity = 0.8
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 38. Estrategias individuales de conciliación laboral/académica y familiar</b>",
      x = 0.05,
      font = list(size = 13),
      xanchor = "left",
      pad = list(b = 10, t = 10)
    ),
    xaxis = list(
      title = "",
      range = c(0, 60),
      ticksuffix = "%",
      showgrid = T,
      zerolinecolor = "#E5E5E5"
    ),
    yaxis = list(
      title = "",
      tickfont = list(size = 12),
      categoryorder = "array"
    ),
    margin = list(l = 300, r = 50, b = 80, t = 100),
    hoverlabel = list(
      bgcolor = "white",
      bordercolor = "#8D69F3",
      font = list(color = "black", size = 12),
      align = "left"
    ),
    plot_bgcolor = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
  ))

p38

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Efectos subjetivos en las personas cuidadoras por género----
# Preparar datos
efectos <- cuidados %>%
  mutate(
    across(
      starts_with("efecto_"),
      ~ case_when(
        .x == "1 (Muy en Desacuerdo)" ~ 1,
        .x == "2" ~ 2,
        .x == "3" ~ 3,
        .x == "4" ~ 4,
        .x == "5 (Muy de Acuerdo)" ~ 5,
        T ~ NA_real_
    ))) %>%
  filter(genero %in% c("Femenino", "Masculino")) %>%
  select(starts_with("efecto"), genero) %>%
  pivot_longer(
    cols = starts_with("efecto_"),
    names_to = "efecto",
    values_to = "valor"
  ) %>%
  filter(!is.na(valor), !is.na(genero)) %>%
  mutate(
    efecto = recode(efecto,
                    "efecto_1" = "Estrés",
                    "efecto_2" = "Cansancio permanente",
                    "efecto_3" = "No poder desarrollar trayectoria laboral/académica",
                    "efecto_4" = "No tener suficiente tiempo libre",
                    "efecto_5" = "Me siento bien de cuidar una persona",
                    "efecto_6" = "No tiene ningún efecto"),
    positivo = valor %in% c(4, 5)
  ) %>%
  group_by(efecto, genero) %>%
  summarise(
    total = n(),
    positivos = sum(positivo),
    porcentaje = positivos/total*100,
    .groups = 'drop'
  )

# Calcular totales
totales <-  efectos %>%
  group_by(efecto) %>%
  summarise(
    genero = "Total",
    total = sum(total),
    positivos = sum(positivos),
    porcentaje = positivos/total * 100
  )

# Combinar ambos datasets
efectos_totales <- bind_rows(efectos, totales) %>%
  mutate(
    efecto = factor(efecto, levels = c("Estrés", "Cansancio permanente", "No poder desarrollar trayectoria laboral/académica",
                                       "No tener suficiente tiempo libre", "Me siento bien de cuidar una persona",
                                       "No tiene ningún efecto")),
    genero = factor(genero, levels = c("Femenino", "Masculino", "Total")),
    texto_tooltip = paste0("Porcentaje: ", round(porcentaje, 0), "%")
  )

# Graficar
p39 <- plot_ly(
  data = efectos_totales,
  x = ~porcentaje,
  y = ~efecto,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E", "Total" = "grey80"),
  type = "bar",
  orientation = "h",
  hoverinfo = "text",
  text = ~texto_tooltip,
  textposition = "none",
  marker = list(
    line = list(color = "white", width = 1),
    opacity = 0.8  #
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 39. Efectos del cuidado por género</b>",
      x = 0.3, 
      font = list(size = 13),
      xanchor = "left",
      pad = list(t = 20)  
    ),
    xaxis = list(
      title = "",
      range = c(0, 80),
      ticksuffix = "%",
      showgrid = T,
      gridcolor = "#f0f0f0",  
      zerolinecolor = "#f0f0f0"
    ),
    yaxis = list(
      title = "",
      tickfont = list(size = 12),
      ticklen = 10,  
      tickcolor = "transparent",  
      automargin = T,  
      margin = list(l = 20) 
    ),
    barmode = "group",
    margin = list(l = 220, r = 150, b = 80, t = 100),  
    hoverlabel = list(
      bgcolor = "white",
      font = list(color = "black", size = 12),
      bordercolor = "#8D69F3"
    ),
    legend = list(
      orientation = "v", 
      x = 1.02,  
      y = 0.5,   
      xanchor = "left", 
      title = list(text = "<b>Género</b>", font = list(size = 12)),
      font = list(size = 12),
      bgcolor = "rgba(255,255,255,0.7)"  
    ),
    plot_bgcolor = "rgba(0,0,0,0)",  
    paper_bgcolor = "rgba(0,0,0,0)",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )))

p39

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Apoyos desde la comunidad universitaria----
apoyos <- cuidados %>%
  select(starts_with("apoyo_")) %>%
  pivot_longer(cols = everything(), 
               names_to = "apoyo", 
               values_to = "respuesta") %>%
  filter(!is.na(respuesta)) %>%
  mutate(
    apoyo = recode(
      apoyo, 
      "apoyo_1" = "Jefatura",
      "apoyo_2" = "Pares",
      "apoyo_3" = "Jefatura de carrera",
      "apoyo_4" = "Organización estudiantil",
      "apoyo_5" = "Organización gremial"
    ),
    apoyo = factor(apoyo,
                               levels = c("Jefatura",
                                          "Pares",
                                          "Jefatura de carrera",
                                          "Organización estudiantil",
                                          "Organización gremial"),  
                               ordered = T),
    respuesta = factor(respuesta, 
                       levels = c("Nunca", "A veces", "Frecuentemente", "Siempre", "No aplica"),
                       ordered = T)
  ) %>%
  group_by(apoyo) %>%
  summarise(
    total_respuestas = n(),
    positivas = sum(respuesta %in% c("Siempre", "Frecuentemente"), na.rm = T),
    porcentaje = positivas/total_respuestas*100,
    .groups = 'drop'
  ) %>%
  mutate(
    texto_tooltip = paste0(
      "<b>Porcentaje: ", round(porcentaje, 0), "%</b>"
    )
  )

# Graficar
p40 <- plot_ly(
  data = apoyos,
  x = ~porcentaje,
  y = ~apoyo,
  type = "bar",
  orientation = "h",
  hoverinfo = "text",
  text = ~texto_tooltip,
  textposition = 'none',
  marker = list(
    color = "#8D69F3",
    opacity = 0.8
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 40. Apoyos desde la comunidad universitaria</b>",
      x = 0.25,
      font = list(size = 13),
      xanchor = "left",
      pad = list(b = 10, t = 10)
    ),
    xaxis = list(
      title = "",
      range = c(0, 40),
      ticksuffix = "%",
      showgrid = T,
      zerolinecolor = "#E5E5E5"
    ),
    yaxis = list(
      title = "",
      tickfont = list(size = 12)
    ),
    margin = list(l = 300, r = 50, b = 80, t = 100),
    hoverlabel = list(
      bgcolor = "white",
      bordercolor = "#8D69F3",
      font = list(color = "black", size = 12),
      align = "left"
    ),
    plot_bgcolor = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
  )

)

p40

#///////////////////////////////////////////////////////////////////////////////
# Gráficos Encuesta LGBTQIA+
# Gráfico. Respeto de derechos de personas LGBTIQA+ según identidad de género----
# Preparar datos
respeto_1 <- bd_lgbt_2 %>%
  filter(ident_gen != "Otro (especifique)") %>%
  group_by(ident_gen) %>%
  summarise(
    total_respuestas = n(),
    positivas = sum(x6_1 == "Sí", na.rm = T),
    porcentaje = positivas/total_respuestas*100,
    .groups = 'drop'
  ) %>%
  mutate(
    texto_tooltip = paste0(
      "Cantidad de personas: ", total_respuestas, "<br>",
      "Porcentaje: ", round(porcentaje, 0), "%</b>"
    )
  )
# Gráficar
p41 <- plot_ly(
  data = respeto_1,
  x = ~ident_gen,
  y = ~porcentaje,
  marker = list(color = "#FF69B4"),
  type = "bar",
  text = ~paste0(porcentaje, "%"),
  hoverinfo = "text",
  hovertext = ~texto_tooltip,
  textposition = 'none'
) %>%
  layout(title = list(
    text = "<b>Gráfico 41. ¿Se respetan los derechos de las personas LGBTIQA+ en la USACH?, según identidad de género</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
    font = list(size = 13),
    x = 0.5,
    xanchor = 'center',
    yanchor = 'top'
  ),
  xaxis = list(title = "", tickangle = 0),
  yaxis = list(title = "", ticksuffix = "%"),
  barmode = "group",          
  showlegend = F,
  margin = list(b = 120, t = 80),
  annotations = list(
    list(
      x = 0.5,   
      y = -0.225,  
      xref = "paper",  
      yref = "paper",
      text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
      showarrow = F,
      font = list(size = 10.5, color = "grey"),
      align = "center"
    )
  )
  )

p41

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Respeto de derechos LGBTIQA+ según orientación sexual----
# Preparar datos
respeto_2 <- bd_lgbt_2 %>%
  filter(orient_sex != "Otro (especifique)" & orient_sex != "demisexual") %>%
  mutate(orient_sex = recode(orient_sex,
                             "No sabe/no contesta" = "NS/NC*"
  )) %>%
  group_by(orient_sex) %>%
  summarise(
    total_respuestas = n(),
    positivas = sum(x6_1 == "Sí", na.rm = T),
    porcentaje = positivas/total_respuestas*100,
    .groups = 'drop'
  ) %>%
  mutate(
    texto_tooltip = paste0(
      "Cantidad de personas: ", total_respuestas, "<br>",
      "Porcentaje: ", round(porcentaje, 0), "%</b>"
    )
  )
# Gráficar
p42 <- plot_ly(
  data = respeto_2,
  x = ~orient_sex,
  y = ~porcentaje,
  marker = list(color = "#FF69B4"),
  type = "bar",
  text = ~paste0(porcentaje, "%"),
  hoverinfo = "text",
  hovertext = ~texto_tooltip,
  textposition = 'none'
) %>%
  layout(title = list(
    text = "<b>Gráfico 42. ¿Se respetan los derechos de las personas LGBTIQA+ en la USACH?, según orientación sexual</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
    font = list(size = 13),
    x = 0.5,
    xanchor = 'center',
    yanchor = 'top'
  ),
  xaxis = list(title = "", tickangle = 0),
  yaxis = list(title = "", ticksuffix = "%"),
  barmode = "group",          
  showlegend = F,
  margin = list(b = 120, t = 80),
  annotations = list(
    list(
      x = 0.5,   
      y = -0.225,  
      xref = "paper",  
      yref = "paper",
      text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>NS/NC* = No sabe/No contesta</i>",
      showarrow = F,
      font = list(size = 10.5, color = "grey"),
      align = "center"
    )
  )
  )

p42

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Sufrió discriminación en la Usach por identidad de género----
# Preparar datos
discr_1 <- bd_lgbt_2 %>%
  mutate(ident_gen = case_when(
    ident_gen == "Transmasculino" ~ "Trans",
    ident_gen == "Transfemenino" ~ "Trans",
    T ~ ident_gen
  )) %>%
  filter(ident_gen %in% c("Femenino", "Masculino", "Género diverso", "Trans")) %>%
  group_by(ident_gen) %>%
  summarise(
    total_respuestas = n(),
    positivas = sum(x6_2 == "Sí", na.rm = T),
    porcentaje = positivas/total_respuestas*100,
    .groups = 'drop'
  ) %>%
  mutate(
    texto_tooltip = paste0(
      "Cantidad de personas: ", total_respuestas, "<br>",
      "Porcentaje: ", round(porcentaje, 0), "%</b>"))
  
# Gráficar
p43 <- plot_ly(
  data = discr_1,
  x = ~ident_gen,
  y = ~porcentaje,
  marker = list(color = "#FF69B4"),
  type = "bar",
  text = ~paste0(porcentaje, "%"),
  hoverinfo = "text",
  hovertext = ~texto_tooltip,
  textposition = 'none'
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 43. ¿Sufrió discriminación alguna vez en la USACH?</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0),
    yaxis = list(title = "", ticksuffix = "%"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
    )

p43

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Sufrió discriminación en la Usach por orientación sexual---
# Preparar datos
discr_2 <- bd_lgbt_2 %>%
  filter(orient_sex %in% c("Demisexual", "Pansexual", "Gay", "Lesbiana", "Bisexual")) %>%
  group_by(orient_sex) %>%
  summarise(
    total_respuestas = n(),
    positivas = sum(x6_3 == "Sí", na.rm = T),
    porcentaje = positivas/total_respuestas*100,
    .groups = 'drop'
  ) %>%
  mutate(
    texto_tooltip = paste0(
      "Cantidad de personas: ", total_respuestas, "<br>",
      "Porcentaje: ", round(porcentaje, 0), "%</b>"))

# Gráficar
p44 <- plot_ly(
  data = discr_2,
  x = ~orient_sex,
  y = ~porcentaje,
  marker = list(color = "#FF69B4"),
  type = "bar",
  text = ~paste0(porcentaje, "%"),
  hoverinfo = "text",
  hovertext = ~texto_tooltip,
  textposition = 'none'
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 44. ¿Sufrió discriminación alguna vez en la USACH durante 2023?</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,20)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p44

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Situaciones de violencia por identidad de género----
# Preparar datos - Femenino
violencias_fem <- data.frame(
  tipo_violencia = c("Hostigamiento", "Le ridiculizaron", "Le acosaron sexualmente"),
  porcentaje = c(31, 44, 37)
)

# Graficar
p45 <- plot_ly(
  data = violencias_fem,
  x = ~tipo_violencia,          
  y = ~porcentaje,              
  type = "bar",
  color = ~tipo_violencia,      
  colors = colorRampPalette(brewer.pal(3, "Set1"))(3), 
  text = ~paste0(porcentaje, "%"), 
  textposition = 'none',
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 45. Tipos de violencia por identidad de género</b><br><span style='font-size:12px'>Femenino</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,50)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p45

#///////////////////////////////////////////////////////////////////////////////
# Preparar datos - Masculino
violencias_masc <- data.frame(
  tipo_violencia = c("Le ridiculizaron", "Le insultaron", "Recibió hostigamiento"),
  porcentaje = c(50, 38, 31)
)

# Graficar
p46 <- plot_ly(
  data = violencias_masc,
  x = ~tipo_violencia,          
  y = ~porcentaje,              
  type = "bar",
  color = ~tipo_violencia,
  colors = colorRampPalette(brewer.pal(3, "Set1"))(3), 
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 46. Tipos de violencia por identidad de género</b><br><span style='font-size:12px'>Masculino</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p46

#///////////////////////////////////////////////////////////////////////////////
# Preparar datos - Trans
violencias_trans <- data.frame(
  tipo_violencia = c("No respetaron su IG*", "Dificultaron el derecho a registrar su IG*",
                    "Le ridiculizaron"),
  porcentaje = c(91, 26, 22)
)

# Graficar
p47 <- plot_ly(
  data = violencias_trans,
  x = ~tipo_violencia,          
  y = ~porcentaje,              
  type = "bar",
  color = ~tipo_violencia,
  colors = colorRampPalette(brewer.pal(3, "Set1"))(3), 
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 47. Tipos de violencia por identidad de género</b><br><span style='font-size:12px'>Trans</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,100)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>IG* = Identidad de Género</i>",
        showarrow = F,
        font = list(size = 10, color = "grey"),
        align = "center"
      )
    )
  )

p47

#///////////////////////////////////////////////////////////////////////////////
# Preparar datos - Género diverso
violencias_gd <- data.frame(
  tipo_violencia = c("No respetaron su IG*", "Le ridiculizaron", 
                     "Dificultaron el derecho a registrar su IG*"),
  porcentaje = c(88, 29, 21)
  
)

# Graficar
p48 <- plot_ly(
  data = violencias_gd,
  x = ~tipo_violencia,          
  y = ~porcentaje,              
  type = "bar",
  color = ~tipo_violencia,      
  colors = colorRampPalette(brewer.pal(3, "Set1"))(3), 
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 48. Tipos de violencia por identidad de género</b><br><span style='font-size:12px'>Género diverso</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,90)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 100, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>IG* = Identidad de Género</i>",
        showarrow = F,
        font = list(size = 10, color = "grey"),
        align = "center"
      )
    )
  )

p48

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Situaciones de violencia por orientación sexual----
# Preparar datos - Lesbiana
violencias_lesb <- data.frame(
  tipo_violencia = c("No respetaron su IG*", "Le ridiculizaron", "Le acosaron sexualmente",
                     "Le insultaron", "Le gritaron"),
  porcentaje = c(41, 36, 36, 32, 32)
)

# Graficar
p49 <- plot_ly(
  data = violencias_lesb,
  x = ~tipo_violencia,          
  y = ~porcentaje,              
  type = "bar",
  color = ~tipo_violencia,      
  colors = colorRampPalette(brewer.pal(5, "BuPu"))(5),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 49. Tipos de violencia por orientación sexual</b><br><span style='font-size:12px'>Lesbiana</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(
      title = "",
      tickangle = 0,          
      automargin = T,
      categoryorder = "total ascending"
    ),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,50)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80), 
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>IG* = Identidad de Género</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p49

#///////////////////////////////////////////////////////////////////////////////
# Preparar datos - Gay
violencias_gay <- data.frame(
  tipo_violencia = c("Le ridiculizaron", "Le insultaron", "No respetaron su IG*",
                     "Le gritaron", "Hostigamiento"),
  porcentaje = c(46, 46, 29, 25, 25)
  
)

# Graficar
p50 <- plot_ly(
  data = violencias_gay,
  x = ~tipo_violencia,          
  y = ~porcentaje,              
  type = "bar",
  color = ~tipo_violencia,      
  colors = colorRampPalette(brewer.pal(5, "BuPu"))(5),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 50. Tipos de violencia por orientación sexual</b><br><span style='font-size:13px'>Gay</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,50)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>IG* = Identidad de Género</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p50

#///////////////////////////////////////////////////////////////////////////////
# # Preparar datos - Bisexual
violencias_bi <- data.frame(
  tipo_violencia = c("Le insultaron", "Le acosaron sexualmente", "Le ridiculizaron", 
                     "No respetaron su IG*"),
  porcentaje = c(24, 29, 35, 37)
)

# Graficar
p51 <- plot_ly(
  data = violencias_bi,
  x = ~tipo_violencia,          
  y = ~porcentaje,              
  type = "bar",
  color = ~tipo_violencia,      
  colors = colorRampPalette(brewer.pal(4, "BuPu"))(4),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 51. Tipo de violencia por orientación sexual</b><br><span style='font-size:13px'>Bisexual</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,40)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>IG* = Identidad de Género</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p51

#///////////////////////////////////////////////////////////////////////////////
# # Preparar datos - Pansexual
violencias_pan <- data.frame(
  tipo_violencia = c("Le acosaron sexualmente", "Hostigamiento", "Le ridiculizaron",
                     "No respetaron su IG*"),
  porcentaje = c(25, 25, 36, 59)
)

# Graficar
p52 <- plot_ly(
  data = violencias_pan,
  x = ~tipo_violencia,          
  y = ~porcentaje,              
  type = "bar",
  color = ~tipo_violencia,      
  colors = colorRampPalette(brewer.pal(4, "BuPu"))(4),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 52. Tipos de violencia por orientación sexual</b><br><span style='font-size:13px'>Pansexual</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 100, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>IG* = Identidad de Género</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p52

# //////////////////////////////////////////////////////////////////////////////
# Gráfico. Experiencia en la universidad como persona LGTBQIA+----
experiencias <- bd_lgbt_1 %>%
  select(x5_1:x5_7) %>%
  pivot_longer(
    cols = x5_1:x5_7,
    names_to = "variable",
    values_to = "respuesta"
  ) %>%
  mutate(
    variable = case_when(
      variable == "x5_1" ~ "Ayuda psicosocial",
      variable == "x5_2" ~ "Salud sexual y reproductiva",
      variable == "x5_3" ~ "Infraestructura",
      variable == "x5_4" ~ "Temáticas de interés",
      variable == "x5_5" ~ "Sensibilización y prevención en VG*",
      variable == "x5_6" ~ "Cursos de formación",
      variable == "x5_7" ~ "Espacio seguro",
      T ~ variable  
    )
  ) %>%
  mutate(
    categoria = case_when(
      respuesta %in% c(1, 2) ~ "Negativo",
      respuesta %in% c(3, 4) ~ "Positivo",
      T ~ NA_character_
    )
  ) %>%
  filter(!is.na(categoria)) %>%  
  group_by(variable) %>%
  summarise(
    total_respuestas = n(),
    negativas = sum(categoria == "Negativo"),
    positivas = sum(categoria == "Positivo"),
    porcentaje_neg = (negativas / total_respuestas) * 100,
    porcentaje_pos = (positivas / total_respuestas) * 100,
    .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = starts_with("porcentaje_"),
    names_to = "categoria",
    values_to = "porcentaje",
    names_prefix = "porcentaje_"
  ) %>%
  mutate(
    categoria = ifelse(categoria == "neg", "Negativa", "Positiva")
  )

# Graficar
p53 <- plot_ly(
  data = experiencias,
  y = ~variable,          
  x = ~porcentaje,        
  color = ~categoria,
  colors = c("Negativa" = "#41776E", "Positiva" = "#8D69F3"),
  type = "bar",
  textposition = "none",
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Valoración:", categoria,
    "<br>Porcentaje:", round(porcentaje, 1), "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 53. Calidad de vida en la USACH como persona LGBTQIA+</b>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center'
    ),
    yaxis = list(
      title = "",
      categoryorder = "array",
      categoryarray = unique(experiencias$variable),
      automargin = T
    ),
    xaxis = list(
      title = "",
      range = c(0, 100),
      ticksuffix = "%"
    ),
    barmode = "stack",
    margin = list(l = 150, t = 80, b = 100),
    legend = list(
      orientation = "v", 
      x = 1.02,  
      y = 0.5,   
      xanchor = "left", 
      title = list(text = "<b>Valoración</b>", font = list(size = 12)),
      font = list(size = 12),
      bgcolor = "rgba(255,255,255,0.7)"  
    ),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>VG* = Violencia de Género</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p53
