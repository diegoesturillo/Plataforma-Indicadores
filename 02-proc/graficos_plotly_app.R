# Gráficos en formato plotly
# Carga de paquetes
pacman::p_load(here, plotly, RColorBrewer, readxl, scales, tidyverse)

# Cargar datos----
# Cargar lista original
load(here("01-input", "data-proc", "datos_ema.RData"))

# Para género y grado académico/JCE
ema <- datos$ACAD_GRADO
ema_jce <- datos$ACAD_GRADO_JCE
acad_grados <- datos$ACAD_GRADO
acad_grados_jce <- datos$ACAD_GRADO_JCE

# Para jerarquías académicas/JCE
ema_jq <- datos$ACAD_JQ
ema_jq_jce <- datos$ACAD_JQ_JCE

# Para cantidad de publicaciones académicas por género y por área de conocimiento
load(here("01-input/data-proc/publicaciones.RData"))

# Para tasa de retención por género y área del conocimiento
tr_g <- datos$MAT_PRE_GENERAL
tr_ac <- datos$MAT_PRE_AC

# Para puestos directivos
ema_puestos <- datos$ACAD_DIRECCION

# Para matrícula de pregrado por género y por área del conocimiento
load(here("01-input", "data-proc", "matricula usach 2018-2025.RData"))

# Para titulación por género y área de conocimiento
load(here("01-input", "data-proc", "titulados.RData"))

# Para cuidados y corresponsabilidad
load(here("01-input", "data-proc", "bd_cuidados.RData"))

# Para Encuesta LGBTQIA+
load(here("01-input", "data-proc", "bd_lgbt.RData"))

# Pestaña Estamento académico----
# Gráfico. Cantidad de académicos por género en la USACH (2018-2024)
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
      text = "<b>Gráfico 1. Estamento académico y docente por género (2018-2024)</b>",
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
# Gráfico. Cantidad de académicos por género y grado académico
# Preparar datos - grado doctorado
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
      text = "<b>Gráfico 2. Estamento académico y docente por género y grado académico (2018-2024)</b><br><span style='font-size:13px'>Doctorado</span>",
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

# Preparar datos - grado magíster
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
      text = "<b>Gráfico 3. Estamento académico y docente por género y grado académico (2018-2024)</b><br><span style='font-size:13px'>Magíster</span>",
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

# Preparar datos - grado licenciado
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
      text = "<b>Gráfico 4. Estamento académico y docente por género y grado académico(2018-2024)</b><br><span style='font-size:13px'>Título profesional y/o licenciatura</span>",
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
# Pestaña Estamento académico JCE----
# Gráfico. Cantidad de JCE por género
# Calcular proporciones
academicos_usach_jce <- ema_jce %>%
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
                           "<br>Cantidad:", format(cantidad, big.mark = ".", decimal.mark = ","),
                           "<br>Proporción: ", ifelse(genero == "Femenino", prop_f, prop_m), "%"
    )
  )

# Graficar
p5 <- plot_ly() %>%
  add_trace(
    data = filter(academicos_usach_jce, genero == "Femenino"),
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
    data = filter(academicos_usach_jce, genero == "Masculino"),
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
      text = "<b>Gráfico 5. Jornadas Completas Equivalentes (JCE) por género (2018-2024)</b>",
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

p5

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Cantidad de académicos por género y grado académico
# Preparar datos - grado doctorado
acad_grado_doc_jce <- acad_grados_jce %>%
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
p6 <- plot_ly(
  data = acad_grado_doc_jce,
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
      text = "<b>Gráfico 6. Jornadas Completas Equivalentes (JCE) por género y grado académico (2018-2024)</b><br><span style='font-size:13px'>Doctorado</span>",
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

p6

# Preparar datos - grado magíster
acad_grado_mg_jce <- acad_grados_jce %>%
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
p7 <- plot_ly(
  data = acad_grado_mg_jce,
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
      text = "<b>Gráfico 7.Jornadas Completas Equivalentes (JCE) por género y grado académico (2018-2024)</b><br><span style='font-size:13px'>Magíster</span>",
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

p7

# Preparar datos - grado licenciado
acad_grado_lic_jce <- acad_grados_jce %>%
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
p8 <- plot_ly(
  data = acad_grado_lic_jce,
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
      text = "<b>Gráfico 8. Jornadas Completas Equivalentes (JCE) por género y grado académico(2018-2024)</b><br><span style='font-size:13px'>Título profesional y/o licenciatura</span>",
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

p8

#///////////////////////////////////////////////////////////////////////////////
# Pestaña Jerarquía académica----
# Gráfico. Académicos por género y jerarquía - 3 jerarquías
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
      str_detect(categoria, "oj") ~ "Otras jerarquías",
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
    jerarquia = factor(jerarquia, levels = c("Titular", "Asociado/a", "Asistente")),
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
p9 <- academicos_jerarquia %>%
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
      text = "<b>Gráfico 9. Estamento académico por género y jerarquía (2018-2024)</b><br><span style='font-size:13px'>Titular, asociado/a y asistente</span>",
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

p9

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Académicos por género y jerarquía - 4 jerarquías
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
      str_detect(categoria, "oj") ~ "Otras jerarquías",
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
    jerarquia = factor(jerarquia, levels = c("Instructor/a", "Adjunto/a", "Otras jerarquías", "Sin jerarquía")),
    genero = factor(genero, levels = c("Mujeres", "Hombres"))) %>%
  filter(jerarquia %in% c("Instructor/a", "Adjunto/a", "Otras jerarquías", "Sin jerarquía")) %>%
  mutate(
    grupo = factor(
      interaction(jerarquia, genero, sep = " "),
      levels = c(
        "Instructor/a Mujeres", "Instructor/a Hombres",
        "Adjunto/a Mujeres", "Adjunto/a Hombres",
        "Otras jerarquías Mujeres", "Otras jerarquías Hombres",
        "Sin jerarquía Mujeres", "Sin jerarquía Hombres")))

# Graficar
p10 <- academicos_jerarquia_2 %>%
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
      "Otras jerarquías Mujeres" = "#FF00FF",
      "Otras jerarquías Hombres" = "#00FF99",
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
      text = "<b>Gráfico 10. Estamento académico por género y jerarquía (2018-2024)</b><br><span style='font-size:13px'>Instructor/a, adjunto/a, otras jerarquías y sin jerarquía</span>",
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

p10

#///////////////////////////////////////////////////////////////////////////////
# Pestaña jerarquía académica JCE----
# Gráfico. JCE por jerarquías y género - 3 jerarquías
# Preparar datos
academicos_jerarquia_3 <- ema_jq_jce %>%
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
      str_detect(categoria, "oj") ~ "Otras jerarquías",
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
    jerarquia = factor(jerarquia, levels = c("Titular", "Asociado/a", "Asistente")),
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
p11 <- academicos_jerarquia_3 %>%
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
      text = "<b>Gráfico 11. Jornadas Completas Equivalentes (JCE) por género y jerarquía (2018-2024)</b><br><span style='font-size:13px'>Titular, asociado/a y asistente</span>",
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

p11

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. JCE por jerarquía y género - 4 jerarquías
# Preparar datos
academicos_jerarquia_4 <- ema_jq_jce %>%
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
      str_detect(categoria, "oj") ~ "Otras jerarquías",
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
    jerarquia = factor(jerarquia, levels = c("Instructor/a", "Adjunto/a", "Otras jerarquías", "Sin jerarquía")),
    genero = factor(genero, levels = c("Mujeres", "Hombres"))) %>%
  filter(jerarquia %in% c("Instructor/a", "Adjunto/a", "Otras jerarquías", "Sin jerarquía")) %>%
  mutate(
    grupo = factor(
      interaction(jerarquia, genero, sep = " "),
      levels = c(
        "Instructor/a Mujeres", "Instructor/a Hombres",
        "Adjunto/a Mujeres", "Adjunto/a Hombres",
        "Otras jerarquías Mujeres", "Otras jerarquías Hombres",
        "Sin jerarquía Mujeres", "Sin jerarquía Hombres")))

# Graficar
p12 <- academicos_jerarquia_4 %>%
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
      "Otras jerarquías Mujeres" = "#FF00FF",
      "Otras jerarquías Hombres" = "#00FF99",
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
      text = "<b>Gráfico 12. Jornadas Completas Equivalentes (JCE) por género y jerarquía (2018-2024)</b><br><span style='font-size:13px'>Instructor/a, adjunto/a, otras jerarquías y sin jerarquía</span>",
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

p12

#///////////////////////////////////////////////////////////////////////////////
# Pestaña Publicaciones----
# Gráfico. Cantidad de publicaciones en revistas indexadas por género
# Preparar datos
recuento_anual <- pub %>%
  filter(año <= 2024) %>%
  group_by(idpublicacion, año) %>%
  summarise(
    n_mujeres = sum(sexo == "M"),
    n_hombres = sum(sexo == "H"),
    .groups = 'drop'
  ) %>%
  mutate(
    tipo_genero = case_when(
      n_mujeres == 0 ~ "Solo hombres",
      n_hombres == 0 ~ "Solo mujeres",
      T ~ "Mixta"
    )
  ) %>%
  group_by(año, tipo_genero) %>% 
  summarise(conteo = n(), .groups = 'drop')

p13 <- plot_ly(
  data = recuento_anual,
  x = ~año,
  y = ~conteo,
  color = ~tipo_genero,
  type = "scatter",
  mode = "lines+markers",  
  colors = c("Solo mujeres" = "#8D69F3", "Solo hombres" = "#41776E", "Mixta" = "#E6A"),  
  marker = list(size = 8),
  line = list(width = 3),
  text = ~paste("Año:", año, "<br>Tipo:", tipo_genero, "<br>Publicaciones:", conteo),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 13. Número de publicaciones únicas por género (2018-2024)</b>",
      font = list(size = 12),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(
      title = list(text = "<b>Autoría</b>"),
      orientation = "v", 
      x = 1.05,         
      y = 0.5,          
      xanchor = "left",  
      yanchor = "center" 
    ),
    margin = list(t = 80, b = 120),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.35,  
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

# Preparar datos - indexación en WOS
recuento_anual_wos <- pub %>%
  filter(año <= 2024,
         wos == 1) %>%
  group_by(idpublicacion, año) %>%
  summarise(
    n_mujeres = sum(sexo == "M"),
    n_hombres = sum(sexo == "H"),
    .groups = 'drop'
  ) %>%
  mutate(
    tipo_genero = case_when(
      n_mujeres == 0 ~ "Solo hombres",
      n_hombres == 0 ~ "Solo mujeres",
      T ~ "Mixta"
    )
  ) %>%
  group_by(año, tipo_genero) %>% 
  summarise(conteo = n(), .groups = 'drop')

# Graficar
p14 <- plot_ly(
  data = recuento_anual_wos,
  x = ~año,
  y = ~conteo,
  color = ~tipo_genero,
  type = "scatter",
  mode = "lines+markers",  
  colors = c("Solo mujeres" = "#8D69F3", "Solo hombres" = "#41776E", Mixta = "#E6A"),  
  marker = list(size = 8),
  line = list(width = 3),
  text = ~paste("Año:", año, "<br>Tipo:", tipo_genero, "<br>Publicaciones:", conteo),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "<b> Gráfico 14. Número de publicaciones en revistas indexadas por género (2018-2024)</b><br><span style='font-size:13px'>Indexación en Web of Science (WOS)</span>",
      font = list(size = 12),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Autoría</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

p14

# Preparar datos - indexación en Scopus
recuento_anual_scopus <- pub %>%
  filter(año <= 2024,
         scopus == 1) %>%
  group_by(idpublicacion, año) %>%
  summarise(
    n_mujeres = sum(sexo == "M"),
    n_hombres = sum(sexo == "H"),
    .groups = 'drop'
  ) %>%
  mutate(
    tipo_genero = case_when(
      n_mujeres == 0 ~ "Solo hombres",
      n_hombres == 0 ~ "Solo mujeres",
      T ~ "Mixta"
    )
  ) %>%
  group_by(año, tipo_genero) %>% 
  summarise(conteo = n(), .groups = 'drop')

# Graficar
p15 <- plot_ly(
  data = recuento_anual_scopus,
  x = ~año,
  y = ~conteo,
  color = ~tipo_genero,
  type = "scatter",
  mode = "lines+markers",  
  colors = c("Solo mujeres" = "#8D69F3", "Solo hombres" = "#41776E", "Mixta" = "#E6A"),  
  marker = list(size= 8),
  line = list(width = 3),
  text = ~paste("Año:", año, "<br>Tipo:", tipo_genero, "<br>Publicaciones:", conteo),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "<b> Gráfico 15. Número de publicaciones en revistas indexadas por género (2018-2024)</b><br><span style='font-size:13px'>Indexación en Scopus</span>",
      font = list(size = 12),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Autoría</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

p15

# Preparar datos - indexación en PubMed
recuento_anual_pubmed <- pub %>%
  filter(año <= 2024,
         pubmed_otro_indexador == 1) %>%
  group_by(idpublicacion, año) %>%
  summarise(
    n_mujeres = sum(sexo == "M"),
    n_hombres = sum(sexo == "H"),
    .groups = 'drop'
  ) %>%
  mutate(
    tipo_genero = case_when(
      n_mujeres == 0 ~ "Solo hombres",
      n_hombres == 0 ~ "Solo mujeres",
      T ~ "Mixta"
    )
  ) %>%
  group_by(año, tipo_genero) %>% 
  summarise(conteo = n(), .groups = 'drop')


# Graficar
p16 <- plot_ly(
  data = recuento_anual_pubmed,
  x = ~año,
  y = ~conteo,
  color = ~tipo_genero,
  type = "scatter",
  mode = "lines+markers", 
  colors = c("Solo mujeres" = "#8D69F3", "Solo hombres" = "#41776E", "Mixta" = "#E6A"),  
  marker = list(size = 8),
  line = list(width = 3),
  text = ~paste("Año:", año, "<br>Tipo:", tipo_genero, "<br>Publicaciones:", conteo),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "<b> Gráfico 16. Número de publicaciones en revistas indexadas por género (2018-2024)</b><br><span style='font-size:13px'>Indexación en PubMed</span>",
      font = list(size = 12),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Autoría</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

p16

#///////////////////////////////////////////////////////////////////////////////
# Preparar datos - indexación en PubMed
recuento_anual_scielo <- pub %>%
  filter(año <= 2024) %>%
  filter(!is.na(codigo_scielo) & codigo_scielo != "") %>%
  group_by(idpublicacion, año) %>%
  summarise(
    n_mujeres = sum(sexo == "M"),
    n_hombres = sum(sexo == "H"),
    .groups = 'drop'
  ) %>%
  mutate(
    tipo_genero = case_when(
      n_mujeres == 0 ~ "Solo hombres",
      n_hombres == 0 ~ "Solo mujeres",
      T ~ "Mixta"
    )
  ) %>%
  group_by(año, tipo_genero) %>% 
  summarise(conteo = n(), .groups = 'drop')


# Graficar
p_scielo <- plot_ly(
  data = recuento_anual_scielo,
  x = ~año,
  y = ~conteo,
  color = ~tipo_genero,
  type = "scatter",
  mode = "lines+markers",  # Líneas + puntos
  colors = c("Solo mujeres" = "#8D69F3", "Solo hombres" = "#41776E", "Mixta" = "#E6A"),  
  marker = list(size = 8),
  line = list(width = 3),
  text = ~paste("Año:", año, "<br>Tipo:", tipo_genero, "<br>Publicaciones:", conteo),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "<b> Gráfico 17. Número de publicaciones en revistas indexadas por género (2018-2023)</b><br><span style='font-size:13px'>Indexación en Scielo</span>",
      font = list(size = 12),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Autoría</b>")),
    margin = list(t = 80, b = 100),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    ))

p_scielo

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Publicaciones por área de conocimiento (y género)
# Preparar datos
pub_ac <- pub %>%
  filter(año %in% c(2018:2024)) %>%
  group_by(idpublicacion, año, disciplina_oecd) %>%
  summarize(
    n_mujeres = sum(sexo == "M"),
    n_hombres = sum(sexo == "H"),
    .groups = 'drop'
  ) %>%
  mutate(
    tipo_genero = case_when(
      n_mujeres == 0 ~ "Solo hombres",
      n_hombres == 0 ~ "Solo mujeres",
      T ~ "Mixta"
    )
  ) %>%
  group_by(año, disciplina_oecd, tipo_genero) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  group_by(año, disciplina_oecd) %>%
  mutate(
    text_hover = paste(
      "<b>Área:</b> ", first(disciplina_oecd),
      "<br><b>Año:</b> ", first(año),
      "<br><b>Total:</b> ", sum(conteo),
      "<br>----",
      "<br><b>Solo mujeres:</b> ", sum(conteo[tipo_genero == "Solo mujeres"]),
      "<br><b>Solo hombres:</b> ", sum(conteo[tipo_genero == "Solo hombres"]),
      "<br><b>Mixtas:</b> ", sum(conteo[tipo_genero == "Mixta"])
    )
  ) %>%
  group_by(año, disciplina_oecd) %>%
  summarize(
    total = sum(conteo),
    text_hover = first(text_hover),  
    .groups = 'drop'
  )

# Graficar
p17 <- plot_ly(
  data = pub_ac,
  x = ~año,
  y = ~total,
  color = ~disciplina_oecd,
  type = "scatter",
  mode = "lines+markers",
  colors = RColorBrewer::brewer.pal(6, "Paired"),  
  marker = list(size = 8),
  line = list(width = 3),
  text = ~text_hover,  
  hoverinfo = "text",
  name = ~disciplina_oecd
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 18. Publicaciones únicas por disciplina OECD (2018-2024)</b>",
      font = list(size = 12),
      x = 0.5
    ),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(
      title = list(text = "<b>Disciplina OECD</b>"),
      orientation = "v",  
      x = 1.05,          
      y = 0.5,          
      xanchor = "left",   
      yanchor = "center"  
    ),
    margin = list(t = 100, b = 150, r = 150),
    hovermode = "closest",
    annotations = list(
      list(
        x = 0.5,
        y = -0.4,
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
# Pestaña Puestos Directivos----
# Gráfico. Puestos directivos por género - 2 posiciones
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
p18 <- subplot(subplots_1, nrows = 2, shareX = T, shareY = T, titleY = T) %>%
  layout(
    title = list(
      text = "<b>Gráfico 19. Puestos directivos por género</b><br><span style='font-size:12px'>Junta Directiva y Consejo Académico</span>",
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

p18

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Puestos directivos por género - Vicerrectorías
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
p19 <- ema_puestos_x %>%
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
    text = "<b> Gráfico 20. Puestos directivos por género</b><br><span style='font-size:12px'>Vicerrectorías</span>",
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

p19

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Puestos directivos por género - 2 posiciones
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
p20 <- subplot(subplots_2, nrows = 2, shareX = T, shareY = T, titleY = T) %>%
  layout(
    title = list(
      text = "<b>Gráfico 21. Puestos directivos por género</b><br><span style='font-size:12px'>Decanato y Vicedecanato</span>",
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

p20

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Puestos directivos por género - Dirección o jefatura
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
p21 <- ema_puestos_3 %>%
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
    text = "<b> Gráfico 22. Puestos directivos por género</b><br><span style='font-size:12px'>Dirección o Jefatura</span>",
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

p21

#///////////////////////////////////////////////////////////////////////////////
# Pestaña Matrícula por género----
# Gráfico. Matrícula pregrado por género - Matrícula total 2018-2024
# Calcular proporciones
ema_mat_total <- matricula_año %>%
  select(año, mt_f, mt_m) %>%
  pivot_longer(
    cols = c(mt_f, mt_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "mt_f" = "Femenino",
                    "mt_m" = "Masculino")) %>%
  group_by(año) %>%
  mutate(
    total_anual = sum(cantidad),
    proporcion = round((cantidad/total_anual)*100)) %>%
  ungroup()

# Graficar
p22 <- ema_mat_total %>%
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
      text = "<b>Gráfico 23. Matrícula total de pregrado por género (2018-2025)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    xaxis = list(title = "", tickvals = 2018:2025),
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

p22

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Matrícula pregrado por género - Matrícula 1er año 2018-2024
# Calcular proporciones y transformar datos
ema_mat_1 <- matricula_año %>%
  select(año, m1_f, m1_m) %>%
  pivot_longer(
    cols = c(m1_f, m1_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "m1_f" = "Femenino",
                    "m1_m" = "Masculino")) %>%
  group_by(año) %>%
  mutate(
    total_anual = sum(cantidad),
    proporcion = round((cantidad/total_anual)*100)) %>%
  ungroup()

# Graficar
p23 <- ema_mat_1 %>%
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
    text = "<b>Gráfico 24. Matrícula de 1er año de pregrado por género (2018-2025)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2025),
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

p23

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Tasa de retención por género 2018-2023
# Calcular proporciones
ema_tr <- tr_g %>%
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
p24 <- ema_tr %>%
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
    text = "<b>Gráfico 25. Tasa de retención de 1er año por género (2018-2023)</b>",
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

p24

#///////////////////////////////////////////////////////////////////////////////
# Pestaña Matrícula por área del conocimiento----
# Gráfico. Matrícula total por área del conocimiento genérica 2018-2024
# Preparar datos
ema_mat_area <- matricula_ac %>%
  mutate(
    prop_f_mt = mt_f/mt,
    prop_m_mt = mt_m/mt,
    prop_f_m1 = m1_f/m1,
    prop_m_m1 = m1_m/m1)

# Graficar
p25 <- ema_mat_area %>%
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
    text = "<b>Gráfico 26. Matrícula total de pregrado por área del conocimiento (2018-2025)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2025, tickangle = 0),
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

p25

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Matrícula 1er año por área de conocimiento genérica 2018-2024
# Graficar
p26 <- ema_mat_area %>%
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
    text = "<b>Gráfico 27. Matrícula de 1er año de pregrado por área del conocimiento (2018-2024)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2025, tickangle = 0),
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

p26

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Tasa de retención por área de conocimiento genérica 2018-2023
# Preparar datos
ema_tr_area <- tr_ac %>%
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

p27 <- subplot(plots_area, nrows = n_rows, shareX = T, shareY = T, margin = 0.04) %>%
  layout(title = list(
    text = "<b>Gráfico 28. Tasa de retención de 1er año por área del conocimiento y género (2018-2023)</b>",
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
p27 <- p27 %>%
  layout(annotations = annotations)

p27

#///////////////////////////////////////////////////////////////////////////////
# Pestaña Titulación por género y área del conocimiento----
# Gráfico. Titulados por género 2018-2024
titulados_g <- titulados_general_pre %>%
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
p28 <- titulados_g %>%
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
    text = "<b>Gráfico 29. Titulación de pregrado por género (2018-2024)</b>",
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

p28

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados por género y área de conocimiento genérica 2018-2024
p29 <- titulados_ac_pre %>%
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
    text = "<b>Gráfico 30. Titulación de pregrado por área del conocimiento (2018-2024)</b>",
    font = list(size = 13),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024, tickangle = 0),
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

p29

#///////////////////////////////////////////////////////////////////////////////
# Pestaña Desglose titulación por área del conocimiento----
# Gráfico. Titulados administración por género y total 2018-2024
# Filtar datos
titulados_ac_1 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Administración y Comercio",
         año >= 2018)

p30 <- plot_ly(
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
      text = "<b>Gráfico 31. Titulación de pregrado por género</b><br><span style='font-size:13px'>Administración y Comercio (2018-2024)</span>",
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
# Gráfico. Titulados arte y arquitectura por género y total 2018-2024
titulados_ac_2 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Arte y Arquitectura",
         año >= 2018)

p31 <- plot_ly(titulados_ac_2, x = ~año) %>%
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
      text = "<b>Gráfico 32. Titulación de pregrado por género</b><br><span style='font-size:13px'>Arte y Arquitectura (2018-2024)</span>",
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
#Gráfico. Titulados ciencias básicas por género y total 2018-2024
titulados_ac_3 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Ciencias Básicas",
         año >= 2018)

p32 <- plot_ly(titulados_ac_3, x = ~año) %>%
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
      text = "<b>Gráfico 33. Titulación de pregrado por género</b><br><span style='font-size:13px'>Ciencias Básicas (2018-2024)</span>",
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
# Gráfico. Titulados ciencias sociales por género y total 2018-2024
titulados_ac_4 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Ciencias Sociales",
         año >= 2018)

p33 <- plot_ly(titulados_ac_4, x = ~año) %>%
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
      text = "<b>Gráfico 34. Titulación de pregrado por género</b><br><span style='font-size:13px'>Ciencias Sociales (2018-2024)</span>",
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
# Gráfico. Titulados derecho por género y total 2018-2024
titulados_ac_5 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Derecho",
         año >= 2018)

p34 <- plot_ly(titulados_ac_5, x = ~año) %>%
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
      text = "<b>Gráfico 35. Titulación de pregrado por género</b><br><span style='font-size:13px'>Derecho (2023-2024)</span>",
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

p34

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados educación por género y total 2018-2024
titulados_ac_6 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Educación",
         año >= 2018)

p35 <- plot_ly(titulados_ac_6, x = ~año) %>%
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
      text = "<b>Gráfico 36. Titulación de pregrado por género</b><br><span style='font-size:13px'>Educación (2018-2024)</span>",
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

p35

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados humanidades por género y total 2018-2024
titulados_ac_7 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Humanidades",
         año >= 2018)

p36 <- plot_ly(titulados_ac_7, x = ~año) %>%
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
      text = "<b>Gráfico 37. Titulación de pregrado por género</b><br><span style='font-size:13px'>Humanidades (2018-2024)</span>",
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

p36

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados salud por género y total 2018-2024
titulados_ac_8 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Salud",
         año >= 2018)

p37 <- plot_ly(titulados_ac_8, x = ~año) %>%
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
      text = "<b>Gráfico 38. Titulación de pregrado por género</b><br><span style='font-size:13px'>Salud (2018-2024)</span>",
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

p37

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Titulados tecnología por género y total 2018-2024
titulados_ac_9 <- titulados_ac_pre %>%
  filter(area_del_conocimiento=="Tecnología",
         año >= 2018)

p38 <- plot_ly(titulados_ac_9, x = ~año) %>%
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
      text = "<b>Gráfico 39. Titulación de pregrado por género</b><br><span style='font-size:13px'>Tecnología (2018-2024)</span>",
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

p38

#///////////////////////////////////////////////////////////////////////////////
# Pestaña Cuidados y Corresponsabilidad----
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
p39 <- plot_ly(
  data = tipo_cuidado,
  x = ~menores_mayores,
  y = ~porcentaje,
  color = ~menores_mayores,  
  colors = c("Sólo NNA" = "darkmagenta", 
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
    title = list(text = "<b>Gráfico 40. Proporción de personas cuidadas según grupo etario</b>", 
                 x = 0.5, 
                 font = list(size = 13)),
    xaxis = list(title = "",
                 categoryorder = "array",  
                 categoryarray = c("Sólo NNA", "Sólo personas adultas", "Ambos"),
                 tickangle = 0),
    yaxis = list(title = "",
                 ticksuffix = "%",
                 range = c(0,70)),  
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p39

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Cuidado de NNA y personas adultas por género
# Preparar datos
porcentajes <- cuidados %>%
  filter(genero %in% c("Masculino", "Femenino")) %>%
  filter(menores_mayores %in% c("Solo menores", "Solo mayores", "Menores y mayores")) %>%
  mutate(total_general = n()) %>%
  count(genero, menores_mayores, total_general) %>%
  group_by(genero) %>%
  mutate(total_genero = sum(n)) %>%
  ungroup() %>%
  mutate(
    tipo_cuidado_gráfico = case_when(
      menores_mayores == "Solo menores" ~ "Solo NNA",
      menores_mayores == "Solo mayores" ~ "Solo persona adulta",
      menores_mayores == "Menores y mayores" ~ "Ambos"
    ),
    porcentaje_general = round((n/total_general)*100, 1),  
    porcentaje_genero = round((n/total_genero)*100, 1)      
  ) %>%
  mutate(
    hovertext_barras = paste0(
      "Género: ", genero, "<br>",
      "Tipo de cuidado: ", case_when(
        tipo_cuidado_gráfico == "Solo NNA" ~ "Cuidado solo de menores",
        tipo_cuidado_gráfico == "Solo persona adulta" ~ "Cuidado solo de mayores",
        tipo_cuidado_gráfico == "Ambos" ~ "Cuidado de menores y mayores"
      ), "<br>",
      "Del total de cuidadores: ", n, "/", total_general, " (", porcentaje_general, "%)<br>",
      "Dentro del género: ", n, "/", total_genero, " (", porcentaje_genero, "%)"
    )
  )

# Gráfico 
p40 <- plot_ly(
  data = porcentajes,
  x = ~genero,
  y = ~porcentaje_general,  
  color = ~tipo_cuidado_gráfico,
  colors = c("Solo NNA" = "steelblue2", 
             "Solo persona adulta" = "grey80", 
             "Ambos" = "darkmagenta"),
  type = "bar",
  text = ~hovertext_barras,
  hoverinfo = "text",
  textposition = "none"
) %>%
  layout(
    barmode = "group",
    title = list(text = "<b>Gráfico 41. Distribución del trabajo de cuidado por género</b>", 
                 x = 0.5, font = list(size = 13)),
    xaxis = list(title = ""),
    yaxis = list(
      title = "", 
      ticksuffix = "%",
      range = c(0, max(porcentajes$porcentaje_general) * 1.2)  
    ),
    legend = list(
      orientation = "v",  
      x = 1,             
      y = 0.5,           
      xanchor = "left",   
      title = list(text = "<b>Persona cuidada</b>")  
    ),
    margin = list(r = 120, b = 120),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.25,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>Porcentaje calculado sobre el total de personas cuidadoras</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p40

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Cantidad de horas dedicadas al cuidado
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
p41 <- plot_ly(
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
      text = "<b> Gráfico 42. Distribución de horas diarias de cuidado por género</b>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p41

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
p42 <- plot_ly(
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
      text = "<b> Gráfico 43. Distribución de días semanales de cuidado por género</b>",
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
      font = list(size = 12)  
    ),
    margin = list(r = 150, b = 100),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p42

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Estrategias de conciliación individual
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
p43 <- plot_ly(
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
      text = "<b>Gráfico 44. Estrategias individuales de conciliación laboral/académica y familiar</b>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
  ))

p43

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Efectos subjetivos en las personas cuidadoras por género
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
p44 <- plot_ly(
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
    opacity = 0.8  
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 45. Efectos del cuidado por género</b>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )))

p44

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Apoyos desde la comunidad universitaria
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
p45 <- plot_ly(
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
      text = "<b>Gráfico 46. Apoyos desde la comunidad universitaria</b>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
  )

)

p45

#///////////////////////////////////////////////////////////////////////////////
# Pestaña LGBTIQA+----
# Gráfico. Respeto de derechos de personas LGBTIQA+ según identidad de género
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
p46 <- plot_ly(
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
    text = "<b>Gráfico 47. ¿Se respetan los derechos de las personas LGBTIQA+ en la USACH?, según identidad de género</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
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
      text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i>",
      showarrow = F,
      font = list(size = 10.5, color = "grey"),
      align = "center"
    )
  )
  )

p46

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Respeto de derechos LGBTIQA+ según orientación sexual
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
  ) %>%
  mutate(orient_sex = factor(orient_sex,
                         levels = c("Asexual",
                                    "Bisexual",
                                    "Demisexual",
                                    "Gay",
                                    "Lesbiana",
                                    "Pansexual",
                                    "NS/NC*"),
                         ordered = T))
# Gráficar
p47 <- plot_ly(
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
    text = "<b>Gráfico 48. ¿Se respetan los derechos de las personas LGBTIQA+ en la USACH?, según orientación sexual</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
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
      text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>NS/NC* = No sabe/No contesta</i>",
      showarrow = F,
      font = list(size = 10.5, color = "grey"),
      align = "center"
    )
  )
  )

p47

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Sufrió discriminación en la Usach por identidad de género
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
p48 <- plot_ly(
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
      text = "<b>Gráfico 49. ¿Sufrió discriminación alguna vez en la USACH?</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
    )

p48

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
p49 <- plot_ly(
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
      text = "<b>Gráfico 50. ¿Sufrió discriminación alguna vez en la USACH durante 2023?</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p49

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Situaciones de violencia por identidad de género
# Preparar datos - Femenino
violencias_fem <- tibble(
  tipo_violencia = c("Hostigamiento", "Le ridiculizaron", "Le acosaron sexualmente"),
  porcentaje = c(31, 44, 37)
)

# Graficar
p50 <- plot_ly(
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
      text = "<b>Gráfico 51. Tipos de violencia experimentada por identidad de género</b><br><span style='font-size:12px'>Femenino  - personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>Los porcentajes de las categorías no son aditivos</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p50

#///////////////////////////////////////////////////////////////////////////////
# Preparar datos - Masculino
violencias_masc <- tibble(
  tipo_violencia = c("Le ridiculizaron", "Le insultaron", "Recibió hostigamiento"),
  porcentaje = c(50, 38, 31)
)

# Graficar
p51 <- plot_ly(
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
      text = "<b>Gráfico 52. Tipos de violencia experimentada por identidad de género</b><br><span style='font-size:12px'>Masculino  - personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>Los porcentajes de las categorías no son aditivos</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p51

#///////////////////////////////////////////////////////////////////////////////
# Preparar datos - Trans
violencias_trans <- tibble(
  tipo_violencia = c("No respetaron su IG*", "Dificultaron el derecho a registrar su IG*",
                    "Le ridiculizaron"),
  porcentaje = c(91, 26, 22)
)

# Graficar
p52 <- plot_ly(
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
      text = "<b>Gráfico 53. Tipos de violencia experimentada por identidad de género</b><br><span style='font-size:12px'>Trans  - personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>IG* = Identidad de Género</i><br><i>Los porcentajes de las categorías no son aditivos</i>",
        showarrow = F,
        font = list(size = 10, color = "grey"),
        align = "center"
      )
    )
  )

p52

#///////////////////////////////////////////////////////////////////////////////
# Preparar datos - Género diverso
violencias_gd <- tibble(
  tipo_violencia = c("No respetaron su IG*", "Le ridiculizaron", 
                     "Dificultaron el derecho a registrar su IG*"),
  porcentaje = c(88, 29, 21)
  
)

# Graficar
p53 <- plot_ly(
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
      text = "<b>Gráfico 54. Tipos de violencia experimentada por identidad de género</b><br><span style='font-size:12px'>Género diverso  - personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>IG* = Identidad de Género</i><br><i>Los porcentajes de las categorías no son aditivos</i>",
        showarrow = F,
        font = list(size = 10, color = "grey"),
        align = "center"
      )
    )
  )

p53

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Situaciones de violencia por orientación sexual
# Preparar datos - Lesbiana
violencias_lesb <- tibble(
  tipo_violencia = c("No respetaron su IG*", "Le ridiculizaron", "Le acosaron sexualmente",
                     "Le insultaron", "Le gritaron"),
  porcentaje = c(41, 36, 36, 32, 32)
)

# Graficar
p54 <- plot_ly(
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
      text = "<b>Gráfico 55. Tipos de violencia experimentada por orientación sexual</b><br><span style='font-size:12px'>Lesbiana  - personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>IG* = Identidad de Género</i><br><i>Los porcentajes de las categorías no son aditivos</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p54

#///////////////////////////////////////////////////////////////////////////////
# Preparar datos - Gay
violencias_gay <- tibble(
  tipo_violencia = c("Le ridiculizaron", "Le insultaron", "No respetaron su IG*",
                     "Le gritaron", "Hostigamiento"),
  porcentaje = c(46, 46, 29, 25, 25)
  
)

# Graficar
p55 <- plot_ly(
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
      text = "<b>Gráfico 56. Tipos de violencia por orientación sexual</b><br><span style='font-size:13px'>Gay  - personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>IG* = Identidad de Género</i><br><i>Los porcentajes de las categorías no son aditivos</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p55

#///////////////////////////////////////////////////////////////////////////////
# # Preparar datos - Bisexual
violencias_bi <- tibble(
  tipo_violencia = c("Le insultaron", "Le acosaron sexualmente", "Le ridiculizaron", 
                     "No respetaron su IG*"),
  porcentaje = c(24, 29, 35, 37)
)

# Graficar
p56 <- plot_ly(
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
      text = "<b>Gráfico 57. Tipo de violencia experimentada por orientación sexual</b><br><span style='font-size:13px'>Bisexual  - personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>IG* = Identidad de Género</i><br><i>Los porcentajes de las categorías no son aditivos</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p56

#///////////////////////////////////////////////////////////////////////////////
# # Preparar datos - Pansexual
violencias_pan <- tibble(
  tipo_violencia = c("Le acosaron sexualmente", "Hostigamiento", "Le ridiculizaron",
                     "No respetaron su IG*"),
  porcentaje = c(25, 25, 36, 59)
)

# Graficar
p57 <- plot_ly(
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
      text = "<b>Gráfico 58. Tipos de violencia experimentada por orientación sexual</b><br><span style='font-size:13px'>Pansexual  - personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>IG* = Identidad de Género</i><br><i>Los porcentajes de las categorías no son aditivos</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p57

# //////////////////////////////////////////////////////////////////////////////
# Gráfico. Experiencia en la universidad como persona LGTBQIA+
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

# Filtar sólo las evaluaciones positivas
experiencias_p <- experiencias %>%
  filter(categoria == "Positiva")

# Graficar
p58 <- plot_ly(
  data = experiencias_p,
  y = ~variable,          
  x = ~porcentaje,        
  color = ~categoria,
  colors = c("Positiva" = "#8D69F3"),  
  type = "bar",
  textposition = "none",
  hoverinfo = "text",
  hovertext = ~paste("Porcentaje:", round(porcentaje, 1), "%")  
) %>%
  layout(hoverlabel = list(
    bgcolor = "white",
    bordercolor = "#8D69F3",  
    font = list(
      color = "black",  
      size = 12
    )
  ),
    title = list(
      text = "<b>Gráfico 59. Valoración positiva de la calidad de vida en la USACH como persona LGBTIQA+</b>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center'
    ),
    yaxis = list(
      title = "",
      categoryorder = "array",
      categoryarray = unique(experiencias_p$variable),
      automargin = T
    ),
    xaxis = list(
      title = "",
      range = c(0, 80),
      ticksuffix = "%"
    ),
    barmode = "stack",
    margin = list(t = 40, b = 100),
    showlegend = F,  
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2023).</i><br><i>VG* = Violencia de Género</i><br><i> Contempla la suma de las notas 3 y 4 (escala 1 a 4).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p58

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Compromiso USACH con el reconocimiento diversidad género
compromiso <- tibble(genero = c(
  "Femenino", "Masculino", "Género diverso", "Trans*", "Prefiero no decir"),
  porcentaje = c(88, 83, 63, 92, 67))

# Graficar
p59 <- plot_ly(
  data = compromiso,
  x = ~genero,          
  y = ~porcentaje,              
  type = "bar",
  color = ~genero,      
  colors = colorRampPalette(brewer.pal(5, "RdPu"))(5),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 60. ¿La USACH está comprometida con el reconocimiento de la diversidad de género?</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>*Contempla ponderación entre trans masculino (82%) y trans femenino (100%).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p59

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. USACH como espacio seguro y libre de discriminación
seguro <- tibble(genero = c( "Femenino", "Masculino", "Género diverso", "Trans*", "Prefiero no decir"),
                 porcentaje = c(69, 75, 61, 53, 50))

# Graficar
p60 <- plot_ly(
  data = seguro,
  x = ~genero,          
  y = ~porcentaje,              
  type = "bar",
  color = ~genero,      
  colors = colorRampPalette(brewer.pal(5, "RdPu"))(5),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 61. ¿La USACH es un espacio seguro (libre de discriminación y violencia)?</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,80)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>*Contempla ponderación entre trans masculino (55%) y trans femenino (50%).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p60

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Plataformas de comunicación
com <- tibble(genero = c( "Femenino", "Masculino", "Género diverso", "Trans*", "Prefiero no decir"),
              porcentaje = c(79, 82, 59, 82, 50))

# Graficar
p61 <- plot_ly(
  data = com,
  x = ~genero,          
  y = ~porcentaje,              
  type = "bar",
  color = ~genero,      
  colors = colorRampPalette(brewer.pal(5, "RdPu"))(5),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 62. ¿Las plataformas de comunicación de la USACH integran y valoran la diversidad de género?</b><br><span style='font-size:13px'>Personas que responden 'Sí'</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,90)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>*Contempla ponderación entre trans masculino (91%) y trans femenino (67%).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p61

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Elementos de desigualdad. Notas 1-7
desigual <- tibble(v1 = c("Contenidos bibliográficos integran PG*", "En sala de clases se usa lenguaje inclusivo y no sexista",
                          "En sala de clases se respeta la IG*"),
                   nota = c(3.2, 3.6, 4.6))


# Graficar
p62 <- plot_ly(
  data = desigual,
  x = ~nota,          
  y = ~v1,              
  type = "bar",
  orientation = 'h',
  color = ~v1,      
  colors = colorRampPalette(brewer.pal(3, "RdPu"))(3),
  textposition = 'none',
  text = ~paste0(nota),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Nota:", nota
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 63. Elementos de desigualdad: dimensión relacional con el profesorado</b><br><span style='font-size:13px'>Evaluación con nota de 1 a 7</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, range = c(1,7), tickvals = 1:7),
    yaxis = list(title = "", categoryorder = "total ascending"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.3,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>PG* = Perspectiva de género</i><br><i>IG* = Identidad de género.</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p62

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Respeto identidad de género en trámites. Notas 1-7
respeto_ig <- tibble(genero = c("Femenino", "Masculino", "Género diverso", "Trans*", "Prefiero no decir", "Total"),
                     nota = c(4.3, 4.6, 3.9, 4.1, 3, 4))

# Graficar
p63 <- plot_ly(
  data = respeto_ig,
  x = ~nota,          
  y = ~genero,              
  type = "bar",
  orientation = 'h',
  color = ~genero,      
  colors = colorRampPalette(brewer.pal(6, "RdPu"))(6),
  textposition = 'none',
  text = ~paste0(nota),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Nota:", nota
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 64. Elementos de desigualdad: respeto identidad de género en trámites</b><br><span style='font-size:13px'>Evaluación con nota de 1 a 7</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, range = c(1,5), tickvals = 1:5),
    yaxis = list(title = "", categoryorder = "total ascending"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>*Contempla el promedio ponderado entre trans masculino (3,7) y trans femenino(4,7).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p63

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Demandas del estudiantado para la comunidad trans-GD
admision <- tibble(genero = c("Femenino", "Masculino", "Género diverso", "Trans*", "Prefiero no decir"),
                   porcentaje = c(87, 67, 93, 100, 50))

# Graficar
p64 <- plot_ly(
  data = admision,
  x = ~genero,          
  y = ~porcentaje,              
  type = "bar",
  color = ~genero,      
  colors = colorRampPalette(brewer.pal(5, "RdPu"))(5),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 65. Es fundamental que la USACH cuente con un proceso de admisión que reconozca la DG*</b><br><span style='font-size:13px'>Personas que están 'de acuerdo' o 'muy de acuerdo'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>DG* = diversidad de género</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p64

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Aprobación sobre cupo de ingreso trans
cupo <- tibble(genero = c("Femenino", "Masculino", "Género diverso", "Trans*", "Prefiero no decir", "Total"),
               nota = c(4.1, 3.2, 5.1, 5.1, 4.3, 4))
cupo <- cupo %>%
  mutate(
    genero = factor(genero,
                    levels = c("Total",
                               "Prefiero no decir",
                               "Trans*",
                               "Género diverso",
                               "Masculino",
                               "Femenino"),
                    ordered = T))

# Graficar
p65 <- plot_ly(
  data = cupo,
  x = ~nota,          
  y = ~genero,              
  type = "bar",
  orientation = 'h',
  color = ~genero,      
  colors = colorRampPalette(brewer.pal(6, "RdPu"))(6),
  textposition = 'none',
  text = ~paste0(nota),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Nota:", nota
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 66. Percepción sobre cupo de ingreso trans</b><br><span style='font-size:13px'>Evaluación con nota de 1 a 7'</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, range = c(1,6), tickvals = 1:6),
    yaxis = list(title = "", categoryorder = "array", autorange = "reversed"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>*Contempla el promedio ponderado entre trans masculino (5,0) y trans femenino(5,5).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p65

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Reconocimiento diversidad: Firmar listas de asistencia o evaluaciones con nombre social
firmar <- tibble(genero = c("Femenino", "Masculino", "Género diverso", "Trans", "Prefiero no decir"),
                   porcentaje = c(89, 66, 95, 100, 67))

# Graficar
p66 <- plot_ly(
  data = firmar,
  x = ~genero,          
  y = ~porcentaje,              
  type = "bar",
  color = ~genero,      
  colors = colorRampPalette(brewer.pal(5, "RdPu"))(5),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 67. Firmar con nombre social aunque no se haya realizado el cambio legal o institucional</b><br><span style='font-size:13px'>Personas que están 'de acuerdo' o 'muy de acuerdo'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p66

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Infraestructura respetuosa de la diversidad de género
infra <- tibble(genero = c("Femenino", "Masculino", "Género diverso", "Trans*", "Prefiero no decir"),
                porcentaje = c(83, 66, 93, 88, 83))


# Graficar
p67 <- plot_ly(
  data = infra,
  x = ~genero,          
  y = ~porcentaje,              
  type = "bar",
  color = ~genero,      
  colors = colorRampPalette(brewer.pal(5, "RdPu"))(5),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 68. Infraestructura respetuosa a la diversidad de género</b><br><span style='font-size:13px'>Personas que están 'de acuerdo' o 'muy de acuerdo'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>*Contempla la ponderación entre trans masculino (91%) y trans femenino (83%).</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p67

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Servicio de salud mental para acompañar procesión de transción
salud <- tibble(genero = c("Femenino", "Masculino", "Género diverso", "Trans*", "Prefiero no decir"),
                porcentaje = c(73, 68, 76, 76, 67))

# Graficar
p68 <- plot_ly(
  data = salud,
  x = ~genero,          
  y = ~porcentaje,              
  type = "bar",
  color = ~genero,      
  colors = colorRampPalette(brewer.pal(5, "RdPu"))(5),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 69. Servicio de salud mental para acompañar procesos de transición</b><br><span style='font-size:13px'>Personas que están 'de acuerdo' o 'muy de acuerdo'</span>",
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
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>*Contempla la ponderación entre trans masculino (82%) y trans femenino (67%).</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p68

#///////////////////////////////////////////////////////////////////////////////
# Pestaña Violencias de género----
# Gráficos. Violencia de género. Expresión, vivencia y reconocimiento
vg_1 <- tibble(v1 = c("La vestimenta de las mujeres no está relacionado a ser víctimas de acoso",
                      "Las personas LGBTIQA+ tienen derecho a expresar afectividades en público",
                      "Los hombres son tan víctimas como las mujeres de violencia de género"),
               porcentaje = c(96, 95, 61))

# Graficar
p69 <- plot_ly(
  data = vg_1,
  x = ~porcentaje,          
  y = ~v1,              
  type = "bar",
  orientation = 'h',
  color = ~v1,      
  colors = colorRampPalette(brewer.pal(3, "PuBu"))(3),
  textposition = 'none',
  text = ~paste0(porcentaje),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 70. Expresión, vivencias y reconocimiento</b><br><span style='font-size:13px'>Personas que están 'de acuerdo' o 'muy de acuerdo' con los enunciados</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(
      title = "", 
      tickangle = 0, 
      range = c(0, 100),
      ticksuffix = "%"  
    ),
    yaxis = list(
      title = "", 
      categoryorder = "array", 
      autorange = "reversed",
      ticklabelposition = "outside right",  
      tickfont = list(size = 11)  
    ),
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

p69

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Tránsito y consolidación de percepciones
vg_2 <- tibble(v1 = c("Las denuncias de acoso son mayoritariamente verdaderas",
                      "Casos de VG* deben ser abordados por la comunidad en conjunto",
                      "No debe naturalizarse la VG* en espacios cotidianos ni laborales",
                      "No es algo puntual ni una exageración de quienes viven VG*"),
               porcentaje = c(91, 93, 94, 97))

# Graficar
p70 <- plot_ly(
  data = vg_2,
  x = ~porcentaje,          
  y = ~v1,              
  type = "bar",
  orientation = 'h',
  color = ~v1,      
  colors = colorRampPalette(brewer.pal(3, "PuBu"))(3),
  textposition = 'none',
  text = ~paste0(porcentaje),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 71. Tránsito y consolidación de percepciones</b><br><span style='font-size:13px'>Personas que están 'de acuerdo' o 'muy de acuerdo' con los enunciados</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(
      title = "", 
      tickangle = 0, 
      range = c(0, 100),
      ticksuffix = "%"  
    ),
    yaxis = list(
      title = "", 
      categoryorder = "array", 
      autorange = "reversed",
      ticklabelposition = "outside right",  
      tickfont = list(size = 11)  
    ),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>VG* = Violencia de género</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p70

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Ni el alcohol ni las drogas son causantes de VG
vg_3 <- tibble(estamento = c("Funcionariado", "Estamento académico/profesorado por hora", "Estudiantes"),
               porcentaje = c(78, 86, 83))

# Graficar
p71 <- plot_ly(
  data = vg_3,
  x = ~porcentaje,          
  y = ~estamento,              
  type = "bar",
  orientation = 'h',
  color = ~estamento,      
  colors = colorRampPalette(brewer.pal(3, "PuBu"))(3),
  textposition = 'none',
  text = ~paste0(porcentaje),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 72. Ni el alcohol ni las drogas son causantes de VG*</b><br><span style='font-size:13px'>Personas que están 'de acuerdo' o 'muy de acuerdo' con los enunciados. Por estamento</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(
      title = "", 
      tickangle = 0, 
      range = c(0, 100),
      ticksuffix = "%"  
    ),
    yaxis = list(
      title = "", 
      categoryorder = "array", 
      autorange = "reversed",
      ticklabelposition = "outside right",  
      tickfont = list(size = 11)  
    ),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>VG* = Violencia de género</i>",
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p71

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Las personas que ejercen violencia tiene problemas psicológicos
vg_4 <- tibble(genero = c("Femenino", "Masculino", "Trans/Género diverso", "Prefiero no decir"),
               porcentaje = c(56, 67, 47, 63))

# Graficar
p72 <- plot_ly(
  data = vg_4,
  x = ~genero,          
  y = ~porcentaje,              
  type = "bar",
  color = ~genero,      
  colors = colorRampPalette(brewer.pal(4, "PuBu"))(4),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 73. Las personas que ejercen violencia tienen problemas psicológicos</b><br><span style='font-size:13px'>Personas que están 'de acuerdo' o 'muy de acuerdo' con los enunciados. Por género</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,70)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p72

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. USACH espacio seguro mujeres/disidencias
vg_5 <- tibble(genero = c("Para mujeres", "Para población LGBTIQA+"),
                  porcentaje = c(55, 63))
# Graficar
p73 <- plot_ly(
  data = vg_5,
  x = ~genero,          
  y = ~porcentaje,              
  type = "bar",
  color = ~genero,      
  colors = c("Para mujeres" = "#FF69B4", "Para población LGBTIQA+" = "#6600FF"),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 74. La USACH es un espacio seguro</b><br><span style='font-size:13px'>Personas que están 'de acuerdo' o 'muy de acuerdo' con los enunciados</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, categoryorder = "total ascending"),
    yaxis = list(title = "", ticksuffix = "%", range = c(0,70)),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p73

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Lugares donde ocurren los hechos de violencia
vg_6 <- tibble(
  estamento = rep(c("Funcionario", "Académico", "Estudiantil"), each = 5),
  v1 = rep(c("Al interior del campus", 
             "Fuera del campus por actividades laborales o académicas", 
             "Fuera del campus en contexto no universitario", 
             "TICs* con personas de la universidad", 
             "No recuerda"), 
           times = 3),
  porcentaje = c(
    # Porcentajes para funcionarios
    96, 6, 6, 9, 5,
    # Porcentajes para académicos 
    90, 4, 9, 16, 2,
    # Porcentajes para estudiantes
    82, 8, 31, 32, 5
  )
)

# Graficar
p74 <-plot_ly(
  data = vg_6,
  y = ~v1,
  x = ~porcentaje,
  color = ~estamento,
  type = "bar",
  orientation = "h",
  text = ~paste("Estamento:", estamento, "<br>Lugar:", v1, "<br>Porcentaje:", porcentaje, "%"),
  colors = colorRampPalette(brewer.pal(3, "PuBu"))(3),
  hoverinfo = "text",
  textposition = "none"
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 75. Lugar donde ocurren hechos de violencia</b><br><span style='font-size:13px'>Por estamento</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, range = c(0,100), ticksuffix = "%"),
    yaxis = list(title = "", categoryorder = "total ascending"),
    barmode = "group",          
    legend = list(
      orientation = "v", 
      x = 1.02,  
      y = 0.5,   
      xanchor = "left", 
      title = list(text = "<b>Estamento</b>", font = list(size = 12)),
      font = list(size = 12)
    ),
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>TICs* = Tecnologías de la Información y Comunicación</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p74

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Percepciones: ¿Qué tipos de violencia ocurren en la USACH y cómo se da según género? - Mujeres
vg_7 <- tibble(v1 = c("Mansplainning", "Miradas sexualizadas", "Bromas u ofensas", "Tocaciones", "Hostigamiento por redes sociales",
                      "Ofensas por no usar IG* dominante"),
               porcentaje = c(46, 40, 30, 23, 10, 10))

# Graficar
p75 <- plot_ly(
  data = vg_7,
  x = ~porcentaje,          
  y = ~v1,              
  type = "bar",
  color = ~v1, 
  orientation = "h",
  colors = "#9933FF",
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 76. Tipos de violencia de género que se dan en la USACH</b><br><span style='font-size:13px'>Mujeres</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, ticksuffix = "%", range = c(0,50)),
    yaxis = list(title = "",  categoryorder = "total ascending"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>IG* = Identidad de género</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p75

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Percepciones: ¿Qué tipos de violencia ocurren en la USACH y cómo se da según género? - Personas trans y GD
vg_8 <- tibble(v1 = c("Mansplainning", "Miradas sexualizadas", "Bromas u ofensas", "Tocaciones", "Hostigamiento por redes sociales",
                      "Ofensas por no usar IG* dominante"),
               porcentaje = c(44, 49, 35, 32, 14, 21))

# Graficar
p76 <- plot_ly(
  data = vg_8,
  x = ~porcentaje,          
  y = ~v1,              
  type = "bar",
  color = ~v1, 
  orientation = "h",
  colors = "#9933FF",
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 77. Tipos de violencia de género que se dan en la USACH</b><br><span style='font-size:13px'>Personas trans y de género diverso</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, ticksuffix = "%", range = c(0,50)),
    yaxis = list(title = "",  categoryorder = "total ascending"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>IG* = Identidad de género</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p76

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Percepciones: ¿Qué tipos de violencia ocurren en la USACH y cómo se da según género? - Hombres
vg_9 <- tibble(v1 = c("Mansplainning", "Miradas sexualizadas", "Bromas u ofensas", "Tocaciones", "Hostigamiento por redes sociales",
                      "Ofensas por no usar IG* dominante"),
               porcentaje = c(11, 31, 9, 9, 5, 5))

# Graficar
p77 <- plot_ly(
  data = vg_9,
  x = ~porcentaje,          
  y = ~v1,              
  type = "bar",
  color = ~v1, 
  orientation = "h",
  colors = "#9933FF",
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 78. Tipos de violencia de género que se dan en la USACH</b><br><span style='font-size:13px'>Hombres</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, ticksuffix = "%", range = c(0,40)),
    yaxis = list(title = "",  categoryorder = "total ascending"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>IG* = Identidad de género</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p77

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Percepciones: ¿Qué tipos de violencia ocurren en la USACH y cómo se da según género? - Prefiero no decir
vg_10 <- tibble(v1 = c("Mansplainning", "Miradas sexualizadas", "Bromas u ofensas", "Tocaciones", "Hostigamiento por redes sociales",
                      "Ofensas por no usar IG* dominante"),
               porcentaje = c(31, 49, 34, 28, 13, 19))

# Graficar
p78 <- plot_ly(
  data = vg_10,
  x = ~porcentaje,          
  y = ~v1,              
  type = "bar",
  color = ~v1, 
  orientation = "h",
  colors = "#9933FF",
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 79. Tipos de violencia de género que se dan en la USACH</b><br><span style='font-size:13px'>Prefiero no decir mi género</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, ticksuffix = "%", range = c(0,50)),
    yaxis = list(title = "",  categoryorder = "total ascending"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>IG* = Identidad de género</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p78

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Consecuencias de la violencia de género
vg_11 <- tibble(
  estamento = rep(c("Funcionario", "Académico", "Estudiantil"), each = 4),
  v1 = rep(c("Malestar físico", 
             "Malestar psicológico", 
             "Bajo rendimiento o productividad/notas", 
             "Pérdida de oportunidades laborales/académicas"), 
           times = 3),
  porcentaje = c(
    # Porcentaje para funcionarios
    25, 47, 21, 24,
    # Porcentaje para académicos
    25, 41, 21, 27,
    # Porcentaje para estudiantes
    19, 39, 17, 9
  )) %>%
  mutate(
    v1 = factor(v1,
                    levels = c("Malestar físico",
                               "Malestar psicológico",
                               "Bajo rendimiento o productividad/notas",
                               "Pérdida de oportunidades laborales/académicas"),
                    ordered = T))
  

# Graficar
p79 <-plot_ly(
  data = vg_11,
  y = ~v1,
  x = ~porcentaje,
  color = ~estamento,
  type = "bar",
  orientation = "h",
  text = ~paste("Estamento:", estamento, "<br>Lugar:", v1, "<br>Porcentaje:", porcentaje, "%"),
  colors = colorRampPalette(brewer.pal(3, "PuBu"))(3),
  hoverinfo = "text",
  textposition = "none"
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 80. Consecuencias de la violencia de género</b><br><span style='font-size:13px'>Por estamento</span>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", tickangle = 0, range = c(0,60), ticksuffix = "%"),
    yaxis = list(title = "", categoryorder = "array", autorange = "reversed"),
    barmode = "group",          
    legend = list(
      orientation = "v", 
      x = 1.02,  
      y = 0.5,   
      xanchor = "left", 
      title = list(text = "<b>Estamento</b>", font = list(size = 12)),
      font = list(size = 12)
    ),
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i><br><i>Puede señalar más de una consecuencia</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p79

#///////////////////////////////////////////////////////////////////////////////
# Gráfico. Expectativas de quienes sufrieron violencia de género
vg_12 <- tibble(v1 = c("Validación de la experiencia", "Campañas, talleres o actividades preventivas",
                       "Apoyo de autoridades/jefaturas", "Tratamiento psicológico", "Apoyo académico/laboral",
                       "Orientación jurídica"),
                porcentaje = c(51, 33, 21, 20, 14, 12))

# Graficar
p80 <- plot_ly(
  data = vg_12,
  x = ~porcentaje,          
  y = ~v1,              
  type = "bar",
  orientation = "h",
  color = ~v1,      
  colors = colorRampPalette(brewer.pal(6, "PuBu"))(6),
  textposition = 'none',
  text = ~paste0(porcentaje, "%"),    
  hoverinfo = "text",
  hovertext = ~paste(
    "<br>Porcentaje:", porcentaje, "%"
  )
) %>%
  layout(
    title = list(
      text = "<b>Gráfico 81. Expectativas de quienes sufrieron violencia de género</b>",
      font = list(size = 13),
      x = 0.5,
      xanchor = 'center',
      yanchor = 'top'
    ),
    xaxis = list(title = "", ticksuffix = "%", range = c(0,60)),
    yaxis = list( title = "", tickangle = 0, categoryorder = "total ascending"),
    barmode = "group",          
    showlegend = F,
    margin = list(b = 120, t = 80),
    annotations = list(
      list(
        x = 0.5,   
        y = -0.225,  
        xref = "paper",  
        yref = "paper",
        text = "<i>Fuente: Observatorio Género y Diversidad (2025).</i>" ,
        showarrow = F,
        font = list(size = 10.5, color = "grey"),
        align = "center"
      )
    )
  )

p80
