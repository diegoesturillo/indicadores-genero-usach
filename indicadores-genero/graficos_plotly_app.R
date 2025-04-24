# Gráficos en formato plotly
# Carga de paquetes
pacman::p_load(here, ggrepel, plotly, purrr, readxl, rio, scales, tidyverse)

# Gráfico 1. Cantidad de académicos por género en la USACH (2018-2024)----
# Cargar datos
ema <- read_excel(here("01-input", "data-orig", "CONSOLIDADO EMA 2018-2024_USACH.xlsx"))

# Calcular proporciones
academicos <- ema %>%
  mutate(
    prop_fem = round((n_acad_f / n_acad_total)*100, 0),
    prop_mas = round((n_acad_m / n_acad_total)*100, 0)
  ) %>%
  pivot_longer(
    cols = c(n_acad_f, n_acad_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "n_acad_f" = "Femenino",
                    "n_acad_m" = "Masculino"),
    prop_texto = ifelse(genero == "Femenino",
                        paste0(prop_fem, "%"),
                        paste0(prop_mas, "%")),
    texto_tooltip = paste0(
      "Año: ", año,
      "<br>Género: ", genero,
      "<br>Cantidad: ", cantidad,
      "<br>Proporción: ", prop_texto
    )
  )

# Graficar
plot_acad_genero <- plot_ly(
  data = academicos,
  x = ~año,
  y = ~cantidad,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
  type = 'scatter',
  mode = 'lines+markers',
  text = ~texto_tooltip,
  hoverinfo = 'text',
  line = list(width = 2),
  marker = list(size = 6)
) %>%
  layout(
    title = list(
      text = "<b>Académicos por género (2018-2024)</b>",
      font = list(size = 14),
      x = 0.5
    ),
    xaxis = list(title = "", dtick = 1),
    yaxis = list(title = "", range = c(500,2500)),
    legend = list(
      title = list(text = "<b>Género</b>"),
      orientation = "h",
      x = 0.15,
      y = -0.2),
    margin = list(t = 80),
    hovermode = "closest"
  )

plot_acad_genero

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 2. Cantidad de académicos por género y grado académico----
# Cargar datos
acad_grados <- read_excel(here("01-input", "data-orig","CONSOLIDADO EMA 2018-2024_USACH.xlsx"))

# Crear base larga con cantidades y proporciones
academicos_grados <- acad_grados %>%
  transmute(
    año,
    `Doctorado Mujer` = n_acad_doc_f,
    `Doctorado Hombre` = n_acad_doc_m,
    `Magíster Mujer` = n_acad_mg_f,
    `Magíster Hombre` = n_acad_mg_m,
    `Licenciatura Mujer` = n_acad_lic_f,
    `Licenciatura Hombre` = n_acad_lic_m,
    prop_doc_f = round(n_acad_doc_f / n_acad_doc * 100, 0),
    prop_doc_m = round(n_acad_doc_m / n_acad_doc * 100, 0),
    prop_mg_f = round(n_acad_mg_f / n_acad_mg * 100, 0),
    prop_mg_m = round(n_acad_mg_m / n_acad_mg * 100, 0),
    prop_lic_f = round(n_acad_lic_f / n_acad_lic * 100, 0),
    prop_lic_m = round(n_acad_lic_m / n_acad_lic * 100, 0)
  ) %>%
  pivot_longer(
    cols = starts_with(c("Doctorado", "Magíster", "Licenciatura")),
    names_to = "grupo",
    values_to = "n"
  ) %>%
  mutate(
    genero = if_else(str_detect(grupo, "Mujer"), "Mujer", "Hombre"),
    grado = case_when(
      str_detect(grupo, "Doctorado") ~ "Doctorado",
      str_detect(grupo, "Magíster") ~ "Magíster",
      str_detect(grupo, "Licenciatura") ~ "Licenciatura"
    ),
    prop = case_when(
      grupo == "Doctorado Mujer" ~ prop_doc_f,
      grupo == "Doctorado Hombre" ~ prop_doc_m,
      grupo == "Magíster Mujer" ~ prop_mg_f,
      grupo == "Magíster Hombre" ~ prop_mg_m,
      grupo == "Licenciatura Mujer" ~ prop_lic_f,
      grupo == "Licenciatura Hombre" ~ prop_lic_m
    ),
    text = paste0(
      "Año: ", año, "<br>",
      "Grado: ", grado, "<br>",
      "Género: ", genero, "<br>",
      "Cantidad: ", n, "<br>",
      "Proporción: ", prop, "%"
    )
  )

# Ordenar factor de grado
academicos_grados$grado <- factor(
  academicos_grados$grado,
  levels = c("Doctorado", "Magíster", "Licenciatura")
)
academicos_grados$grupo<- factor(
  academicos_grados$grupo,
  levels = c("Doctorado Mujer","Doctorado Hombre",
             "Magíster Mujer", "Magíster Hombre", 
             "Licenciatura Mujer", "Licenciatura Hombre")
)

# Colores
colores <- c(
  "Doctorado Mujer" = "#8D69F3",
  "Doctorado Hombre" = "#41776E",
  "Magíster Mujer" = "#BDA6FF",
  "Magíster Hombre" = "#8AC0B5",
  "Licenciatura Mujer" = "#D1C5FA",
  "Licenciatura Hombre" = "#A9D6CD"
)

# Gráfico con facet
p_facet <- ggplot(academicos_grados, aes(x = año, y = n, color = grupo, text = text, group = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~grado, ncol = 1, strip.position = "top", scales = "free_y") +
  scale_color_manual(values = colores) +
  labs(x = "", y = "", color = "") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold"),
    strip.placement = "outside"
  )

plot_acad_grado <- ggplotly(p_facet, tooltip = "text") %>%
  layout(
    title = list(
      text = "<b>Académicos por género y grado académico (2018-2024)</b>",
      font = list(size = 14),
      x = 0.5),
    legend = list(title = list(text = "<b>Grado y género</b>")),
    margin = list(t = 80))

plot_acad_grado

# Gráfico 3. Académicos por género y jerarquía - 3 jerarquías----
# Carga de datos
ema_jq <- read_excel(here("01-input", "data-orig", "CONSOLIDADO EMA 2018-2024_USACH.xlsx"), 
                     sheet = "ACAD_JQ")

# Preparar datos con cantidades absolutas y proporciones internas
academicos_jerarquia <- ema_jq %>%
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
    proporcion = round((valor / total_jerarquia)*100, 0)
  ) %>%
  ungroup() %>%
  filter(tipo=="detalle") %>%
  rename(cantidad = valor) %>%
  select(año, genero, jerarquia, cantidad, proporcion) %>%
  mutate(
    grupo = interaction(jerarquia, genero, sep = " ", lex.order = T),
    jerarquia = factor(jerarquia, levels = c("Titular", "Asociado/a", "Asistente", "Instructor/a", "Adjunto/a", "Sin jerarquía")),
    genero = factor(genero, levels = c("Mujeres", "Hombres"))
  ) %>%
  filter(jerarquia %in% c("Titular", "Asociado/a", "Asistente")) %>%
  mutate(
    grupo = factor(
      interaction(jerarquia, genero, sep = " "),
      levels = c(
        "Titular Mujeres", "Titular Hombres",
        "Asociado/a Mujeres", "Asociado/a Hombres",
        "Asistente Mujeres", "Asistente Hombres"
      )
    )
  )

# Graficar
plot_jerarquias <- academicos_jerarquia %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,  
    color = ~grupo,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c(
      "Titular Mujeres" = "#6A5ACD",
      "Titular Hombres" = "#3C8DAD",
      "Asociado/a Mujeres" = "#9370DB",
      "Asociado/a Hombres" = "#6495ED",
      "Asistente Mujeres" = "#D8BFD8",
      "Asistente Hombres" = "#87CEEB"
    ),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Jerarquía:</b> ", jerarquia, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Cantidad:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", proporcion, "%"
    ),
    hoverinfo = 'text'
  ) %>%
  layout(
    title = list(
      text = "<b>Académicos por género y jerarquía (1) (2018-2024)</b>",
      font = list(size = 14),
      x = 0.5
    ),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Jerarquía y género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

plot_jerarquias

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 4. Académicos por género y jerarquía - 3 jerarquías----
# Preparar datos con cantidades absolutas y proporciones internas
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
    proporcion = round((valor / total_jerarquia)*100, 0)
  ) %>%
  ungroup() %>%
  filter(tipo=="detalle") %>%
  rename(cantidad = valor) %>%
  select(año, genero, jerarquia, cantidad, proporcion) %>%
  mutate(
    grupo = interaction(jerarquia, genero, sep = " ", lex.order = T),
    jerarquia = factor(jerarquia, levels = c("Titular", "Asociado/a", "Asistente", "Instructor/a", "Adjunto/a", "Sin jerarquía")),
    genero = factor(genero, levels = c("Mujeres", "Hombres"))
  ) %>%
  filter(jerarquia %in% c("Instructor/a", "Adjunto/a", "Sin jerarquía")) %>%
  filter(año!=2024) %>%
  mutate(
    grupo = factor(
      interaction(jerarquia, genero, sep = " "),
      levels = c(
        "Instructor/a Mujeres", "Instructor/a Hombres",
        "Adjunto/a Mujeres", "Adjunto/a Hombres",
        "Sin jerarquía Mujeres", "Sin jerarquía Hombres"
      )
    )
  )

# Graficar
plot_jerarquias_2 <- academicos_jerarquia_2 %>%
  plot_ly(
    x = ~año,
    y = ~cantidad,
    color = ~grupo,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c(
      "Instructor/a Mujeres" = "#A291FB",
      "Instructor/a Hombres" = "#4B5DFF",
      "Adjunto/a Mujeres" = "#B3C7F9",
      "Adjunto/a Hombres" = "#2D76F3",
      "Sin jerarquía Mujeres" = "#C8D8FF",
      "Sin jerarquía Hombres" = "#4472CA"
    ),
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Jerarquía:</b> ", jerarquia, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Cantidad:</b> ", cantidad, "<br>",
      "<b>Proporción:</b> ", proporcion, "%"
    ),
    hoverinfo = 'text'
  ) %>%
  layout(
    title = list(
      text = "<b>Académicos por género y jerarquía (2) (2018-2023)</b>",
      font = list(size = 14),
      x = 0.5
    ),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Jerarquía y género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

plot_jerarquias_2

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 5. Cantidad de publicaciones en revistas indexadas por género----
# Carga de datos
ema_pub <- read_excel(here("01-input", "data-orig", "CONSOLIDADO EMA 2018-2024_USACH.xlsx"), 
sheet = "ACAD_PUB")

acad_pub <- ema_pub %>%
  mutate(
    prop_f = round(n_pub_f/n_pub*100,0),
    prop_m = round(n_pub_m/n_pub*100,0)
  ) %>%
  pivot_longer(
    cols = c(n_pub_f, n_pub_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "n_pub_f" = "Femenino",
                    "n_pub_m" = "Masculino"),
    proporcion = case_when(
      genero == "Femenino" ~ prop_f,
      genero == "Masculino" ~ prop_m
    )
  )

# Graficar
plot_pub <- acad_pub %>%
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
      "<b>Proporción:</b> ", proporcion, "%"
    ),
    hoverinfo = 'text'
  ) %>%
  layout(
    title = list(
      text = "<b>Publicaciones en revistas indexadas por género (2018-2024)</b>",
      font = list(size = 14),
      x = 0.5
    ),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = "", range = c(0, 1800), dtick = 200),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest"
  ) %>%
  config(displayModeBar = FALSE)

plot_pub

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 6. Puestos directivos por género - 2 posiciones----
# Cargar datos
ema_puestos <- read_excel(here("01-input", "data-orig","CONSOLIDADO EMA 2018-2024_USACH.xlsx"),
                          sheet = "ACAD_DIRECCION")

# Filtrar puestos
ema_puestos_1 <- ema_puestos %>%
  filter(puesto %in% c("Junta Directiva", "Consejo Académico"))

# Calcular proporciones
ema_puestos_1 <- ema_puestos_1 %>%
  group_by(año, puesto) %>%
  mutate(
    total_puesto = sum(cantidad),
    prop = round((cantidad / total_puesto) * 100)
  ) %>%
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
      "<b>Proporción:</b> ", prop, "%"
    ),
    hoverinfo = 'text'
  )

plot_puestos_1

# Separar en dos subplots, uno por puesto
puestos_split_1 <- split(ema_puestos_1, ema_puestos_1$puesto)

subplots_1 <- lapply(puestos_split_1, function(df) {
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
      "<b>Proporción:</b> ", prop, "%"
    ),
    hoverinfo = 'text',
    showlegend = FALSE
  )
})

# Combinar subplots
plot_jd_ca <- subplot(subplots_1, nrows = 2, shareX = T, shareY = T, titleY = T) %>%
  layout(
    title = list(
      text = "<b>Puestos directivos por género (1)</b>",
      font = list(size = 14),
      x = 0.75
    ),
    legend = list(title = list(text = "Género")),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    yaxis2 = list(title = ""),
    margin = list(t = 80),
    annotations = list(
      list(x = 0, y = 1.02, text = "<b>Junta Directiva</b>", showarrow = FALSE, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11)),
      list(x = 0, y = 0.47, text = "<b>Consejo Académico</b>", showarrow = FALSE, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11))
    )
  )

plot_jd_ca

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 7. Puestos directivos por género - 2 posiciones----
# Filtrar puestos
ema_puestos_2 <- ema_puestos %>%
  filter(puesto %in% c("Decanato", "Vicedecanato"))

# Calcular proporciones
ema_puestos_2 <- ema_puestos_2 %>%
  group_by(año, puesto) %>%
  mutate(
    total_puesto = sum(cantidad),
    prop = round((cantidad / total_puesto) * 100)
  ) %>%
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
      "<b>Proporción:</b> ", prop, "%"
    ),
    hoverinfo = 'text'
  )

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
      "<b>Proporción:</b> ", prop, "%"
    ),
    hoverinfo = 'text',
    showlegend = FALSE
  )
})

# Combinar subplots
plot_d_vd <- subplot(subplots_2, nrows = 2, shareX = T, shareY = T, titleY = T) %>%
  layout(title = list(
    text = "<b>Puestos directivos por género (2)</b>",
    font = list(size = 14),
    x = 0.75),
    legend = list(title = list(text = "Género")),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    yaxis2 = list(title = ""),
    margin = list(t = 80),
    annotations = list(
      list(x = 0, y = 1.02, text = "<b>Decanato</b>", showarrow = FALSE, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11)),
      list(x = 0, y = 0.47, text = "<b>Vicedecanato</b>", showarrow = FALSE, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11))
    )
  )

plot_d_vd

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 8 . Puestos directivos por género - Dirección o jefatura----
# Filtrar puestos
ema_puestos_3 <- ema_puestos %>%
  filter(puesto=="Dirección o jefatura")

# Calcular proporciones
ema_puestos_3 <- ema_puestos_3 %>%
  group_by(año, puesto) %>%
  mutate(
    total_puesto = sum(cantidad),
    prop = round((cantidad / total_puesto) * 100)
  ) %>%
  ungroup()

# Graficar
plot_dir_jef <- ema_puestos_3 %>%
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
      "<b>Proporción:</b> ", prop, "%"
    ),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b>Puestos directivos por género - Dirección o jefatura</b>",
    font = list(size = 14),
    x = 0.5),
    title = "",
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 20),
    hovermode = "closest"
  )

plot_dir_jef

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 9. Matrícula pregrado por género - Matrícula total 2018-2024----
# Carga de datos
ema_mat <- read_excel(here("01-input", "data-orig", "CONSOLIDADO EMA 2018-2024_USACH.xlsx"), 
                      sheet = "MAT_PRE_GENERAL")

# Calcular proporciones y transoformar datos
ema_mat_total <- ema_mat %>%
  select(año, mt_pre_f, mt_pre_m) %>%
  pivot_longer(
    cols = c(mt_pre_f, mt_pre_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "mt_pre_f" = "Femenino",
                    "mt_pre_m" = "Masculino")) %>%
  group_by(año) %>%
  mutate(
    total_anual = sum(cantidad),
    proporcion = round((cantidad / total_anual) * 100)
  ) %>%
  ungroup()

# Graficar
plot_mat_total <- ema_mat_total %>%
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
      "<b>Proporción:</b> ", proporcion, "%"
    ),
    hoverinfo = 'text'
  ) %>%
  layout(title = list(
    text = "<b>Matrícula total de pregrado por género (2018-2024)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = "",
      tickvals = seq(8000, 14000, 1000),
      range = c(8000, 14000)
    ),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_mat_total

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 10. Matrícula pregrado por género - Matrícula 1er año 2018-2024----
# Calcular proporciones y transformar datos
ema_mat_1 <- ema_mat %>%
  select(año, m1_pre_f, m1_pre_m) %>%
  pivot_longer(
    cols = c(m1_pre_f, m1_pre_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "m1_pre_f" = "Femenino",
                    "m1_pre_m" = "Masculino")) %>%
  group_by(año) %>%
  mutate(
    total_anual = sum(cantidad),
    proporcion = round((cantidad / total_anual) * 100)
  ) %>%
  ungroup()

# Graficar
plot_mat_1 <- ema_mat_1 %>%
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
      "<b>Proporción:</b> ", proporcion, "%"
    ),
    hoverinfo = 'text'
  ) %>%
  layout(title = list(
    text = "<b>Matrícula de 1er año de pregrado por género (2018-2024)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = "",
      tickvals = seq(1500, 3500, 500),
      range = c(1500, 3500)
    ),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_mat_1

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 11. Tasa de retención por género 2018-2023----
# Calcular proporciones
ema_tr <- ema_mat %>%
  select(año, tasa_ret_f, tasa_ret_m) %>%
  pivot_longer(
    cols = c(tasa_ret_f, tasa_ret_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "tasa_ret_f" = "Femenino",
                    "tasa_ret_m" = "Masculino")) 

# Graficar
plot_tr <- ema_tr %>%
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
      "<b>Proporción:</b> ", cantidad, "%"
    ),
    hoverinfo = 'text'
  ) %>%
  layout(title = list(
    text = "<b>Tasa de retención por género (2018-2023)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = "",
      tickvals = seq(0.7, 0.9, 0.1),
      range = c(0.7, 0.9),
      tickformat = ".0%"
    ),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_tr

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 12. Matrícula total por área de conocimiento genérica 2018-2024----
# Cargar datos
ema_mat_area <- read_excel(here("01-input", "data-orig", "CONSOLIDADO EMA 2018-2024_USACH.xlsx"), 
                           sheet = "MAT_PRE_AC") 

# Preparar datos
ema_mat_area <- ema_mat_area %>%
  mutate(
    prop_f_mt = mt_f/mt,
    prop_m_mt = mt_m/mt,
    prop_f_m1 = m1_f/m1,
    prop_m_m1 = m1_m/m1
  )

# Graficar
plot_mat_area <- ema_mat_area %>%
  plot_ly(
    x = ~año,
    y = ~mt,
    color = ~area_del_conocimiento,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Área:</b> ", area_del_conocimiento, "<br>",
      "<b>Matrícula total:</b> ", comma(mt, big.mark = ".", decimal.mark = ","), "<br>",
      "<b>Proporción mujeres:</b> ", percent(prop_f_mt, accuracy = 1, decimal.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  layout(title = list(
    text = "<b>Matrícula total de pregrado por área de conocimiento (2018-2024)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024, tickangle = 0),
    yaxis = list(
      title = "",
      tickvals = seq(0, 11000, 1000),
      tickformat = ".0f",
      separatethousands = T
    ),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50)
  )

plot_mat_area

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 13. Matrícula 1er año por área de conocimiento genérica 2018-2024----
# Graficar
plot_mat_area_1 <- ema_mat_area %>%
  plot_ly(
    x = ~año,
    y = ~m1,
    color = ~area_del_conocimiento,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste0(
      "<b>Año:</b> ", año, "<br>",
      "<b>Área:</b> ", area_del_conocimiento, "<br>",
      "<b>Matrícula total:</b> ", comma(m1, big.mark = ".", decimal.mark = ","), "<br>",
      "<b>Proporción mujeres:</b> ", percent(prop_f_m1, accuracy = 1, decimal.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  layout(title = list(
    text = "<b>Matrícula de 1er año de pregrado por área de conocimiento (2018-2024)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024, tickangle = 0),
    yaxis = list(
      title = "",
      tickvals = seq(0, 3000, 500),
      range = c(0, 3000),
      autorange = FALSE,      
      tickformat = ".0f",
      separatethousands = T
    ),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50)
  )

plot_mat_area_1

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 14. Tasa de retención por área de conocimiento genérica 2018-2023----
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
      "<b>Tasa de retención:</b> ", porcentaje
    )
  ) %>%
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
    showlegend = FALSE
  ) %>%
    layout(
      yaxis = list(
        title = "",
        tickvals = seq(50, 100, 10),
        range = c(50, 100),
        ticksuffix = "%"
      ),
      xaxis = list(title = "", tickvals = 2018:2024)
    )
})

# Crear subplots
n_cols <- 3
n_rows <- ceiling(length(plots_area) / n_cols)

plot_tr_area <- subplot(plots_area, nrows = n_rows, shareX = T, shareY = T, margin = 0.04) %>%
  layout(title = list(
    text = "<b>Tasa de retención por área de conocimiento y género (2018-2023)</b>",
    font = list(size = 14),
    x = 0.5),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

# Agregar anotaciones
annotations <- lapply(seq_along(names(area_split)), function(i) {
  col_i <- (i - 1) %% n_cols
  row_i <- floor((i - 1) / n_cols)
  x_pos <- (col_i + 0.5) / n_cols
  y_pos <- 1 - (row_i / n_rows)
  
  list(
    x = x_pos,
    y = y_pos,
    text = paste0("<b>", names(area_split)[i], "</b>"),
    showarrow = FALSE,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    font = list(size = 13)
  )
})

# Añadir anotaciones al layout
plot_tr_area <- plot_tr_area %>%
  layout(annotations = annotations)

plot_tr_area

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 15. Titulados por género 2018-2023----
# Cargar datos
load(here("01-input", "data-proc", "titulados.RData"))

titulados_g <- titulados_g %>%
  select(anio, titulados_mujeres, titulados_hombres, prop_fem, prop_mas) %>%
  pivot_longer(
    cols = c(titulados_mujeres, titulados_hombres),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "titulados_mujeres" = "Femenino",
                    "titulados_hombres" = "Masculino")) %>%
  group_by(anio) %>%
  mutate(
    total_anual = sum(cantidad),
    proporcion = round((cantidad / total_anual)*100, 0)) %>%
  ungroup()

# Graficar
plot_titulados <- titulados_g %>%
  plot_ly(
    x = ~anio,
    y = ~cantidad,
    color = ~genero,
    type = 'scatter',
    mode = 'lines+markers',
    colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
    text = ~paste0(
      "<b>Año:</b> ", anio, "<br>",
      "<b>Género:</b> ", genero, "<br>",
      "<b>Matrícula:</b> ", label_comma(big.mark = ".", decimal.mark = ",")(cantidad), "<br>",
      "<b>Proporción:</b> ", proporcion, "%"
    ),
    hoverinfo = 'text'
  ) %>%
  layout(title = list(
    text = "<b>Titulados de pregrado por género (2018-2023)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(
      title = "",
      tickvals = seq(500, 2500, 500),
      range = c(500, 2500)
    ),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_titulados

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 16. Titulados por género y área de conocimiento genérica 2018-2023----
# Cargar datos
load(here("01-input", "data-proc", "titulados.RData"))

# Graficar
plot_titulados_area <- titulados_ac %>%
  plot_ly(
    x = ~anio,
    y = ~total_titulados,
    color = ~area_del_conocimiento,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste0(
      "<b>Año:</b> ", anio, "<br>",
      "<b>Área:</b> ", area_del_conocimiento, "<br>",
      "<b>Titulados total:</b> ", comma(total_titulados, big.mark = ".", decimal.mark = ","), "<br>",
      "<b>Proporción mujeres:</b> ", percent(prop_fem/100, accuracy = 1, decimal.mark = ",")
    ),
    hoverinfo = 'text'
  ) %>%
  layout(title = list(
    text = "<b>Titulados por área de conocimiento (2018-2023)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2023, tickangle = 0),
    yaxis = list(
      title = "",
      tickvals = seq(0, 2000, 500),
      range = c(0, 2000),
      autorange = FALSE,      
      tickformat = ".0f",
      separatethousands = T
    ),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50)
  )

plot_titulados_area
