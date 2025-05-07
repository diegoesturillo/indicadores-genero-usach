# Gráficos en formato plotly
# Carga de paquetes
pacman::p_load(here, ggrepel, plotly, purrr, RColorBrewer, readxl, rio, scales, tidyverse, viridis)

# Gráfico 1. Cantidad de académicos por género en la USACH (2018-2024)----
# Cargar datos
ema <- read_excel(here("01-input", "data-orig", "CONSOLIDADO EMA 2018-2024_USACH.xlsx"))

# Calcular proporciones
academicos <- ema %>%
  mutate(
    prop_fem = round((n_acad_f/n_acad_total)*100, 0),
    prop_mas = round((n_acad_m/n_acad_total)*100, 0)
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
      "<br>Proporción: ", prop_texto))

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
  marker = list(size = 6)) %>%
  layout(
    title = list(
      text = "<b>Estamento académico por género (2018-2024)</b>",
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
    hovermode = "closest")

plot_acad_genero

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 2. Cantidad de académicos por género y grado académico----
# Cargar datos
acad_grados <- read_excel(here("01-input", "data-orig","CONSOLIDADO EMA 2018-2024_USACH.xlsx"))

# Preparar datos - grado doctorado----
acad_grado_doc <- acad_grados %>%
  select(año, n_acad_doc_f, n_acad_doc_m)%>%
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
plot_acad_grado_doc <- plot_ly(
  data = acad_grado_doc,
  x = ~año,
  y = ~cantidad,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
  type = 'scatter',
  mode = 'lines+markers',
  text = ~paste("<b>Año:</b> ", año, "<br>",
                "<b>Género:</b> ", genero, "<br>",
                "<b>Cantidad:</b> ", cantidad, "<br>",
                "<b>Proporción:</b> ", proporcion, "%"),
  hoverinfo = 'text'
) %>%
  layout(
    title = list(
      text = "<b>Estamento académico por género y grado doctorado (2018-2024)</b>",
      font = list(size = 14),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest"
  )

plot_acad_grado_doc

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
plot_acad_grado_mg <- plot_ly(
  data = acad_grado_mg,
  x = ~año,
  y = ~cantidad,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
  type = 'scatter',
  mode = 'lines+markers',
  text = ~paste("<b>Año:</b> ", año, "<br>",
                "<b>Género:</b> ", genero, "<br>",
                "<b>Cantidad:</b> ", cantidad, "<br>",
                "<b>Proporción:</b> ", proporcion, "%"),
  hoverinfo = 'text'
) %>%
  layout(
    title = list(
      text = "<b>Estamento académico por género y grado magíster (2018-2024)</b>",
      font = list(size = 14),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest"
  )

plot_acad_grado_mg

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
plot_acad_grado_lic <- plot_ly(
  data = acad_grado_lic,
  x = ~año,
  y = ~cantidad,
  color = ~genero,
  colors = c("Femenino" = "#8D69F3", "Masculino" = "#41776E"),
  type = 'scatter',
  mode = 'lines+markers',
  text = ~paste("<b>Año:</b> ", año, "<br>",
                "<b>Género:</b> ", genero, "<br>",
                "<b>Cantidad:</b> ", cantidad, "<br>",
                "<b>Proporción:</b> ", proporcion, "%"),
  hoverinfo = 'text'
) %>%
  layout(
    title = list(
      text = "<b>Estamento académico por género y grado licenciatura (2018-2024)</b>",
      font = list(size = 14),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = "", rangemode = "tozero"),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest"
  )

plot_acad_grado_lic

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 3. Académicos por género y jerarquía - 3 jerarquías----
# Carga de datos
ema_jq <- read_excel(here("01-input", "data-orig", "CONSOLIDADO EMA 2018-2024_USACH.xlsx"), 
                     sheet = "ACAD_JQ")

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
plot_jerarquias <- academicos_jerarquia %>%
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
      text = "<b>Estamento académico por género y jerarquía (2018-2024)</b>",
      font = list(size = 14),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Jerarquía y género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

plot_jerarquias

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 4. Académicos por género y jerarquía - 3 jerarquías----
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
plot_jerarquias_2 <- academicos_jerarquia_2 %>%
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
      text = "<b>Estamento académico por género y jerarquía (2018-2023)</b>",
      font = list(size = 14),
      x = 0.5),
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
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(
    title = list(
      text = "<b>Publicaciones en revistas indexadas por género (2018-2024)</b>",
      font = list(size = 14),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

plot_pub

# Preparar datos - indexación en WOS----
wos_pub <- ema_pub %>%
  mutate(
    prop_f = round(n_pub_wos_f/n_pub_wos*100,0),
    prop_m = round(n_pub_wos_m/n_pub_wos*100,0)) %>%
  pivot_longer(
    cols = c(n_pub_wos_f, n_pub_wos_m),
    names_to = "genero",
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "n_pub_wos_f" = "Femenino",
                    "n_pub_wos_m" = "Masculino"),
    proporcion = case_when(
      genero == "Femenino" ~ prop_f,
      genero == "Masculino" ~ prop_m))

# Graficar
plot_pub_wos <- wos_pub %>%
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
      text = "<b>Publicaciones en revistas indexadas por género en WOS (2019-2024)</b>",
      font = list(size = 14),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

plot_pub_wos

# Preparar datos - indexación en Scopus----
scopus_pub <- ema_pub %>%
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
plot_pub_scopus <- scopus_pub %>%
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
      text = "<b>Publicaciones en revistas indexadas por género en Scopus (2019-2024)</b>",
      font = list(size = 14),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

plot_pub_scopus

# Preparar datos - indexación en Scielo----
scielo_pub <- ema_pub %>%
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
plot_pub_scielo <- scielo_pub %>%
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
      text = "<b>Publicaciones en revistas indexadas por género en Scielo (2019-2024)</b>",
      font = list(size = 14),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

plot_pub_scielo

# Preparar datos - indexación en otras bases----
otras_pub <- ema_pub %>%
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
plot_pub_otras <- otras_pub %>%
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
      text = "<b>Publicaciones en revistas indexadas por género en otras bases de datos (2019-2024)</b>",
      font = list(size = 14),
      x = 0.5),
    xaxis = list(title = "", tickvals = 2019:2024),
    yaxis = list(title = ""),
    legend = list(title = list(text = "<b>Género</b>")),
    margin = list(t = 80),
    hovermode = "closest")

plot_pub_otras

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
plot_jd_ca <- subplot(subplots_1, nrows = 2, shareX = T, shareY = T, titleY = T) %>%
  layout(
    title = list(
      text = "<b>Puestos directivos por género (1)</b>",
      font = list(size = 14),
      x = 0.75),
    legend = list(title = list(text = "Género")),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(title = ""),
    yaxis2 = list(title = ""),
    margin = list(t = 80),
    annotations = list(
      list(x = 0, y = 1.02, text = "<b>Junta Directiva</b>", showarrow = FALSE, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11)),
      list(x = 0, y = 0.47, text = "<b>Consejo Académico</b>", showarrow = FALSE, xref = "paper", yref = "paper", 
           xanchor = "left", yanchor = "bottom", font = list(size = 11))))

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
    showlegend = FALSE)})

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
           xanchor = "left", yanchor = "bottom", font = list(size = 11))))

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
    prop = round((cantidad/total_puesto)*100)) %>%
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
      "<b>Proporción:</b> ", prop, "%"),
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
    hovermode = "closest")

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
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b>Matrícula total de pregrado por género (2018-2024)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = "",
      tickvals = seq(8000, 14000, 1000),
      range = c(8000, 14000)),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest")

plot_mat_total

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 10. Matrícula pregrado por género - Matrícula 1er año 2018-2024----
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
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b>Matrícula de 1er año de pregrado por género (2018-2024)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = "",
      tickvals = seq(1500, 3500, 500),
      range = c(1500, 3500)),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest")

plot_mat_1

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 11. Tasa de retención por género 2018-2023----
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
      "<b>Proporción:</b> ", cantidad, "%"),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b>Tasa de retención por género (2018-2023)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024),
    yaxis = list(
      title = "",
      tickvals = seq(0.7, 0.9, 0.1),
      range = c(0.7, 0.9),
      tickformat = ".0%"),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest")

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
    prop_m_m1 = m1_m/m1)

# Graficar
plot_mat_area <- ema_mat_area %>%
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
    text = "<b>Matrícula total de pregrado por área de conocimiento (2018-2024)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2024, tickangle = 0),
    yaxis = list(
      title = "",
      tickformat = ".0f",
      separatethousands = T),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50))

plot_mat_area

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 13. Matrícula 1er año por área de conocimiento genérica 2018-2024----
# Graficar
plot_mat_area_1 <- ema_mat_area %>%
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
      "<b>Matrícula total:</b> ", comma(m1, big.mark = ".", decimal.mark = ","), "<br>",
      "<b>Proporción mujeres:</b> ", percent(prop_f_m1, accuracy = 1, decimal.mark = ",")),
    hoverinfo = 'text') %>%
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
      separatethousands = T),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50))

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
    showlegend = FALSE) %>%
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

plot_tr_area <- subplot(plots_area, nrows = n_rows, shareX = T, shareY = T, margin = 0.04) %>%
  layout(title = list(
    text = "<b>Tasa de retención por área de conocimiento y género (2018-2023)</b>",
    font = list(size = 14),
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
    showarrow = FALSE,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    font = list(size = 13))})

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
    values_to = "cantidad") %>%
  mutate(
    genero = recode(genero,
                    "titulados_mujeres" = "Femenino",
                    "titulados_hombres" = "Masculino")) %>%
  group_by(anio) %>%
  mutate(
    total_anual = sum(cantidad),
    proporcion = round((cantidad/total_anual)*100, 0)) %>%
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
      "<b>Titulados:</b> ", label_comma(big.mark = ".", decimal.mark = ",")(cantidad), "<br>",
      "<b>Proporción:</b> ", proporcion, "%"),
    hoverinfo = 'text') %>%
  layout(title = list(
    text = "<b>Titulación de pregrado por género (2018-2023)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(
      title = "",
      tickvals = seq(500, 2500, 500),
      range = c(500, 2500)),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest")

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
    colors = colorRampPalette(brewer.pal(8, "Accent"))(8),
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
    text = "<b>Titulación de pregrado por área de conocimiento (2018-2023)</b>",
    font = list(size = 14),
    x = 0.5),
    xaxis = list(title = "", tickvals = 2018:2023, tickangle = 0),
    yaxis = list(
      title = "",
      tickvals = seq(0, 2000, 500),
      range = c(0, 2000),
      autorange = FALSE,      
      tickformat = ".0f",
      separatethousands = T),
    legend = list(title = list(text = "<b>Área del conocimiento</b>")),
    hovermode = "closest",
    margin = list(t = 50))

plot_titulados_area

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 17. Titulados administración por género y total 2018-2023----
# Filtar datos
titulados_ac_1 <- titulados_ac %>%
  filter(area_del_conocimiento=="Administración y Comercio")

plot_titulados_ayc <- plot_ly(titulados_ac_1, x = ~anio) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", anio, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", anio, "<br>Masculino:", titulados_hombres,
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
    hovertext = ~paste("Año:", anio, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Titulación de pregrado por género - Administración y Comercio (2018-2023)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_titulados_ayc

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 18. Titulados arte y arquitectura por género y total 2018-2023----
titulados_ac_2 <- titulados_ac %>%
  filter(area_del_conocimiento=="Arte y Arquitectura")

plot_titulados_aya <- plot_ly(titulados_ac_2, x = ~anio) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", anio, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", anio, "<br>Masculino:", titulados_hombres,
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
    hovertext = ~paste("Año:", anio, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Titulación de pregrado por género - Arte y Arquitectura (2018-2023)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_titulados_aya

#///////////////////////////////////////////////////////////////////////////////
#Gráfico 19. Titulados ciencias básicas por género y total 2018-2023----
titulados_ac_3 <- titulados_ac %>%
  filter(area_del_conocimiento=="Ciencias Básicas")

plot_titulados_cb <- plot_ly(titulados_ac_3, x = ~anio) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", anio, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", anio, "<br>Masculino:", titulados_hombres,
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
    hovertext = ~paste("Año:", anio, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Titulación de pregrado por género - Ciencias Básicas (2018-2023)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_titulados_cb

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 20. Titulados ciencias sociales por género y total 2018-2023----
titulados_ac_4 <- titulados_ac %>%
  filter(area_del_conocimiento=="Ciencias Sociales")

plot_titulados_cs <- plot_ly(titulados_ac_4, x = ~anio) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", anio, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", anio, "<br>Masculino:", titulados_hombres,
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
    hovertext = ~paste("Año:", anio, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Titulación de pregrado por género - Ciencias Sociales (2018-2023)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_titulados_cs

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 21. Titulados derecho por género y total 2018-2023----
titulados_ac_5 <- titulados_ac %>%
  filter(area_del_conocimiento=="Derecho")

plot_titulados_d <- plot_ly(titulados_ac_5, x = ~anio) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", anio, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", anio, "<br>Masculino:", titulados_hombres,
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
    hovertext = ~paste("Año:", anio, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Titulación de pregrado por género - Derecho (2018-2023)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2023),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_titulados_d

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 22. Titulados educación por género y total 2018-2023----
titulados_ac_6 <- titulados_ac %>%
  filter(area_del_conocimiento=="Educación")

plot_titulados_ed <- plot_ly(titulados_ac_6, x = ~anio) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", anio, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", anio, "<br>Masculino:", titulados_hombres,
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
    hovertext = ~paste("Año:", anio, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Titulación de pregrado por género - Educación (2018-2023)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_titulados_ed

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 23. Titulados humanidades por género y total 2018-2023----
titulados_ac_7 <- titulados_ac %>%
  filter(area_del_conocimiento=="Humanidades")

plot_titulados_h <- plot_ly(titulados_ac_7, x = ~anio) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", anio, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", anio, "<br>Masculino:", titulados_hombres,
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
    hovertext = ~paste("Año:", anio, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Titulación de pregrado por género - Humanidades (2018-2023)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_titulados_h

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 24. Titulados salud por género y total 2018-2023----
titulados_ac_8 <- titulados_ac %>%
  filter(area_del_conocimiento=="Salud")

plot_titulados_s <- plot_ly(titulados_ac_8, x = ~anio) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", anio, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", anio, "<br>Masculino:", titulados_hombres,
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
    hovertext = ~paste("Año:", anio, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Titulación de pregrado por género - Salud (2018-2023)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_titulados_s

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 25. Titulados tecnología por género y total 2018-2023----
titulados_ac_9 <- titulados_ac %>%
  filter(area_del_conocimiento=="Tecnología")

plot_titulados_t <- plot_ly(titulados_ac_9, x = ~anio) %>%
  add_bars(
    y = ~titulados_mujeres,
    name = "Femenino",
    marker = list(color = "#8D69F3"),
    hovertext = ~paste("Año:", anio, "<br>Femenino:", titulados_mujeres,
                       "<br>Proporción:", prop_fem, "%"),
    hoverinfo = "text"
  ) %>%
  add_bars(
    y = ~titulados_hombres,
    name = "Masculino",
    marker = list(color = "#41776E"),
    hovertext = ~paste("Año:", anio, "<br>Masculino:", titulados_hombres,
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
    hovertext = ~paste("Año:", anio, "<br>Total:", total_titulados),
    hoverinfo = "text"
  ) %>%
  layout(
    title = list(
      text = "<b>Titulación de pregrado por género - Tecnología (2018-2023)</b>",
      font = list(size = 13),
      x = 0.5
    ),
    barmode = 'group',
    xaxis = list(title = "", tickvals = 2018:2023),
    yaxis = list(
      title = ""),
    separators = ".", 
    legend = list(title = list(text = "<b>Género<br>")),
    margin = list(t = 50),
    hovermode = "closest"
  )

plot_titulados_t
