# Gráficos Encuesta EMA. Datos 2018-2024----
# Carga de paquetes
pacman::p_load(ggh4x,ggrepel, rio, plotly, readxl, scales, tidyverse)

# Gráfico 1. Cantidad de académicos por género en la USACH (2018-2024)----
# Carga de datos
ema <- import("01-input/data-orig/CONSOLIDADO EMA 2018-2024_USACH.xlsx")

# Calcular proporciones y pivotar datos
academicos <- ema %>%
  mutate(
    prop_acad_f = round((n_acad_f/n_acad_total)*100, 0),
    prop_acad_m = round((n_acad_m/n_acad_total)*100, 0)) %>%
  pivot_longer(
    cols = c(prop_acad_f, prop_acad_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "prop_acad_f" = "Femenino",
                    "prop_acad_m" = "Masculino")) %>%
  select(año, genero, cantidad)
# Graficar
ggplot(academicos, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_text_repel(
    aes(label = label_percent(scale = 1)(cantidad)),
    size = 3.5,
    nudge_x = ,
    direction = "y",
    show.legend = FALSE,
    color = "black"
  ) +
  scale_color_manual(values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(breaks = seq(20, 80, 10),
                     limits = c(20, 80),
                     labels = label_percent(scale = 1)) + 
  labs(
    title = "",
    x = "",
    y = "",
    color = "Género") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom")

# Guardar gráfico
ggsave(
  filename = "03-output/academicos por género.png",
  plot = p_acad,        
  width = 1653 / 330,        
  height = 993 / 330,        
  dpi = 330,
  units = "in")

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 2. Cantidad de académicos por género y grado académico----
# Calcular proporciones y pivotar datos
academicos_grados <- ema %>%
  mutate(
    prop_acad_doc_f = round((n_acad_doc_f/n_acad_doc)*100, 0),
    prop_acad_doc_m = round((n_acad_doc_m/n_acad_doc)*100, 0),
    prop_acad_mg_f = round((n_acad_mg_f/n_acad_mg)*100, 0),
    prop_acad_mg_m = round((n_acad_mg_m/n_acad_mg)*100, 0),
    prop_acad_lic_f = round((n_acad_lic_f/n_acad_lic)*100, 0),
    prop_acad_lic_m = round((n_acad_lic_m/n_acad_lic)*100, 0)
  ) %>%
  pivot_longer(
    cols = c(prop_acad_doc_f, prop_acad_doc_m,
             prop_acad_mg_f, prop_acad_mg_m,
             prop_acad_lic_f, prop_acad_lic_m),
    names_to = "categoria",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = case_when(
      str_detect(categoria, "_f") ~ "Mujer",
      str_detect(categoria, "_m") ~ "Hombre"
    ),
    grado = case_when(
      str_detect(categoria, "doc") ~ "Doctorado",
      str_detect(categoria, "mg") ~ "Magíster",
      str_detect(categoria, "lic") ~ "Licenciatura"
    )
  ) %>%
  filter(!is.na(grado))

# Ordenar grados y género
academicos_grados$grado <- factor(
  academicos_grados$grado,
  levels = c("Doctorado", "Magíster", "Licenciatura"))
academicos_grados$grado <- factor(academicos_grados$grado,
                                       levels = c("Doctorado", "Magíster", "Licenciatura"))

academicos_grados$genero <- factor(academicos_grados$genero,
                                        levels = c("Mujer", "Hombre"))

academicos_grados$grupo <- interaction(academicos_grados$grado, 
                                             academicos_grados$genero,
                                             sep = " ",
                                             lex.order = TRUE)
# Graficar
ggplot(academicos_grados, aes(x = año, y = cantidad, color = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = academicos_grados %>%
      group_by(grado, genero),
    aes(label = label_percent(scale = 1)(cantidad)),
    color = "black",
    size = 3.5,
    segment.color = "black"
  ) +
  scale_color_manual(
    name = "Grado y Género",
    values = c(
      "Doctorado Mujer" = "#8D69F3",
      "Doctorado Hombre" = "#41776E",
      "Magíster Mujer" = "#BDA6FF",
      "Magíster Hombre" = "#8AC0B5",
      "Licenciatura Mujer" = "#D1C5FA",
      "Licenciatura Hombre" = "#A9D6CD"
    )
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  facet_wrap(~ grado, ncol = 1, strip.position = "top", scales = "free_y") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    strip.placement = "outside")

# Guardar
ggsave(
  filename = "03-output/academicos por género y grado.png",
  plot = p_acad_grados,        
  width = 1600 / 330,        
  height = 1600 / 330,        
  dpi = 330,
  units = "in")

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 3 y 4. Académicos por género y jerarquía-----
# Carga de datos
ema_jq <- read_excel("01-input/data-orig/CONSOLIDADO EMA 2018-2024_USACH.xlsx", sheet = "ACAD_JQ")

# Calcular proporciones y pivotar datos 
academicos_jerarquia <- ema_jq %>%
  mutate(
    prop_acad_titular_f = round((n_acad_titular_f/n_acad_titular)*100, 0),
    prop_acad_titular_m = round((n_acad_titular_m/n_acad_titular)*100, 0),
    prop_acad_asoc_f = round((n_acad_asoc_f/n_acad_asoc)*100, 0),
    prop_acad_asoc_m = round((n_acad_asoc_m/n_acad_asoc)*100, 0),
    prop_acad_asis_f = round((n_acad_asis_f/n_acad_asis)*100, 0),
    prop_acad_asis_m = round((n_acad_asis_m/n_acad_asis)*100, 0),
    prop_acad_inst_f = round((n_acad_inst_f/n_acad_inst)*100, 0),
    prop_acad_inst_m = round((n_acad_inst_m/n_acad_inst)*100, 0),
    prop_acad_adj_f = round((n_acad_adj_f/n_acad_adj)*100, 0),
    prop_acad_adj_m = round((n_acad_adj_m/n_acad_adj)*100, 0),
    prop_acad_sj_f = round((n_acad_sj_f/n_acad_sj)*100, 0),
    prop_acad_sj_m = round((n_acad_sj_m/n_acad_sj)*100, 0)
  ) %>%
  pivot_longer(
    cols = starts_with("prop_acad_"),
    names_to = "categoria",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = case_when(
      str_detect(categoria, "_f") ~ "Mujeres",
      str_detect(categoria, "_m") ~ "Hombres"
    ),
    jerarquia = case_when(
      str_detect(categoria, "titular") ~ "Titular",
      str_detect(categoria, "asoc") ~ "Asociado/a",
      str_detect(categoria, "asis") ~ "Asistente",
      str_detect(categoria, "inst") ~ "Instructor/a",
      str_detect(categoria, "adj") ~ "Adjunto/a",
      str_detect(categoria, "sj") ~ "Sin jerarquía"
    )
  ) %>%
  filter(!is.na(jerarquia), !is.na(genero), !is.na(cantidad)) 

# Orden jerarquía y género
academicos_jerarquia$jerarquia <- factor(
  academicos_jerarquia$jerarquia,
  levels = c("Titular", "Asociado/a", "Asistente", "Instructor/a", "Adjunto/a", "Sin jerarquía")
)

academicos_jerarquia$genero <- factor(
  academicos_jerarquia$genero,
  levels = c("Mujeres", "Hombres")
)

# Filtrar solo 3 jerarquías
acad_jq_1 <- academicos_jerarquia %>%
  filter(jerarquia %in% c("Titular", "Asociado/a", "Asistente")) %>%
  mutate(grupo = interaction(jerarquia, genero, sep = " ", lex.order = TRUE))

# Graficar
ggplot(acad_jq_1, aes(x = año, y = cantidad, color = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = acad_jq_1 %>%
      drop_na(cantidad),
    aes(label = label_percent(scale = 1)(cantidad)),
    color = "black",
    size = 3.5,
    force = 5,
    segment.color = "black"
  ) +
  scale_color_manual(
    name = "Jerarquía y Género",
    values = c(
      "Titular Mujeres" = "#6A5ACD",
      "Titular Hombres" = "#3C8DAD",
      "Asociado/a Mujeres" = "#9370DB",
      "Asociado/a Hombres" = "#6495ED",
      "Asistente Mujeres" = "#D8BFD8",
      "Asistente Hombres" = "#87CEEB"
    )
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~ jerarquia, ncol = 1, strip.position = "top", scales = "free_y") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9))

# Guardar gráfico
ggsave(
  filename = "03-output/academicos por género y jerarquía.png",
  plot = p_acad_jq_1,        
  width = 1600 / 330,        
  height = 1600 / 330,        
  dpi = 330,
  units = "in")

#///////////////////////////////////////////////////////////////////////////////
# Filtrar  3 jerarquías
acad_jq_2 <- academicos_jerarquia %>%
  filter(jerarquia %in% c("Instructor/a", "Adjunto/a", "Sin jerarquía")) %>%
  mutate(grupo = interaction(jerarquia, genero, sep = " ", lex.order = TRUE))


# Graficar - 2das jerarquías
ggplot(acad_jq_2, aes(x = año, y = cantidad, color = grupo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = acad_jq_2 %>%
      group_by(jerarquia, genero),
    aes(label = label_percent(scale = 1)(cantidad)),
    color = "black",
    size = 3.5,
    segment.color = "black",
    force = 5
  ) +
  scale_color_manual(
    name = "Jerarquía y Género",
    values = c(
      "Instructor/a Mujeres" = "#A291FB",
      "Instructor/a Hombres" = "#4B5DFF",
      "Adjunto/a Mujeres" = "#B3C7F9",
      "Adjunto/a Hombres" = "#2D76F3",
      "Sin jerarquía Mujeres" = "#C8D8FF",
      "Sin jerarquía Hombres" = "#4472CA"
    )
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~ jerarquia, ncol = 1, strip.position = "top", scales = "free_y") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9))

# Guardar gráfico
ggsave(
  filename = "03-output/academicos por género y jerarquía (2).png",
  plot = p_acad_jq_2,        
  width = 1600 / 330,        
  height = 1600 / 330,        
  dpi = 330,
  units = "in")
#///////////////////////////////////////////////////////////////////////////////
# Gráfico 5. Cantidad de publicaciones en revistas indexadas por género----
# Carga de datos
ema_pub <- read_excel("01-input/data-orig/CONSOLIDADO EMA 2018-2024_USACH.xlsx", sheet = "ACAD_PUB")

# Pivotar datos
acad_pub <- ema_pub %>%
  select(año, n_pub_f, n_pub_m) %>%
  pivot_longer(
    cols = c(n_pub_f, n_pub_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "n_pub_f" = "Femenino",
                    "n_pub_m" = "Masculino"))

# Graficar
ggplot(data = acad_pub, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = acad_pub %>%
      group_by(genero),
    aes(label = cantidad),
    color = "black",
    size = 3.5,
    segment.color = "black") +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(breaks = seq(0, 1800, 200),
                     limits = c(0, 1800)) +
  labs(
    title = "",
    x = "",
    y = "") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9))

# Guardar gráfico
ggsave(
  filename = "03-output/publicaciones académicas por género.png",
  plot = p_acad_pub,        
  width = 1653 / 330,        
  height = 993 / 330,        
  dpi = 330,
  units = "in")

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 5.1. Publicaciones académicas por género (proporciones)----
# calcular proporciones y pivotar datos
acad_pub_1 <- ema_pub %>%
  mutate(
    prop_pub_f = round((n_pub_f/n_pub)*100, 0),
    prop_pub_m = round((n_pub_m/n_pub)*100, 0)) %>%
  select(año, prop_pub_f, prop_pub_m) %>%
  pivot_longer(
    cols = c(prop_pub_f, prop_pub_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "prop_pub_f" = "Femenino",
                    "prop_pub_m" = "Masculino"))

# Graficar
ggplot(data = acad_pub_1, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = acad_pub_1 %>%
      group_by(genero),
    aes(label = label_percent(scale = 1)(cantidad)),
    color = "black",
    size = 3.5,
    segment.color = "black") +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(breaks = seq(15, 90, 15),
                     limits = c(15, 90),
                     labels = label_percent(scale = 1)) +
  labs(
    title = "",
    x = "",
    y = "") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9))
# Guardar gráfico

# Gráfico 6. Puestos directivos por género - 2 posiciones----
# Carga de datos
ema_puestos <- read_excel("01-input/data-orig/CONSOLIDADO EMA 2018-2024_USACH.xlsx", sheet = "ACAD_DIRECCION")

# Filtrado
ema_puestos_1 <- ema_puestos %>%
  filter(puesto %in% c("Junta Directiva", "Consejo Académico", "Vicerrectoría",
                       "Decanato", "Vicedecanato", "Dirección o jefatura"))

# Graficar
ggplot(data = ema_puestos_1, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = ema_puestos_1 %>%
      group_by(puesto, genero),
    aes(label = cantidad),
    color = "black",
    size = 3.5,
    segment.color = "black"
  ) +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(breaks = seq(0,30, 10),
                     limits = c(0, 30)) +
  facet_wrap(~puesto, ncol = 1) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold"))

ggsave(
  filename = "03-output/puestos directivos por género(1).png",
  plot = p_acad_puestos,        
  width = 1653 / 330,        
  height = 993 / 330,        
  dpi = 330,
  units = "in")

# # Gráfico 7. Puestos directivos por género - 2 posiciones----
# Filtrado
ema_puestos_2 <- ema_puestos %>%
  filter(puesto %in% c("Decanato", "Vicedecanato")) %>%
  filter(!is.na(cantidad))

# Graficar
ggplot(data = ema_puestos_2, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = ema_puestos_2 %>%
      group_by(puesto, genero),
    aes(label = cantidad),
    color = "black",
    size = 3.5,
    segment.color = "black"
  ) +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(breaks = seq(0,15, 5),
                     limits = c(0, 15)) +
facet_wrap(~puesto, ncol = 1, scales = "fixed", strip.position = "top") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold"),
    strip.placement = "outside")

# Guardar
ggsave(
  filename = "03-output/puestos directivos por género(2).png",
  plot = p_acad_puestos,        
  width = 1653 / 330,        
  height = 993 / 330,        
  dpi = 330,
  units = "in")

# # Gráfico 8. Puestos directivos por género - 2 posiciones----
# Filtrado
ema_puestos_3 <- ema_puestos %>%
  filter(puesto=="Dirección o jefatura") %>%
  filter(!is.na(cantidad))

# Graficar
ggplot(data = ema_puestos_3, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = ema_puestos_3 %>%
      group_by(puesto, genero),
    aes(label = cantidad),
    color = "black",
    size = 3.5,
    segment.color = "black"
  ) +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(breaks = seq(0,75, 15),
                     limits = c(0, 75)) +
  facet_wrap(~puesto, ncol = 1, scales = "fixed", strip.position = "top") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold"),
    strip.placement = "outside")

# Guardar
ggsave(
  filename = "03-output/puestos directivos por género(3).png",
  plot = p_acad_puestos,        
  width = 1653 / 330,        
  height = 993 / 330,        
  dpi = 330,
  units = "in")
# Gráfico 8.1. - Puestos directivos por género en proporciones - 2 posiciones----
# Transformación de datos
ema_prop_mujeres <- ema_puestos %>%
  group_by(año, puesto) %>%
  summarise(
    total = sum(cantidad, na.rm = TRUE),
    mujeres = sum(cantidad[genero == "Femenino"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(prop_mujeres = mujeres / total) %>%
  filter(puesto=="Dirección o jefatura")

ggplot(ema_prop_mujeres, aes(x = año, y = prop_mujeres)) +
  geom_line(color = "#8D69F3", linewidth = 1) +
  geom_point(color = "#8D69F3", size = 2) +
  geom_text_repel(
    data = ema_prop_mujeres %>%
      group_by(puesto),
    aes(label = scales::percent(prop_mujeres, accuracy = 0.1)),
    color = "black",
    size = 3.5,
    segment.color = "black"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = 2018:2024) +
  facet_wrap(~puesto) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold")
  )

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 9. Matrícula pregrado por género - Matrícula total 2018-2024----
# Carga de datos
ema_mat <- read_excel("01-input/data-orig/CONSOLIDADO EMA 2018-2024_USACH.xlsx", sheet = "MAT_PRE_GENERAL")

# Pivotar datos
ema_mat_long <- ema_mat %>%
  select(año, mt_pre_f, mt_pre_m) %>%
  pivot_longer(
    cols = c(mt_pre_f, mt_pre_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "mt_pre_f" = "Femenino",
                    "mt_pre_m" = "Masculino"))

# Graficar
p_est_mt <- ggplot(data = ema_mat_long, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = ema_mat_long %>%
      group_by(genero),
    aes(label = label_comma(big.mark = ".", decimal.mark = ",")(cantidad)),
    color = "black",
    size = 3.5,
    segment.color = "black"
  ) +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(breaks = seq(8000, 14000, by = 1000),
                     limits = c(8000,14000)) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9))

# Guardar
ggsave(
  filename = "03-output/matricula total 2018-2024.png",
  plot = p_est_mt,        
  width = 1653 / 330,        
  height = 993 / 330,        
  dpi = 330,
  units = "in")

# Gráfico 10. Matrícula pregrado por género - Matrícula 1er año 2018-2024----
ema_mat_long_2 <- ema_mat %>%
  select(año, m1_pre_f, m1_pre_m) %>%
  pivot_longer(
    cols = c(m1_pre_f, m1_pre_m),
    names_to = "genero",
    values_to = "cantidad"
  ) %>%
  mutate(
    genero = recode(genero,
                    "m1_pre_f" = "Femenino",
                    "m1_pre_m" = "Masculino"))

# Graficar
p_est_m1 <- ggplot(data = ema_mat_long_2, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = ema_mat_long_2 %>%
      group_by(genero),
    aes(label = label_comma(big.mark = ".", decimal.mark = ",")(cantidad)),
    color = "black",
    size = 3.5,
    segment.color = "black"
  ) +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(breaks = seq(2000, 3500, by = 500),
                     limits = c(2000,3500)) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9))

# Guardar
ggsave(
  filename = "03-output/matricula 1er año 2018-2024.png",
  plot = p_est_m1,        
  width = 1653 / 330,        
  height = 993 / 330,        
  dpi = 330,
  units = "in")

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 11. Tasa de retención por género 2018-2024----
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
                    "tasa_ret_m" = "Masculino")) %>%
  filter(!is.na(cantidad))

# Graficar
p_tr <- ggplot(data = ema_tr, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = ema_tr %>%
      group_by(genero),
    aes(label = label_percent(scale = 1)(cantidad)),
    color = "black",
    size = 3.5,
    segment.color = "black"
  ) +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(
    breaks = seq(70, 90, by = 5),
    limits = c(70, 90),
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9))

# Guardar 
ggsave(
  filename = "03-output/tasa de retención por género 2018-2024.png",
  plot = p_tr,        
  width = 1653 / 330,        
  height = 993 / 330,        
  dpi = 330,
  units = "in")

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 12. Matrícula total por área de conocimiento genérica 2018-2024----
# Cargar datos
ema_mat_area <- read_excel("01-input/data-orig/CONSOLIDADO EMA 2018-2024_USACH.xlsx", sheet = "MAT_PRE_AC") 
# Transformar datos
ema_mat_area_1 <- ema_mat_area %>%
  pivot_longer(cols = c(prop_f_mt, prop_m_mt),
               names_to = "genero",
               values_to = "cantidad") %>%
  mutate(genero = recode(genero,
                         "prop_f_mt" = "Femenino",
                         "prop_m_mt" = "Masculino")) %>%
  select(año, area_del_conocimiento, genero, cantidad) %>%
  filter(!is.na(cantidad))

# Graficar
ggplot(data = ema_mat_area_1, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = ema_mat_area_1 %>%
      group_by(area_del_conocimiento, genero),
    aes(label = label_percent(scale = 1)(cantidad)),
    color = "black",
    size = 3.5,
    segment.color = "black",
    max.overlaps = 30,
    box.padding = 0.4,
    
  ) +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(
    labels = label_percent(scale = 1)
  ) +
  facet_wrap(~area_del_conocimiento, scales = "free_y", ncol = 3) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold")
  )

# Guardar

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 13. Matrícula 1er año por área de conocimiento genérica 2018-2024----
# Transformar datos
ema_mat_area_2 <- ema_mat_area %>%
  pivot_longer(cols = c(prop_f_m1, prop_m_m1),
               names_to = "genero",
               values_to = "cantidad") %>%
  mutate(genero = recode(genero,
                         "prop_f_m1" = "Femenino",
                         "prop_m_m1" = "Masculino")) %>%
  mutate(cantidad = round(cantidad), 0) %>%
  select(año, area_del_conocimiento, genero, cantidad) %>%
  filter(!is.na(cantidad))

# Graficar
ggplot(data = ema_mat_area_2, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = ema_mat_area_2 %>%
      group_by(area_del_conocimiento, genero),
    aes(label = label_percent(scale = 1)(cantidad)),
    color = "black",
    size = 3.5,
    segment.color = "black",
    max.overlaps = 30,
    box.padding = 0.4,
    point.padding = 0.4,
    force = 5
  ) +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(
    labels = label_percent(scale = 1)
  ) +
  facet_wrap(~area_del_conocimiento, scales = "free_y", ncol = 3) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold")
  )

# Guardar

#///////////////////////////////////////////////////////////////////////////////
# Gráfico 14. Tasa de retención por área de conocimiento genérica 2018-2024----
# Transformar datos
ema_tr_area<- ema_mat_area %>%
  pivot_longer(cols = c(tasa_ret_f, tasa_ret_m),
               names_to = "genero",
               values_to = "cantidad") %>%
  mutate(genero = recode(genero,
                         "tasa_ret_f" = "Femenino",
                         "tasa_ret_m" = "Masculino")) %>%
  select(año, area_del_conocimiento, genero, cantidad) %>%
  filter(!is.na(cantidad))

# Graficar
ggplot(data = ema_tr_area, aes(x = año, y = cantidad, color = genero)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text_repel(
    data = ema_tr_area %>%
      group_by(genero),
    aes(label = label_percent(scale = 1)(cantidad)),
    color = "black",
    size = 3.5,
    segment.color = "black",
    max.overlaps = 30
  ) +
  scale_color_manual(
    name = "Género",
    values = c("Femenino" = "#8D69F3", "Masculino" = "#41776E")
  ) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_y_continuous(
    breaks = seq(50, 100, by = 10),
    limits = c(50, 100),
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  facet_wrap(~area_del_conocimiento, scales = "free_y", ncol = 3) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold"))

# Guardar 