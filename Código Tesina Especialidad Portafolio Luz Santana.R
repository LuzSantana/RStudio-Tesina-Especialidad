

# ------------------------------------------------------------------
# Código final para obtener el grado de Especialista
# Santana Jaimes Luz María Concepción
# IIMAS
# Octubre 2025
# ------------------------------------------------------------------

# Librerías

library(readr)
library(tidyverse)     # dplyr, ggplot2, stringr, etc.
library(mice)
library(naniar)        # visualización de NA
library(factoextra)    # gráficos PCA
library(conflicted)    # evita choques de nombres
library(MASS)      
library(ggplot2)
library(ggnewscale)
library(pROC)

# Especificamos
conflict_prefer("select",  "dplyr")
conflict_prefer("filter",  "dplyr")
conflict_prefer("rename",  "dplyr")
conflict_prefer("mutate",  "dplyr")


# ------------------------------------------------------------------------------
#                          PREPARACIÓN DE DATOS
#-------------------------------------------------------------------------------
# Cargamos los datos
df <- read_csv("BD_2020_ABM_Redes.csv", skip = 1)

# Aquí coloqué las variables que son importantes en este segundo estudio
vars_requeridas <- c("ID", "Age", "Sex", "Sleep_Hours", "Exercise_Hours")


df_filtrado <- df %>%
  filter(if_all(all_of(vars_requeridas), ~ !is.na(.)))

# Verificamos el número de filas
cat("Filas con información completa en variables clave:", nrow(df_filtrado), "\n")


# Nos quedamos con 1609 participantes, pero voy a checar el porcentaje de valores 
# faltantes en las demás variables.

missing_summary <- df_filtrado %>%
  summarise(across(
    everything(),
    ~ sum(is.na(.))
  )) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) %>%
  mutate(
    Missing_Perc = (Missing_Count / nrow(df_filtrado)) * 100
  ) %>%
  arrange(desc(Missing_Count))

# Ver resultado
print(missing_summary, n= Inf)


# Por lo anterior sabemos que en 10 columnas hay más del 30% de valores faltantes, por lo tanto
# se eliminarán.

# Ponemos un filtro para que sean detectadas.
columnas_a_excluir <- missing_summary %>%
  filter(Missing_Perc > 30) %>%
  pull(Variable)

# Las quitamos del df_limpio
df_limpio <- df_filtrado %>%
  select(-any_of(columnas_a_excluir))

# Verificamos
cat("Columnas excluidas por exceso de NA (>30%):", length(columnas_a_excluir), "\n")

# Quitamos columnas de factores de conversión
columnas_a_eliminar <- c(
  "CF 0.0555","CF 6 pmol/L","CF 0.0259","CF 0.02592",
  "CF 0.01129","CF 59.48 umol/L","CF 88.42 umol/L",
  "CF /100","CF + 273.15", "C.P."
)

df_limpio <- df_limpio %>%
  dplyr::select(-any_of(columnas_a_eliminar))


# -------------------- Filtrado exitoso, vamos a checar los NA's ------------------

missing_summary_limpio <- df_limpio %>%
  summarise(across(
    everything(),
    ~ sum(is.na(.))
  )) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) %>%
  mutate(
    Missing_Perc = (Missing_Count / nrow(df_filtrado)) * 100
  ) %>%
  arrange(desc(Missing_Count))

# Ver resultado
print(missing_summary_limpio, n= Inf)

# Esto era para quitar todos los NA, pero, si lo hacemos nos quedamos con 12 personas sanas

# Vamos a comprobar que ya no se tengan NA's

missing_summary_limpio <- df_limpio %>%
  summarise(across(
    everything(),
    ~ sum(is.na(.))
  )) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) %>%
  mutate(
    Missing_Perc = (Missing_Count / nrow(df_filtrado)) * 100
  ) %>%
  arrange(desc(Missing_Count))

# Ver resultado
print(missing_summary_limpio, n= Inf)


# Se logró hacer el filtrado con ningun NA, por lo tanto ya no es necesario hacer imputación de esas variables.
# Vamos a detectar cuántos sanos están en este filtrado.


# Colocamos el filtro por ID
ids_a_filtrar <- c(
  316143437, 316158521, 316258391, 316145022, 315223923, 315214903, 315023259, 316210489, 316081768, 316217495,
  315111084, 316115102, 316287276, 316327622, 316337735, 316197605, 316196567, 315578869, 112001553, 315335037,
  315114054, 315139471, 315265417, 315086911, 315278040, 315314216, 315201428, 315132180, 315042588, 315012785,
  314190370, 315025957, 313017083, 314042664, 313162860, 313168604, 314268312, 315224023, 313206403, 313219256,
  313076998, 313547881, 313252424, 314688419, 313081642, 315222342, 316064330, 312356574, 312295455, 312204822,
  312092924, 312092962, 312246282, 312332587, 312067872, 312211415, 312215420, 312066507, 313124329, 312510361,
  316175038, 314633828, 314083838, 312167020, 312304867, 416003455, 312350510, 312272386, 312324155, 312024910,
  312089322, 311687224, 311575859, 311016262, 311183898, 312290003, 311241215, 107003160, 309224767, 309059956,
  305071624, 416080984, 313026096, 313209758, 316325336, 316335779, 316273749, 316325518, 316161680, 113004179,
  316329767, 316214535, 419051008, 316256311, 316173515, 316149219, 316274203, 315223930, 312298016, 310336156,
  312104416, 315024957, 313091315, 311168055, 312674003, 312005179, 313584442, 316257923, 316163677, 316315395,
  316233716, 419051235, 316187279, 315142042, 316032382, 316197564, 312019929, 316196000, 113004344, 315196117,
  316093655, 316236535, 316114535, 315130911, 316057211, 316070036, 315077692, 419041390, 315263774, 313163960,
  419051101, 315192827, 315280841, 313305197, 110003630, 313041097, 416005129, 313199363, 313332274, 313204777,
  313144363, 312032988, 312209281, 312315991, 312048598, 312276944, 314581932, 312142153, 312001535, 312305792,
  312010067, 312069632, 416108701, 312061791, 311175961, 419050829, 312722331, 310086583, 316171157, 312172190,
  310298089, 310016520, 410053012, 414008629, 312005337, 313081692, 316288527, 316164045, 312092760, 316072528,
  313108604, 315115879, 31006564, 416080954, 3142809508, 316003517, 313064909, 418049888, 316275457, 309691239,
  313151360, 514027807, 313308325, 312534237, 31237234, 316251103, 110002384, 312339940, 3132064013, 314242542,
  332290340, 3416063659, 416040532, 827863, 312245089, 316260556, 316100142, 311709616
)

df_limpio <- df_limpio %>%
  mutate(Grupo_Salud = if_else(ID %in% ids_a_filtrar, 1, 0))

# Como hay ID's duplicadas, vamos a seleccionar las que tengan menos NA's de las dos y si es empate
# que el filtro seleccione a la primera
df_limpio <- df_limpio %>%
  group_by(ID) %>%
  mutate(Total_NAs = rowSums(is.na(across(everything())))) %>%
  slice_min(order_by = Total_NAs, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-Total_NAs)

# Verificar cuántos sanos quedaron en este subconjunto
cat("Sanos encontrados en df_limpio:", sum(df_limpio$Grupo_Salud == 1), "\n")


# Tenemos 125 personas sanas identificadas

# Vamos a ver cuáles sanos fueron eliminados

# IDs sanos que no están en df_limpio
ids_faltantes <- setdiff(ids_a_filtrar, df_limpio$ID)

cat("IDs sanos que NO están en df_limpio:", length(ids_faltantes), "\n")
print(ids_faltantes)


# Por el filtro de las 5 variables principales quedaron fuera 73 sanos
# Faltan los siguientes:
# 313124329  416080984  313026096  313209758  316325336  316335779  316273749  316325518  316161680
# 113004179  316329767  316214535  419051008  316256311  316173515  316149219  316274203  315223930
# 312298016  310336156  312104416  315024957  313091315  311168055  312674003  312005179  313584442
# 316257923  316163677  316315395  316233716  419051235  316187279  315142042  316032382  316197564
# 113004344  316114535  419041390  110003630  419050829  312722331  312005337  313081692  316288527
# 316164045  312092760  316072528  313108604   31006564  416080954 3142809508  316003517  313064909
# 316275457  309691239  313151360  514027807  313308325  312534237   31237234  316251103  110002384
# 312339940 3132064013  314242542  332290340 3416063659  416040532     827863  312245089  316260556
# 316100142



# Vamos a contar los NA's para ver si tenemos que imputar

missing_summary_limpio <- df_limpio %>%
  summarise(across(
    everything(),
    ~ sum(is.na(.))
  )) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) %>%
  mutate(
    Missing_Perc = (Missing_Count / nrow(df_filtrado)) * 100
  ) %>%
  arrange(desc(Missing_Count))

# Ver resultado
print(missing_summary_limpio, n= Inf)

ids_presentes <- intersect(ids_a_filtrar, df$ID)
length(ids_presentes)

ids_inexistentes <- setdiff(ids_a_filtrar, df$ID)
length(ids_inexistentes)
print(ids_inexistentes)


df %>%
  filter(ID %in% ids_a_filtrar) %>%
  count(ID) %>%
  filter(n > 1)

# Ya tenemos la base que utilizaremos, entonces vamos a imputar los valores que faltan de las variables
# fisiológicas, para el df_limpio


# -----------------------------------------------------------------------------------------------------
#                                 Imputación con MICE
# -----------------------------------------------------------------------------------------------------

# Vamos a especificar las variables

cat_vars <- c("Sex", "Education2")  # Categóricas con más de 2 niveles

bin_vars <- c(  # Binarias (0/1)
  "Hyperglycemia", "Sedentary", "High HbA1c", "Low HDL", "High LDL",
  "Hypertriglyceridemia", "Hyperuricemia", "Low eGFR",
  "Overweight", "Systolic hypertension", "Diastolic hypertension",
  "High Blood Pressure", "High Temperature", "Insulin resistance", "Low height"
)


# Creamos copia de la base para imputar
df_imputar <- df_limpio %>%
  select(-any_of(c(cat_vars, bin_vars, "Data_base", "ID", "Grupo_Salud")))  # Quitamos variables categóricas y binarias

df_imputar <- df_imputar %>%
  mutate(
    Basal_Insulin = as.numeric(str_replace(Basal_Insulin, ",", "."))  # cambia coma por punto y lo convierte
  )

# Verificamos que sólo hay variables numéricas
str(df_imputar)

# Establecemos método de imputación: solo "pmm" para numéricas
metodos <- make.method(df_imputar)
metodos[] <- "pmm"

# Ejecutamos imputación
imputacion <- mice(
  data = df_imputar,
  m = 5,
  method = metodos,
  maxit = 10,
  seed = 2025
)

# Extraemos el primer conjunto imputado
df_imputado <- complete(imputacion, 1)


# Unimos nuevamente con las variables excluidas:
df_imputado_final <- bind_cols(
  df_imputado,
  df_limpio %>% select(any_of(c(cat_vars, bin_vars)))
)

# Verificamos
summary(df_imputado_final)


# Ya quedó la imputación para las variables numéricas, vamos a corroborarlo
missing_summary_imputado_final <- df_imputado_final %>%
  summarise(across(
    everything(),
    ~ sum(is.na(.))
  )) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) %>%
  mutate(
    Missing_Perc = (Missing_Count / nrow(df_imputado_final)) * 100
  ) %>%
  arrange(desc(Missing_Count))

# Ver resultado
print(missing_summary_imputado_final, n= Inf)


df_imputado_final <- bind_cols(
  df_imputado,
  df_limpio %>% select(ID, Sex, Education2, Grupo_Salud)
)



# Vamos a reordenar las columnas como en df_limpio
df_imputado_final <- df_imputado_final %>%
  select(ID, Age, Sex, Education2, everything())

# Se logró la imputación 



# ---------- Preparación ----------
# Factor de grupos de edad: 18–28, 29–60, >60
# Le pondremos df6 en relación a que se formarán los 6 grupos de la tesina anterior

# Identificamos TODAS las columnas HOMA (con o sin "2", mayúsculas, etc.)
cols_homa <- names(df_imputado_final)[grepl("^HOMA", names(df_imputado_final), ignore.case = TRUE)]
cols_homa



df6 <- df_imputado_final %>%
  select(-any_of(cols_homa)) %>% 
  mutate(
    Age_group = cut(
      Age,
      breaks = c(18, 28, 60, Inf),
      labels = c("Joven", "Adulto", "Mayor"),
      include.lowest = TRUE, right = TRUE
    ),
    Group6 = interaction(Sex, Age_group, sep = "_")
  ) %>%
  drop_na(Age_group)                                  # por si tenemos edades fuera de rango

# Contamos cuántos hay por grupo

df6 %>% count(Age_group)
df6 %>% count(Sex)

df6 %>% 
  count(Sex, Age_group) %>% 
  tidyr::pivot_wider(names_from = Age_group, values_from = n, values_fill = 0)

# Solo se usarán variables numéricas para PCA/LDA
num_vars6 <- df6 %>%
  select(-ID, -Sex, -Education2, -Grupo_Salud, -Age_group, -Group6) %>%
  select(where(is.numeric))

# Verificación: no debe quedar ninguna HOMA
stopifnot(!any(grepl("^HOMA", names(num_vars6), ignore.case = TRUE)))

# ------------------------------------------------------------------------------
# Colocamos una paleta de colores para los 6 grupos que se mantendrá a lo largo de
# las diferentes secciones.
niveles_g6 <- c("F_Joven","M_Joven","F_Adulto","M_Adulto","F_Mayor","M_Mayor")

pal6 <- c(
  "F_Joven"  = "#FF1493",  # rosa joven
  "M_Joven"  = "#00BFFF",  # azul joven
  "F_Adulto" = "#AB82FF",  # rosa adulto
  "M_Adulto" = "#008000",  # azul adulto
  "F_Mayor"  = "#FF4500",  # rosa mayor
  "M_Mayor"  = "#B8860B"   # azul mayor
)


# Aseguramos el mismo orden de niveles en todo
df6 <- df6 %>%
  mutate(
    Group6   = interaction(Sex, Age_group, sep = "_"),
    Group6   = factor(Group6, levels = niveles_g6),
    Age_group = factor(Age_group, levels = c("Joven","Adulto","Mayor"))
  )


# ------------------------------------------------------------------------------
#                                     Sexo y Edad
# ------------------------------------------------------------------------------
# Conteo de personas por grupo
conteo_grupos <- df6 %>%
  count(Group6)

# Aseguramos el orden de niveles
conteo_grupos <- conteo_grupos %>%
  mutate(Group6 = factor(Group6, levels = niveles_g6))

# Vamos a ponerle la actualización del porcentaje
# Calcular porcentaje
conteo_grupos <- conteo_grupos %>%
  dplyr::mutate(porcentaje = 100 * n / sum(n))

# Gráfico con porcentaje centrado dentro de las barras
ggplot(conteo_grupos, aes(x = Group6, y = n, fill = Group6)) +
  geom_col(width = 0.7) +
  # Porcentaje centrado dentro de la barra
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "black", fontface = "bold", size = 4
  ) +
  # Cantidad arriba de la barra
  geom_text(
    aes(label = n),
    vjust = -0.5, fontface = "bold", size = 4
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +  # espacio para los números arriba
  scale_fill_manual(values = pal6, breaks = niveles_g6, name = "Sexo × Edad") +
  labs(title = "Número de personas por grupo",
       x = "Grupo", y = "Frecuencia") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")


# Cajas y bigotes de los 6 grupos
# Por: HORAS DE EJERCICIO
ggplot(df6, aes(x = Age_group, y = Exercise_Hours, fill = Group6)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = pal6, breaks = niveles_g6, name = "Sexo × Edad") +
  labs(title = "Distribución de horas de ejercicio",
       x = "Grupo de edad", y = "Horas de ejercicio") +
  theme_minimal(base_size = 12)

# Por: HORAS DE SUEÑO
ggplot(df6, aes(x = Age_group, y = Sleep_Hours, fill = Group6)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(values = pal6, breaks = niveles_g6, name = "Sexo × Edad") +
  labs(title = "Distribución de horas de sueño",
       x = "Grupo de edad", y = "Horas de sueño") +
  theme_minimal(base_size = 12)




# ------------------------------------------------------------------------------
#                                     Sanos
# ------------------------------------------------------------------------------

# Vamos a hacer un gráfico de frecuencias
df6 %>%
  drop_na(Age_group, Grupo_Salud) %>%
  filter(Age_group == "Joven") %>%
  mutate(
    Grupo_Salud = factor(Grupo_Salud, levels = c(0,1),
                         labels = c("No sano","Sano"))
  ) %>%
  count(Grupo_Salud) %>%
  ggplot(aes(x = Grupo_Salud, y = n, fill = Grupo_Salud)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = n, y = n/2),
            color = "black", size = 5, fontface = "bold") +
  labs(
    title = "Frecuencia de jóvenes (18–28) por condición de salud",
    x = "Grupo Salud", y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c("No sano" = "grey80", "Sano" = "red"))


# Frecuencias y % por sexo de los sanos comparados con los no sanos
conteo_sexo_salud <- df6 %>%
  drop_na(Sex, Grupo_Salud) %>%
  mutate(
    Grupo_Salud = factor(Grupo_Salud, levels = c(0,1),
                         labels = c("No sano","Sano"))
  ) %>%
  count(Sex, Grupo_Salud) %>%
  group_by(Sex) %>%
  mutate(porcentaje = 100 * n / sum(n)) %>%
  ungroup()

pd <- position_dodge(width = 0.7)

ggplot(conteo_sexo_salud, aes(x = Sex, y = n, fill = Grupo_Salud)) +
  geom_col(width = 0.6, position = pd) +
  geom_text(
    aes(y = n/2, label = paste0(round(porcentaje, 1), "%")),
    position = pd, size = 4, fontface = "bold", color = "black"
  ) +
  geom_text(
    aes(label = n),
    position = pd, vjust = -0.6, size = 4, fontface = "bold"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = "Frecuencia por sexo y condición de salud",
    x = "Sexo", y = "Frecuencia", fill = "Grupo Salud"
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c("No sano" = "grey80", "Sano" = "red"))



# ------------------------------------------------------------------------------
# Hasta ahora hemos trabajado con variables en inglés
# entonces para los gráficos presentables tendremos que 
# traducir todas las variables a español.
# ------------------------------------------------------------------------------

# Mapeo: nombre_original -> nombre_en_español
map_es <- c(
  "Age"                = "Edad",
  "Fasting_Glucose"    = "Glucosa en ayunas",  # si en tus datos es Fasting_Glucose, cámbialo aquí
  "HbA1c"              = "Hemoglobina glicosilada",
  "Exercise_Hours"    = "Horas de ejercicio",
  "Sleep_Hours"        = "Horas de sueño",
  "HDL"                = "HDL",
  "LDL"                = "LDL",
  "Triglycerides"      = "Triglicéridos",
  "Systolic"           = "Presión Sistólica",
  "Diastolic"          = "Presión Diastólica",
  "Waist"              = "Cintura",
  "Basal_Insulin"      = "Insulina Basal",
  "Weight"             = "Peso",
  "Uric Acid"          = "Ácido Úrico",
  "Height"             = "Altura",
  "Creatinine"         = "Creatinina",
  "Axilar_temperature" = "Temperatura Axilar",
  "eGFR"               = "eGFR"
)

# Renombrar en num_vars6 
for (nm in names(map_es)) {
  if (nm %in% colnames(num_vars6)) {
    colnames(num_vars6)[colnames(num_vars6) == nm] <- map_es[[nm]]
  }
}


# --------------------------------------------------------------------------
#                                  PCA
# --------------------------------------------------------------------------
pca6 <- prcomp(num_vars6, center = TRUE, scale. = TRUE)


p_pca <- fviz_pca_biplot(
  pca6,
  geom.ind = "point",
  habillage = df6$Group6,
  addEllipses = TRUE, ellipse.level = 0.68,
  repel = TRUE, label = "var",
  col.var = "steelblue",
  palette = pal6          # <- clave
) + labs(title = "PCA (PC1 vs PC2)")
p_pca

summary(pca6)

cat("\n== LOADINGS ==\n")
print(round(pca6$rotation, 4))

# SCORES (todas las observaciones)
cat("\n== SCORES (primeras 10 filas) ==\n")
print(round(head(pca6$x, 10), 4))

scores <- cbind(Group6 = df6$Group6, as.data.frame(pca6$x))
head(scores, 10)


# Obtener loadings
loadings <- as.data.frame(pca6$rotation[, 1:2])   # PC1 y PC2
loadings

# Con toda la información anterior podemos darnos cuenta de que debemos trabajar
# con los primeros 3 PCs.


# Vamos a hacer una imagen para ayudar en la explicación de la tesina por medio de 
# la mención de cuadrantes (I, II, III, IV) en el plano.

p_pca <- fviz_pca_biplot(
  pca6,
  geom.ind = "point",
  habillage = df6$Group6,
  addEllipses = TRUE, ellipse.level = 0.68,
  repel = TRUE, label = "var",
  col.var = "steelblue",
  palette = pal6
) +
  labs(title = "PCA (PC1 vs PC2)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # línea horizontal
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")    # línea vertical

p_pca


p_pca <- p_pca +
  annotate("text", x = 4.5,  y = 6,  label = "I",   size = 8,  fontface = "bold") +
  annotate("text", x = -4.5, y = 6,  label = "II",  size = 8, fontface = "bold") +
  annotate("text", x = -4.5, y = -6, label = "III", size = 8, fontface = "bold") +
  annotate("text", x = 4.5,  y = -6, label = "IV",  size = 8, fontface = "bold")

p_pca

# Esto imprime el mismo gráfico de PCA per con una división con cuatro cuadrantes
# especificados con números romanos.


# ------------------------------------------------------------------------------
#                      Gráficos por comparaciones de dimensiones
# ------------------------------------------------------------------------------
# Ya tenemos PC1 vs PC2 entonces faltan sólo dos gráficos.
# --- PC1 vs PC3 con TODAS las variables ---
p_pca_13_full <- fviz_pca_biplot(
  pca6,
  axes = c(1, 3),              # PC1 vs PC3
  geom.ind = "point",
  habillage = df6$Group6,
  palette = pal6,
  addEllipses = TRUE, ellipse.level = 0.68,
  label = "var", repel = TRUE, 
  col.var = "steelblue",
  pointsize = 1.2, labelsize = 4, arrowsize = 0.6
) +
  labs(
    title = "PCA (PC1 vs PC3)",
    x = paste0("Dim1 (", round(100 * summary(pca6)$importance[2,1], 1), "%)"),
    y = paste0("Dim3 (", round(100 * summary(pca6)$importance[2,3], 1), "%)")
  )
p_pca_13_full



# --- PC2 vs PC3 con TODAS las variables ---
p_pca_23_full <- fviz_pca_biplot(
  pca6,
  axes = c(2, 3),              # PC2 vs PC3
  geom.ind = "point",
  habillage = df6$Group6,
  palette = pal6,
  addEllipses = TRUE, ellipse.level = 0.68,
  label = "var", repel = TRUE, 
  col.var = "steelblue",
  pointsize = 1.2, labelsize = 4, arrowsize = 0.6
) +
  labs(
    title = "PCA (PC2 vs PC3)",
    x = paste0("Dim2 (", round(100 * summary(pca6)$importance[2,2], 1), "%)"),
    y = paste0("Dim3 (", round(100 * summary(pca6)$importance[2,3], 1), "%)")
  )
p_pca_23_full


# Vamos a ver las primeras 5 variables más importantes por si se necesitaran más 
# adelante.

# Calcular importancia (magnitud en el plano PC1–PC2)
#loadings$importance <- rowSums(loadings^2)

# Seleccionar las 5 variables más importantes
#vars_top5 <- rownames(loadings)[order(loadings$importance, decreasing = TRUE)][1:5]

# Graficar solo esas variables
#p_pca <- fviz_pca_biplot(
#  pca6,
#  geom.ind = "point",
#  habillage = df6$Group6,
#  addEllipses = TRUE, ellipse.level = 0.68,
#  repel = TRUE, label = "var",
#  col.var = "steelblue",
#  palette = pal6,
#  select.var = list(name = vars_top5)   # <- aquí el filtro
#) + labs(title = "PCA (6 grupos: Sexo × Edad, top 5 variables)")
#p_pca


# ------------------------------------------------------------------------------
#                           PCA con Sanos en color rojo
# ------------------------------------------------------------------------------

# Vamos a poner a los sanos mediante la etiqueta de Grupo_Salud
salud_alineado <- factor(df6$Grupo_Salud, levels = c(0,1), labels = c("No sano", "Sano"))

# PC1 vs PC2
p_pca_sanos <- fviz_pca_biplot(
  pca6,
  geom.ind = "point",
  habillage = salud_alineado,
  #addEllipses = TRUE, ellipse.level = 0.68,
  repel = TRUE, label = "var",
  col.var = "steelblue",
  palette = c("grey80", "red"),
  pointshape = 19,
) + labs(title = "PCA (PC1 vs PC2)")
p_pca_sanos


# Top 5 variables para PC1–PC2
#loadings <- as.data.frame(pca6$rotation[, 1:2])
#loadings$importance <- rowSums(loadings^2)
#vars_top5 <- rownames(loadings)[order(loadings$importance, decreasing = TRUE)][1:5]

#p_pca_sanos_top5 <- fviz_pca_biplot(
#  pca6,
#  geom.ind = "point",
#  habillage = salud_alineado,
#  #addEllipses = TRUE, ellipse.level = 0.68,
#  repel = TRUE, label = "var",
#  col.var = "steelblue",
#  palette = c("grey80", "red"),
#  pointshape = 19,
#  select.var = list(name = vars_top5)
#) + labs(title = "PCA (PC1 vs PC2 – top 5)")
#p_pca_sanos_top5

# PC1 vs PC3 
p_pca_13 <- fviz_pca_biplot(
  pca6, axes = c(1, 3),
  geom.ind   = "point",
  habillage  = salud_alineado,
  repel      = TRUE, label = "var",
  #addEllipses= TRUE, ellipse.level = 0.68,
  col.var    = "steelblue",
  palette    = c("grey80", "red"),
  pointshape = 19
) + labs(title = "PCA (PC1 vs PC3)")
p_pca_13


# Top 5 variables para PC1–PC3
#vars_top5_13 <- names(sort(rowSums(pca6$rotation[, c(1,3)]^2), decreasing = TRUE))[1:5]
#p_pca_13_top5 <- fviz_pca_biplot(
#  pca6, axes = c(1,3),
#  geom.ind="point", habillage=salud_alineado,
#  repel=TRUE, label="var", 
#  #addEllipses=TRUE, ellipse.level=0.68,
#  col.var="steelblue", palette=c("grey80","red"),
#  pointshape=19,
#  select.var = list(name = vars_top5_13)
#) + labs(title = "PCA (PC1 vs PC3 – top 5)")
#p_pca_13_top5

# PC2 vs PC3 
p_pca_23 <- fviz_pca_biplot(
  pca6, axes = c(2, 3),
  geom.ind   = "point",
  habillage  = salud_alineado,
  repel      = TRUE, label = "var",
  #addEllipses= TRUE, ellipse.level = 0.68,
  col.var    = "steelblue",
  palette    = c("grey80", "red"),
  pointshape = 19
) + labs(title = "PCA (PC2 vs PC3)")
p_pca_23

# Top-5 variables para PC2–PC3
#vars_top5_23 <- names(sort(rowSums(pca6$rotation[, c(2,3)]^2), decreasing = TRUE))[1:5]
#p_pca_23_top5 <- fviz_pca_biplot(
#  pca6, axes = c(2,3),
#  geom.ind="point", habillage=salud_alineado,
#  repel=TRUE, label="var", 
#  #addEllipses=TRUE, ellipse.level=0.68,
#  col.var="steelblue", palette=c("grey80","red"),
#  pointshape=19,
#  select.var = list(name = vars_top5_23)
#) + labs(title = "PCA (PC2 vs PC3 – top 5)")
#p_pca_23_top5


# ------------------------------------------------------------------------------
#                          EL 3D
# ------------------------------------------------------------------------------

# Usa el PCA que ya graficas con fviz (pca6). Si no existiera, cae en pca_resultado.
pca_src <- if (exists("pca6")) pca6 else pca_resultado

# PCs + grupo salud (alineado a df6)
pc_df <- as.data.frame(pca_src$x)[, 1:3] %>%
  mutate(Grupo_Salud = df6$Grupo_Salud)

# Varianza explicada para etiquetar ejes
var_exp <- (pca_src$sdev^2) / sum(pca_src$sdev^2)
var_PC1 <- round(100 * var_exp[1], 1)
var_PC2 <- round(100 * var_exp[2], 1)
var_PC3 <- round(100 * var_exp[3], 1)

# Figura 3D (solo gris y rojo, sin capa de outliers)
plot_ly(
  pc_df,
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d", mode = "markers",
  color  = ~factor(Grupo_Salud, levels = c(0,1), labels = c("No sano","Sano")),
  colors = c("grey70", "red"),
  marker = list(size = 4, opacity = 0.85)
) %>%
  layout(
    scene = list(
      xaxis = list(title = paste0("PC1 (", var_PC1, "%)")),
      yaxis = list(title = paste0("PC2 (", var_PC2, "%)")),
      zaxis = list(title = paste0("PC3 (", var_PC3, "%)"))
    ),
    title = "Nube de datos PCA (3D)"
  )

# Se hizo un video de la simulación y se colocó en el texto de la tesina.





# ------------------------------------------------------------------------------
#                                     QDA
# ------------------------------------------------------------------------------
# Predicción de los sanos

# Etiqueta binaria (mismo orden que usaste en LDA)
yS <- factor(base18$Grupo_Salud, levels = c(0,1), labels = c("NoSano","Sano"))

# Misma familia de predictores que en PCA (como X_sano), quitando var=0 por CLASE
X_qda <- base18 %>% select(all_of(colnames(X_sano)))

keep_qda <- sapply(names(X_qda), function(nm) {
  sds <- tapply(X_qda[[nm]], yS, sd, na.rm = TRUE)   # sd dentro de cada clase
  all(!is.na(sds) & sds > 0)
})
X_qda2 <- X_qda[, keep_qda, drop = FALSE]

# Estandarizar (igual que hiciste en LDA)
X_std_qda <- as.data.frame(scale(X_qda2))

# Ensamble final
dat_qda <- bind_cols(Salud = yS, X_std_qda)


# --- Ajuste QDA con priors balanceadas ---
fit_salud_qda <- qda(Salud ~ ., data = dat_qda, prior = c(0.5, 0.5))

# --- Rendimiento en entrenamiento ---
pred_tr_qda <- predict(fit_salud_qda)
tab_tr_qda  <- table(Real = dat_qda$Salud, Pred = pred_tr_qda$class)
acc_tr_qda  <- mean(pred_tr_qda$class == dat_qda$Salud)

cat("\nQDA Salud — Accuracy (train):", round(acc_tr_qda, 3), "\n"); print(tab_tr_qda)
cat("\nRecall por clase (train):\n"); print(round(prop.table(tab_tr_qda, 1), 3))

# --- LOOCV manual (qda no tiene CV=TRUE) ---
# Nota: para ser 100% estricto, el escalado debería recalcularse dentro de cada fold.
# Aquí replicamos tu pipeline (escalado global) para comparabilidad con LDA.

pred_cv_qda <- vector("character", nrow(dat_qda))

for (i in seq_len(nrow(dat_qda))) {
  fit_i <- qda(Salud ~ ., data = dat_qda[-i, ], prior = c(0.5, 0.5))
  pred_i <- predict(fit_i, dat_qda[i, -1, drop = FALSE])$class
  pred_cv_qda[i] <- as.character(pred_i)
}

pred_cv_qda <- factor(pred_cv_qda, levels = levels(dat_qda$Salud))
tab_cv_qda  <- table(Real = dat_qda$Salud, Pred = pred_cv_qda)
acc_cv_qda  <- mean(pred_cv_qda == dat_qda$Salud)


cat("\nQDA Salud — Accuracy (LOOCV):", round(acc_cv_qda, 3), "\n"); print(tab_cv_qda)

# --- (Opcional) Probabilidades posteriores en train ---
# Útil para umbrales / análisis de calibración
post_train <- as.data.frame(pred_tr_qda$posterior)  # columnas: NoSano, Sano
head(post_train)




# -------------------------------------------------------------------------------
#                      RESUMEN DE LOS FALSOS POSITIVOS

# VAMOS A VER QUIÉNES SON LOS FALSOS POSITIVOS

# Usa el que aplique a tu caso:
ID_vec <- base18$ID
# ID_vec <- df_imputado_final$ID

# --- TRAIN ---
pred_tr_lda <- predict(fit_salud)  # posterior y class
res_lda_tr <- dat_lda %>%
  mutate(
    ID         = ID_vec,
    Predicho   = pred_tr_lda$class,
    Prob_Sano  = pred_tr_lda$posterior[, "Sano"]
  ) %>%
  select(ID, Real = Salud, Predicho, Prob_Sano)

fp_lda_tr <- res_lda_tr %>% filter(Real == "NoSano", Predicho == "Sano")
ids_fp_lda_tr <- unique(fp_lda_tr$ID)

cat("\nIDs falsos positivos LDA (TRAIN):\n")
print(ids_fp_lda_tr)

# --- LOOCV ---
fit_cv_lda <- lda(Salud ~ ., data = dat_lda, CV = TRUE, prior = c(0.5, 0.5))

res_lda_cv <- dat_lda %>%
  mutate(
    ID        = ID_vec,
    Predicho  = fit_cv_lda$class,
    Prob_Sano = fit_cv_lda$posterior[, "Sano"]
  ) %>%
  select(ID, Real = Salud, Predicho, Prob_Sano)

fp_lda_cv <- res_lda_cv %>% filter(Real == "NoSano", Predicho == "Sano")
ids_fp_lda_cv <- unique(fp_lda_cv$ID)

cat("\nIDs falsos positivos LDA (LOOCV):\n")
print(ids_fp_lda_cv)



# QDA
# --- TRAIN ---
pred_tr_qda <- predict(fit_salud_qda)  # tiene $class y $posterior

res_qda_tr <- dat_qda %>%
  mutate(
    ID         = ID_vec,
    Predicho   = pred_tr_qda$class,
    Prob_Sano  = pred_tr_qda$posterior[, "Sano"]
  ) %>%
  select(ID, Real = Salud, Predicho, Prob_Sano)

fp_qda_tr <- res_qda_tr %>% filter(Real == "NoSano", Predicho == "Sano")
ids_fp_qda_tr <- unique(fp_qda_tr$ID)

cat("\nIDs falsos positivos QDA (TRAIN):\n")
print(ids_fp_qda_tr)

# --- LOOCV manual para QDA (con probas) ---
n <- nrow(dat_qda)
pred_cv_qda_class <- character(n)
pred_cv_qda_probS <- numeric(n)

for (i in seq_len(n)) {
  fit_i <- qda(Salud ~ ., data = dat_qda[-i, ], prior = c(0.5, 0.5))
  pr_i  <- predict(fit_i, dat_qda[i, -1, drop = FALSE])
  pred_cv_qda_class[i] <- as.character(pr_i$class)
  pred_cv_qda_probS[i] <- pr_i$posterior[, "Sano"]
}

res_qda_cv <- dat_qda %>%
  mutate(
    ID        = ID_vec,
    Predicho  = factor(pred_cv_qda_class, levels = levels(Salud)),
    Prob_Sano = pred_cv_qda_probS
  ) %>%
  select(ID, Real = Salud, Predicho, Prob_Sano)

fp_qda_cv <- res_qda_cv %>% filter(Real == "NoSano", Predicho == "Sano")

ids_fp_qda_cv <- unique(fp_qda_cv$ID)

cat("\nIDs falsos positivos QDA (LOOCV):\n")
print(ids_fp_qda_cv)




# ----------------------------------------------------------------
#            Gráficos con los FP y FN detectados con QDA
# ----------------------------------------------------------------
# Vamos a poner a los sanos el gráfico que hicimos con anterioridad
p_pca_sanos <- fviz_pca_biplot(
  pca6,
  geom.ind = "point",
  habillage = salud_alineado,
  #addEllipses = TRUE, ellipse.level = 0.68,
  repel = TRUE, label = "var",
  col.var = "steelblue",
  palette = c("grey80", "red"),
  pointshape = 19,
) + labs(title = "PCA (PC1 vs PC2)")
p_pca_sanos

# FP/FN en PC1–PC2 
# IDs de FP / FN desde el QDA (LOOCV)
ids_fp <- res_qda_cv %>%
  filter(Real == "NoSano", Predicho == "Sano") %>%
  pull(ID) %>% unique()

ids_fn <- res_qda_cv %>%
  filter(Real == "Sano", Predicho == "NoSano") %>%
  pull(ID) %>% unique()

# Coordenadas PC1–PC2 alineadas al PCA 
idx_active <- seq_len(nrow(pca6$x))              # filas activas del PCA
IDs_alineados <- df6$ID[idx_active]              # ID en el mismo orden
scores_12 <- data.frame(
  PC1 = pca6$x[idx_active, 1],
  PC2 = pca6$x[idx_active, 2],
  ID  = as.character(IDs_alineados),
  stringsAsFactors = FALSE
)

# Subconjuntos de puntos a marcar
fp_pts_12 <- dplyr::filter(scores_12, ID %in% as.character(ids_fp))
fn_pts_12 <- dplyr::filter(scores_12, ID %in% as.character(ids_fn))

# Añadir aros y leyenda extra al lado derecho abajo de las etiquetas enteriores de sano/no sano
p_pca_sanos <- p_pca_sanos +
  ggnewscale::new_scale_color() +
  geom_point(
    data = fp_pts_12, aes(x = PC1, y = PC2, color = "Falsos Positivos"),
    shape = 21, fill = NA, stroke = 1.2, size = 3
  ) +
  geom_point(
    data = fn_pts_12, aes(x = PC1, y = PC2, color = "Falsos Negativos"),
    shape = 21, fill = NA, stroke = 1.2, size = 3
  ) +
  scale_color_manual(
    name   = NULL,
    values = c("Falsos Positivos" = "navy", "Falsos Negativos" = "skyblue"),
    breaks = c("Falsos Positivos","Falsos Negativos")
  ) +
  guides(color = guide_legend(override.aes = list(shape = 21, fill = NA)))

# Mostrar
p_pca_sanos



# --- PC1 vs PC3 (sanos en rojo) ---
p_pca_13 <- fviz_pca_biplot(
  pca6, axes = c(1, 3),
  geom.ind   = "point",
  habillage  = salud_alineado,
  repel      = TRUE, label = "var",
  #addEllipses= TRUE, ellipse.level = 0.68,
  col.var    = "steelblue",
  palette    = c("grey80", "red"),
  pointshape = 19
) + labs(title = "PCA (PC1 vs PC3)")


# ====== FP/FN en PC1–PC3 (aros huecos + leyenda) ======

scores_13 <- data.frame(
  PC1 = pca6$x[idx_active, 1],
  PC3 = pca6$x[idx_active, 3],
  ID  = as.character(IDs_alineados),
  stringsAsFactors = FALSE
)

# Subconjuntos de puntos a marcar
fp_pts_13 <- dplyr::filter(scores_13, ID %in% as.character(ids_fp))
fn_pts_13 <- dplyr::filter(scores_13, ID %in% as.character(ids_fn))

# Añadir aros y leyenda extra 
p_pca_13 <- p_pca_13 +
  ggnewscale::new_scale_color() +
  geom_point(
    data = fp_pts_13, aes(x = PC1, y = PC3, color = "Falsos Positivos"),
    shape = 21, fill = NA, stroke = 1.2, size = 3
  ) +
  geom_point(
    data = fn_pts_13, aes(x = PC1, y = PC3, color = "Falsos Negativos"),
    shape = 21, fill = NA, stroke = 1.2, size = 3
  ) +
  scale_color_manual(
    name   = NULL,
    values = c("Falsos Positivos" = "navy", "Falsos Negativos" = "skyblue"),
    breaks = c("Falsos Positivos","Falsos Negativos")
  ) +
  guides(color = guide_legend(override.aes = list(shape = 21, fill = NA)))

# Mostrar
p_pca_13



# --- PC2 vs PC3 (sanos en rojo) ---
p_pca_23 <- fviz_pca_biplot(
  pca6, axes = c(2, 3),
  geom.ind   = "point",
  habillage  = salud_alineado,
  repel      = TRUE, label = "var",
  #addEllipses= TRUE, ellipse.level = 0.68,
  col.var    = "steelblue",
  palette    = c("grey80", "red"),
  pointshape = 19
) + labs(title = "PCA (PC2 vs PC3)")



# ====== FP/FN en PC2–PC3 (aros huecos + leyenda) ======

# recrea SIEMPRE el base antes de añadir aros
p_pca_23 <- fviz_pca_biplot(
  pca6, axes = c(2, 3),
  geom.ind = "point", habillage = salud_alineado,
  repel = TRUE, label = "var",
  col.var = "steelblue", palette = c("grey80","red"),
  pointshape = 19
) + labs(title = "PCA (PC2 vs PC3)")


scores_23 <- data.frame(
  PC2 = pca6$x[idx_active, 2],
  PC3 = pca6$x[idx_active, 3],
  ID  = as.character(IDs_alineados),
  stringsAsFactors = FALSE
)

# Subconjuntos a marcar
fp_pts_23 <- dplyr::filter(scores_23, ID %in% as.character(ids_fp))
fn_pts_23 <- dplyr::filter(scores_23, ID %in% as.character(ids_fn))

# Añadir aros y leyenda SIN tocar tu paleta de Sano/No sano
p_pca_23 <- p_pca_23 +
  ggnewscale::new_scale_color() +
  geom_point(
    data = fp_pts_23, aes(x = PC2, y = PC3, color = "Falsos Positivos"),
    shape = 21, fill = NA, stroke = 1.2, size = 3
  ) +
  geom_point(
    data = fn_pts_23, aes(x = PC2, y = PC3, color = "Falsos Negativos"),
    shape = 21, fill = NA, stroke = 1.2, size = 3
  ) +
  scale_color_manual(
    name   = NULL,
    values = c("Falsos Positivos" = "navy",
               "Falsos Negativos" = "skyblue"),
    breaks = c("Falsos Positivos", "Falsos Negativos")
  ) +
  guides(color = guide_legend(override.aes = list(shape = 21, fill = NA)))

# Mostrar
p_pca_sanos
p_pca_13
p_pca_23



# -------------------------------------------------------------------------------------
#                                         LOGÍSTICO
# -------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------
# Regresión logística con las mismas variables que QDA
# Salida: Betas, p, p-ajustada, OR e IC
# ------------------------------------------------------------------------------------

# Usa exactamente las variables numéricas de QDA
X_logit <- num_vars6  
# Define la respuesta Salud (misma codificación que QDA)
y_logit <- factor(df6$Grupo_Salud, levels = c(0,1), labels = c("NoSano","Sano"))

# Data frame final
dat_logit <- bind_cols(Salud = y_logit, X_logit)

# 4) Construye fórmula con backticks (por si hay espacios o símbolos raros)
term_names <- names(X_logit)
terms_bt   <- paste(sprintf("`%s`", term_names), collapse = " + ")
form_logit <- as.formula(paste("Salud ~", terms_bt))

# Ajustamos el modelo logístico
fit_logit <- glm(form_logit, data = dat_logit, family = binomial())

# Resultados: Betas, p, OR e IC95%
tabla_or <- broom::tidy(fit_logit, exponentiate = TRUE, conf.int = TRUE) %>%
  arrange(p.value)

print(summary(fit_logit))
print(tabla_or, n = Inf)
#write.xlsx(tabla_or, "Tabla8_Regresion_Logistica.xlsx")

# Diagnósticos rápidos
# Multicolinealidad
suppressWarnings({
  vifs <- tryCatch(car::vif(fit_logit), error = function(e) NA)
})
cat("\nVIFs:\n")
print(vifs)

# AUC
prob_train <- predict(fit_logit, type = "response")
roc_curve  <- pROC::roc(dat_logit$Salud, prob_train, levels = c("NoSano","Sano"), direction = "<")
cat("\nAUC:", pROC::auc(roc_curve), "\n")
plot(roc_curve)


#                          ACTUALIZACIÓN DEL 08 DE SEPTIEMBRE
# ---------------------------------------------------------------------------------------------
# Vamos a encontrar la mejor p para hacer el corte entre sano y no sano
# ---------------------------------------------------------------------------------------------

# ?pROC::coords

# ---  Umbral óptimo por Youden ---
best_youden <- coords(
  roc_curve, 
  x = "best", best.method = "youden",
  ret = c("threshold","sensitivity","specificity","accuracy","ppv","npv"),
  transpose = FALSE
)

thr_youden <- as.numeric(best_youden["threshold"])
cat("\n--- Óptimo Youden ---\n")
print(best_youden)

# --- Umbral óptimo por cercanía a (0,1) ---
best_closest <- coords(
  roc_curve, 
  x = "best", best.method = "closest.topleft",
  ret = c("threshold","sensitivity","specificity","accuracy","ppv","npv"),
  transpose = FALSE
)

thr_closest <- as.numeric(best_closest["threshold"])
cat("\n--- Óptimo más cercano a (0,1) ---\n")
print(best_closest)

# Clasificamos usando el umbral thr_closest 
umbral_final <-   thr_closest    

pred_class <- ifelse(prob_train >= umbral_final, "Sano", "NoSano")
pred_class <- factor(pred_class, levels = c("NoSano","Sano"))


# Matriz de confusión y métricas simples
tab_thr <- table(Real = dat_logit$Salud, Pred = pred_class)
acc_thr <- sum(diag(tab_thr)) / sum(tab_thr)
sens_thr <- tab_thr["Sano","Sano"] / sum(tab_thr["Sano",])
esp_thr  <- tab_thr["NoSano","NoSano"] / sum(tab_thr["NoSano",])

cat("\n--- Rendimiento con umbral_final =", round(umbral_final, 4), "---\n")
print(tab_thr)
cat(sprintf("Accuracy=%.3f  Sens=%.3f  Esp=%.3f\n", acc_thr, sens_thr, esp_thr))



# =======================================================

# --- Clasificación usando el umbral thr_youden ---
umbral_final <-   thr_youden    

pred_class <- ifelse(prob_train >= umbral_final, "Sano", "NoSano")
pred_class <- factor(pred_class, levels = c("NoSano","Sano"))

# Matriz de confusión y métricas simples
tab_thr <- table(Real = dat_logit$Salud, Pred = pred_class)
acc_thr <- sum(diag(tab_thr)) / sum(tab_thr)
sens_thr <- tab_thr["Sano","Sano"] / sum(tab_thr["Sano",])
esp_thr  <- tab_thr["NoSano","NoSano"] / sum(tab_thr["NoSano",])

cat("\n--- Rendimiento con umbral_final =", round(umbral_final, 4), "---\n")
print(tab_thr)
cat(sprintf("Accuracy=%.3f  Sens=%.3f  Esp=%.3f\n", acc_thr, sens_thr, esp_thr))


# --- Anotar el punto óptimo en la curva ROC ya graficada ---
plot.roc(roc_curve, print.auc = TRUE, legacy.axes = TRUE)
points(1 - as.numeric(best_youden["specificity"]),
       as.numeric(best_youden["sensitivity"]),
       pch = 19, cex = 1.2, col = "red")
text(1 - as.numeric(best_youden["specificity"]),
     as.numeric(best_youden["sensitivity"]),
     labels = sprintf(" Youden: p=%.3f", thr_youden), pos = 4, col = "red")

points(1 - as.numeric(best_closest["specificity"]),
       as.numeric(best_closest["sensitivity"]),
       pch = 19, cex = 1.2, col = "blue")
text(1 - as.numeric(best_closest["specificity"]),
     as.numeric(best_closest["sensitivity"]),
     labels = sprintf(" Closest: p=%.3f", thr_closest), pos = 4, col = "blue")



# --- Ejes en español ---

plot.roc(
  roc_curve,
  print.auc = TRUE,
  legacy.axes = TRUE,
  xlab = "1 - Especificidad",
  ylab = "Sensibilidad",
  main = "Curva ROC con puntos óptimos"
)

points(1 - as.numeric(best_youden["specificity"]),
       as.numeric(best_youden["sensitivity"]),
       pch = 19, cex = 1.2, col = "red")
text(1 - as.numeric(best_youden["specificity"]),
     as.numeric(best_youden["sensitivity"]),
     labels = sprintf(" Youden: p=%.3f", thr_youden), pos = 4, col = "red")

points(1 - as.numeric(best_closest["specificity"]),
       as.numeric(best_closest["sensitivity"]),
       pch = 19, cex = 1.2, col = "blue")
text(1 - as.numeric(best_closest["specificity"]),
     as.numeric(best_closest["sensitivity"]),
     labels = sprintf(" Closest: p=%.3f", thr_closest), pos = 4, col = "blue")




