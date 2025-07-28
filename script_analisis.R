### Exceso de mortalidad por DM en Argentina durante la pandemia de COVID-19
### Autora: Tamara Ricardo
### Fecha modificación:



# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  PHEindicatormethods,    # Estandarizar tasas
  mcp,                    # Regresión joinpoint
  gtsummary,
  scico,
  patchwork,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
## Identificadores provincia
provincias <- import("cod_pcias_arg.csv")


## Defunciones anuales 2005-23 (DEIS)
def_raw <- bind_rows(
  # 2005-2022
  import("raw/defunciones-ocurridas-y-registradas-en-la-republica-argentina-entre-los-anos-2005-2022.csv"),
  # 2023
  import("raw/defuncion2023.csv")
)


## Población Censo 2001 (INDEC)
pob_01_raw <- import("raw/_tmp_5724381.xlsX",
                     range = "B12:D563")


## Población Censo 2010 (INDEC)
pob_10_raw <- import("raw/_tmp_5850861.xlsX", 
                     range = "B12:D563")


## Población Censo 2022 (INDEC)
pob_22_raw <- import("raw/_tmp_5850841.xlsX", 
                     range = "B12:D610")


## USAR PROYECCIONES PARA TASA BRUTA
proy_2010_raw <- import("raw/c2_proyecciones_prov_2010_2040.xls")



# Limpiar datos -----------------------------------------------------------
## Defunciones 2005-23
def <- def_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  rename(anio_defun = anio,
         prov_id = jurisdiccion_de_residencia_id,
         cie10_id = cie10_causa_id,
         cie10_cat = cie10_clasificacion) |> 
  
  # Filtrar datos ausentes provincia
  filter(!prov_id %in% c(98, 99)) |> 
  
  # Filtrar datos ausentes sexo
  filter(between(sexo_id, 1, 2)) |> 
  
  # Filtrar datos ausentes y menores de 15 años grupo edad
  filter(between(grupo_edad, "02.De 15 a 34 anios", "05.De 75 anios y mas")) |> 
  
  # Renombrar grupos edad
  mutate(grupo_edad = fct_relabel(grupo_edad,
                                  ~ c("15 a 34 años",
                                      "35 a 54 años",
                                      "55 a 74 años",
                                      "75+ años"))) |> 
  
  # Filtrar muertes por DM
  filter(between(cie10_id, "E10", "E14")) |> 
  
  # Añadir etiquetas provincias
  left_join(provincias) |> 
  
  # Crear variable para Censo
  mutate(anio_censo = case_when(
    between(anio_defun, 2005, 2009) ~ "2001",
    between(anio_defun, 2010, 2019) ~ "2010",
    .default = "2022"
  )) |> 

  # Contar muertes por año, provincia, sexo y grupo etario
  count(anio_censo, anio_defun, prov_id, prov_nombre, grupo_edad, sexo,
        wt = cantidad, name = "defun_dm") 
  
  
## Población Censo 2001
pob_01 <- pob_01_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  rename(masculino = x2,
         femenino = x3) |> 
  
  # Crear id de Censo
  mutate(anio_censo = "2001") |> 
  
  # Crear id de provincia
  mutate(prov_id = if_else(str_detect(x1, "AREA"),
                           str_sub(x1, start = 7), NA)) |> 
  
  # Completar filas
  fill(prov_id, .direction = "down") |> 
  
  # Filtrar datos
  filter(between(x1, "15-19 Años", "95 y más Años")) |> 
  
  # Crear variable para grupo etario
  mutate(grupo_edad = case_when(
    between(x1, "15-19 Años", "30-34 Años") ~ "15 a 34 años",
    between(x1, "35-39 Años", "50-54 Años") ~ "35 a 54 años",
    between(x1, "55-59 Años", "70-74 Años") ~ "55 a 74 años",
    .default = "75+ años"),
    x1 = NULL) |> 
  
  # Base long por sexo
  pivot_longer(cols = c(masculino, femenino),
               names_to = "sexo") |> 
  
  # Población e id provincia a numérico
  mutate(across(.cols = c(prov_id, value), 
                .fns =  ~ parse_number(.x))) |> 
  
  # Contar población por provincia, sexo y grupo etario
  count(anio_censo, prov_id, grupo_edad, sexo,
        wt = value, name = "poblacion")


## Población Censo 2010
pob_10 <- pob_10_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  rename(masculino = x2,
         femenino = x3) |> 
  
  # Crear id de Censo
  mutate(anio_censo = "2010") |> 
  
  # Crear id de provincia
  mutate(prov_id = if_else(str_detect(x1, "AREA"),
                           str_sub(x1, start = 7), NA)) |> 
  
  # Completar filas
  fill(prov_id, .direction = "down") |> 
  
  # Filtrar datos
  filter(between(x1, "15 a 19", "95 y más")) |> 
  
  # Crear variable para grupo etario
  mutate(grupo_edad = case_when(
    between(x1, "15-19", "30-34") ~ "15 a 34 años",
    between(x1, "35-39", "50-54") ~ "35 a 54 años",
    between(x1, "55-59", "70-74") ~ "55 a 74 años",
    .default = "75+ años"),
    x1 = NULL) |> 
  
  # Base long por sexo
  pivot_longer(cols = c(masculino, femenino),
               names_to = "sexo") |> 
  
  # Población e id provincia a numérico
  mutate(across(.cols = c(prov_id, value), 
                .fns =  ~ parse_number(.x))) |> 
  
  # Contar población por provincia, sexo y grupo etario
  count(anio_censo, prov_id, grupo_edad, sexo,
        wt = value, name = "poblacion")


## Población Censo 2022
pob_22 <- pob_22_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  rename(femenino = x2,
         masculino = x3) |> 
  
  # Crear id de Censo
  mutate(anio_censo = "2022") |> 
  
  # Crear id de provincia
  mutate(prov_id = if_else(str_detect(x1, "AREA"),
                           str_sub(x1, start = 7), NA)) |> 
  
  # Completar filas
  fill(prov_id, .direction = "down") |> 
  
  # Filtrar datos
  filter(between(x1, "100 a 104", "95 a 99")) |> 
  
  # Crear variable para grupo etario
  mutate(grupo_edad = case_when(
    between(x1, "15 a 19", "30 a 34") ~ "15 a 34 años",
    between(x1, "35 a 39", "50 a 54") ~ "35 a 54 años",
    between(x1, "55 a 59", "70 a 74") ~ "55 a 74 años",
    .default = "75+ años"),
    x1 = NULL) |> 
  
  # Base long por sexo
  pivot_longer(cols = c(masculino, femenino),
               names_to = "sexo") |> 
  
  # Población e id provincia a numérico
  mutate(across(.cols = c(prov_id, value), 
                .fns =  ~ parse_number(.x))) |> 
  
  # Contar población por provincia, sexo y grupo etario
  group_by(anio_censo,prov_id, grupo_edad, sexo) |> 
  summarise(poblacion = sum(value, na.rm = TRUE),
            .groups = "drop")

  
# Unir datasets -----------------------------------------------------------
datos_join <- def |> 
  left_join(
  # Población por provincia, grupo etario y sexo (INDEC)
  bind_rows(pob_01,
            pob_10,
            pob_22)) |> 
  
  # Población estándar Censo 2010
  left_join(pob_10 |> 
              select(prov_id, grupo_edad, sexo,
                     pob_est_2010 = poblacion)) |> 
  
  # Crear variable para región
  mutate(region = case_when(
    prov_id == 2 ~ "CABA",
    prov_id %in% c(6, 14, 30, 42, 82) ~ "Pampeana",
    prov_id %in% c(10, 38, 46, 66, 86, 90) ~ "Noroeste",
    prov_id %in% c(18, 22, 34, 54) ~ "Noreste",
    prov_id %in% c(50, 70, 74) ~ "Cuyo",
    .default = "Patagónica"
  ), .before = prov_id) |> 
  
  # Calcular tasa bruta de mortalidad
  phe_rate(x = defun_dm,
           n = poblacion,
           type = "standard") |> 
  
  # Renombrar variables
  rename(tasa_br = value,
         tasa_br_lower = lowercl,
         tasa_br_upper = uppercl) |> 

  # Variables caracter a factor
  mutate(across(.cols = where(is.character),
                .fns = ~ factor(.x)))
  

### Limpiar environment
rm(list = setdiff(ls(), "datos_join"))


# Calcular tasas brutas y estandarizadas ----------------------------------
## Tasa bruta de mortalidad por año, provincia, sexo y grupo etario
tasa_bruta_prov <- datos_join |> 
 # Calcular tasas
  phe_rate(x = defun_dm,
           n = poblacion)


## Tasa ajustada de mortalidad por año, provincia, sexo y grupo etario
tasa_est_prov <- datos_join |> 
  # Calcular tasas
  group_by(anio_defun, prov_id, prov_nombre, grupo_edad, sexo) |> 
  calculate_dsr(x = defun_dm,
                n = poblacion,
                stdpop = pob_est_2010)



# Tasa bruta de mortalidad por año, región, sexo y grupo etario
tasa_bruta_reg <- datos_join |> 
  # Calcular muertes por región
  group_by(anio_defun, region, grupo_edad, sexo) |> 
  summarise(across(.cols = c(poblacion, defun_dm),
                   .fns = ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") |> 
  
  # Calcular tasas
  phe_rate(x = defun_dm,
           n = poblacion)


## Tasa ajustada de mortalidad por año, región, sexo y grupo etario
tasa_est_reg <- datos_join |> 
  # Calcular muertes por región
  group_by(anio_defun, region, grupo_edad, sexo) |> 
  summarise(across(.cols = c(poblacion, pob_est_2010, defun_dm),
                   .fns = ~ sum(.x, na.rm = TRUE))) |> 

  # Calcular tasas
  group_by(anio_defun, region, grupo_edad, sexo) |> 
  calculate_dsr(x = defun_dm,
                n = poblacion,
                stdpop = pob_est_2010)
  

# Análisis exploratorio ---------------------------------------------------
## Frecuencia muertes por sexo, año, región, grupo edad
tbl_continuous(datos_join, 
               variable = defun_dm, 
               by = sexo,
               include = c(anio_defun, region, grupo_edad),
               statistic = everything() ~ "{sum}") |> 
  add_overall() |> 
  add_p()


## Muertes por año y sexo
g1 <- datos_join |> 
  count(anio_defun, sexo, wt = defun_dm) |> 
  
  # Gráfico
  ggplot(aes(x = anio_defun, y = n, color = sexo)) +
  geom_point() +
  geom_line() +
  scale_color_scico_d(direction = -1, end = .75) +
  scale_y_continuous(limits = c(0, 6000)) +
  labs(x = "Año", y = "Muertes por DM")


## Tasa bruta de mortalidad por año y sexo
g2 <- datos_join |> 
  # Calcular tasas
  group_by(anio_defun, sexo) |> 
  phe_rate(x = defun_dm,
           n = poblacion) |> 
  
  # Gráfico
  ggplot(aes(x = anio_defun, y = value, color = sexo)) +
  geom_point() +
  geom_line() +
  scale_color_scico_d(direction = -1, end = .75) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = "Año", y = "Mortalidad (100000 hab.)")


## Tasa estandarizada de mortalidad por año y sexo
g3 <- datos_join |> 
  # Calcular tasas
  group_by(anio_defun, sexo) |> 
  calculate_dsr(x = defun_dm,
                n = poblacion,
                stdpop = pob_est_2010) |> 
  
  # Gráfico
  ggplot(aes(x = anio_defun, y = value, color = sexo)) +
  geom_point() +
  geom_line() +
  scale_color_scico_d(direction = -1, end = .75) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = "Año", y = "Mortalidad est. (100000 hab.)") 


## Mostrar gráficos
g1 + g2 + g3 +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme_minimal() &
  theme(legend.position = "bottom")


## Muertes por año, grupo etario y sexo
g1 <- datos_join |> 
  count(anio_defun, grupo_edad, sexo, wt = defun_dm) |> 
  
  # Gráfico
  ggplot(aes(x = anio_defun, y = n, color = paste(sexo, grupo_edad, sep = ":"))) +
  geom_point() +
  geom_line() +
  scale_color_scico_d(direction = -1, end = .75) +
  scale_y_continuous(limits = c(0, 6000)) +
  labs(x = "Año", y = "Muertes por DM")


## Tasa bruta de mortalidad por año y sexo y grupo etario
g2 <- datos_join |> 
  # Calcular tasas
  group_by(anio_defun, grupo_edad, sexo) |> 
  phe_rate(x = defun_dm,
           n = poblacion) |> 
  
  # Gráfico
  ggplot(aes(x = anio_defun, y = value, 
             color = paste(sexo, grupo_edad, sep = ":"))) +
  geom_point() +
  geom_line() +
  scale_color_scico_d(direction = -1, end = .75) +
  #scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Año", y = "Mortalidad est. (100000 hab.)")


## Mostrar gráficos
g1 + g2 +
  plot_layout(guides = "collect", axis_titles = "collect") &
  theme_minimal() &
  theme(legend.position = "bottom")


# Regresión joinpoint -----------------------------------------------------
## Tasa estandarizada por provincia
# Modelo con un punto de cambio
mod <- list(
  value ~ 1 + anio_defun,
  ~ 1 + anio_defun,
  ~ 1 + anio_defun
)

mod1 <- mcp(mod, data = tasa_est_prov)

summary(mod1)
plot(mod1)

