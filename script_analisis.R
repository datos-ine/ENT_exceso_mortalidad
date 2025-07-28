### Exceso de mortalidad por DM en Argentina durante la pandemia de COVID-19
### Autora: Tamara Ricardo
### Fecha modificación:


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  PHEindicatormethods,    # Estandarizar tasas
  mcp,                    # Regresión joinpoint
  scico,
  patchwork,
  tidyverse,
  readxl
)


# Cargar datos ------------------------------------------------------------
## Defunciones anuales 2005-22 (MSAL)
def_raw <-import("clean/defun_2015_2022.csv")



  
  
# Limpiar datos -----------------------------------------------------------

  

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

