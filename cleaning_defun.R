### Exceso de mortalidad por ENTs en Argentina durante la pandemia de COVID-19
### Limpieza de los datasets: Defunciones Generales Mensuales ocurridas y
### registradas en la República Argentina (2015-2022) y 
### Casos de COVID-19 registrados en la República Argentina (2020-2022)
### Disponibles en:
### https://datos.gob.ar/dataset/salud-defunciones-generales-mensuales-ocurridas-registradas-republica-argentina
### Autora: Tamara Ricardo
### Fecha modificación:



# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
  )


# Cargar datos ------------------------------------------------------------
## Defunciones mensuales 2015-2022 (DEIS-MSAL)
defun_raw <-import("raw/arg_def_m_15_22.csv")


## Casos COVID-19 2020-2022 (SNVS-MSAL)
covid_raw <- import("raw/Covid19Casos.csv") |> 
  # Filtrar casos no confirmados
  filter(clasificacion_resumen == "Confirmado")



# Explorar datos crudos ---------------------------------------------------
## Defunciones mensuales
glimpse(defun_raw)

tabyl(defun_raw$region)

tabyl(defun_raw$jurisdiccion)

tabyl(defun_raw$grupo_etario)

tabyl(defun_raw$sexo_nombre)

tabyl(defun_raw$grupo_causa_defuncion_CIE10)


## Casos COVID-19
glimpse(covid_raw)

summary(covid_raw$edad)

tabyl(covid_raw$sexo)

tabyl(covid_raw$edad_años_meses)

tabyl(covid_raw$residencia_pais_nombre)

tabyl(covid_raw$residencia_provincia_nombre)

tabyl(covid_raw$fallecido)



# Limpieza datos defunciones 2015-2022 ------------------------------------
defun <- defun_raw |> 
  # Estandarizar nombres de columnas
  rename(sexo = sexo_nombre,
         prov_id = jurisdiccion,
         cie10_grupo = grupo_causa_defuncion_CIE10,
         cie10_cod = cod_causa_muerte_CIE10) |> 
  
  # Filtrar datos ausentes región
  filter(region != "10.sin especificar.") |> 
  
  # Filtrar datos ausentes sexo
  filter(sexo %in% c("femenino", "masculino")) |> 
  
  # Filtrar menores de 20 años y datos ausentes grupo etario
  filter(between(grupo_etario, "02.de 20 a 39 anios", "07. de 80 anios y mas")) |> 
  
  # Modificar etiquetas región
  mutate(region = fct_relabel(
    region, ~ c("Centro", "NEA", "NOA1", "NOA", "NOA2", "Cuyo1", "Cuyo2", 
                "Patagonia Norte", "Patagonia Sur")
    )) |> 
  
  # Crear columna para id de provincia
  mutate(prov_id = str_remove_all(prov_id, "[^0-9]")) |> 
  
  # Crear variable para nombre de provincia
  mutate(prov_nombre = fct_relabel(
    prov_id, ~ c("Córdoba", "Corrientes", "CABA", "Chaco", "Entre Ríos",
                 "Formosa", "Mendoza", "Misiones", "Buenos Aires",
                 "Santa Fe", "Tucumán", NA)
  )) |> 
  
  #  # Completar nombres faltantes provincia
  # mutate(prov_nombre = case_when(
  #   region == "NOA1" ~ "Jujuy, Salta",
  #   region == "NOA2" ~ "Catamarca, Santiago del Estero",
  #   region == "Cuyo2" ~ "San Juan, San Luis, La Rioja",
  #   region == "Patagonia Norte" ~ "La Pampa, Neuquén, Río Negro",
  #   region == "Patagonia Sur" ~ "Chubut, Santa Cruz, Tierra del Fuego",
  #   .default = prov_nombre
  # )) |> 
  
  # Modificar etiquetas grupo etario
  mutate(grupo_etario = fct_relabel(
    grupo_etario, ~ c("20-39 años", "40-49 años", "50-59 años",
                      "60-69 años", "70-79 años", "80+ años")
  )) |> 
  
  # Crear columna para causas de muerte
  mutate(causa_def = case_when(
    # Cáncer
    cie10_grupo == "0200 T MALIGNOS" ~ "Cáncer",
    # Cardiovasculares
    cie10_grupo == "0900 ENF  DEL SISTEMA CIRCULATORIO" ~ "Enfermedad cardiovascular",
    # Diabetes
    cie10_grupo == "0300 DIABETES MELLITUS" ~ "Diabetes mellitus",
    # Respiratorias crónicas: J30-J98
    between(cie10_cod, "J30", "J98") ~ "Enfermedad respiratoria crónica",
    # COVID-19
    cie10_cod == "U07" ~ "COVID-19",
    .default = "Otra"
  )) |> 
  
  # Agrupar datos
  count(anio_def, mes_def, region, prov_id, prov_nombre, grupo_etario, sexo, causa_def,
        wt = cantidad)


### Explorar datos
tabyl(defun$region)

tabyl(defun$grupo_etario)

tabyl(defun$sexo)

tabyl(defun$causa_def)



# Limpieza datos defunciones COVID-19 -------------------------------------
defun_covid <- covid_raw |> 
  
  # Estandarizar nombres de columnas
  rename(prov_id = residencia_provincia_id,
         prov_nombre = residencia_provincia_nombre) |> 
  
  # Filtrar casos de no residentes en el país
  filter(residencia_pais_nombre == "Argentina") |> 
  
  # Filtrar datos ausentes provincia
  filter(between(prov_id, 2, 94)) |> 
  
  # Filtrar datos ausentes sexo
  filter(sexo != "NR") |> 
  
  # Filtrar datos menores de edad
  filter(edad_años_meses == "Años" & edad >= 20) |> 
  
  # Filtrar casos no fallecidos
  filter(fallecido == "SI") |> 
  
  # Seleccionar columnas relevantes
  select(id_evento_caso, sexo, edad, prov_id, prov_nombre, fecha_fallecimiento) |> 
  
  # Modificar etiquetas sexo
  mutate(sexo = fct_relabel(sexo, ~ c("femenino", "masculino"))) |> 
  
  # Modificar formato fecha fallecimiento
  mutate(fecha_fallecimiento = ymd(fecha_fallecimiento)) |> 
  
  # Crear variables para año y mes de defunción
  mutate(anio_def = year(fecha_fallecimiento),
         mes_def = month(fecha_fallecimiento)) |> 
  
  # Crear variable para grupo etario
  mutate(grupo_edad = case_when(
    between(edad, 20, 39) ~ "20-39 años",
    between(edad, 40, 49) ~ "40-49 años",
    between(edad, 50, 59) ~ "50-59 años",
    between(edad, 60, 69) ~ "60-69 años",
    between(edad, 70, 79) ~ "70-79 años",
    .default = "80+ años"
  )) |> 
  
  # Contar fallecimientos por año, mes, provincia, grupo etario y sexo
  count(anio_def, mes_def, prov_id, prov_nombre, grupo_edad, sexo,
        name = "defun_covid") |> 
  
  # Añadir filas faltantes (combinaciones sin defunciones)
  complete(nesting(anio_def, mes_def), 
           nesting(prov_id, prov_nombre),
           grupo_edad, sexo,
           fill = list(defun_covid = 0)
  ) |> 
  
  # Crear variable para región
  mutate(region = case_when(
    prov_nombre %in% c("Corrientes", "Chaco", "Formosa", "Misiones") ~ "NEA",
    prov_nombre %in% c("Jujuy", "Salta") ~ "NOA1",
    prov_nombre == "Tucumán" ~ "NOA",
    prov_nombre %in% c("Catamarca", "Santiago del Estero") ~ "NOA2",
    prov_nombre == "Mendoza" ~ "Cuyo1",
    prov_nombre %in% c("La Rioja", "San Juan", "San Luis") ~ "Cuyo2",
    prov_nombre %in% c("La Pampa", "Neuquén", "Río Negro") ~ "Patagonia Norte",
    prov_nombre %in% c("Chubut", "Santa Cruz", "Tierra del Fuego") ~ "Patagonia Sur",
    .default = "Centro"
  ), .after = prov_nombre)



# Guardar datos limpios ---------------------------------------------------
## Defunciones 2015-2022
export(defun, file = "clean/defun_2015_2022.csv")

## Defunciones COVID-19
export(defun_covid, file = "clean/arg_defun_covid_2020_2022.csv")


## Limpiar environment
rm(list = ls())
