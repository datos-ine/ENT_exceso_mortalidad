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

tabyl(defun_raw$sexo_nombre)

tabyl(defun_raw$grupo_etario)

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
  
  # Modificar etiquetas grupo etario
  mutate(grupo_etario = fct_relabel(
    grupo_etario, ~ c("20-39 años", "40-49 años", "50-59 años",
                      "60-69 años", "70-79 años", "80+ años")
  )) |> 
  
  # Crear variable para id numérico de provincia
  mutate(prov_id = str_extract(jurisdiccion, "\\d+")) |> 
  
  # Crear variable para etiqueta de provincia
  mutate(prov_nombre = case_when(
    prov_id == "14" ~ "Córdoba",
    prov_id == "30" ~ "Entre Ríos",
    prov_id == "6" ~ "Buenos Aires",
    prov_id == "90" ~ "Tucumán",
    prov_id == "99" ~ NA,
    .default = str_remove_all(jurisdiccion, "[0-9]|\\.")
  )) |> 
  
  # Crear variable para causas de muerte
  mutate(causa_def = case_when(
    # Diabetes mellitus (E10:E14)
    between(cie10_cod, "E10", "E14") ~ "Diabetes mellitus",
    # Tumores malignos (C00:C99)
    between(cie10_cod, "C00", "C99") ~ "Cáncer",
    # Respiratorias crónicas (J40:J47)
    between(cie10_cod, "J40", "J47") ~ "Enf. respiratorias crónicas",
    # Alzheimer y demencias (F00, F01, F03, G30:G31)
    cie10_cod %in% c("F00", "F01", "F03", "G30", "G31") ~ "Alzheimer/Demencias",
    # Enfermedad renal crónica (N18)
    cie10_cod == "N18" ~ "Enf. renal crónica",
    # Enfermedad de Parkinson (G20)
    cie10_cod == "G20" ~ "Enf. de Parkinson",
    # Accidente cerebrovascular - ACV (I60:I69)
    between(cie10_cod, "I60", "I69") ~ "ACV",
    # Enfermedad cardíaca isquémica (I20:I25)
    between(cie10_cod, "I20", "I25") ~ "Enf.cardíaca isquémica",
    # Hipertensión arterial (I10:I13)
    between(cie10_cod, "I10", "I15") ~ "Hipertensión",
    # Otras enfermedades cardiovasculares
    between(cie10_cod, "I00", "I09")|
      between(cie10_cod, "I14", "I19")|
      between(cie10_cod, "I26", "I59")|
      between(cie10_cod, "I70", "I99") ~ "Enf. cardiovasculares",
    # COVID-19
    cie10_cod == "U07" ~ "COVID-19",
    # Otras causas
    .default = "Otra"
  )) |> 
  
  # Agrupar por causa
  count(region, prov_id, prov_nombre, anio_def, mes_def,
        sexo, grupo_etario, causa_def,
        wt = cantidad, name = "n_def")


### Explorar datos
tabyl(defun$region)

tabyl(defun$grupo_etario)

tabyl(defun$sexo)

tabyl(defun$causa_def)



# Limpieza datos COVID-19 -------------------------------------------------
datos_covid <- covid_raw |> 
  
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
  
  # Modificar etiquetas sexo
  mutate(sexo = fct_relabel(sexo, ~ c("femenino", "masculino"))) |> 
  
  # Modificar formato fechas 
  mutate(across(.cols = starts_with("fecha"),
                .fns = ~ ymd(.x))) |> 
  
  # Crear variable para grupo etario
  mutate(grupo_edad = case_when(
    between(edad, 20, 39) ~ "20-39 años",
    between(edad, 40, 49) ~ "40-49 años",
    between(edad, 50, 59) ~ "50-59 años",
    between(edad, 60, 69) ~ "60-69 años",
    between(edad, 70, 79) ~ "70-79 años",
    .default = "80+ años"
  )) |> 
  
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
  

### Crear dataset de fallecidos COVID-19
defun_covid <- datos_covid |> 
  # Filtrar no fallecidos
  filter(fallecido == "SI") |> 
  
  # Crear variables para año y mes de defunción
  mutate(anio_def = year(fecha_fallecimiento),
         mes_def = month(fecha_fallecimiento)) |> 
  
  # Agrupar fallecimientos por COVID-19
  count(region, prov_id, prov_nombre, anio_def, mes_def,
        grupo_edad, sexo, name = "n_def_covid") |> 
  
  # Completar filas faltantes
  complete(nesting(region, prov_id, prov_nombre),
           anio_def, mes_def,
           grupo_edad, sexo,
           fill = list(n_def_covid = 0))



# Guardar datos limpios ---------------------------------------------------
## Defunciones 2015-2022
export(defun, file = "clean/defun_2015_2022.csv")

## Defunciones COVID-19
export(defun_covid, file = "clean/arg_defun_covid_2020_2022.csv")


## Limpiar environment
rm(list = ls())
