### Exceso de mortalidad por ENTs en Argentina durante la pandemia de COVID-19
### Limpieza del dataset: Casos de COVID-19 registrados en la República Argentina (2020-2022)
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
# Casos COVID-19 2020-2022 (SNVS-MSAL)
covid_raw <- import("raw/Covid19Casos.csv") |>
  # Filtrar casos no confirmados
  filter(clasificacion_resumen == "Confirmado")


# Explorar datos crudos ---------------------------------------------------
glimpse(covid_raw)

summary(covid_raw$edad)

tabyl(covid_raw$sexo)

tabyl(covid_raw$edad_años_meses)

tabyl(covid_raw$residencia_pais_nombre)

tabyl(covid_raw$residencia_provincia_nombre)

tabyl(covid_raw$fallecido)


# Limpieza datos COVID-19 -------------------------------------------------
datos_covid <- covid_raw |>

  # Estandarizar nombres de columnas
  rename(
    prov_id = residencia_provincia_id,
    prov_nombre = residencia_provincia_nombre
  ) |>

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
  mutate(across(.cols = starts_with("fecha"), .fns = ~ ymd(.x))) |>

  # Crear variable para grupo etario
  mutate(
    grupo_edad = case_when(
      between(edad, 20, 39) ~ "20-39 años",
      between(edad, 40, 49) ~ "40-49 años",
      between(edad, 50, 59) ~ "50-59 años",
      between(edad, 60, 69) ~ "60-69 años",
      between(edad, 70, 79) ~ "70-79 años",
      .default = "80+ años"
    )
  ) |>

  # Crear variable para región
  mutate(
    region = case_when(
      prov_nombre %in% c("Corrientes", "Chaco", "Formosa", "Misiones") ~ "NEA",
      prov_nombre %in% c("Jujuy", "Salta") ~ "NOA1",
      prov_nombre == "Tucumán" ~ "NOA",
      prov_nombre %in% c("Catamarca", "Santiago del Estero") ~ "NOA2",
      prov_nombre == "Mendoza" ~ "Cuyo1",
      prov_nombre %in% c("La Rioja", "San Juan", "San Luis") ~ "Cuyo2",
      prov_nombre %in% c("La Pampa", "Neuquén", "Río Negro") ~
        "Patagonia Norte",
      prov_nombre %in% c("Chubut", "Santa Cruz", "Tierra del Fuego") ~
        "Patagonia Sur",
      .default = "Centro"
    ),
    .after = prov_nombre
  )


### Crear dataset de fallecidos COVID-19
defun_covid <- datos_covid |>
  # Filtrar no fallecidos
  filter(fallecido == "SI") |>

  # Crear variables para año y mes de defunción
  mutate(
    anio_def = year(fecha_fallecimiento),
    mes_def = month(fecha_fallecimiento)
  ) |>

  # Agrupar fallecimientos por COVID-19
  count(
    region,
    prov_id,
    prov_nombre,
    anio_def,
    mes_def,
    grupo_edad,
    sexo,
    name = "n_def_covid"
  ) |>

  # Completar filas faltantes
  complete(
    nesting(region, prov_id, prov_nombre),
    anio_def,
    mes_def,
    grupo_edad,
    sexo,
    fill = list(n_def_covid = 0)
  )


# Guardar datos limpios ---------------------------------------------------
 export(defun_covid, file = "clean/arg_defun_covid_2020_2022.csv")


## Limpiar environment
rm(list = ls())