### Exceso de mortalidad por ENTs en Argentina durante la pandemia de COVID-19
### Limpieza de los datasets: Defunciones Generales Mensuales ocurridas y
### registradas en la República Argentina (2015-2022)
### Disponibles en:
### https://datos.gob.ar/dataset/salud-defunciones-generales-mensuales-ocurridas-registradas-republica-argentina
### Autora: Tamara Ricardo
### Fecha modificación:


# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Códigos GBD-CIE10
gbd_cie10_raw <- import("raw/IHME_GBD_2021_COD_CAUSE_ICD_CODE_MAP_Y2024M05D16.XLSX", skip = 1) 


## Defunciones mensuales (DEIS-MSAL)
defun_raw <- import("raw/arg_def_m_15_22.csv") 


## Explorar datos crudos
glimpse(defun_raw)

tabyl(defun_raw$region)

tabyl(defun_raw$jurisdiccion)

tabyl(defun_raw$sexo_nombre)

tabyl(defun_raw$grupo_etario)

tabyl(defun_raw$grupo_causa_defuncion_CIE10)


# Limpiar códigos CIE10 --------------------------------------------------
gbd_cie10 <- gbd_cie10_raw |>
  # Descartar clasificación CIE9
  select(-ICD9) |> 
  
  # Seleccionar códigos basura
  filter(str_detect(Cause, "Garbage")) |> 
  
  # Quitar 4to dígito códigos
  mutate(ICD10 = str_replace_all(ICD10, "\\.[0-9]", "")) |> 
  
  # Reemplazar guión por coma
  mutate(ICD10 = str_replace_all(ICD10, "\\-", "\\, ")) |> 
  
  # Crear una fila por valor
  separate_rows(ICD10, sep = ",") |> 
  
  # Quitar espacios en blanco
  mutate(ICD10 = str_trim(ICD10, side = "both")) |> 
  
  # Quitar duplicados
  distinct()


# Limpiar dataset defunciones --------------------------------------------
defun <- defun_raw |>
  # Estandarizar nombres de columnas
  rename(
    sexo = sexo_nombre,
    cie10_grupo = grupo_causa_defuncion_CIE10,
    cie10_cod = cod_causa_muerte_CIE10
  ) |>

  # Filtrar datos ausentes región
  filter(region != "10.sin especificar.") |>
  # Filtrar datos ausentes sexo
  filter(sexo %in% c("femenino", "masculino")) |>

  # Filtrar menores de 20 años y datos ausentes grupo etario
  filter(between(
    grupo_etario,
    "02.de 20 a 39 anios",
    "07. de 80 anios y mas"
  )) |>

  # Modificar etiquetas región
  mutate(
    region = fct_relabel(
      region,
      ~ c(
        "Centro",
        "NEA",
        "NOA1",
        "NOA",
        "NOA2",
        "Cuyo1",
        "Cuyo2",
        "Patagonia Norte",
        "Patagonia Sur"
      ))) |>

  # Modificar etiquetas grupo etario
  mutate(
    grupo_etario = fct_relabel(
      grupo_etario,
      ~ c(
        "20-39 años",
        "40-49 años",
        "50-59 años",
        "60-69 años",
        "70-79 años",
        "80+ años"
      ))) |>

  # Crear variable para id numérico de provincia
  mutate(prov_id = str_extract(jurisdiccion, "\\d+")) |>

  # Crear variable para etiqueta de provincia
  mutate(
    prov_nombre = case_when(
      prov_id == "14" ~ "Córdoba",
      prov_id == "30" ~ "Entre Ríos",
      prov_id == "6" ~ "Buenos Aires",
      prov_id == "90" ~ "Tucumán",
      prov_id == "99" ~ NA,
      .default = str_remove_all(jurisdiccion, "[0-9]|\\.")
    )) |>

  # Crear columna para causa de muerte
  mutate(
    causa_def = case_when(
      # Diabetes mellitus (E10:E14)
      between(cie10_cod, "E10", "E14") ~ "Diabetes mellitus",
      # Respiratorias crónicas (J30:J99)
      between(cie10_cod, "J30", "J99") ~ "Enf. respiratoria crónica",
      # Cardiovasculares (I00:I99)
      between(cie10_cod, "I00", "I99") ~ "Enf. cardiovascular",
      # Neoplasias (C0:D48)
      between(cie10_cod, "C00", "D48") ~ "Neoplasias",
      .default = "Otra"
    )) |>

  # Quitar códigos basura
  mutate(
    causa_def_c = if_else(
      cie10_cod %in% gbd_cie10$ICD10,
      "Código basura",
      causa_def
    ))


### Explorar datos limpios
tabyl(defun, causa_def, causa_def_c)


# Guardar datos limpios ---------------------------------------------------
export(defun, file = "clean/defun_2015_2022.csv")
