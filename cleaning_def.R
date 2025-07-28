### Exceso de mortalidad por ENT en Argentina durante la pandemia de COVID-19
### Limpieza del dataset de Defunciones Generales Mensuales ocurridas y
### registradas en la República Argentina (2015-2022), disponible en:
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
## Defunciones mensuales 2015-22 (MSAL)
defun_raw <-import("raw/arg_def_m_15_22.csv")


## Explorar datos crudos
glimpse(defun_raw)

tabyl(defun_raw$region)

tabyl(defun_raw$jurisdiccion)

tabyl(defun_raw$grupo_etario)

tabyl(defun_raw$grupo_causa_defuncion_CIE10)


# Limpieza de datos -------------------------------------------------------
## Defunciones mensuales 2015-2022 (MSAL)
defun <- defun_raw |> 
  # Estandarizar nombres de columnas
  rename(sexo = sexo_nombre,
         prov_id = jurisdiccion,
         cie10_cat = grupo_causa_defuncion_CIE10,
         cie10_cod = cod_causa_muerte_CIE10) |> 
  
  # Filtrar datos faltantes región
  filter(region != "10.sin especificar.") |> 
  
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
  
   # Completar nombres faltantes provincia
  mutate(prov_nombre = case_when(
    region == "NOA1" ~ "Jujuy, Salta",
    region == "NOA2" ~ "Catamarca, Santiago del Estero",
    region == "Cuyo2" ~ "San Juan, San Luis, La Rioja",
    region == "Patagonia Norte" ~ "La Pampa, Neuquén, Río Negro",
    region == "Patagonia Sur" ~ "Chubut, Santa Cruz, Tierra del Fuego",
    .default = prov_nombre
  )) |> 
  
  # Modificar etiquetas grupo etario
  mutate(grupo_etario = fct_relabel(
    grupo_etario, ~ c("20-39 años", "40-49 años", "50-59 años",
                      "60-69 años", "70-79 años", "80+ años")
  )) |> 
  
  # Crear columna para causas de muerte
  mutate(causa_def = case_when(
    # Cáncer: I00-I99
    cie10_cat == "0200 T MALIGNOS" ~ "Cáncer",
    # Cardiovascular: I00–I99
    cie10_cat == "0900 ENF  DEL SISTEMA CIRCULATORIO" ~ "Enfermedad cardiovascular",
    # Diabetes: E10–E14
    cie10_cat == "0300 DIABETES MELLITUS" ~ "Diabetes mellitus",
    # Respiratorias crónicas: J30-J98
    between(cie10_cod, "J30", "J98") ~ "Enfermedad respiratoria crónica",
    .default = "Otra"
  )) |> 
  
  # Agrupar datos
  count(anio_def, mes_def, region, prov_id, prov_nombre, sexo, grupo_etario, causa_def,
        wt = cantidad)


# Guardar datos limpios ---------------------------------------------------
export(defun, file = "clean/defun_2015_2022.csv")


## Limpiar environment
rm(list = ls())
