### Exceso de mortalidad por ENT en Argentina durante la pandemia de COVID-19
### Limpieza de los datasets de proyecciones poblacionales 2010-2040 y población
### según censos nacionales 2010 y 2022
### Autora: Tamara Ricardo
### Última modificación:



# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse,
  readxl
)


# Cargar datos ------------------------------------------------------------
## Población Censo 2010 (INDEC)
pob_10_raw <- import("raw/_tmp_5850861.xlsX", 
                     range = "B12:D562")


## Población Censo 2022 (INDEC)
pob_22_raw <- import("raw/_tmp_5850841.xlsX", 
                     range = "B12:D609")


## Proyecciones 2010-2021
# Ruta del archivo de Excel
indec_10 <- "raw/c2_proyecciones_prov_2010_2040.xls" 

# Cargar/unir hojas
proy_10_21_raw <- excel_sheets(indec_10)[-c(1:2)] |>  # Listar hojas por provincia
  # Crear columna para la provincia
  set_names() |> 
  
  # Leer filas para 2010-2015 y unir por provincia
  map(~ read_excel(indec_10, sheet = .x, range = "A3:X28")) |> 
  list_rbind(names_to = "prov") |> 
  
  # Unir filas para 2016-2021
  bind_cols(
    excel_sheets(indec_10)[-c(1:2)] |>  # Listar hojas por provincia
      # Crear columna para la provincia
      set_names() |> 
      
      # Leer filas para 2016-2021 y unir por provincia
      map(~ read_excel(indec_10, sheet = .x, range = "A31:X56")) |> 
      list_rbind(names_to = "prov")
  )



# Limpiar datos -----------------------------------------------------------
## Población Censo 2010
pob_10 <- pob_10_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  rename(masculino = x2,
         femenino = x3) |> 
  
  # Crear id de provincia
  mutate(prov_id = if_else(str_detect(x1, "AREA"),
                           str_sub(x1, start = 7), NA)) |> 
  
  # Completar filas
  fill(prov_id, .direction = "down") |> 
  
  # Filtrar menores de 20 años y totales
  filter(between(x1, "20-24", "95 y más")) |> 
  
  # Crear variable para grupo etario
  mutate(grupo_edad = case_when(
    between(x1, "20-24", "35-39") ~ "20-39 años",
    x1 %in% c("40-44", "45-49") ~ "40-49 años",
    x1 %in% c("50-54", "55-59") ~ "50-59 años",
    x1 %in% c("60-64", "65-69") ~ "60-69 años",
    x1 %in% c("70-74", "75-79") ~ "70-79 años",
    .default = "80+ años"
    )) |> 
  
  # Base long por sexo
  pivot_longer(cols = c(masculino, femenino),
               names_to = "sexo") |> 
  
  # Población e id provincia a numérico
  mutate(across(.cols = c(prov_id, value), 
                .fns =  ~ parse_number(.x))) |> 
  
  # Contar población por provincia, sexo y grupo etario
  count(prov_id, grupo_edad, sexo,
        wt = value, name = "pob_est_2010")


## Población Censo 2022
pob_22 <- pob_22_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  rename(femenino = x2,
         masculino = x3) |> 
  
  ## Crear id de provincia
  mutate(prov_id = if_else(str_detect(x1, "AREA"),
                           str_sub(x1, start = 7), NA)) |> 
  
  # Completar filas
  fill(prov_id, .direction = "down") |> 
  
  # Filtrar menores de 20 años y totales
  filter(!str_detect(x1, "15 a 19|AREA|Edad|Total") & !is.na(x1)) |> 
  
  # Crear variable para grupo etario
  mutate(grupo_edad = case_when(
    between(x1, "20 a 24", "35 a 39") ~ "20-39 años",
    x1 %in% c("40 a 44", "45 a 49") ~ "40-49 años",
    x1 %in% c("50 a 54", "55 a 59") ~ "50-59 años",
    x1 %in% c("60 a 64", "65 a 69") ~ "60-69 años",
    x1 %in% c("70 a 74", "75 a 79") ~ "70-79 años",
    .default = "80+ años"
  )) |> 
  
  # Base long por sexo
  pivot_longer(cols = c(masculino, femenino),
               names_to = "sexo") |> 
  
  # Población e id provincia a numérico
  mutate(across(.cols = c(prov_id, value), 
                .fns =  ~ parse_number(.x))) |> 
  
  # Contar población por provincia, sexo y grupo etario
  count(prov_id, grupo_edad, sexo,
        wt = value, name = "pob_est_2022")
  

## Proyecciones 2010-2021
proy_10_21 <- proy_10_21_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  
  # Seleccionar columnas relevantes
  select(prov_id = prov_1,
         grupo_edad_5 = edad_2,
         masculino_2010 = x4,
         femenino_2010 = x5,
         masculino_2011 = x8,
         femenino_2011 = x9,
         masculino_2012 = x12,
         femenino_2012 = x13,
         masculino_2013 = x16,
         femenino_2013 = x17,
         masculino_2014 = x20,
         femenino_2014 = x21,
         masculino_2015 = x24,
         femenino_2015 = x25,
         masculino_2016 = x29,
         femenino_2016 = x30,
         masculino_2017 = x33,
         femenino_2017 = x34,
         masculino_2018 = x37,
         femenino_2018 = x38,
         masculino_2019 = x41,
         femenino_2019 = x42,
         masculino_2020 = x45,
         femenino_2020 = x46,
         masculino_2021 = x49,
         femenino_2021 = x50,
         ) |> 
  
  # Filtrar filas con valores ausentes
  drop_na() |> 
  
  # Filtrar <20 años y totales
  filter(!grupo_edad_5 %in% c("Total", "0-4", "5-9", "10-14", "15-19")) |> 
  
  # Limpiar id numérico de provincia
  mutate(prov_id = str_sub(prov_id, 1, 2) |> 
           parse_number()) |> 
  
  # Formato long
  pivot_longer(cols = c(masculino_2010:femenino_2021)) |> 
  
  # Crear columnas para año y sexo
  separate(name, into = c("sexo", "anio"), sep = "_") |> 
  
  # Crear variable para grupo etario
  mutate(grupo_edad = case_when(
    between(grupo_edad_5, "20-24", "35-39") ~ "20-39 años",
    grupo_edad_5 %in% c("40-44", "45-49") ~ "40-49 años",
    grupo_edad_5 %in% c("50-54", "55-59") ~ "50-59 años",
    grupo_edad_5 %in% c("60-64", "65-69") ~ "60-69 años",
    grupo_edad_5 %in% c("70-74", "75-79") ~ "70-79 años",
    .default = "80+ años"
  )) |> 
  
  # Convertir año y proyección a numérico
  mutate(across(.cols = c(anio, value),
                .fns = ~ parse_number(.x))) |> 
  
  # Agrupar población por año, provincia, sexo y grupo etario
  count(anio, prov_id, grupo_edad, sexo,
        wt = value, name = "proy_pob")



# Unir datasets -----------------------------------------------------------
proy_pob_join <- proy_10_21 |> 
  left_join(pob_10) |> 
  left_join(pob_22)


## Exportar datos limpios
export(proy_pob_join, file = "clean/proy_pob_2010_21.csv")
