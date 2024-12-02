####1: Cargar paquetes y limpiar el entorno

library(pacman) 
# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels, 
       stargazer,
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample,
       skimr,
       mice,
       brulee # redes neuronalescon multiples capas
       ) 
rm(list = ls())
######
#####2 Cargar datos
#####
setwd("D:/clases/Machine learning para economia aplicada/Taller 3")
train <- read.csv("train.csv")
test <- read.csv("test.csv")


Estaciones_policia <- st_read("CP.geojson.txt") %>%
  st_transform(3116)

ts <- read.csv("Estaciones_Troncales_de_TRANSMILENIO.csv") %>%
  st_as_sf(coords = c("X", "Y"), crs = 3116)

restaurantes <- st_read("egba.GPKG") %>%
  st_transform(3116)

EC <- st_read("EC.geojson.txt")%>%
  st_transform(3116)

# Leer la base de datos de ciclorutas
ciclorutas <- st_read("ciclorruta.geojson") %>%
  st_transform(3116) # Transformar al CRS proyectado para Bogotá

###### Funcion de procesado

procesar_datos <- function(db) {
  
  
  # Seleccionar las columnas con datos faltantes
  vars_to_impute <- db[, c("surface_total", "surface_covered", "rooms", "bathrooms")]
  vars_to_impute <- vars_to_impute[, colSums(is.na(vars_to_impute)) < nrow(vars_to_impute)]
  
  # Aplicar mice
  imputed_data <- mice(vars_to_impute, m = 5, method = 'pmm', maxit = 10, seed = 123)
  # Reemplazar los valores imputados en el dataset original
  db[, c("surface_total", "surface_covered", "rooms", "bathrooms")] <- complete(imputed_data)
  
  # 3. Convertir a objeto espacial (sf)
  db_sf <- db %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) # WGS84
  
  # 4. Filtro geográfico: Obtener límites de Bogotá
  limites <- getbb("Bogota Colombia", format_out = "matrix")
  
  if (is.null(limites) || any(is.na(limites))) {
    stop("No se pudieron obtener los límites de Bogotá. Verifica la conexión a OSM.")
  }
  
  limites_bbox <- st_as_sfc(st_bbox(c(
    xmin = limites[1, "min"],
    xmax = limites[1, "max"],
    ymin = limites[2, "min"],
    ymax = limites[2, "max"]
  ), crs = 4326)) %>%
    st_transform(3116) # Transformar a CRS proyectado
  
  # Filtrar las propiedades dentro de Bogotá
  db_sf <- db_sf %>%
    st_transform(3116) %>%
    filter(st_intersects(geometry, limites_bbox, sparse = FALSE)) # Filtrar propiedades dentro de Bogotá
  
  if (nrow(db_sf) == 0) {
    stop("El filtro espacial dejó sin observaciones. Revisa los límites o la proyección de las coordenadas.")
  }
  
  # 5. Inclusión de nuevas variables espaciales
  
  # Parques
  parques_sf <- opq(bbox = limites) %>%
    add_osm_feature(key = "leisure", value = "park") %>%
    osmdata_sf() %>%
    .$osm_polygons %>%
    st_transform(3116)
  
  db_sf <- db_sf %>%
    mutate(distancia_parque = st_distance(geometry, parques_sf) %>% apply(1, min) / 1000)
  
  
  # Estaciones de TransMilenio
  db_sf <- db_sf %>%
    mutate(distancia_transmi = st_distance(geometry, ts) %>% apply(1, min) / 1000)
  
  # Estaciones de Policía
  db_sf <- db_sf %>%
    mutate(distancia_policia = st_distance(geometry, Estaciones_policia) %>% apply(1, min) / 1000)
  
  # Restaurantes y bares
  db_sf <- db_sf %>%
    mutate(num_restaurantes = sapply(st_intersects(st_buffer(geometry, dist = 1500), restaurantes), length))
  
  # Visualización: Histograma de restaurantes cercanos
  ggplot(db_sf, aes(x = num_restaurantes)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Distribución de Restaurantes Cercanos", x = "Número de Restaurantes", y = "Frecuencia") +
    theme_minimal()
  
  # Establecimientos comerciales
  db_sf <- db_sf %>%
    mutate(distancia_EC = st_distance(geometry, EC) %>% apply(1, min) / 1000)
  
  # Calcular la distancia más cercana entre cada propiedad y las ciclorutas
  db_sf <- db_sf %>%
    mutate(distancia_cicloruta = st_distance(geometry, ciclorutas) %>%
             apply(1, min) / 1000) # Convertir la distancia a kilómetros
  
  # 6. Regresar a DataFrame
  db <- db_sf
  
  
  db <- db %>%
    mutate(
      distancia_parque_2 = distancia_parque^2,
      distancia_transmi_2 = distancia_transmi^2,
      distancia_policia_2 = distancia_policia^2,
      distancia_EC_2 = distancia_EC^2,
      distancia_cicloruta_2 = distancia_cicloruta^2
    )
  
  
  ###Variables de texto  
  
  #minusculas
  db <- db %>% mutate(description = str_to_lower(description))
  # Eliminamos tildes
  db <- db %>% mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
  # Eliminamos caracteres especiales
  db <- db %>% mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
  # Eliminamos espacios extras
  db <- db %>% mutate(description = str_trim(gsub("\\s+", " ", description)))
  
  # Se crea una nueva columna llamada property_type_2. Si "casa" está en la descripción, se asigna "Casa" a esta columna; de lo contrario, se mantiene el valor original de property_type
  db <- db %>%
    mutate(property_type_2 = ifelse(grepl("casa", description), "Casa", property_type))
  
  # Se repite el caso anterior pero ahora buscamos apartamento o apto.
  db <- db %>%
    mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "Apartamento", property_type_2))%>%
    select(-property_type) 
  
  db <- db %>%
    mutate(n_pisos= str_extract(description, "(\\w+|\\d+) pisos")) %>%
    mutate(n_pisos= ifelse(property_type_2=="Casa", n_pisos, NA)) 
  numeros_escritos <- c( "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
  numeros_numericos <- as.character(2:10)
  
  
  db <- db %>%
    mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))
  
  db <- db %>%
    mutate(n_pisos_numerico = as.integer(str_extract(n_pisos, "\\d+")))  %>%
    mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
    mutate(n_pisos_numerico = if_else(n_pisos_numerico>10, 1, n_pisos_numerico)) ### quedarnos casas de hasta 10 pisos. 
  
  db <- db %>%
    mutate(piso_info= str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))
  
  
  numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
  numeros_numericos <- as.character(1:10)
  
  db <- db %>%
    mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos)))
  
  db <- db %>%
    mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))
  
  db <- db %>%
    mutate(piso_numerico = ifelse(piso_numerico > 20, NA, piso_numerico)) %>%
    mutate(piso_numerico = ifelse(property_type_2=="Casa", 1, piso_numerico))
  db %>%
    filter(property_type_2 == "Apartamento") %>%
    count(piso_numerico)
  db <- db %>%
    mutate(piso_numerico = replace_na(piso_numerico, 1))
  
  db <- db %>%
    mutate(piscina = ifelse(grepl("\\bpiscina\\b", description, ignore.case = TRUE), 1, 0))%>%
    mutate(terraza = ifelse(grepl("\\bterraza\\b", description, ignore.case = TRUE), 1, 0))%>%
    mutate(gimnasio = ifelse(grepl("\\bgimnasio\\b", description, ignore.case = TRUE), 1, 0))
  
  # Verificar el resultado
  head(db)
  

  ### Retornar el dataframe procesado
  return(db)
}

train <- procesar_datos(train)
test <- procesar_datos(test)

# Limpiar el ambiente y preparar las variables necesarias
rm(list = setdiff(ls(), c("test", "train", "train_cop")))
 
##########
#######Modelos
##########

#####Elastic net

elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

grid_values <- grid_regular(penalty(range = c(-2,1)), levels = 50) %>%
  expand_grid(mixture = c(0, 0.25,  0.5, 0.75,  1))

set.seed(86936)
db_fold <- vfold_cv(train, v = 10)


rec_1 <- recipe(price ~ distancia_parque + distancia_transmi + 
                  distancia_policia + distancia_EC + distancia_cicloruta + 
                  surface_total + rooms + bathrooms + property_type_2 + piscina + terraza+bedrooms
                  +terraza+ gimnasio, 
                data = train) %>% 
  step_interact(terms = ~ distancia_parque:property_type_2 + distancia_EC:property_type_2) %>% 
  step_poly(distancia_parque, distancia_EC, distancia_cicloruta, degree = 2) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


workflow_1 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(elastic_net_spec)


set.seed(86936)

tune_res1 <- tune_grid(
  workflow_1,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = db_fold,  # Folds de validación cruzada
  grid = grid_values,        # Grilla de valores de penalización
  metrics = metric_set(rmse)  # métrica
)


collect_metrics(tune_res1)

best_penalty_1 <- select_best(tune_res1, metric = "rmse")
EN_final1 <- finalize_workflow(workflow_1, best_penalty_1)
EN_final1_fit <- fit(EN_final1, data = train)

#Sacamos las predicciones sobre los datos de test 
predictiones_1 <- predict(EN_final1_fit , new_data = test)

write.csv(predictiones_1,"EN_test1", row.names = FALSE)



# Crear predicciones
predicciones_1 <- predict(EN_final1_fit, new_data = test)

# Convertir a un data.frame estándar para manejar las columnas fácilmente
test_clean <- test %>%
  as.data.frame() %>%   # Quitar atributos espaciales (si existen)
  select(property_id)   # Seleccionar solo property_id

# Agregar las predicciones
test_predictions <- test_clean %>%
  mutate(price = predicciones_1$.pred)

# Verificar el resultado
head(test_predictions)
test_predictions <- test_predictions %>%
  mutate(price = as.integer(price))

# Guardar como archivo CSV (opcional)
write.csv(test_predictions, "EN_TEST1.csv", row.names = FALSE)

nrow(test)/nrow(train)

skim(test_predictions)

#####
#random forest
######
library(tidymodels)
library(spatialsample)
library(sf)

# Limpiar el ambiente y preparar las variables necesarias
rm(list = setdiff(ls(), c("test", "train", "train_cop")))

# Definir la fórmula del modelo
formula <- as.formula(
  "price ~ distancia_parque + distancia_transmi + 
   distancia_policia + distancia_EC + distancia_cicloruta + 
   surface_total + rooms + bathrooms + property_type_2 + piscina + 
   num_restaurantes + terraza + bedrooms + gimnasio+ piso_numerico"
)

# Crear la receta para preprocesamiento
recipe_rf <- recipe(formula, data = train) %>%
  step_novel(all_nominal_predictors()) %>%   # Manejo de clases no vistas
  step_dummy(all_nominal_predictors()) %>%  # Variables categóricas a dummies
  step_zv(all_predictors())                 # Eliminar predictores constantes

# Especificar el modelo Random Forest
rf_tune <- rand_forest(
  mtry = tune(),       # Número de predictores a considerar en cada división
  min_n = tune(),      # Mínima cantidad de observaciones en nodos terminales
  trees = 500          # Número de árboles
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

# Crear los folds espaciales
train_sf <- st_as_sf(
  train,
  coords = c("lon", "lat"),  # Ajustar nombres si tus coordenadas tienen otros nombres
  crs = 4326
)

set.seed(86936)
block_folds <- spatial_block_cv(train_sf, v = 10)

# Definir grilla de hiperparámetros
grid_values_rf <- grid_regular(
  mtry(range = c(3, 10)),      # Rango de predictores seleccionados en cada división
  min_n(range = c(5, 20)),     # Rango de tamaño mínimo de nodos
  levels = 5                   # Niveles en cada rango
)

# Crear el workflow
workflow_rf <- workflow() %>%
  add_recipe(recipe_rf) %>%
  add_model(rf_tune)

# Entrenar el modelo con validación cruzada espacial
set.seed(86936)
tune_rf <- tune_grid(
  workflow_rf,
  resamples = block_folds,
  grid = grid_values_rf,
  metrics = metric_set(mae)  # Métrica de evaluación
)

# Seleccionar los mejores hiperparámetros
best_rf <- select_best(tune_rf, metric = "mae")

# Finalizar el modelo con los mejores hiperparámetros
rf_final <- finalize_workflow(workflow_rf, best_rf)



# Ajustar el modelo final con los datos completos de entrenamiento
rf_final_fit <- fit(rf_final, data = train)

# Mostrar resultados del modelo ajustado
rf_final_fit

#
# Generar predicciones en el conjunto de test
predicciones <- predict(rf_final_fit, new_data = test)

# Ver las primeras filas de las predicciones
head(predicciones)

# Guardar las predicciones en un archivo CSV llamado "RD1"
write.csv(predicted_price, "RD1.csv", row.names = FALSE)




#Neural network
#######


recipe_nnet <- recipe( formula  , data = train) %>%
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 


train_sf <- st_as_sf(
  train, 
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)

set.seed(86936)
block_folds <- spatial_block_cv(train_sf, v = 10)

nnet_tune <- 
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    dropout =tune(),
    epochs = tune(),
    learn_rate =tune(),
    activation = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("brulee") #trace 0 previene la verbosidad del entrenamiento


grid_values <- crossing( #`crossing` nos permite generar una grilla rectangular con la combinación de todos los hiperparámetros. 
  hidden_units = list(c(5,10), c(10,20), c(20,10) ),
  penalty = 10^seq(from=-3,to=-2, by=0.5 ),
  dropout =0,
  epochs = 50,
  learn_rate =0.1,
  activation = list(c("relu", "relu"))
)


workflow_tune <- workflow() %>% 
  add_recipe(recipe_nnet) %>%
  add_model(nnet_tune) 

set.seed(86936)

tune_nnet <- tune_grid(
  workflow_tune,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  grid = grid_values,        # Grilla de valores 
  metrics = metric_set(mae)  # métrica
)

tune_nnet

best_tune_nnet <- select_best(tune_nnet, metric = "mae")
best_tune_nnet$hidden_units


recipe_nnet <- recipe( formula  , data = train) %>%
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

set.seed(394012)
nn_final<-brulee_mlp(recipe_nnet, 
                     train,
                     hidden_units = c(10,20),
                     penalty = best_tune_nnet$penalty[1],
                     dropout = best_tune_nnet$dropout[1],
                     epochs = best_tune_nnet$epochs[1],
                     learn_rate =best_tune_nnet$learn_rate[1],
                     activation = c("relu", "relu"),
                     validation= 0, 
) 


prediction<- predict(nn_final, new_data = test)
nn_final


# Crear el data frame con las predicciones y el 'property_id' del conjunto de test
predicted_price <- test %>%
  select(property_id) %>%
  bind_cols(prediction %>% select(.pred)) %>%
  rename(price = .pred)

# Eliminar la columna geometry después de realizar las predicciones
predicted_price <- test %>%
  select(property_id) %>%
  bind_cols(prediction %>% select(.pred)) %>%
  rename(price = .pred) %>%
  st_drop_geometry()  # Elimina la geometría después de las predicciones

# Verificar el data frame con las predicciones
head(predicted_price)
write.csv(predicted_price, "predicciones_ANN2.csv", row.names = FALSE)

#########

##Analisis de resultados
rm(list = setdiff(ls(), c("train", "test","nn_final")))

ANN<- read.csv("predicciones_ANN2.csv")
Elastic_net <- read.csv("predicciones_EN_0.01_1.csv")
RD <- read.csv("RD1.csv")


# Asegúrate de que todas las tablas tienen la variable property_id
# Combinar los datos en un solo dataframe
df_combined <- rbind(
  data.frame(Model = "Random forest", property_id = RD$property_id, Price = RD$price),
  data.frame(Model = "Elastic net", property_id = Elastic_net$property_id, Price = Elastic_net$price),
  data.frame(Model = "Neural network", property_id = ANN$property_id, Price = ANN$price)
)

# Convertir precios a millones de pesos
df_combined$Price <- df_combined$Price / 1e6

# Graficar las densidades
library(ggplot2)
ggplot(df_combined, aes(x = Price, color = Model, fill = Model)) +
  geom_density(alpha = 0.3, kernel = "gaussian") +  # Graficar densidades con transparencia
  labs(
    title = "Distribución de Densidad de Precios Predichos",
    x = "Precio Predicho (Millones de COP)",
    y = "Densidad",
    color = "Modelo",
    fill = "Modelo"
  ) +
  theme_minimal() +
  scale_x_continuous(
    labels = scales::comma_format(scale = 1, suffix = "M")
  ) +
  coord_cartesian(xlim = c(0, 1500))  # Enfocarse en el rango 0-1000 millones



# 1. Crear un data frame combinando las predicciones de los tres modelos
predictions <- data.frame(
  property_id = ANN$property_id,  # Identificador de la propiedad
  price_ann = ANN$price/1000000,      # Precio predicho por el modelo ANN
  price_elastic_net = Elastic_net$price/1000000,  # Precio predicho por el modelo Elastic Net
  price_rd = RD$price/1000000         # Precio predicho por el modelo RD
)

stargazer(predictions,type = "text")



