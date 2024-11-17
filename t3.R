#Taller 3 Big Data

# librerias ---------------------------------------------------------------

library(pacman) 

# Cargar o Instalar librerias

p_load(tidyverse, # Manipular bases de datos
       rio, # Importar datos fácilmente
       sf, # Leer/escribir/manipular datos espaciales
       tidymodels, # entrenamiento de modelos
       rattle, # Interfaz gráfica para el modelado de datos
       tmaptools, # geocode_OSM()
       osmdata, # Get OSM's data 
       ggplot2,#Realizar graficos
       leaflet, # Mapas interactivos
       stargazer, # Estadisticas descriptivas
       spatialsample) # Muestreo espacial para modelos de aprendizaje automático

#Establecer directorio de trabajo 

setwd("C:/Users/Marto/Documents/big data/t3")


# Cragar las bases  -------------------------------------------------------

train<-read.csv("C:/Users/Marto/Documents/big data/t3/train.csv")
test<-read.csv("C:/Users/Marto/Documents/big data/t3/test.csv")

dim(train)
# tenemos cerca de 39 mil inmuebles y 16 variables para train
dim(test)
# Tenemos cerca de 10 mil inmuebles y 16 variables para test 
table(train$operation_type)
table(test$operation_type)
#Nos asegurramos que las bases solo contiene operaciones de venta 


# Limpiezar datos  --------------------------------------------------------

ptrain <- train  %>%
  count(property_type)
ptrain
ptest<-test %>% 
  count(property_type)
ptest

# De  lo anterior se puede evidenciar que solo tenemos 2 tipos de propiedades cas y apartamento 

train %>%
  mutate(title=na_if(title,"")) %>%
  mutate(description=na_if(description,""))


test %>%
  mutate(title=na_if(title,"")) %>%
  mutate(description=na_if(description,""))

#distribucion de numero de baños, habitaciones, dromitorios

tema_personalizado<-theme(legend.position="left",
                          panel.background =element_rect(fill = "white"),
                          panel.grid.major = element_line(color = "gray"),
                          axis.text.x = element_text(colour = "black"),
                          axis.text.y = element_text(colour = "black"))

# Habitaciones ------------------------------------------------------------

grafico1<-ggplot(train, aes(x=rooms)) + 
  geom_bar( col="#607B8B", fill="#607B8B")+
  tema_personalizado+
  ggtitle("Distribución número de habitaciones", subtitle = "Train")+
  labs(y="Fecuencia", x="Número de habitaciones")
grafico1
ggsave(filename = "Grafico_hab_train.png", plot = grafico1, device = "png", width = 10, height = 10, units = "in", limitsize = TRUE)

mediana_rooms<- median(train$rooms, na.rm = TRUE)
mediana_rooms

#De aqui podemos  evidenciar que la moda  y la mediana es de 3 habitaciones, razon por la cual vamos a imputar con 3 los missing

grafico2<-ggplot(test, aes(x=rooms)) + 
  geom_bar( col="#607B8B", fill="#607B8B")+
  tema_personalizado+
  ggtitle("Distribución número de habitaciones", subtitle = "Test")+
  labs(y="Fecuencia", x="Número de habitaciones")
grafico2
ggsave(filename = "Grafico_hab_test.png", plot = grafico2, device = "png", width = 10, height = 10, units = "in", limitsize = TRUE)

mediana_rooms<- median(test$rooms, na.rm = TRUE)
mediana_rooms

#Sin embargo, podemos evidenciar que en test lla moda tambien es 3 habitaciones sin embargo, podria considerarse imputar 2 habitaciones qu es la mediana  


# Baños -------------------------------------------------------------------


grafico3<-ggplot(train, aes(x=bathrooms)) + 
  geom_bar( col="#607B8B", fill="#607B8B")+
  tema_personalizado+
  ggtitle("Distribución número de baños", subtitle = "Train")+
  labs(y="Fecuencia", x="Número de baños")
grafico3
ggsave(filename = "Grafico_baños_train.png", plot = grafico3, device = "png", width = 10, height = 10, units = "in", limitsize = TRUE)

mediana_bathrooms<- median(train$bathrooms, na.rm = TRUE)
mediana_bathrooms

#De aqui podemos  evidenciar que la moda  es dos baños y la mediana es de 3 baños, razon por la cual vamos deberia considerarse las dos opciones

grafico4<-ggplot(test, aes(x=bathrooms)) + 
  geom_bar( col="#607B8B", fill="#607B8B")+
  tema_personalizado+
  ggtitle("Distribución número de baños", subtitle = "Test")+
  labs(y="Fecuencia", x="Número de baños")
grafico4
ggsave(filename = "Grafico_baños_test.png", plot = grafico4, device = "png", width = 10, height = 10, units = "in", limitsize = TRUE)

mediana_bathrooms<- median(test$bathrooms, na.rm = TRUE)
mediana_bathrooms

#Sin embargo, podemos evidenciar que en test la moda es 2 baños y la mediana 3 baños, razon por la cual vamos deberia considerarse las dos opciones para imputar


# Dormitorios  ------------------------------------------------------------


grafico5<-ggplot(train, aes(x=bedrooms)) + 
  geom_bar( col="#607B8B", fill="#607B8B")+
  tema_personalizado+
  ggtitle("Distribución número de dormitorios", subtitle = "Train")+
  labs(y="Fecuencia", x="Número de dormitorios")
grafico5
ggsave(filename = "Grafico_dormitorios_train.png", plot = grafico5, device = "png", width = 10, height = 10, units = "in", limitsize = TRUE)

mediana_bathrooms<- median(train$bedrooms, na.rm = TRUE)
mediana_bathrooms

#De aqui podemos  evidenciar que la moda  es 3 dormitoriosy la mediana es de 3 dormitorios.

grafico6<-ggplot(test, aes(x=bedrooms)) + 
  geom_bar( col="#607B8B", fill="#607B8B")+
  tema_personalizado+
  ggtitle("Distribución número de dormitorios", subtitle = "Test")+
  labs(y="Fecuencia", x="Número de dormitorios")
grafico6
ggsave(filename = "Grafico_dormitorios_test.png", plot = grafico6, device = "png", width = 10, height = 10, units = "in", limitsize = TRUE)

mediana_bathrooms<- median(test$bedrooms, na.rm = TRUE)
mediana_bathrooms

#Podemos evidenciar que en test la moda es 3 dormitorios y la mediana 2 dormitorios,razon por la cual vamos deberia considerarse las dos opciones para imputar



# Area --------------------------------------------------------------------

mediana_st_train<- median(train$surface_total, na.rm = TRUE)
mediana_st_train
mediana_st_test<- median(test$surface_total, na.rm = TRUE)
mediana_st_test

mean_st_train<- mean(train$surface_total, na.rm = TRUE)
mean_st_train
mean_st_test<- mean(test$surface_total, na.rm = TRUE)
mean_st_test

#Podemos evidenciar que existe una menor diferencia entre test y train utilizando la mediana 

mediana_sc_train<- median(train$surface_covered, na.rm = TRUE)
mediana_sc_train
mediana_sc_test<- median(test$surface_covered, na.rm = TRUE)
mediana_sc_test

mean_sc_train<- mean(train$surface_covered, na.rm = TRUE)
mean_sc_train
mean_sc_test<- mean(test$surface_covered, na.rm = TRUE)
mean_sc_test

#Podemos evidenciar que existe una menor diferencia entre test y train utilizando la media.
# Imputar missing values --------------------------------------------------

train <- train %>%
  mutate(rooms = replace_na(rooms, 3),
         bedrooms = replace_na(bedrooms, 3),
         bathrooms = replace_na(bathrooms, 3),
         surface_covered = replace_na(surface_covered, mediana_st_train),
         surface_total = replace_na(surface_total,floor(mean_sc_train)))

test <- test %>%
  mutate(rooms = replace_na(rooms, 3),
         bedrooms = replace_na(bedrooms, 3),
         bathrooms = replace_na(bathrooms, 3),
         surface_covered = replace_na(surface_covered, mediana_st_test),
         surface_total = replace_na(surface_total,floor(mean_sc_test)))


# Estadistica descriptiva -------------------------------------------------

stargazer(train,type="text")
stargazer(test,type="text")
# primera vizualización ---------------------------------------------------

# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)
train <- train %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A5D8F",
                           property_type == "Casa" ~ "#3FB260"))
# Encontramos el queremos que sea el centro del mapa 
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

# Creamos el plot
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addCircles(lng = train$lon, 
             lat = train$lat, 
             col = train$color,
             fillOpacity = 1,
             opacity = 1,
             popup = html)

# Usando ggplot -----------------------------------------------------------

bog <- st_read(dsn = 'C:/Users/Marto/Documents/big data/t3/Loca.shx')
#Asignar CRS
bog <- st_set_crs(bog, 4326)
localidades<-st_transform(bog,4626)
#grafico  de Bogota 
ggplot()+
  geom_sf(data=localidades, color = "black")
#transformamos los datos a geografico
sf_train <- st_as_sf(train, coords = c("lon", "lat"), crs=4626)
sf_test <- st_as_sf(train, coords = c("lon", "lat"), crs=4626)
#Realizamos un grafico por precio de mt2 para apartamentos
ggplot()+
  geom_sf(data=localidades, color = "black") + 
  geom_sf(data=sf_train,aes(color = precio_por_m2) ,shape=15, size=0.3)+
  theme_bw()

ggplot()+
  geom_sf(data=localidades, color = "black") + 
  geom_sf(data=sf_test,aes(color = precio_por_m2) ,shape=15, size=0.3)+
  theme_bw()

#Note que no tenemos  datos ni para sumapaz y para usme 

localidades_filtradas <- localidades[-c(9,14), ]
ggplot()+
  geom_sf(data=localidades_filtradas, color = "black")

ggplot()+
  geom_sf(data=localidades_filtradas, color = "black") + 
  geom_sf(data=sf_train,aes(color = precio_por_m2) ,shape=15, size=0.3)+
  theme_bw()

ggplot()+
  geom_sf(data=localidades_filtradas, color = "black") + 
  geom_sf(data=sf_test,aes(color = precio_por_m2) ,shape=15, size=0.3)+
  theme_bw()


# Datos espaciales --------------------------------------------------------

#Datos geoespaciales disponibles

available_tags("leisure")

parques <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park")

# Cambiamos el formato para que sea un objeto sf (simple features)
parques_sf <- osmdata_sf(parques)

# De las features del parque nos interesa su geometría y donde están ubicados 
parques_geometria <- parques_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) 

# Guardemos los polígonos de los parques 
parques_geometria <- st_as_sf(parques_sf$osm_polygons)

# Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
centroides <- st_centroid(parques_geometria, byid = T)

centroides <- centroides %>%
  mutate(x=st_coordinates(centroides)[, "X"]) %>%
  mutate(y=st_coordinates(centroides)[, "Y"]) 

# Visualizando en un mapa 

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parques_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centroides$x, 
             lat = centroides$y, 
             col = "darkblue", opacity = 0.5, radius = 1)


centroides_sf <- st_as_sf(centroides, coords = c("x", "y"), crs=4326)
sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)

# Distancia a los parques  ------------------------------------------------


dist_matrixtrain <- st_distance(x = sf_train, y = centroides_sf)
dim(dist_matrixtrain)

dist_matrixtest <- st_distance(x = sf_test, y = centroides_sf)
dim(dist_matrixtest)

# Calculamos la distancia minima a cada propiedad

dist_min_parquetrain <- apply(dist_matrixtrain, 1, min)  
train <- train %>%
  mutate(dis_parque=dist_min_parquetrain)


dist_min_parquetest <- apply(dist_matrixtest, 1, min)  
test <- test %>%
  mutate(dis_parque=dist_min_parquetest)

# Consideremos si el tamaño del parque influye en el precio 

posicion_train <- apply(dist_matrixtrain, 1, function(x) which(min(x) == x))
posicion_test <- apply(dist_matrixtest, 1, function(x) which(min(x) == x))

# De la geometría de los parques extraemos el área

areas <- st_area(parques_geometria)

# Agregamos la variable  a la basa de datos 

train  <- train %>%
  mutate(area_parque = as.numeric(areas[posicion_train]))
test  <- test %>%
  mutate(area_parque = as.numeric(areas[posicion_test]))

# Distancia a restaurantes  -----------------------------------------------

# Cargar los datos de los restaurantes 

restaurantes <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "restaurant")

# Cambiamos el formato para que sea un objeto sf (simple features)

restaurantes_sf <- osmdata_sf(restaurantes)

# De las features de los restaurantes nos interesa su geometría y donde están ubicados 

restaurantes_geometria <- restaurantes_sf$osm_points %>% 
  dplyr::select(osm_id, name) 

# Dado que OSM trata a los restaurantes como puntos podemos calcular la distancia  sin calcular el centroide 

dist_matrixtrainrest <- st_distance( x=sf_train,y=restaurantes_geometria)
dim(dist_matrixtrainrest)

dist_matrixtestrest <- st_distance(x=sf_test,y=restaurantes_geometria)
dim(dist_matrixtestrest)


# Calculamos la distancia minima a cada propiedad

dist_min_restrain <- apply(dist_matrixtrain, 1, min)  
train <- train %>%
  mutate(dis_rest=dist_min_restrain)


dist_min_restest <- apply(dist_matrixtest, 1, min)  
test <- test %>%
  mutate(dis_rest=dist_min_restest)


# Distancia al centro de bogota  ------------------------------------------

#Vamos a aproximarnos al centro como la  plaza de bolivar 

plaza_bolivar_osm <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "name", value = "Plaza de Bolívar") 

# Cambiamos el formato para que sea un objeto sf (simple features)

centro_sf <- osmdata_sf(plaza_bolivar_osm)

# De las features de los restaurantes nos interesa su geometría y donde están ubicados 

centro_geometria <- centro_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) 

#Calculamos el centroide para  calcular la distnacia 

centroides_centro <- st_centroid(centro_geometria, byid = T)

centroides_centro <- centroides_centro %>%
  mutate(x=st_coordinates(centroides_centro)[, "X"]) %>%
  mutate(y=st_coordinates(centroides_centro)[, "Y"]) 

# Visualizando en un mapa 

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = centro_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = centro_geometria$name) %>%
  addCircles(lng = centroides_centro$x, 
             lat = centroides_centro$y, 
             col = "darkblue", opacity = 0.5, radius = 1)

# Definimos los centroides como datos espaciales

centroides_centro_sf <- st_as_sf(centroides_centro, coords = c("x", "y"), crs=4326)

# Calculamos la distancia desde cada propiedad a la Plaza de Bolívar

dist_matrixtrainplaza <- st_distance(x=sf_train, y=centroides_centro_sf)
dim(dist_matrixtrainplaza)

dist_matrixtestplaza <- st_distance(x=sf_test, y=centroides_centro_sf)
dim(dist_matrixtestplaza)

# Calculamos la distancia minima a cada propiedad

dist_min_centrotrain <- apply(dist_matrixtrainplaza, 1, min)  
train <- train %>%
  mutate(dis_centro=dist_min_centrotrain)


dist_min_centrotest <- apply(dist_matrixtestplaza, 1, min)  
test <- test %>%
  mutate(dis_centro=dist_min_centrotest)


# Distancia aeropuerto  ---------------------------------------------------

# Cargamos de OSM la ubicación del aeropuerto el dorado 

aeropuerto_osm <- opq(bbox = getbb("Bogotá, Colombia")) %>%
  add_osm_feature(key = "aeroway", value = "terminal") %>%
  add_osm_feature(key = "name", value = "Aeropuerto El Dorado")

# Cambiamos el formato para que sea un objeto sf (simple features)

dorado_sf <- osmdata_sf(aeropuerto_osm)

# De las features de los restaurantes nos interesa su geometría y donde están ubicados 

dorado_geometria <- dorado_sf$osm_polygons

# Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto

centroides_dorado <- st_centroid(dorado_geometria, byid = T)

centroides_dorado <- centroides_dorado %>%
  mutate(x=st_coordinates(centroides_dorado)[, "X"]) %>%
  mutate(y=st_coordinates(centroides_dorado)[, "Y"]) 

centroides_dorado_sf <- st_as_sf(centroides_dorado, coords = c("x", "y"), )

# Calculamos la distancia desde cada propiedad al aeropuerto

dist_matrixtraindorado <- st_distance(x=sf_train, y=centroides_dorado_sf)
dim(dist_matrixtraindorado)

dist_matrixtestdorado <- st_distance(x=sf_test, y=centroides_dorado_sf)
dim(dist_matrixtestdorado)

# Calculamos la distancia minima a cada propiedad

dist_min_doradotrain <- apply(dist_matrixtraindorado, 1, min)  
train <- train %>%
  mutate(dis_dorado=dist_min_doradotrain)


dist_min_doradotest <- apply(dist_matrixtestdorado, 1, min)  
test <- test %>%
  mutate(dis_centro=dist_min_doradotest)


# Distancia a estaciones de transmilenio  ---------------------------------

#Cargamos las bus stop que aparecen en osm

estaciones_osm <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "highway", value = "bus_stop")

# Cambiamos el formato para que sea un objeto sf

estaciones_sf <- osmdata_sf(estaciones_osm)

# Extraemos las geometrías de las estaciones de TransMilenio

estaciones_geometria <- estaciones_sf$osm_points

# Convertimos estaciones_geometria a sf con el CRS adecuado, si es necesario

estaciones_geometria <- st_as_sf(estaciones_geometria, crs = 4326)

# Calculamos la distancia desde cada propiedad a las estaciones

dist_matrixtrainest <- st_distance(x = sf_train, y = estaciones_geometria)
dim(dist_matrixtrainest)

dist_matrixtestest <- st_distance(x = sf_test, y = estaciones_geometria)
dim(dist_matrixtestest)

# Calculamos la distancia mínima a cada propiedad 

dist_min_esttrain <- apply(dist_matrixtrainest, 1, min)
train <- train %>%
  mutate(dis_estacion = dist_min_esttrain)

dist_min_esttest <- apply(dist_matrixtestest, 1, min)
test <- test %>%
  mutate(dis_estacion = dist_min_esttest)

                       # Distancia a centros comerciales -----------------------------------------

#Cargamos los datos de centros comerciales 

centros_comerciales_osm <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "shop", value = "mall")

# Cambiamos el formato para que sea un objeto sf (simple features)

centro_comercial_sf <- osmdata_sf(centros_comerciales_osm)

# De las features del parque nos interesa su geometría y donde están ubicados 

centro_comercial_geometria <- centro_comercial_sf$osm_polygons %>% 
  dplyr::select(osm_id, name)
head(centro_comercial_geometria)

# Guardemos los polígonos de los centros comerciales 
centro_comercial_geometria <- st_as_sf(centro_comercial_sf$osm_polygons)

# Calculamos el centroide de cada centro comercial para aproximar su ubicación como un solo punto 

centroides_centro_comercial <- st_centroid(centro_comercial_geometria, byid = T)

centroides_centro_comercial <- centroides_centro_comercial %>%
  mutate(x=st_coordinates(centroides_centro_comercial)[, "X"]) %>%
  mutate(y=st_coordinates(centroides_centro_comercial)[, "Y"]) 

centroides_sf_centro_comercial <- st_as_sf(centroides_centro_comercial, coords = c("x", "y"), crs=4326)

dist_matrixtraincc <- st_distance(x = sf_train, y = centroides_sf_centro_comercial)
dim(dist_matrixtraincc)

dist_matrixtestcc <- st_distance(x = sf_test, y = centroides_sf_centro_comercial)
dim(dist_matrixtestcc)

# Calculamos la distancia minima a cada propiedad

dist_min_cctrain <- apply(dist_matrixtraincc, 1, min)  
train <- train %>%
  mutate(dis_cc=dist_min_cctrain)


dist_min_cctest <- apply(dist_matrixtestcc, 1, min)  
test <- test %>%
  mutate(dis_cc=dist_min_cctest)

# Distancia a hospitales  -------------------------------------------------

#Cargamos los datos de hospitales

hospitales_osm <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "hospital")

# Cambiamos el formato para que sea un objeto sf (simple features)

hospitales_sf <- osmdata_sf(hospitales_osm)

# De las features de hospitales nos interesa su geometría y donde están ubicados 

hospitales_geometria <- hospitales_sf$osm_polygons %>% 
  dplyr::select(osm_id, name)
head(hospitales_geometria)

# Guardemos los polígonos de los hospitales

hospitales_geometria <- st_as_sf(hospitales_sf$osm_polygons)

# Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 

centroides_hospitales <- st_centroid(hospitales_geometria, byid = T)

centroides_hospitales <- centroides_hospitales %>%
  mutate(x=st_coordinates(centroides_hospitales)[, "X"]) %>%
  mutate(y=st_coordinates(centroides_hospitales)[, "Y"]) 

# Visualizando en un mapa 

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = hospitales_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = hospitales_geometria$name) %>%
  addCircles(lng = centroides_hospitales$x, 
             lat = centroides_hospitales$y, 
             col = "darkblue", opacity = 0.5, radius = 1)


centroides_sf_hospitales <- st_as_sf(centroides_hospitales, coords = c("x", "y"), crs=4326)

dist_matrixtrainhosp <- st_distance(x = sf_train, y = centroides_sf_hospitales)
dim(dist_matrixtrainhosp)

dist_matrixtesthosp <- st_distance(x = sf_test, y = centroides_sf_hospitales)
dim(dist_matrixtesthosp)

# Calculamos la distancia minima a cada propiedad

dist_min_hosptrain <- apply(dist_matrixtrainhosp, 1, min)  
train <- train %>%
  mutate(dis_hosp=dist_min_hosptrain)

dist_min_hosptest <- apply(dist_matrixtesthosp, 1, min)  
test <- test %>%
  mutate(dis_hosp=dist_min_hosptest)


# Distancia a colegios -----------------------------------------

#Cargamos los datos de colegios

col_osm <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "school")

# Cambiamos el formato para que sea un objeto sf (simple features)

col_sf <- osmdata_sf(col_osm)

# De las features de los colegios nos interesa su geometría y donde están ubicados 

col_geometria <-col_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) %>%
  dplyr::filter(!str_detect(name, "Distrital"))
head(col_geometria)

# Guardemos los polígonos de los colegios

col_geometria <- st_as_sf(col_sf$osm_polygons)

# Calculamos el centroide de cada colegio para aproximar su ubicación como un solo punto 

centroides_col <- st_centroid(col_geometria, byid = T)

centroides_col <- centroides_col %>%
  mutate(x=st_coordinates(centroides_col)[, "X"]) %>%
  mutate(y=st_coordinates(centroides_col)[, "Y"]) 

# Visualizando en un mapa 

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = col_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = col_geometria$name) %>%
  addCircles(lng = centroides_col$x, 
             lat = centroides_col$y, 
             col = "darkblue", opacity = 0.5, radius = 1)


centroides_sf_col <- st_as_sf(centroides_col, coords = c("x", "y"), crs=4326)

dist_matrixtraincol <- st_distance(x = sf_train, y = centroides_sf_col)
dim(dist_matrixtraincol)

dist_matrixtestcol <- st_distance(x = sf_test, y = centroides_sf_col)
dim(dist_matrixtestcol)

# Calculamos la distancia minima a cada propiedad

dist_min_coltrain <- apply(dist_matrixtraincol, 1, min)  
train <- train %>%
  mutate(dis_col=dist_min_coltrain)

dist_min_coltest <- apply(dist_matrixtestcol, 1, min)  
test <- test %>%
  mutate(dis_col=dist_min_coltest)


# Distancia a universidades -----------------------------------------------

#Cargamos los datos de universidades

uni_osm <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "university")

# Cambiamos el formato para que sea un objeto sf (simple features)

uni_sf <- osmdata_sf(uni_osm)

# De las features de las universidades nos interesa su geometría y donde están ubicados 

uni_geometria <-uni_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) 
  
head(uni_geometria)

# Guardemos los polígonos de los colegios

uni_geometria <- st_as_sf(uni_sf$osm_polygons)

# Calculamos el centroide de cada universidad para aproximar su ubicación como un solo punto 

centroides_uni <- st_centroid(uni_geometria, byid = T)

centroides_uni <- centroides_uni %>%
  mutate(x=st_coordinates(centroides_uni)[, "X"]) %>%
  mutate(y=st_coordinates(centroides_uni)[, "Y"]) 

# Visualizando en un mapa 

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = uni_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = uni_geometria$name) %>%
  addCircles(lng = centroides_uni$x, 
             lat = centroides_uni$y, 
             col = "darkblue", opacity = 0.5, radius = 1)


centroides_sf_uni <- st_as_sf(centroides_uni, coords = c("x", "y"), crs=4326)

dist_matrixtrainuni <- st_distance(x = sf_train, y = centroides_sf_uni)
dim(dist_matrixtrainuni)

dist_matrixtestuni <- st_distance(x = sf_test, y = centroides_sf_uni)
dim(dist_matrixtestuni)

# Calculamos la distancia minima a cada propiedad

dist_min_unitrain <- apply(dist_matrixtrainuni, 1, min)  
train <- train %>%
  mutate(dis_uni=dist_min_unitrain)

dist_min_unitest <- apply(dist_matrixtestuni, 1, min)  
test <- test %>%
  mutate(dis_uni=dist_min_unitest)


# Distancia a  estacion de policia  ---------------------------------------

#Cargamos los datos de  estaciones de Policia

pol_osm <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "police")

# Cambiamos el formato para que sea un objeto sf (simple features)

pol_sf <- osmdata_sf(pol_osm)

# De las features de las estaciones de policia nos interesa su geometría y donde están ubicados 

pol_geometria <-pol_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) 

head(pol_geometria)

# Guardemos los polígonos de las estaciones de policia

pol_geometria <- st_as_sf(pol_sf$osm_polygons)

# Calculamos el centroide de cada universidad para aproximar su ubicación como un solo punto 

centroides_pol <- st_centroid(pol_geometria, byid = T)

centroides_pol <- centroides_pol %>%
  mutate(x=st_coordinates(centroides_pol)[, "X"]) %>%
  mutate(y=st_coordinates(centroides_pol)[, "Y"]) 

# Visualizando en un mapa 

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = pol_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = pol_geometria$name) %>%
  addCircles(lng = centroides_pol$x, 
             lat = centroides_pol$y, 
             col = "darkblue", opacity = 0.5, radius = 1)


centroides_sf_pol <- st_as_sf(centroides_pol, coords = c("x", "y"), crs=4326)

dist_matrixtrainpol <- st_distance(x = sf_train, y = centroides_sf_pol)
dim(dist_matrixtrainpol)

dist_matrixtestpol <- st_distance(x = sf_test, y = centroides_sf_pol)
dim(dist_matrixtestpol)

# Calculamos la distancia minima a cada propiedad

dist_min_poltrain <- apply(dist_matrixtrainpol, 1, min)  
train <- train %>%
  mutate(dis_pol=dist_min_poltrain)

dist_min_poltest <- apply(dist_matrixtestpol, 1, min)  
test <- test %>%
  mutate(dis_pol=dist_min_poltest)

# Texto como datos ---------------------------------------------------------
 
# Homogenizamos el texto 

# Pasamos toda la descripcion a minuscula 

train <- train %>%
  mutate(description = str_to_lower(description))

test <- test %>%
  mutate(description = str_to_lower(description))

# Eliminamos las tíldes 

train <- train %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

test <- test %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

# Eliminamos carcateres especiales 

train <- train %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

test <- test %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

# Eliminamos espacios extra 

train <- train %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

test <- test %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))


# Número de pisos ---------------------------------------------------------

train <- train %>%
  mutate(n_pisosc= str_extract(description, "(\\w+|\\d+) (pisos|niveles)")) %>%
  mutate(n_pisosc= ifelse(property_type=="Casa", n_pisosc, NA))

test <- test %>%
  mutate(n_pisosc= str_extract(description, "(\\w+|\\d+) (pisos|niveles)")) %>%
  mutate(n_pisosc= ifelse(property_type=="Casa", n_pisosc, NA))

train <- train %>%
  mutate(n_pisos= str_extract(description, "duplex")) %>%
  mutate(n_pisos= ifelse(property_type=="Apartamento", n_pisos, n_pisosc))

test <- test %>%
  mutate(n_pisos= str_extract(description, "duplex")) %>%
  mutate(n_pisos= ifelse(property_type=="Apartamento", n_pisos,  n_pisosc))

# Pasamos de letras a números 

numeros_escritos <- c( "dos|duplex", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
numeros_numericos <- as.character(2:10)

train <- train %>%
  mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))

test <- test %>%
  mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))

train <- train %>%
  mutate(n_pisos_numerico = as.integer(str_extract(n_pisos, "\\d+")))  %>%
  mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
  mutate(n_pisos_numerico = if_else(n_pisos_numerico>10, 1, n_pisos_numerico)) %>%
  select(-n_pisosc, -n_pisos)

test <- test %>%
  mutate(n_pisos_numerico = as.integer(str_extract(n_pisos, "\\d+")))  %>%
  mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
  mutate(n_pisos_numerico = if_else(n_pisos_numerico>10, 1, n_pisos_numerico)) %>%
  select(-n_pisosc, -n_pisos)

# Ubicación del piso  -----------------------------------------------------


train <- train %>%
  mutate(piso_info= str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))

test <- test %>%
  mutate(piso_info= str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))

numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo|sptimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei", "undecimo|once|ultimo")
numeros_numericos <- as.character(1:11)

train <- train %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos)))

test <- test %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos)))

train <- train %>%
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

test <- test %>%
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

train <- train %>%
  mutate(piso_numerico = ifelse(piso_numerico > 30, NA, piso_numerico)) %>%
  mutate(piso_numerico = ifelse(property_type=="Casa", 1, piso_numerico))

test <- test %>%
  mutate(piso_numerico = ifelse(piso_numerico > 30, NA, piso_numerico)) %>%
  mutate(piso_numerico = ifelse(property_type=="Casa", 1, piso_numerico))

# En este caso vamos a imputar la media  a las NA

mean_piso_train<- mean(train$piso_numerico, na.rm = TRUE)
mean_piso_train
mean_piso_test<- mean(test$piso_numerico, na.rm = TRUE)
mean_piso_test

train <- train %>%
  mutate(piso_numerico = replace_na(piso_numerico, floor(mean_piso_train))) %>%
  select(-piso_info)

test <- test %>%
  mutate(piso_numerico = replace_na(piso_numerico, floor(mean_piso_train))) %>%
  select(-piso_info)

# Predicciones ------------------------------------------------------------

sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)

# Elastic net -------------------------------------------------------------

elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Definimos la grilla de párametrospenalty

grid_values <- grid_regular(penalty(range = c(-3, 3)), levels = 50) %>%
  expand_grid(mixture = seq(0, 1, by = 0.05))

set.seed(86936)
db_fold <- vfold_cv(train, v = 5)

# Definimos la primera receta 

rec_1 <- recipe(price ~ dis_parque + area_parque + dis_rest + dis_centro + dis_estacion + dis_cc + dis_hosp + dis_col + dis_uni + dis_pol + rooms + bathrooms + bedrooms + property_type + piso_numerico+ n_pisos_numerico , data = train) %>%
  step_interact(terms = ~ dis_centro:property_type + dis_col:property_type + dis_rest:property_type + dis_cc:property_type + dis_hosp:property_type  ) %>% # creamos interacciones con el tipo de propiedad
  step_interact(terms = ~ dis_centro:piso_numerico + dis_col:piso_numerico + dis_centro:piso_numerico ) %>% # Crea interacción con el piso donde se encuentra el apto. 
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores.

# Definimos  flujo de trabajo

workflow_1 <- workflow() %>% 
  # Agregar la receta de pre-procesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(elastic_net_spec)

# Entrenamiento de hiperparametros 

set.seed(86936)

tune_res1 <- tune_grid(
  workflow_1,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = db_fold,  # Folds de validación cruzada
  grid = grid_values,        # Grilla de valores de penalización
  metrics = metric_set(rmse)  # métrica
)

collect_metrics(tune_res1)

best_penalty_1 <- select_best(tune_res1, metric = "rmse")
best_penalty_1

EN_final1 <- finalize_workflow(workflow_1, best_penalty_1)

EN_final1_fit <- fit(EN_final1, data = train)

predicciones <- predict(EN_final1_fit , new_data = test)

# Combinar property_id con las predicciones

predictionsample <- test %>%
  select(property_id) %>%  # Seleccionar solo el property_id
  mutate(price = predicciones $.pred)  # Añadir las predicciones como nueva columna

head(predictionsample)

# Aproximamos
predictionsample <- predictionsample %>%
  mutate(price=floor(price))

# Leer el archivo de template para asegurar el formato de salida

template <- read.csv("submission_template.csv")
head(template)

# Guardar el archivo de predicciones

name<- paste0("EN_lambda_", "0001", "_alpha_" , "15", ".csv")  
write.csv(predictionsample,name, row.names = FALSE)

# Distancia a  discotecas -------------------------------------------------

#Cargamos los datos de discotecas

disco_osm <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "nightclub")

# Cambiamos el formato para que sea un objeto sf (simple features)

disco_sf <- osmdata_sf(disco_osm)

# De las features de discotecas nos interesa su geometría y donde están ubicados 

disco_geometria <- disco_sf$osm_polygons %>% 
  dplyr::select(osm_id, name)
head(disco_geometria)

# Guardemos los polígonos de las discotecas

disco_geometria <- st_as_sf(disco_sf$osm_polygons)

# Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 

centroides_disco <- st_centroid(disco_geometria, byid = T)

centroides_disco <- centroides_disco %>%
  mutate(x=st_coordinates(centroides_disco)[, "X"]) %>%
  mutate(y=st_coordinates(centroides_disco)[, "Y"]) 

# Visualizando en un mapa 

leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = disco_geometria, col = "red",weight = 10,
              opacity = 0.8, popup = disco_geometria$name) %>%
  addCircles(lng = centroides_disco$x, 
             lat = centroides_disco$y, 
             col = "darkblue", opacity = 0.5, radius = 1)


centroides_sf_disco <- st_as_sf(centroides_disco, coords = c("x", "y"), crs=4326)

dist_matrixtraindisco <- st_distance(x = sf_train, y = centroides_sf_disco)
dim(dist_matrixtraindisco)

dist_matrixtestdisco <- st_distance(x = sf_test, y = centroides_sf_disco)
dim(dist_matrixtestdisco)

# Calculamos la distancia minima a cada propiedad

dist_min_discotrain <- apply(dist_matrixtraindisco, 1, min)  
train <- train %>%
  mutate(dis_disco=dist_min_discotrain)

dist_min_discotest <- apply(dist_matrixtestdisco, 1, min)  
test <- test %>%
  mutate(dis_disco=dist_min_discotest)


# Añadimos otra variable a partir de la descripcion  ----------------------

# Homogenizamos el texto 

# Pasamos toda la descripcion a minuscula 

train <- train %>%
  mutate(description = str_to_lower(description))

test <- test %>%
  mutate(description = str_to_lower(description))

# Eliminamos las tíldes 

train <- train %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

test <- test %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

# Eliminamos carcateres especiales 

train <- train %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

test <- test %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

# Eliminamos espacios extra 

train <- train %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

test <- test %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

# Estraemos informacion si la propiedad tiene terraza

train <- train %>%
  mutate(terraza= str_extract(description, "\\b(terraza|balcon)\\b"))

test <- test %>%
  mutate(terraza= str_extract(description, "\\b(terraza|balcon)\\b"))

# Creamos una variable dummy que toma el valor de 1 si hay terraza y 0 de lo contrario 

train <- train %>%
  mutate(terraza= if_else(!is.na(terraza), 1, 0))

test <- test %>%
  mutate(terraza= if_else(!is.na(terraza), 1, 0))

# Tiene  garaje  ----------------------------------------------------------

train <- train %>%
  mutate(garaje= str_extract(description, "\\b(garaje|garajes|parqueadero|parqueaderos|parqueo)\\b"))

test <- test %>%
  mutate(garaje= str_extract(description, "\\b(garaje|garajes|parqueadero|parqueaderos|parqueo)\\b"))

# Creamos una variable dummy que toma el valor de 1 si hay terraza y 0 de lo contrario 

train <- train %>%
  mutate(garaje = if_else(!is.na(garaje), 1, 0))

test <- test %>%
  mutate(garaje = if_else(!is.na(garaje), 1, 0))

# Capacidad garaje --------------------------------------------------------

# Eliminar la palabra "para" de la descripción en train y test

train <- train %>%
  mutate(description = str_replace_all(description, "\\bpara\\b", ""))

test <- test %>%
  mutate(description = str_replace_all(description, "\\bpara\\b", ""))

# Extraer datos del garaje o parqueadero

train <- train %>%
  mutate(cap_garaje= str_extract(description, "(\\w+|\\d+)?\\s*\\b(garaje|garajes|parqueadero|parqueaderos|parqueo|parquedero)\\b\\s*(\\w+|\\d+)?"))

test <- test %>%
  mutate(cap_garaje= str_extract(description, "(\\w+|\\d+)?\\s*\\b(garaje|garajes|parqueadero|parqueaderos|parqueo|parquedero)\\b\\s*(\\w+|\\d+)?"))

# Pasamos de letras a números 

numeros_escritos <- c("uno|primero|primer", "doble|dos", "tres", "cuatro", "cinco")
numeros_numericos <- as.character(1:5)

train <- train %>%
  mutate(cap_garaje_info = str_replace_all(cap_garaje, setNames(numeros_numericos,numeros_escritos)))

test <- test %>%
  mutate(cap_garaje_info = str_replace_all(cap_garaje, setNames(numeros_numericos,numeros_escritos)))

# Extraemos el valor númerico de la capacidad 

train <- train %>%
  mutate(capgaraje = as.integer(str_extract(cap_garaje_info, "\\d+"))) %>%
  mutate(capgaraje = if_else(is.na(capgaraje) & garaje == 1, 1,if_else(is.na(capgaraje), 0, capgaraje))) %>%
  select(-cap_garaje_info,-cap_garaje)

test <- test %>%
  mutate(capgaraje = as.integer(str_extract(cap_garaje_info, "\\d+"))) %>%
  mutate(capgaraje = if_else(is.na(capgaraje) & garaje == 1, 1,if_else(is.na(capgaraje), 0, capgaraje))) %>%
  select(-cap_garaje_info,-cap_garaje)

# Deposito o bodega -------------------------------------------------------

train <- train %>%
  mutate(deposito= str_extract(description, "\\b(deposito|bodega)\\b"))

test <- test %>%
  mutate(deposito= str_extract(description, "\\b(deposito|bodega)\\b"))

# Creamos una variable dummy que toma el valor de 1 si hay terraza y 0 de lo contrario 

train <- train %>%
  mutate(deposito = if_else(!is.na(deposito), 1, 0))

test <- test %>%
  mutate(deposito = if_else(!is.na(deposito), 1, 0))

sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)


elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Definimos la grilla de párametrospenalty

grid_values <- grid_regular(penalty(range = c(-3, 3)), levels = 50) %>%
  expand_grid(mixture = seq(0, 1, by = 0.05))

set.seed(86936)
db_fold <- vfold_cv(train, v = 10)

# Definimos la segunda receta 

rec_2 <- recipe(price ~ dis_parque + area_parque + dis_rest + dis_centro + dis_estacion + dis_cc + dis_hosp + dis_col + dis_uni + dis_pol + dis_disco + rooms + bathrooms + bedrooms + property_type + piso_numerico+ n_pisos_numerico + garaje + capgaraje + deposito + terraza, data = train) %>%
  step_interact(terms = ~ dis_centro:property_type + dis_col:property_type + dis_rest:property_type + dis_cc:property_type + dis_hosp:property_type + dis_disco:property_type  ) %>% # creamos interacciones con el tipo de propiedad
  step_interact(terms = ~ dis_cc:piso_numerico + dis_col:piso_numerico + dis_centro:piso_numerico + dis_disco:property_type + dis_hosp:piso_numerico  ) %>% # Crea interacción con el piso donde se encuentra el apto. 
  step_interact(terms = ~ dis_cc:garaje + dis_col:garaje + dis_centro:garaje + dis_disco:garaje + dis_hosp:garaje )  %>% #Creamos iterraciones con garaje
  step_interact(terms = ~ dis_cc:terraza + dis_col:terraza + dis_centro:terraza + dis_disco:terraza + dis_hosp:terraza )  %>%  #Creamos iteracciones con si tiene terraza
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores.

# Definimos  flujo de trabajo

workflow_2 <- workflow() %>% 
  # Agregar la receta de pre-procesamiento de datos. En este caso la receta 1
  add_recipe(rec_2) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(elastic_net_spec)

# Entrenamiento de hiperparametros 

set.seed(200447)

tune_res2 <- tune_grid(
  workflow_2,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = db_fold,  # Folds de validación cruzada
  grid = grid_values,        # Grilla de valores de penalización
  metrics = metric_set(rmse)  # métrica
)

collect_metrics(tune_res2)

best_penalty_2 <- select_best(tune_res2, metric = "rmse")
best_penalty_2

EN_final2 <- finalize_workflow(workflow_2, best_penalty_2)

EN_final2_fit <- fit(EN_final2, data = train)

predicciones <- predict(EN_final2_fit , new_data = test)

# Combinar property_id con las predicciones

predictionsample <- test %>%
  select(property_id) %>%  # Seleccionar solo el property_id
  mutate(price = predicciones $.pred)  # Añadir las predicciones como nueva columna

head(predictionsample)

# Aproximamos
predictionsample <- predictionsample %>%
  mutate(price=floor(price))

# Leer el archivo de template para asegurar el formato de salida

template <- read.csv("submission_template.csv")
head(template)

# Guardar el archivo de predicciones

name<- paste0("EN_lambda_", "0001", "_alpha_" , "35", ".csv")  
write.csv(predictionsample,name, row.names = FALSE)
# Elastic net 3  ----------------------------------------------------------

sf_test <- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)
sf_train <- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)


elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Definimos la grilla de párametrospenalty

grid_values <- grid_regular(penalty(range = c(-3, 3)), levels = 50) %>%
  expand_grid(mixture = seq(0, 1, by = 0.05))

set.seed(86936)
db_fold <- vfold_cv(train, v = 10)

# Definimos la tercera receta 

rec_3 <- recipe(price ~ dis_parque + area_parque + dis_rest + dis_centro + dis_estacion + dis_cc + dis_hosp + dis_col + dis_uni + dis_pol + dis_disco + rooms + bathrooms + bedrooms + property_type + piso_numerico+ n_pisos_numerico + garaje + capgaraje + deposito + terraza, data = train) %>%
  step_interact(terms = ~ dis_parque:property_type + area_parque:property_type + dis_rest:property_type + dis_centro:property_type + dis_estacion:property_type + dis_cc:property_type + dis_hosp:property_type + dis_col:property_type + dis_uni:property_type + dis_pol:property_type + dis_disco:property_type + rooms:property_type + bathrooms:property_type + bedrooms:property_type + deposito:property_type) %>% # creamos interacciones con el tipo de propiedad
  step_interact(terms = ~ dis_parque:piso_numerico + area_parque:piso_numerico + dis_rest:piso_numerico + dis_centro:piso_numerico + dis_estacion:piso_numerico + dis_cc:piso_numerico + dis_hosp:piso_numerico + dis_col:piso_numerico + dis_uni:piso_numerico + dis_pol:piso_numerico + dis_disco:piso_numerico) %>% # Crea interacción con el piso donde se encuentra el apto. 
  step_interact(terms = ~ dis_parque:garaje  + area_parque:garaje  + dis_rest:garaje  + dis_centro:garaje  + dis_estacion:garaje  + dis_cc:garaje  + dis_hosp:garaje  + dis_col:garaje  + dis_uni:garaje  + dis_pol:garaje  + dis_disco:garaje )  %>% #Creamos iterraciones con garaje
  step_interact(terms = ~ dis_parque:capgaraje + dis_rest:capgaraje  + dis_centro:capgaraje  + dis_estacion:capgaraje  + dis_cc:capgaraje  + dis_hosp:capgaraje  + dis_col:capgaraje  + dis_uni:capgaraje  + dis_pol:capgaraje  + dis_disco:capgaraje )  %>% #Creamos iterraciones con capacidad del garaje
  step_interact(terms = ~ dis_parque:terraza + dis_rest:terraza + dis_centro:terraza + dis_estacion:terraza + dis_cc:terraza + dis_hosp:terraza + dis_col:terraza + dis_uni:terraza + dis_pol:terraza + dis_disco:terraza )  %>%  #Creamos iteracciones con si tiene terraza
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores.

# Definimos  flujo de trabajo

workflow_3 <- workflow() %>% 
  # Agregar la receta de pre-procesamiento de datos. En este caso la receta 1
  add_recipe(rec_3) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(elastic_net_spec)

# Entrenamiento de hiperparametros 

set.seed(200447)

tune_res3 <- tune_grid(
  workflow_3,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = db_fold,  # Folds de validación cruzada
  grid = grid_values,        # Grilla de valores de penalización
  metrics = metric_set(rmse)  # métrica
)

collect_metrics(tune_res3)

best_penalty_3 <- select_best(tune_res3, metric = "rmse")
best_penalty_3

EN_final3 <- finalize_workflow(workflow_3, best_penalty_3)

EN_final3_fit <- fit(EN_final3, data = train)

predicciones <- predict(EN_final3_fit , new_data = test)

# Combinar property_id con las predicciones

predictionsample <- test %>%
  select(property_id) %>%  # Seleccionar solo el property_id
  mutate(price = predicciones $.pred)  # Añadir las predicciones como nueva columna

head(predictionsample)

# Aproximamos
predictionsample <- predictionsample %>%
  mutate(price=floor(price))

# Leer el archivo de template para asegurar el formato de salida

template <- read.csv("submission_template.csv")
head(template)

# Guardar el archivo de predicciones

name<- paste0("EN_lambda_", "0001", "_alpha_" , "95", ".csv")  
write.csv(predictionsample,name, row.names = FALSE)
