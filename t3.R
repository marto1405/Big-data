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

# Crear precio por m2 -----------------------------------------------------

train<-train %>%
  mutate(precio_por_m2= round(price/surface_total,0)) %>%
  mutate(precio_por_m2=precio_por_m2/1000000)

test<-test %>%
  mutate(precio_por_m2= round(price/surface_total,0)) %>%
  mutate(precio_por_m2=precio_por_m2/1000000)

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
#Realizamos un grafico por precio de mt2 para apartamentos
ggplot()+
  geom_sf(data=localidades, color = "black") + 
  geom_sf(data=sf_train%>% filter(property_type== "Apartamento"),aes(color = precio_por_m2) ,shape=15, size=0.3)+
  theme_bw()
#Note que no tenemos  datos ni para sumapaz y para usme 

localidades_filtradas <- localidades[-c(9,14), ]
ggplot()+
  geom_sf(data=localidades_filtradas, color = "black")
ggplot()+
  geom_sf(data=localidades_filtradas, color = "black") + 
  geom_sf(data=sf_train%>% filter(property_type== "Apartamento"),aes(color = precio_por_m2) ,shape=15, size=0.3)+
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

# Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 

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
