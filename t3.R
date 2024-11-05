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


