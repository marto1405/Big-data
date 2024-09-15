rm(list = ls())
require(pacman)
p_load(tidyverse, rvest,jsonlite,skimr,stargazer,rio,MASS, boot)
select<-dplyr::select
################################################################################
### En esta primera parte del código se hará el scraping de la data          ###
################################################################################

url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
dta_url <- read_html(url)
#browseURL(url)
#class(dta_url)
#View(dta_url)
# obtengo los links de cada chunk de la base
links <-  dta_url %>% html_elements(".col-md-9") %>% 
  html_elements("li") %>% html_element("a") %>% 
  html_attr("href")

url2 <- paste(c(url,links[1]),sep="",collapse="")
# Luego de inspeccionar las páginas, encuentro que los datos están en el geih_page_i.html de la forma:
# https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_[i].html
# Puedo crear un loop para ir por todas las páginas y descargar las bases

data <- data.frame()

for (i in 1:length(links)){
  url3 <- paste(c("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html"),
                sep="",collapse="")
  dta_url3 <- read_html(url3)
  db <- dta_url3 %>%
    html_nodes("table") %>% 
    html_table() %>% as.data.frame()
  data <- rbind.data.frame(data,db)
}
View(data)
rm(db)
#write.table(data, file = "C:/Users/Esteban/Dropbox/Estudios/Big Data y Machine Learning/Talleres/Taller 1/data.csv", 
#            sep = "|")

################################################################################
### Arreglo de la base de datos                                              ###
################################################################################

#data <- read.table(file = "C:/Users/Esteban/Dropbox/Estudios/Big Data y Machine Learning/Talleres/Taller 1/data.csv",
#                   header = TRUE, sep = "|")
#View(data)
summary(data$age)
summary(data$totalHoursWorked)
# we will focus only on employed individuals older than eighteen (18) years old
data <- data %>% filter(age>=18 & totalHoursWorked>0)

# Filtro variables que voy a usar 
# Dada la cantidad de variables, que todas deben ser interpretadas en las estadísticas
# descriptivas y que de una u otra forma toca imputar datos faltantes en ellas, 
# es mejor quedarse solo con un subset.
data <- data %>% select(orden,secuencia_p, age, cuentaPropia, cotPension, formal, mes, maxEducLevel, oficio,
p6050, p6240, p6426, p7040, sex, sizeFirm, totalHoursWorked,relab, y_ingLab_m, y_total_m, y_gananciaNeta_m,
p6510, p6545, p6580, p7495, p7500s2, p7500s3, p7505, p7510s3, p7510s5, p7510s6, ingtot, informal)

# En este caso es razonable pensar que no responder es lo mismo que la opción NO
mod <- c("p7040", "p6510", "p6545", "p6580", "p7495", "p7500s2", "p7500s3", "p7505", "p7510s3", "p7510s5", "p7510s6")
rv <- function(x){
  x = case_when(x==2~0, TRUE ~ x)
  x = case_when(x==9~0, TRUE ~ x)
}
data <- data %>% mutate_at(mod, rv)
data <- data %>% mutate_at(mod, ~replace(., is.na(.), 0))

# La variable de ingresos laborales tiene muchos NA. Al ser la variable de 
# interés toca imputar datos de forma más precisa que solo imputando la media.
# Hay personas que tienen ingreso por ganancias y no por salario.
# es razonable pensar que si trabajaran ganarían algo similar a sus ganancias
data %>% select(y_ingLab_m,y_gananciaNeta_m) %>%  stargazer(type = "text")

data2 <- data %>% select(y_ingLab_m,y_gananciaNeta_m) %>% mutate(y_ingLab_m=y_ingLab_m/1000000,
                         y_gananciaNeta_m=y_gananciaNeta_m/1000000)
quantile(data2$y_ingLab_m, c(.01, .10, .25, .50, .75, .90, .99),na.rm = TRUE)
quantile(data2$y_gananciaNeta_m, c(.01, .10, .25, .50, .75, .90, .99),na.rm = TRUE)
rm(data2)
#data %>% filter(is.na(y_ingLab_m)) %>% View()
# Densidad de las distribuciones 
x <- data %>% filter(!is.na(y_ingLab_m) & y_ingLab_m<10000000) %>% select(y_ingLab_m)
x <- x[["y_ingLab_m"]]
y <- data %>% filter(!is.na(y_gananciaNeta_m) & y_gananciaNeta_m<10000000) %>% select(y_gananciaNeta_m)
y <- y[["y_gananciaNeta_m"]]
denx <- density(x)
deny <- density(y)
plot(denx,
     ylim = c(0, max(c(denx$y, deny$y))),
     xlim = c(min(c(denx$x, deny$x)),
              max(c(denx$x, deny$x))))
lines(deny)
polygon(denx, col = rgb(0.78, 0.89, 1, alpha = 0.6))
polygon(deny, col = rgb(0.51, 0.44, 1, alpha = 0.6))
# la distribución de las dos variables es muy similar
sum(is.na(data$y_ingLab_m))
sum(is.na(data$y_ingLab_m) & !is.na(data$y_gananciaNeta_m))
data <-  data %>% mutate(y_ingLab_m = ifelse(is.na(y_ingLab_m) & 
                        !is.na(y_gananciaNeta_m), 
                        y_gananciaNeta_m, y_ingLab_m))
# Para las restantes, podemos hacer una combinación de sexo y nivel educativo
# para poder imputar los datos faltantes.
db_miss <-skim(data) %>% select( skim_variable, n_missing)
#data %>% filter(is.na(maxEducLevel)) %>% View()
# solo hay un missing para maxEducLevel. Reemplazar por el más bajo
data <- data %>% mutate(maxEducLevel = ifelse(is.na(maxEducLevel), 1, maxEducLevel))
rm(db_miss)
data <- data %>% group_by(sex,maxEducLevel) %>% 
  mutate(mean_y_ingLab_m = mean(y_ingLab_m, na.rm=T)) %>% ungroup()
data <- data %>% mutate(y_ingLab_m=ifelse(is.na(y_ingLab_m),
                                          mean_y_ingLab_m,
                                          y_ingLab_m))
# dado que queremos la variable por hora
data = data %>% mutate(y_ingLab_m_ha=y_ingLab_m/(30*totalHoursWorked)*7)
data = data %>% mutate(jefe = ifelse(p6050==1, 1, 0))
summary(data$p6426)
quantile(data$p6426,c(0.9,0.95,0.99,1)) # número de meses en la empresa
data = data %>% mutate(p6426 = ifelse(p6426>400, 400, p6426)) # corto al p99
data = data %>% select(!c(p6050,y_gananciaNeta_m,y_total_m,mean_y_ingLab_m))
data = data %>% mutate(female = ifelse(sex==1,0,1))
rm(denx,deny,x,y)

################################################################################
### Estadísticas descriptivas                                                ###
################################################################################

################################################################################
### 3. Age-wage profle                                                       ###
################################################################################


################################################################################
### 4. Gender earning GAP                                                    ###
################################################################################


################################################################################
### 5. Predicting earning                                                    ###
################################################################################

