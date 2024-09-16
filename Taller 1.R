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
data2 = data %>% select(ingtot,totalHoursWorked,female,
                        age,jefe,cuentaPropia,formal) %>% as.data.frame()
stargazer(data2, type="text",digits=2)

table(data$maxEducLevel)
table(data$oficio)
table(data$sizeFirm)
table(data$relab)

data2 = data2 %>% mutate(ingtot = ingtot/1000000)
data2 = data2 %>% mutate(ingtot = ifelse(ingtot>30,30,ingtot))
ggplot( data = data2, mapping = aes(x = age, y = ingtot, 
                                    color(factor(sex)))) + geom_point()
################################################################################
### 3. Age-wage profle                                                       ###
################################################################################

data <- data %>%
  filter(ingtot > 0) %>%
  mutate(lny = log(ingtot),age2=age^2)


model1 <- lm(lny ~ age + age2, data = data)
summary(model1)

model2 <- lm(lny ~ age , data = data)
summary(model2)


stargazer(model1, model2, type = "text", 
          title = "Age-Wage Profile Comparison",
          dep.var.labels = "log(Wage)",
          covariate.labels = c("Age", "Age Squared"),
          column.labels = c("Model 1", "Model 2"),
          out = "regression_summary.txt")##

#grafico comparcion


ggplot(data, aes(x = age, y = lny)) +
  geom_point(alpha = 0.5) +  # Reduce opacidad de los puntos
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue", linetype = "solid", size = 1.2) +  # Modelo 1 (polinomial)
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red", linetype = "dashed", size = 1.2) +  # Modelo 2 (lineal)
  labs(title = "Estimated Age-Wage Profile", x = "Age", y = "Log(Wage)") +
  xlim(20, 80)
scale_color_manual(name = "Model", values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "top")


# el pico máximo se da en:
b1 = coef(model1)["age"]
b2 = coef(model1)["age2"]
peak_age = -b1/(2*b2)
peak_age
###Error estandar de peak_age

# Obtener la matriz de varianza-covarianza
vcov_matrix <- vcov(model1)

# Extraer las varianzas y covarianzas necesarias
var_b1 <- vcov_matrix["age", "age"]
var_b2 <- vcov_matrix["age2", "age2"]
cov_b1_b2 <- vcov_matrix["age", "age2"]

# Calcular el error estándar de peak_age
se_peak_age <- sqrt(
  (1 / (2 * b2))^2 * var_b1 +
    (b1 / (2 * b2^2))^2 * var_b2 +
    2 * (1 / (2 * b2)) * (b1 / (2 * b2^2)) * cov_b1_b2
)

# Calcular el intervalo de confianza
alpha <- 0.05
z_score <- qnorm(1 - alpha / 2)
conf_interval <- c(
  peak_age - z_score * se_peak_age,
  peak_age + z_score * se_peak_age
)

list(
  peak_age = peak_age,
  se_peak_age = se_peak_age,
  conf_interval = conf_interval
)
########### bootstraping 


# Definir la función para el bootstrap
boot_fn_model1 <- function(data, indices) {
  d <- data[indices, ]  # crea la muestra bootstrap
  fit <- lm(lny ~ age + age2, data = data)
  return(coef(fit))  # retornamos los coeficientes
}


# Realizar bootstrap para el modelo 1
set.seed(10101)
boot_results_model1 <- boot(data = data, statistic = boot_fn_model1, R = 1000)


# Mostrar resultados del bootstrap para el modelo 1
boot_results_model1


# Intervalos de confianza para los coeficientes del modelo 1
boot.ci(boot_results_model1, type = "perc", index = 2)  # Para age
boot.ci(boot_results_model1, type = "perc", index = 3)  # Para age^2


# Definir el rango de valores de edad para predecir
age_grid <- seq(min(data$age), max(data$age), length.out = 100)

# Crear un data frame con las edades y el valor de age^2
pred_data <- data.frame(age = age_grid, age2 = age_grid^2)

# Hacer predicciones usando el modelo 1
predictions <- predict(model1, newdata = pred_data, interval = "confidence", level = 0.95)

# Añadir las predicciones y los intervalos de confianza al data frame
pred_data$fit <- predictions[, "fit"]
pred_data$lwr <- predictions[, "lwr"]
pred_data$upr <- predictions[, "upr"]

# Graficar los resultados
ggplot() +
  geom_point(data = data, aes(x = age, y = lny), alpha = 0.2, color = "darkgray") +  # Puntos originales (más tenues)
  geom_line(data = pred_data, aes(x = age, y = fit), color = "blue", size = 1.2) +  # Curva del modelo
  geom_ribbon(data = pred_data, aes(x = age, ymin = lwr, ymax = upr), alpha = 0.4, fill = "blue") +  # Intervalos de confianza más visibles
  labs(title = "Modelo de Edad-Salario con Intervalos de Confianza Más Evidentes",
       x = "Edad",
       y = "Log(Wage)") +
  theme_minimal()

################################################################################
### 4. Gender earning GAP                                                    ###
################################################################################



##Calumlamos la brecha sin controles
model3 <- lm(lny ~ sex, data = data)
summary(model3)

stargazer(model3, type = "text", title = "Regresión de Log(Wage) sobre Female", 
          covariate.labels = c("male"), 
          dep.var.labels = "Log(Wage)", 
          omit.stat = c("f", "ser", "adj.rsq"))


####Usarenmos FWL

# Regresión de log(wage) sobre los controles

#Filtrar datos completos para las variables usadas en el modelo
db_complete <- data %>%
  filter(complete.cases(age, clase, college, cotPension, cuentaPropia, formal, 
                        maxEducLevel, informal, microEmpresa, sizeFirm, totalHoursWorked))

# Ajustar el modelo de regresión de log(wage) sobre los controles en los datos completos
modelc1 <- lm(lny ~  clase + college + cotPension + cuentaPropia + formal + 
                         maxEducLevel + informal + microEmpresa + sizeFirm + totalHoursWorked, data = db_complete)

# Extraer los residuos de esta regresión y añadir al dataframe completo
db_complete <- db_complete %>%
  mutate(residuos_y = resid(modelc1))

# Ajustar el modelo de regresión de género sobre los controles en los datos completos
modelc2 <- lm(sex ~ clase + college + cotPension + cuentaPropia + formal + 
                         maxEducLevel + informal + microEmpresa + sizeFirm + totalHoursWorked, data = db_complete)

# Extraer los residuos de la regresión de género y añadir al dataframe completo
db_complete <- db_complete %>%
  mutate(residuos_genero = resid(modelc2))

# Crear age2 si aún no está creado
db_complete <- db_complete %>%
  mutate(age2 = age^2)

# Ajustar el modelo Frisch-Waugh-Lovell (FWL) con controles adicionales
modelo_fwl_controlado <- lm(residuos_y ~ residuos_genero, data = db_complete)


# Resumen del modelo FWL controlado
stargazer(modelo_fwl_controlado, type = "text", title = "Modelo FWL con controles adicionales",
          out = "modelo_fwl_controlado.txt")


modelcP <- lm(lny ~ sex +clase + college + cotPension + cuentaPropia + formal + 
                maxEducLevel + informal + microEmpresa + sizeFirm + totalHoursWorked, data = db_complete)

stargazer(modelcP, type = "text", title = "Modelo FWL con controles adicionales",
          out = "modelo_fwl_controlado.txt")


#### las estimaciones coinciden


# Crear la variable de interacción entre age2 y sex
db_complete <- db_complete %>%
  mutate(age2_sex = age2*residuos_genero, age_sex = age*residuos_genero  )



modelo_fwl_edad <- lm(residuos_y ~ residuos_genero+ age+ age2+age_sex+ age2_sex, data = db_complete)

stargazer(modelo_fwl_edad, type = "text", title = "Modelo FWL con controles adicionales",
          out = "modelo_fwl_controlado.txt")


b1 = coef(modelo_fwl_edad)["age"]
b2 = coef(modelo_fwl_edad)["age2"]
c1 = coef(modelo_fwl_edad)["age_sex"]
c2 = coef(modelo_fwl_edad)["age2_sex"]
peak_age_hombres = -(b1+c1)/(2*(b2+c2))
peak_age_hombres

peak_age_mujeres = -(b1)/(2*(b2))
peak_age_mujeres

################################################################################
### 5. Predicting earning                                                    ###
################################################################################

set.seed(11052004)
# dividir los datos en training and testeing
inTrain <- createDataPartition(
  y = data$ingtot,  ## the outcome data are needed
  p = .70, ## The percentage of data in the training
  list = FALSE)
training <- data %>% 
  filter(row_number() %in% inTrain)

testing  <- data %>% 
  filter(!row_number() %in% inTrain)
form1<-lny ~ age + I(age^2)
form2<- lny ~ age + I(age^2) + female + I(age^2)*female
form3<- lny ~ age + I(age^2) + female + I(age^2)*female + clase + college + cotPension + cuentaPropia + formal + maxEducLevel + 
  informal + microEmpresa + sizeFirm + totalHoursWorked
form4 <- lny ~  poly(age, 3, raw=TRUE) +  female + poly(age, 3, raw=TRUE):female + clase + college + cotPension + cuentaPropia + formal +
  poly(age, 3, raw=TRUE):formal + maxEducLevel + informal + poly(age, 3, raw=TRUE):informal + 
  microEmpresa + sizeFirm + totalHoursWorked
form5<- lny ~  poly(age,3,raw=TRUE) +  female + poly(age,3,raw=TRUE):female + clase + poly(age,3,raw=TRUE):clase + college + 
  poly(age,3,raw=TRUE):college + cotPension + poly(age,3,raw=TRUE):cotPension + cuentaPropia + poly(age,3,raw=TRUE):cuentaPropia +
  formal + poly(age,3,raw=TRUE):formal + maxEducLevel + poly(age,3,raw=TRUE):maxEducLevel + informal + poly(age,3,raw=TRUE):informal + 
  microEmpresa + poly(age,3,raw=TRUE):microEmpresa + sizeFirm + poly(age,3,raw=TRUE):sizeFirm + 
  totalHoursWorked + poly(age,3,raw=TRUE):totalHoursWorked
form6<- lny ~  poly(age,3,raw=TRUE) +  poly(female,3,raw=TRUE) + poly(age,3,raw=TRUE):poly(female,3,raw=TRUE) + 
  clase + poly(age,3,raw=TRUE):clase + college + poly(age,3,raw=TRUE):college + cotPension + poly(age,3,raw=TRUE):cotPension + 
  cuentaPropia + poly(age,3,raw=TRUE):cuentaPropia + formal + poly(age,3,raw=TRUE):formal + maxEducLevel + 
  poly(age,3,raw=TRUE):maxEducLevel + informal + poly(age,3,raw=TRUE):informal + microEmpresa + 
  poly(age,3,raw=TRUE):microEmpresa + sizeFirm + poly(age,3,raw=TRUE):sizeFirm + 
  totalHoursWorked + poly(age,3,raw=TRUE):totalHoursWorked
form7<- lny ~  poly(age,3,raw=TRUE) +  poly(female,3,raw=TRUE) + poly(age,3,raw=TRUE):poly(female,3,raw=TRUE) + 
  poly(age,3,raw=TRUE):poly(female,3,raw=TRUE):poly(college,3,raw=TRUE) +  clase + poly(age,3,raw=TRUE):clase + 
  poly(college,3,raw=TRUE) + poly(age,3,raw=TRUE):poly(college,3,raw=TRUE) + cotPension + 
  poly(age,3,raw=TRUE):cotPension + cuentaPropia + poly(age,3,raw=TRUE):cuentaPropia + formal + poly(age,3,raw=TRUE):formal + 
  maxEducLevel + poly(age,3,raw=TRUE):maxEducLevel + informal + poly(age,3,raw=TRUE):informal + microEmpresa + 
  poly(age,3,raw=TRUE):microEmpresa + poly(sizeFirm,3,raw=TRUE) + poly(age,3,raw=TRUE):sizeFirm + 
  poly(college,3,raw=TRUE):poly(sizeFirm,3,raw=TRUE) + totalHoursWorked + poly(age,3,raw=TRUE):totalHoursWorked
form8<- lny ~  poly(age,3,raw=TRUE) +  poly(female,3,raw=TRUE) + poly(age,3,raw=TRUE):poly(female,3,raw=TRUE) + 
  poly(clase,3,raw=TRUE) + poly(clase,3,raw=TRUE):poly(age,3,raw=TRUE) + poly(clase,3,raw=TRUE):poly(female,3,raw=TRUE)+
  poly(college,3,raw=TRUE) + poly(college,3,raw=TRUE):poly(female,3,raw=TRUE) + poly(college,3,raw=TRUE):poly(age,3,raw=TRUE)+
  + cotPension + cuentaPropia + formal +
  poly(maxEducLevel,3,raw=TRUE)+ poly(maxEducLevel,3,raw=TRUE):poly(age,3,raw=TRUE) + poly(maxEducLevel,3,raw=TRUE):poly(female,3,raw=TRUE) +
  poly(maxEducLevel,3,raw=TRUE):poly(college,3,raw=TRUE)  + informal +
  poly(age,3,raw=TRUE):informal + poly(female,3,raw=TRUE):informal + poly(college,3,raw=TRUE):informal + 
  poly(maxEducLevel,3,raw=TRUE):informal + microEmpresa + poly(sizeFirm,3,raw=TRUE) + poly(sizeFirm,3,raw=TRUE):poly(female,3,raw=TRUE) +
  poly(sizeFirm,3,raw=TRUE):poly(maxEducLevel,3,raw=TRUE) + totalHoursWorked + poly(age,3,raw=TRUE):totalHoursWorked + 
  poly(female,3,raw=TRUE):totalHoursWorked

# Estimar los modelos
model <- list(form1, form2, form3, form4, form5, form6, form7, form8)
rmse <- numeric(length(model))
for (i in 1:length(model)) {
  modelo <- lm(model[[i]], data = training)
  predictions <- predict(modelo, testing)
  rmse[i] <- RMSE(predictions, testing$lny)
}
# Revisar datos atipicos

modelo <- lm(form7, data = training)
predictions <- predict(modelo, testing)
testing$prediction_errors <- testing$lny - predictions
ggplot(testing, aes(x = prediction_errors)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of errores predichos",
       x = "Error predicho", y = "Frecuencia") +
  theme_minimal()
ggplot(testing, aes(x = prediction_errors)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of errores predichos",
       x = "Error predicho", y = "Frecuencia") +
  theme_minimal()
percentiles <- quantile(testing$prediction_errors, probs = c(0.01, 0.99))
outliers <- testing[testing$prediction_errors < percentiles[1] | testing$prediction_errors > percentiles[2], ]
head(outliers)

modelo <- lm(form8, data = training)
predictions <- predict(modelo, testing)
testing$prediction_errors <- testing$lny - predictions
ggplot(testing, aes(x = prediction_errors)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of errores predichos",
       x = "Error predicho", y = "Frecuencia") +
  theme_minimal()
ggplot(testing, aes(x = prediction_errors)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of errores predichos",
       x = "Error predicho", y = "Frecuencia") +
  theme_minimal()
percentiles <- quantile(testing$prediction_errors, probs = c(0.01, 0.99))
outliers <- testing[testing$prediction_errors < percentiles[1] | testing$prediction_errors > percentiles[2], ]
head(outliers)

#Realizamos el LOOCV

ctrl <- trainControl(method = "LOOCV")

modelo1c <- train(form8,
                  data = data,
                  method = 'lm', 
                  trControl= ctrl)


modelo2c <- train(form7,
                  data = data,
                  method = 'lm', 
                  trControl= ctrl)
modelo2c
