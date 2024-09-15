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

data <- read.table(file = "C:/Users/Esteban/Dropbox/Estudios/Big Data y Machine Learning/Talleres/Taller 1/data.csv",
                   header = TRUE, sep = "|")
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
p6510, p6545, p6580, p7495, p7500s2, p7500s3, p7505, p7510s3, p7510s5, p7510s6)

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
rm(denx,deny,x,y)

################################################################################
### Estadísticas descriptivas                                                ###
################################################################################

################################################################################
### 3. Age-wage profle                                                       ###
################################################################################

data = data %>% mutate(lny = log(y_ingLab_m_ha))

lm1 = lm(lny ~ age + I(age^2), data = data)
stargazer(lm1, title="Punto 3", type="text", covariate.labels = 
            c("Age","Age2"), column.labels = "(1)", dep.var.labels = "Wage per hour",
          keep.stat = c("n", "rsq"))

ggplot(data=data,mapping = aes(y=lny,x="")) +
  theme_bw() + geom_boxplot() + ggtitle("") + 
  ylab("Ln salario por hora") + xlab("") +
  scale_y_continuous(labels = scales::comma, n.breaks = 6 )
quantile(data$lny, c(.75,.90, 0.95, 0.975,.99,1),na.rm = TRUE)

residuals <- resid(lm1)
ggplot(data=data, mapping = aes(x=residuals))+theme_bw()+geom_density()
summary(residuals)
data = data %>% mutate(m1_std_residuals = studres(lm1))
ggplot(data,aes(y=m1_std_residuals,x=age)) +
  geom_point()+theme_bw()+labs(x ="Observations",y ="Residuals",title="")
# el límite es entre 2 y 3
sum(data$m1_std_residuals<3 & data$m1_std_residuals>-3)
data2 = data %>% filter(m1_std_residuals<3 & m1_std_residuals>-3)
lm2 = lm(lny ~ age + I(age^2), data = data2)
stargazer(lm1, lm2, title="Punto 3", type="text", covariate.labels = 
            c("Age","Age2"), column.labels = c("(1)","(2)"), 
          dep.var.labels = "Wage per hour", keep.stat = c("n", "rsq"))

age_seq <- seq(min(data$age), max(data$age), by = 1)
predict_data <- data.frame(age = age_seq, age2 = age_seq^2)
predicted_wage <- predict(lm1, newdata = predict_data)

ggplot(data = data.frame(age = age_seq, wage = predicted_wage), aes(x = age, y = wage)) +
  geom_line(color = "blue") +
  labs(title = "", x = "Edad", y = "Salarios predichos") +
  theme_minimal()

# el pico máximo se da en:
b1 = coef(lm1)["age"]
b2 = coef(lm1)["I(age^2)"]
peak_age = -b1/(2*b2)
peak_age


# Function to calculate the peak age for a bootstrap sample
peak_age_function <- function(dt, indices) {
  d <- dt[indices, ]
  model <- lm(lny ~ age + I(age^2), data = d)
  beta1 <- coef(model)["age"]
  beta2 <- coef(model)["I(age^2)"]
  peak_age <- -beta1 / (2 * beta2)
  return(peak_age)
}

set.seed(123)
bootstrap_results <- boot(data = data, statistic = peak_age_function, R = 1000)
boot.ci(bootstrap_results, type = "perc")
rm(data2)
################################################################################
### 4. Gender earning GAP                                                    ###
################################################################################
data = data %>% mutate(female = ifelse(sex==1,0,1))
data$female = as_factor(data$female)

# (a)
mf1 = lm(lny ~ female, data = data)
stargazer(mf1, title="Punto 4", type="text", covariate.labels = 
            c("Female"), column.labels = "(1)", dep.var.labels = "Wage per hour",
          keep.stat = c("n", "rsq"))
#(b)
data$maxEducLevel = as.factor(data$maxEducLevel)
data$sizeFirm = factor(data$sizeFirm)
data$oficio = as_factor(data$oficio)
data$relab = as_factor(data$relab)
data$jefe = as_factor(data$jefe)
data$formal = as_factor(data$formal)
data$cuentaPropia = as_factor(data$cuentaPropia)

data = data %>% mutate(age2 = age^2,
                       f_age = (1-sex)*age,
                       f_age2 = (1-sex)*age^2)
# Primero se estima el modelo condicional a todos excepto las variables de interés
m4b1 = lm(lny ~ totalHoursWorked + maxEducLevel +
           sizeFirm + oficio + relab + jefe + formal, data = data)
res1 = resid(m4b1)
# variables de interés sobre los controles
data$female = as.numeric(data$female)
m4bx1 = lm(female ~ totalHoursWorked + maxEducLevel +
            sizeFirm + oficio + relab + jefe + formal, data = data)
resx1 = resid(m4bx1)
m4bx2 = lm(age ~ totalHoursWorked + maxEducLevel +
            sizeFirm + oficio + relab + jefe + formal, data = data)
resx2 = resid(m4bx2)
m4bx3 = lm(age2 ~ totalHoursWorked + maxEducLevel +
            sizeFirm + oficio + relab + jefe + formal, data = data)
resx3 = resid(m4bx3)
m4bx4 = lm(f_age ~ totalHoursWorked + maxEducLevel +
            sizeFirm + oficio + relab + jefe + formal, data = data)
resx4 = resid(m4bx4)
m4bx5 = lm(f_age2 ~ totalHoursWorked + maxEducLevel +
             sizeFirm + oficio + relab + jefe + formal, data = data)
resx5 = resid(m4bx5)

# residuales de 1 vs residuales de 2
m4b = lm(res1 ~ resx1 + resx2 + resx3 + resx4 + resx5)
summary(m4b)

data$female = as.factor(data$female)
reference = lm(lny ~ female + age + age2 + f_age + f_age2 + totalHoursWorked + maxEducLevel +
                 sizeFirm + oficio + relab + jefe + formal, data = data)
summary(reference) # perfecto
data$female = as.numeric(data$female)
# inforporando bootstrap
female_age_function <- function(dt, indices) {
  d <- dt[indices, ]
  m4b_1 = lm(lny ~ totalHoursWorked + maxEducLevel +
              sizeFirm + oficio + relab + jefe + formal, data = d)
  res_1 = resid(m4b_1)
  # variables de interés sobre los controles
  m4b_x1 = lm(female ~ totalHoursWorked + maxEducLevel +
               sizeFirm + oficio + relab + jefe + formal, data = d)
  res_x1 = resid(m4b_x1)
  m4b_x2 = lm(age ~ totalHoursWorked + maxEducLevel +
               sizeFirm + oficio + relab + jefe + formal, data = d)
  res_x2 = resid(m4b_x2)
  m4b_x3 = lm(age2 ~ totalHoursWorked + maxEducLevel +
               sizeFirm + oficio + relab + jefe + formal, data = d)
  res_x3 = resid(m4b_x3)
  m4b_x4 = lm(f_age ~ totalHoursWorked + maxEducLevel +
               sizeFirm + oficio + relab + jefe + formal, data = d)
  res_x4 = resid(m4b_x4)
  m4b_x5 = lm(f_age2 ~ totalHoursWorked + maxEducLevel +
               sizeFirm + oficio + relab + jefe + formal, data = d)
  res_x5 = resid(m4b_x5)
  
  # residuales de 1 vs residuales de 2
  mb = lm(res_1 ~ res_x1 + res_x2 + res_x3 + res_x4 + res_x5)
  return(coef(mb))
}

set.seed(123)
bootstrap_results_female <- boot(data = data, statistic = female_age_function, R = 1000)
print(bootstrap_results_female)
boot.ci(bootstrap_results_female, type = "perc")

# predicted wage

age_seq <- seq(min(data$age), max(data$age), by = 1)
new_data_male <- data.frame(age = age_seq, age2 = age^2, 
                            female = 0, f_age = (1-sex)*age,
                            f_age2 = (1-sex)*age^2 )  # Male (female = 0)
new_data_female <- data.frame(age = age_seq, female = 1)  # Female (female = 1)

predicted_wage_male <- predict(reference, newdata = new_data_male)
predicted_wage_female <- predict(reference, newdata = new_data_female)

plot_data <- data.frame(
  age = c(predicted_wage_male$age, age_seq),
  lnwage = c(predicted_wage_male, predicted_wage_female),
  gender = factor(rep(c("Male", "Female"), each = length(age_seq)))
)

ggplot(data = data.frame(age = age_seq, wage = predicted_wage), aes(x = age, y = wage)) +
  geom_line(color = "blue") +
  labs(title = "", x = "Edad", y = "Salarios predichos") +
  theme_minimal()
data$female = as.factor(data$female)
################################################################################
### 5. Predicting earning                                                    ###
################################################################################

