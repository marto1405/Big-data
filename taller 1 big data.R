
# Librerias ---------------------------------------------------------------

library("pacman")
p_load(rio,
       tidyverse, 
       skimr, 
       visdat, 
       corrplot, 
       stargazer,
       visdat,
       dplyr,
       rvest,
       caret,
       MASS)

# Importar datos ----------------------------------------------------------

base_url<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
html<-".html"

list <- lapply(1:10, function(i) {
  my_url <- paste(base_url, i, html, sep = "")
  my_html <- read_html(my_url)
  table <- my_html %>% html_table()
  df <- as.data.frame(table[[1]]) 
  return(df)
})

df<- bind_rows(list) 

df<- df %>%
     filter(age > 18 & dsi==0)

summary(df$y_ingLab_m)
summary(df$ingtot)    
summary(df$sex)

df<- df %>%
     mutate(lw= ifelse(ingtot > 0, log(ingtot), 0))
# Predecir el  salario  por edad  ---------------------------------------

linear_model<- lm(lw ~ age + I(age^2), data=df  )
df$predict<-predict(linear_model)

ggplot(df, aes(x=age, y=predict)) +
  theme_classic() + 
  theme(panel.grid.major = element_line(color = "gray"),
        plot.title = element_text(hjust = 0.5))+
  geom_point(colour="red",size=3) +
  labs(y="Predicciones del logaritmo del salario", x="edad")+
  ggtitle("Precicciones del salario por edad")

coefS <- coef(linear_model)
print(names(coefS))
p_age <- -coefS["age"] / (2 * coefS["I(age^2)"])
p_age

peakage <-peakage <- function(data, indices) {
  d <- data[indices, ]  
  linear_model <- lm(lw ~ age + I(age^2), data = d)  
  coefs <- coef(linear_model)
  coefs <- -coefs["age"] / (2 * coefs["I(age^2)"]) 
  return(coefs)
}

set.seed(11052004)
rboot<-boot(df,peakage, R = 1000)
boot.ci(rboot, type = "perc")

###Diferencias de genero (4) ----------------------------------------------------

model3 <- lm(lw ~ sex, data = df)
summary(model3)

stargazer(model3, type = "text", title = "Regresión de Log(Wage) sobre Female", 
          covariate.labels = c("male"), 
          dep.var.labels = "Log(Wage)", 
          omit.stat = c("f", "ser", "adj.rsq"))

####Usarenmos FWL

# Regresión de log(wage) sobre los controles

#Filtrar datos completos para las variables usadas en el modelo
df_complete <- df %>%
  filter(complete.cases(age, clase, college, cotPension, cuentaPropia, formal, 
                        maxEducLevel, informal, microEmpresa, sizeFirm, totalHoursWorked))

# Ajustar el modelo de regresión de log(wage) sobre los controles en los datos completos
modelc1_ <- lm(lw ~ age + clase + college + cotPension + cuentaPropia + formal + 
                         maxEducLevel + informal + microEmpresa + sizeFirm + totalHoursWorked, data = df_complete)

# Extraer los residuos de esta regresión y añadir al dataframe completo
df_complete <- df_complete %>%
  mutate(residuos_y = resid(modelc1))

# Ajustar el modelo de regresión de género sobre los controles en los datos completfs
modelc2 <- lm(sex ~ age + clase + college + cotPension + cuentaPropia + formal + 
                         maxEducLevel + informal + microEmpresa + sizeFirm + totalHoursWorked, data = db_complete)

# Extraer los residuos de la regresión de género y añadir al dataframe completo
df_complete <- df_complete %>%
  mutate(residuos_genero = resid(modelc2))

# Gender gap con controles
modelo_fwl_gender <- lm(residuos_y ~ residuos_genero, data = db_complete)


# Resumen del modelo FWL controlado
stargazer(modelo_fwl_gender, type = "text", title = "Modelo FWL con controles adicionales",
          out = "modelo_fwl_controlado.txt")
