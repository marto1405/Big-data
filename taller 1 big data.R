
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


