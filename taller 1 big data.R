
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
     mutate(lw= ifelse(ingtot > 0, log(ingtot), NA))

linear_model<- lm(lw ~ age + I(age^2), data=df  )
summary(linear_model)
  
predict<- predict(linear_model)


