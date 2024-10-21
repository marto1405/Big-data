library(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       glmnet, # To implement regularization algorithms. 
       caret, # creating predictive models
       stargazer,
       ranger
)
rm(list = ls())

setwd("D:/clases/Machine learning para economia aplicada/taller2/data")

train <- read.csv("train_h.csv")

test<- read.csv("test_h.csv")

train <- train %>%mutate(Pobre_c = ifelse(Pobre == 1, "Pobre", "NoPobre"))
table(train$Pobre)
test_h <- read.csv("test_hogares.csv")
EN_1 <- test_h %>% select(id)

# Función para calcular el F1 Score
F1_Score <- function(true_labels, predicted_labels, positive_class) {
  # Convertimos los labels a factores para asegurar consistencia
  true_labels <- factor(true_labels, levels = c("NoPobre", "Pobre"))
  predicted_labels <- factor(predicted_labels, levels = c("NoPobre", "Pobre"))
  
  # Matriz de confusión
  cm <- table(true_labels, predicted_labels)
  
  # Extraemos los valores de la matriz de confusión
  tp <- cm[positive_class, positive_class]  # Verdaderos positivos
  fp <- sum(cm[, positive_class]) - tp  # Falsos positivos
  fn <- sum(cm[positive_class, ]) - tp  # Falsos negativos
  

  precision <- tp / (tp + fp + 1e-10)
  recall <- tp / (tp + fn + 1e-10)
  
  # Calculamos F1
  f1 <- 2 * (precision * recall) / (precision + recall + 1e-10)
  return(f1)
}

# Función de evaluación para caret, que devuelve el F1 Score
f1Score <- function(data, lev = NULL, model = NULL) {
  # Extraemos las predicciones y las observaciones
  f1 <- F1_Score(data$obs, data$pred, positive_class = "Pobre")
  
  # Devolvemos el F1 como una lista con el nombre "F1"
  c(F1 = f1)
}



table(train$Pobre_c)
#######
###Modelo basico (logit)
######

train <- train %>%mutate(Pobre_c = ifelse(Pobre == 1, "Pobre", "NoPobre"))


M1 <- glm(Pobre ~ Edad + Sexo + Dinero_Otros_Hogares + Pagos_Arriendos +
            Ingreso_Desempleado_Otro + Afiliacion_Seguridad_Social +
            Horas_Trabajo_Normales + Menos_100 + Mas_100 +
            Regimen_contributivo + Regimen_especial +  
            Cotizando_Pensiones + max_educbasica + max_educsuperior + n_hijos +
            cuartos_totales + dormitorios + Nper,
          data = train, family = "binomial")


summary(M1)

# Control de entrenamiento con validación cruzada y F1
set.seed(123)
train_control <- trainControl(method = "cv",         # Validación cruzada
                              number = 10,           # 10 folds
                              classProbs = TRUE,     # Calcular probabilidades
                              savePredictions = TRUE,
                              summaryFunction = f1Score)  # Evaluar usando F1


# Entrenar el modelo usando caret (regresión logística)
M1_caret <- train(Pobre_c ~ Edad + Sexo + Dinero_Otros_Hogares + Pagos_Arriendos +
                    Ingreso_Desempleado_Otro + Afiliacion_Seguridad_Social +
                    Horas_Trabajo_Normales + Menos_100 + Mas_100 +
                    Regimen_contributivo + Regimen_especial +  
                    Cotizando_Pensiones + max_educbasica + max_educsuperior + n_hijos +
                    cuartos_totales + dormitorios + Nper+Depto,
                  data = train, 
                  method = "glm", 
                  family = binomial,
                  trControl = train_control)

# Imprimir resultados del modelo
print(M1_caret)


####
#Elastic net
###
library(glmnet)  # Necesario para Elastic Net

# Definir el control de entrenamiento con validación cruzada y F1
set.seed(123)
train_control <- trainControl(method = "cv",         # Validación cruzada
                              number = 10,           # 10 folds
                              classProbs = TRUE,     # Calcular probabilidades
                              savePredictions = TRUE, # Guardar predicciones
                              summaryFunction = f1Score)  # Evaluar usando F1

# Definir una rejilla de búsqueda para 'alpha' y 'lambda'
tune_grid <- expand.grid(alpha = seq(0, 1, length = 5),  # Combinación de Ridge y Lasso
                         lambda = 10^seq(-3, 1, length = 10))  # Diferentes valores de lambda

# Entrenar el modelo usando caret con Elastic Net (glmnet)
M1_elastic_net <- train(Pobre_c ~ Edad + Sexo + Dinero_Otros_Hogares + Pagos_Arriendos +
                          Ingreso_Desempleado_Otro + Afiliacion_Seguridad_Social +
                          Horas_Trabajo_Normales + Menos_100 + Mas_100 +
                          Regimen_contributivo + Regimen_especial +  
                          Cotizando_Pensiones + max_educbasica + max_educsuperior + n_hijos +
                          cuartos_totales + dormitorios + Nper,
                        data = train, 
                        method = "glmnet",  # Elastic Net usa 'glmnet'
                        family = "binomial",  # Modelo binomial para clasificación
                        trControl = train_control, 
                        tuneGrid = tune_grid)  # Grid search para 'alpha' y 'lambda'

# Imprimir los resultados del modelo entrenado
M1_elastic_net


predictSample <- test   %>% 
  mutate(pobre_lab = predict(M1_elastic_net, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

resultados <- merge( EN_1, predictSample, by = "id", all.x = TRUE)

skim(resultados)
# Ver el resultado
head(resultados)

table(resultados$pobre_lab)
predictSample<- resultados %>% 
  mutate(pobre=ifelse(pobre_lab=="Pobre",1,0)) %>% 
  select(id,pobre)
table(predictSample$pobre)
predictSample[is.na(predictSample)] <- 0
skim(predictSample)
name<- paste("EN_lambda", "0.001", "_alpha_" , "025", ".csv") 
write.csv(predictSample,name, row.names = FALSE)
####
#LDA
###
lda = train(Pobre_c~Edad + Sexo + Dinero_Otros_Hogares + Pagos_Arriendos +
              Ingreso_Desempleado_Otro + Afiliacion_Seguridad_Social +
              Horas_Trabajo_Normales + Menos_100 + Mas_100 +
              Regimen_contributivo + Regimen_especial +  
              Cotizando_Pensiones + max_educbasica + max_educsuperior + n_hijos +
              cuartos_totales + dormitorios + Nper, 
                    data=train, 
                    method="lda",
                    trControl = train_control)

lda

predictSample <- test   %>% 
  mutate(pobre_lab = predict(M1_ds, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

resultados <- merge( EN_1, predictSample, by = "id", all.x = TRUE)

skim(resultados)
# Ver el resultado
head(resultados)

table(resultados$pobre_lab)
predictSample<- resultados %>% 
  mutate(pobre=ifelse(pobre_lab=="Pobre",1,0)) %>% 
  select(id,pobre)
table(predictSample$pobre)
predictSample[is.na(predictSample)] <- 0
skim(predictSample)
name<- paste("logit", "dowsampling", ".csv") 
write.csv(predictSample,name, row.names = FALSE)



#############
#modelos con downsample
############

library(caret)

train$Pobre_c <- as.factor(train$Pobre_c)

#1 Realizar downsampling
set.seed(123)  # Para reproducibilidad
train_downsampled <- downSample(x = train[, -which(names(train) == "Pobre_c")], 
                                y = train$Pobre_c, 
                                yname = "Pobre_c")

table(train_downsampled$Pobre_c)



#LDA Dowsample
lda_ds = train(Pobre_c~Edad + Sexo + Dinero_Otros_Hogares + Pagos_Arriendos +
              Ingreso_Desempleado_Otro + Afiliacion_Seguridad_Social +
              Horas_Trabajo_Normales + Menos_100 + Mas_100 +
              Regimen_contributivo + Regimen_especial +  
              Cotizando_Pensiones + max_educbasica + max_educsuperior + n_hijos +
              cuartos_totales + dormitorios + Nper, 
            data=train_downsampled, 
            method="lda",
            trControl = train_control)

lda_ds

predictSample <- test   %>% 
  mutate(pobre_lab = predict(M1_ds, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

resultados <- merge( EN_1, predictSample, by = "id", all.x = TRUE)

skim(resultados)
# Ver el resultado
head(resultados)

table(resultados$pobre_lab)
predictSample<- resultados %>% 
  mutate(pobre=ifelse(pobre_lab=="Pobre",1,0)) %>% 
  select(id,pobre)
table(predictSample$pobre)
predictSample[is.na(predictSample)] <- 0
skim(predictSample)
name<- paste("LDA", "dowsampling", ".csv") 
write.csv(predictSample,name, row.names = FALSE)


###Logit ds

# Entrenar el modelo usando caret (regresión logística)
M1_ds <- train(Pobre_c ~ Edad + Sexo + Dinero_Otros_Hogares + Pagos_Arriendos +
                    Ingreso_Desempleado_Otro + Afiliacion_Seguridad_Social +
                    Horas_Trabajo_Normales + Menos_100 + Mas_100 +
                    Regimen_contributivo + Regimen_especial +  
                    Cotizando_Pensiones + max_educbasica + max_educsuperior + n_hijos +
                    cuartos_totales + dormitorios + Nper,
                  data = train_downsampled, 
                  method = "glm", 
                  family = binomial,
                  trControl = train_control)

# Imprimir resultados del modelo
print(M1_ds)

####
predictSample <- test   %>% 
  mutate(pobre_lab = predict(M1_ds, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

resultados <- merge( EN_1, predictSample, by = "id", all.x = TRUE)

skim(resultados)
# Ver el resultado
head(resultados)

table(resultados$pobre_lab)
predictSample<- resultados %>% 
  mutate(pobre=ifelse(pobre_lab=="Pobre",1,0)) %>% 
  select(id,pobre)
table(predictSample$pobre)
predictSample[is.na(predictSample)] <- 0
skim(predictSample)
name<- paste("logit", "dowsampling", ".csv") 
write.csv(predictSample,name, row.names = FALSE)

####

###Elastic net DS

EN_DS <- train(Pobre_c ~ Edad + Sexo + Dinero_Otros_Hogares + Pagos_Arriendos +
                          Ingreso_Desempleado_Otro + Afiliacion_Seguridad_Social +
                          Horas_Trabajo_Normales + Menos_100 + Mas_100 +
                          Regimen_contributivo + Regimen_especial +  
                          Cotizando_Pensiones + max_educbasica + max_educsuperior + n_hijos +
                          cuartos_totales + dormitorios + Nper,
                        data = train_downsampled, 
                        method = "glmnet",  
                        family = "binomial",  # Modelo binomial para clasificación
                        trControl = train_control, 
                        tuneGrid = tune_grid)  # Grid search para 'alpha' y 'lambda'
 
EN_DS


#####
#Random Forest
####

set.seed(1233)  # Para reproducibilidad
train$Pobre_c <- as.factor(train$Pobre_c)
train_downsampled <- downSample(x = train[, -which(names(train) == "Pobre_c")], 
                                y = train$Pobre_c, 
                                yname = "Pobre_c")

table(train_downsampled$Pobre_c)

ctrl<- trainControl(method = "cv",
                    number = 10,
                    summaryFunction = f1Score,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

mtry_grid <- expand.grid(mtry = c(2, 4, 6, 8, 10, 12),  # Valores de mtry dentro del rango permitido
                         min.node.size = c(1, 5, 10, 30, 50, 100),  # Tamaños mínimos de nodo
                         splitrule = 'gini')
RForest_ds <- train(Pobre_c ~ Edad + Sexo + Dinero_Otros_Hogares + Pagos_Arriendos +
                      Ingreso_Desempleado_Otro + Afiliacion_Seguridad_Social +
                      Horas_Trabajo_Normales + Menos_100 + Mas_100 +
                      Regimen_contributivo + Regimen_especial +  
                      Cotizando_Pensiones + max_educbasica + max_educsuperior + n_hijos +
                      cuartos_totales + dormitorios + Nper, 
                    data = train_downsampled, 
                    method = "ranger",
                    trControl = ctrl,
                    metric="ROC",
                    tuneGrid = mtry_grid,
                    ntree=500)

RForest_ds


importance_values <- varImp(RForest_ds)
print(importance_values)

# Guardar el modelo de Random Forest
save(RForest_ds, file = "modelo_random_forest.RData")

predictSample <- test   %>% 
  mutate(pobre_lab = predict(RForest_ds, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

predictSample <- read.csv("Xgboost_trees.csv")

resultados <- merge( EN_1, predictSample, by = "id", all.x = TRUE)

skim(resultados)
# Ver el resultado
head(resultados)

table(resultados$pobre_lab)
predictSample<- resultados %>% 
  mutate(pobre=ifelse(pobre_lab=="Pobre",1,0)) %>% 
  select(id,pobre)
table(predictSample$pobre)
predictSample[is.na(predictSample)] <- 0
skim(predictSample)
name<- paste("XGboost", ".csv") 
write.csv(predictSample,name, row.names = FALSE)