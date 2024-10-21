require("pacman")
p_load( tidyverse,
        glmnet,
        caret,
        MLmetrics,
        pROC,
        doParallel,
        ranger)

setwd("C:/Users/Marto/Documents/big data")

train_hogares<-read.csv("C:/Users/Marto/Documents/big data/train_hogares.csv")
train_personas<-read.csv("C:/Users/Marto/Documents/big data/train_personas.csv")

table(train_hogares$Pobre)

colnames(train_hogares)

table(train_hogares$Pobre)

train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand)

train_hogares<- train_hogares %>% mutate(Pobre_hand_2=ifelse(Ingtotugarr<Lp*Npersug,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand_2)

colnames(train_personas)
new_hogar_variable <- train_personas  %>% 
  group_by(id) %>%
  summarize(h_Inactivos=sum(Ina,na.rm=TRUE),
            h_Pet= sum(Pet, na.rm = TRUE)   ) %>%
  mutate(h_Inactivosp=h_Inactivos/h_Pet ) %>%
  ungroup()

new_hogar_variable %>%  
  head()

test_hogares<-read.csv("C:/Users/Marto/Documents/big data/test_hogares.csv")
test_personas<-read.csv("C:/Users/Marto/Documents/big data/test_personas.csv")
summary(train_personas$P6040)
summary(test_personas$P6040)

summary(train_personas$P6210)
summary(test_personas$P6210)

summary(train_personas$Oc)
summary(test_personas$Oc)

summary(train_personas$P6100)
summary(test_personas$P6100)

pre_process_personas<-  function(data, ...) {
  
  data <- data %>%
    group_by(id) %>%
    mutate(
    mujer = ifelse(P6020==2,1,0), 
    H_Head = ifelse(P6050== 1, 1, 0), #Household head
    menor = ifelse(P6040<=16,1,0), # Menores
    EducLevel = ifelse(P6210==9,0,P6210), #Replace 9 with 0
    ocupado = ifelse(is.na(Oc),0,1), 
    subsidiado = ifelse(P6100==2,1,0),
    #BT=ifelse(P6240==2,1,0),
    #HX=ifelse(P6510==1,1,0),
    #CP=ifelse(P6920==2,0,1),
    #P=ifelse(P7500s2==1,1,0)
  ) %>% 
    filter(H_Head==1) %>% 
    select(id, Orden,mujer,H_Head,menor,EducLevel,ocupado)
  
  
}

train_personas<- pre_process_personas(train_personas)
test_personas<-  pre_process_personas(test_personas)


train_personas_nivel_hogar<- train_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(mujer,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            maxEducLevel=max(EducLevel,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE)
  )

train_personas_hogar<- train_personas %>% 
  filter(H_Head==1) %>% 
  left_join(train_personas_nivel_hogar)

test_personas_nivel_hogar<- test_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(mujer,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            maxEducLevel=max(EducLevel,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE)
  )


test_personas_hogar<- test_personas %>% 
  filter(H_Head==1) %>% 
  left_join(test_personas_nivel_hogar)


train_hogares<- train_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0),
         varriendo=ifelse(!is.na(P5130),P5130,P5140),
         nhab=P5000,
         nprh=P5010) %>% 
  select(id,Dominio,arrienda,Pobre,varriendo,nprh,nhab)

test_hogares<- test_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0),
         varriendo=ifelse(!is.na(P5130),P5130,P5140),
         nhab=P5000,
         nprh=P5010) %>% 
  select(id,Dominio,arrienda, varriendo,nprh,nhab) #note que pobre esta usente

train<- train_hogares %>% 
  left_join(train_personas_hogar) %>% 
  select(-id) #no longer need id

test<- test_hogares %>% 
  left_join(test_personas_hogar)

train<- train %>% 
  mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Yes")),
         Dominio=factor(Dominio),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria'))
  )

test<- test %>% 
  mutate(Dominio=factor(Dominio),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria'))
  )

#modelo 

mylogit <- glm(Pobre~., data = train, family = "binomial")
summary(mylogit,type="text")

#logit
ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)
set.seed(1410)
mylogit_caret <- train(Pobre~.,
                       data = train, 
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial")


mylogit_caret

pred <- predict(mylogit_caret, newdata=train, type="prob")[,2]

actual <- train$Pobre 

roc_obj_en <- roc(response = actual, 
                  predictor = pred, 
                  levels = c("No", "Yes"), 
                  direction = "<")
rfThresh_en <- coords(roc_obj_en, x = "best", best.method = "closest.topleft")
rfThresh_en

predictSample <- test %>%
  mutate(predictpobre = predict(mylogit_caret, newdata = test, type = "prob")[, "Yes"])
head(predictSample)

predictSample<- predictSample %>% mutate(pobre=factor(ifelse(predictpobre>=rfThresh_en$threshold,1,0),levels = c(1,0)))
predictSample<- predictSample%>% 
  select(id,pobre)
head(predictSample)

template <- read.csv("C:/Users/Marto/Documents/big data/sample_submission.csv")

head(template)

name<- paste0("Logit", ".csv") 






predictSample <- test   %>% 
  mutate(pobre_lab = predict(mylogit_caret, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)


predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)

template <- read.csv("C:/Users/Marto/Documents/big data/sample_submission.csv")

head(template)

name<- paste0("Logit", ".csv") 

write.csv(predictSample,name, row.names = FALSE)

ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    savePredictions = T)



set.seed(098063)

model1 <- train(Pobre~.,
                data=train,
                metric = "Accuracy",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.1),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)



set.seed(098063)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 

ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

model1 <- train(Pobre~.,
                data=train,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.1),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)

model1

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)

template <- read.csv("C:/Users/Marto/Documents/big data/sample_submission.csv")

head(template)

name<- paste0("EN_lambda_", "0001", "_alpha_" , "1", ".csv") 

write.csv(predictSample,name, row.names = FALSE)


set.seed(098063)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 

ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

model2 <- train(Pobre~.,
                data=train,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.1),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)

model2

pred <- predict(model2, newdata=train, type="prob")[,2]  # Probabilidades para "Yes"

# Extraer los valores reales de la variable de respuesta
actual <- train$Pobre  # Ajusta esto a tu variable binaria real

# Crear el objeto ROC
roc_obj_en <- roc(response = actual, 
                  predictor = pred, 
                  levels = c("No", "Yes"), 
                  direction = "<")

# Calcular el mejor umbral
rfThresh_en <- coords(roc_obj_en, x = "best", best.method = "closest.topleft")
rfThresh_en
predictSample <- predictSample %>%
  mutate(predictpobre = predict(model2, newdata = test, type = "prob")[, "Yes"])
head(predictSample)
predictSample<- predictSample  %>% mutate(pobre=factor(ifelse(predictpobre>=rfThresh_en$threshold,1,0),levels = c(1,0)))
predictSample<- predictSample %>% 
  select(id,pobre)
head(predictSample)

template <- read.csv("C:/Users/Marto/Documents/big data/sample_submission.csv")

head(template)

name<- paste0("EN_lambda_", "0001", "_alpha_" , "1", ".csv") 

write.csv(predictSample,name, row.names = FALSE)


#forma funcional 

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

X <- c("poly(arrienda,2,raw=TRUE)", "varriendo","mujer", "menor", 
       "EducLevel","ocupado",
       "poly(nprh,2,raw=TRUE)",
       "poly(varriendo,2,raw=TRUE)",
       "poly(nmenores,2,raw=TRUE)",
       "poly(nmujeres,2,raw=TRUE)")

set.seed(098063)

fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

model3 <- train(formula(paste0("Pobre ~", paste0(X, collapse = " + "))),
                data=train,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.1),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)

model3
stopCluster(cl)

pred <- predict(model3, newdata=train, type="prob")[,2]  # Probabilidades para "Yes"

# Extraer los valores reales de la variable de respuesta
actual <- train$Pobre  # Ajusta esto a tu variable binaria real

# Crear el objeto ROC
roc_obj_en <- roc(response = actual, 
                  predictor = pred, 
                  levels = c("No", "Yes"), 
                  direction = "<")

# Calcular el mejor umbral
rfThresh_en <- coords(roc_obj_en, x = "best", best.method = "closest.topleft")
rfThresh_en
predictSample <- test %>%
  mutate(predictpobre = predict(model3, newdata = test, type = "prob")[, "Yes"])
head(predictSample)
predictSample<- predictSample  %>% mutate(pobre=factor(ifelse(predictpobre>=rfThresh_en$threshold,1,0),levels = c(1,0)))
predictSample<- predictSample %>% 
  select(id,pobre)
head(predictSample)

template <- read.csv("C:/Users/Marto/Documents/big data/sample_submission.csv")

head(template)

name<- paste0("EN_lambda_", "0001", "_alpha_" , "08", ".csv") 

write.csv(predictSample,name, row.names = FALSE)


#otro modelo

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

X <- c("poly(arrienda,2,raw=TRUE)", "varriendo","mujer", "menor", 
       "poly(EducLevel,2,raw=TRUE)","ocupado","nocupados",
       "poly(nprh,2,raw=TRUE)","Dominio","H_Head",
       "poly(varriendo,2,raw=TRUE)",
       "poly(nmenores,2,raw=TRUE)",
       "poly(nmujeres,2,raw=TRUE)")

set.seed(098063)

fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

model4 <- train(formula(paste0("Pobre ~", paste0(X, collapse = " + "))),
                data=train,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.1),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)

model4
stopCluster(cl)

pred <- predict(model4, newdata=train, type="prob")[,2]  # Probabilidades para "Yes"

# Extraer los valores reales de la variable de respuesta
actual <- train$Pobre  # Ajusta esto a tu variable binaria real

# Crear el objeto ROC
roc_obj_en <- roc(response = actual, 
                  predictor = pred, 
                  levels = c("No", "Yes"), 
                  direction = "<")

# Calcular el mejor umbral
rfThresh_en <- coords(roc_obj_en, x = "best", best.method = "closest.topleft")
rfThresh_en
predictSample <- test %>%
  mutate(predictpobre = predict(model4, newdata = test, type = "prob")[, "Yes"])
head(predictSample)
predictSample<- predictSample  %>% mutate(pobre=factor(ifelse(predictpobre>=rfThresh_en$threshold,1,0),levels = c(1,0)))
predictSample<- predictSample %>% 
  select(id,pobre)
head(predictSample)

template <- read.csv("C:/Users/Marto/Documents/big data/sample_submission.csv")

head(template)

name<- paste0("EN_lambda_", "0001", "_alpha_" , "07", ".csv") 

write.csv(predictSample,name, row.names = FALSE)




#nuevos metodos
train<- train %>% 
  mutate(ithedu=EducLevel*H_Head,
         ivarocu=varriendo*ocupado)

test<- test %>% 
  mutate(ithedu=EducLevel*H_Head,
          ivarocu=varriendo*ocupado)

set.seed(098063)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 

ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

model5 <- train(Pobre~.,
                data=train,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.1),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)

model5

pred <- predict(model5, newdata=train, type="prob")[,2]  # Probabilidades para "Yes"

# Extraer los valores reales de la variable de respuesta
actual <- train$Pobre  # Ajusta esto a tu variable binaria real

# Crear el objeto ROC
roc_obj_en <- roc(response = actual, 
                  predictor = pred, 
                  levels = c("No", "Yes"), 
                  direction = "<")

# Calcular el mejor umbral
rfThresh_en <- coords(roc_obj_en, x = "best", best.method = "closest.topleft")
rfThresh_en
predictSample <- test %>%
  mutate(predictpobre = predict(model5, newdata = test, type = "prob")[, "Yes"])
head(predictSample)
predictSample<- predictSample  %>% mutate(pobre=factor(ifelse(predictpobre>=rfThresh_en$threshold,1,0),levels = c(1,0)))
predictSample<- predictSample %>% 
  select(id,pobre)
head(predictSample)

template <- read.csv("C:/Users/Marto/Documents/big data/sample_submission.csv")

head(template)

name<- paste0("EN_lambda_", "0001", "_alpha_" , "1", ".csv") 

write.csv(predictSample,name, row.names = FALSE)


train<- train %>% 
  mutate(perhab=nprh*nhab)

test<- test %>% 
  mutate(perhab=nprh*nhab)

set.seed(098063)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 

ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

model6 <- train(Pobre~.,
                data=train,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.1),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)

model6

pred <- predict(model6, newdata=train, type="prob")[,2]  # Probabilidades para "Yes"

# Extraer los valores reales de la variable de respuesta
actual <- train$Pobre  # Ajusta esto a tu variable binaria real

# Crear el objeto ROC
roc_obj_en <- roc(response = actual, 
                  predictor = pred, 
                  levels = c("No", "Yes"), 
                  direction = "<")

# Calcular el mejor umbral
rfThresh_en <- coords(roc_obj_en, x = "best", best.method = "closest.topleft")
rfThresh_en
predictSample <- test %>%
  mutate(predictpobre = predict(model6, newdata = test, type = "prob")[, "Yes"])
head(predictSample)
predictSample<- predictSample  %>% mutate(pobre=factor(ifelse(predictpobre>=rfThresh_en$threshold,1,0),levels = c(1,0)))
predictSample<- predictSample %>% 
  select(id,pobre)
head(predictSample)

template <- read.csv("C:/Users/Marto/Documents/big data/sample_submission.csv")

head(template)

name<- paste0("EN_lambda_", "0001", "_alpha_" , "1", ".csv") 

write.csv(predictSample,name, row.names = FALSE)













cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

X <- c("poly(arrienda,2,raw=TRUE)", "varriendo","mujer", "menor", 
       "poly(EducLevel,2,raw=TRUE)","ocupado","nocupados",
       "poly(nprh,2,raw=TRUE)","Dominio","H_Head",
       "poly(varriendo,2,raw=TRUE)",
       "ithedu","ivarocu",
       "nmenores","ithedu","ivarocu","perhab",
       "poly(nhab,2,raw=TRUE)",
       "nmujeres")

set.seed(098063)

fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 

ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

model7 <- train(formula(paste0("Pobre ~", paste0(X, collapse = " + "))),
                data=train,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                family="binomial",
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.1),
                  lambda =10^seq(-1, -3, length = 10)
                )
                
)

model7
stopCluster(cl)

pred <- predict(model7, newdata=train, type="prob")[,2]  # Probabilidades para "Yes"

# Extraer los valores reales de la variable de respuesta
actual <- train$Pobre  # Ajusta esto a tu variable binaria real

# Crear el objeto ROC
roc_obj_en <- roc(response = actual, 
                  predictor = pred, 
                  levels = c("No", "Yes"), 
                  direction = "<")

# Calcular el mejor umbral
rfThresh_en <- coords(roc_obj_en, x = "best", best.method = "closest.topleft")
rfThresh_en
predictSample <- test %>%
  mutate(predictpobre = predict(model7, newdata = test, type = "prob")[, "Yes"])
head(predictSample)
predictSample<- predictSample  %>% mutate(pobre=factor(ifelse(predictpobre>=rfThresh_en$threshold,1,0),levels = c(1,0)))
predictSample<- predictSample %>% 
  select(id,pobre)
head(predictSample)

template <- read.csv("C:/Users/Marto/Documents/big data/sample_submission.csv")

head(template)

name<- paste0("EN_lambda_", "0001", "_alpha_" , "08", ".csv") 

write.csv(predictSample,name, row.names = FALSE)



set.seed(91519) # importante ya que usaremos Bootstrap

bagged_tree<- ranger(Pobre~., 
                     data = train,
                     num.trees= 500, ## Número  arboles a estimar.  
                     mtry= 16,   #   usar todas las variables para Bagging.
                     min.node.size  = 1, ## Numero minimo de observaciones en un nodo para intentar 
) 
bagged_tree

bagged_pred <- predict(bagged_tree,
                       data = test, 
                       predict.all = TRUE # para obtener cuál es la predicción de cada árbol.
)

pred <- as.data.frame( bagged_pred$predictions )
dim(pred)

ntrees <- ncol( pred )
probabilities <- rowSums(pred == 2)/ ntrees

aucval_ipred <- Metrics::auc(actual = default,predicted =probabilities)
aucval_ipred

# Random forest

RF<- ranger(Pobre~., 
            data = train,
            num.trees= 500, ## Numero arboles a estimar. Default 500  
            mtry= 4,   # N. var aleatoriamente seleccionadas  
            min.node.size  = 1, ## Numero mínimo de observaciones en un nodo 
            importance="impurity") 
RF

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

mtry_grid<-expand.grid(mtry =c(2,4,6,8), # 8 inclueye bagging
                       min.node.size= c(1, 5, 10, 30, 50, 100), 
                       splitrule= 'gini') #splitrule constante
mtry_grid

set.seed(91519)
cv_RForest <- train(Pobre~., 
                    data = train, 
                    method = "ranger",
                    trControl = ctrl,
                    metric="F",
                    tuneGrid = mtry_grid,
                    ntree=500)

cv_RForest

cv_RForest$finalModel

rf_pred <- predict(cv_RForest, 
                   newdata = test, 
                   type="prob" ## class for class prediction
)

# Obtener las probabilidades para la clase "1" en el conjunto de entrenamiento
train_pred <- predict(cv_RForest, newdata = train, type = "prob")[,2]  # Probabilidades para "pobre"

# Crear el objeto ROC
roc_obj_rf <- roc(response = train$Pobre, 
                  predictor = train_pred, 
                  levels = c("No", "Yes"), 
                  direction = "<")

# Calcular el mejor umbral
rfThresh <- coords(roc_obj_rf, x = "best", best.method = "closest.topleft")
print(rfThresh)

# Aplicar el umbral para convertir probabilidades en predicciones binarias
predictSample <- test %>%
  mutate(pobre = ifelse(rf_pred[, 2] >= rfThresh$threshold, 1, 0))

# Ver las primeras filas del conjunto de prueba con la nueva columna "pobre"
head(predictSample)

# Seleccionar las columnas "id" y "pobre" para el archivo de salida
predictSample<- predictSample %>%
  select(id, pobre)

template <- read.csv("C:/Users/Marto/Documents/big data/sample_submission.csv")

head(template)

name<- paste0("Ramdom forest", ".csv") 

write.csv(predictSample,name, row.names = FALSE)


