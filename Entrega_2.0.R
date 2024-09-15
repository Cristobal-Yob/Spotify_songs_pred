library(data.table)
library(skimr)
library(ggplot2)
library(corrplot)
library(FSelector)
library(tidyverse)
library(randomForest)


library(rpart)
library(rje)
library(leaps)
library(cowplot)
library(factoextra)
library(gridExtra)





setwd("C:\Users\crist\OneDrive\Desktop\Proyectos\Entrega 2\Entrega 2")
Sp <- read_csv("Spotify-2000 (3).csv")  #Se lee la base
Sp<-data.frame(Sp)
#Exploramos la base
summary(Sp)

#Se nota que no existen valores NA

#Se sigue trabajando la base
Sp <- Sp[,-1]
Sp$Top_Genre <-  as.factor(Sp$Top_Genre)
Sp$Artist <-  as.factor(Sp$Artist)
Sp$Title <-  as.factor(Sp$Title)

summary(Sp)

#Se dejan solo variables numericas para calcular la correlación de las variables 
Spnumeric <- Sp[,-1:-3] #Se dejan solo los datos numericos 
sum(duplicated(Sp)) #Por lo cual podemos notar que no existen duplicados


summary(Spnumeric)
skim(Sp)
#Se puede observar al ejecutar esta funcion que ninguna de las variables tiene algun dato faltante, todas tienen un ratio 
#de estar completas de 1, por lo tanto la base no tiene datos faltantes y no nos seria posible imputar ningun dato 
#Se procede a visualizar los datos mediante graficos

densidad<- ggplot(Spnumeric) +
  geom_density(aes(BPM, fill ="BPM", alpha = 0.1)) + 
  geom_density(aes(Energy, fill ="Energy", alpha = 0.1)) + 
  geom_density(aes(Danceability, fill ="Danceability", alpha = 0.1)) + 
  geom_density(aes(Loudness, fill ="Loudness", alpha = 0.1)) + 
  geom_density(aes(Liveness, fill ="Liveness", alpha = 0.1)) + 
  geom_density(aes(Valence, fill ="Valence", alpha = 0.1)) + 
  geom_density(aes(Acousticness, fill ="Acousticness", alpha = 0.1)) + 
  geom_density(aes(Speechiness, fill ="Speechiness", alpha = 0.1)) + 
  scale_x_continuous(name = "Year, BPM, Energy, Danceability, Loudness, Liveness, Valence, Duration, Acousticness, Speechiness and Popularity") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Year, BPM, Energy, Danceability, Loudness, Liveness, Valence, Duration, Acousticness, Speechiness and Popularity") +
  theme_bw() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        text = element_text(size = 10)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

densidad

trend_chart <- function(arg){
  trend_change <- Sp %>% filter(Year>1956) %>% group_by(Year) %>% summarize_at(vars(all_of(arg)), funs(Average = mean))
  chart<- ggplot(data = trend_change, aes(x = Year, y = Average)) + 
    geom_line(color = "#00AFBB", size = 1) +
    scale_x_continuous(breaks=seq(1956, 2018, 10)) + scale_y_continuous(name=paste("",arg,sep=""))
  return(chart)
}

trend_chart_track_popularity<-trend_chart("Popularity")
trend_chart_danceability<-trend_chart("Danceability")
trend_chart_energy<-trend_chart("Energy")
trend_chart_loudness<-trend_chart("Loudness")
trend_chart_duration_ms<-trend_chart("Duration")
trend_chart_speechiness<-trend_chart("Speechiness")

plot_grid(trend_chart_track_popularity, trend_chart_danceability, trend_chart_energy, trend_chart_loudness, trend_chart_duration_ms, trend_chart_speechiness,ncol = 2, label_size = 1)

#Estos graficos se analizara en el informe


#Matriz Correlaciones
correlacion <-round(cor(Spnumeric), 1)
corrplot(correlacion, method="circle", type="upper", bg="orange")
corrplot(correlacion, method="number", type="upper", bg="orange")

#Al analizar la matriz de correlaciones se puede nota que Energy y Acousticness tienen una correlacion negativa del 70 %
#Asi como tambien Loudness y Acoustiness tienen un acorrelacion negativa del 50%
#En la otra vereda se tiene a Energy con Loudness, las cuales tienen una correlacion positiva del 70%
#Y Danceability con Valence tienen una de correlacion de un 50%, por lo cual estas estan estrechamente relacionadas, y 
#cuando son positivas aumentan en la proporcion dicha y en caso contratio disminuye



#Analizando la correlacion de Pearson
weights <- linear.correlation(Popularity~., Spnumeric)
print(weights)
subset <- cutoff.k(weights, 5)
f <- as.simple.formula(subset, "Popularity")
print(f)
#Por metodo de correlacion de pearson se obtiene que los mas importantes son Loudness Year Danceability Liveness y Speechiness
#Luego de analizar los pesos se observa que eston son muy pequeños, por lo cual un modelo lineal no seria el mas 
#preciso para trabajar con estos datos, por lo que utilizaremos alguno de los tres mencionados en el trabajo 

#Popularity ~ Loudness + Danceability + Year + Liveness + Speechiness

#oneR
weights <- oneR(Popularity~., Sp)
print(weights)
subset <- cutoff.k(weights, 5)
f <- as.simple.formula(subset, "Popularity")
print(f)

#Popularity ~ Title + Artist + Top_Genre + Year + Loudness

#Por lo que segun OneR las variables que mejor podrian predecir el modelo son Title + Artist + Top_Genre + Year + Loudness

Sp.rf <- randomForest(Popularity ~., data=Spnumeric)
Sp.rf$importance
varImpPlot(Sp.rf)

#Lo quwe se puede ver graficamente que el años es la variable con mayor impotancia y posterior a esto Duration y Danceability
#Aquella con menor importancia eres Speechiness


evaluator <- function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(Spnumeric))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- Spnumeric[test.idx, , drop=FALSE]
    train <- Spnumeric[train.idx, , drop=FALSE]
    model <- lm(as.simple.formula(subset, "Popularity"), train)
    error.rate = sum((test$Popularity-predict(model, test))^2) / nrow(test)
    return(-error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}
subset <- backward.search(names(Spnumeric)[-5], evaluator)
f <- as.simple.formula(subset, "Popularity")
print(f)

#Popularity ~ Year + BPM + Energy + Danceability + Liveness + Valence + Duration + Speechiness

ref.full<-regsubsets(Popularity~.,data=Spnumeric,nvmax=11)
summary(ref.full)
plot(ref.full,scale = "bic")
plot(ref.full,scale = "Cp")
#Bic y Cp


#Clustering

cluster.input <- Sp[, c('Energy', 'Liveness','BPM', 'Speechiness', 'Acousticness',
                        'Danceability', 'Duration' ,'Loudness','Valence')]

# scale features for input to clustering
cluster.input.scaled <- scale(cluster.input[, c('Energy', 'Liveness','BPM', 'Speechiness', 'Acousticness',
                                                'Danceability', 'Duration' ,'Loudness','Valence')])

k2 <- kmeans(cluster.input.scaled, centers = 2, nstart = 25)
k3 <- kmeans(cluster.input.scaled, centers = 3, nstart = 25)
k4 <- kmeans(cluster.input.scaled, centers = 4, nstart = 25)
k5 <- kmeans(cluster.input.scaled, centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

fviz_nbclust(cluster.input[1:1000,], kmeans, method = "wss")

#Clusters 5 por lo cual 5 es el valor optimo del cluster


#Var
Sp_pca<-prcomp(Spnumeric,center = T,scale. = T)
var_Sp<-Sp_pca$sdev^2
ve<-var_Sp/sum(var_Sp)

plot(ve,xlab="principal componente",ylab="proportion var explaines",ylim=c(0,1),type="b")
plot(cumsum(ve),xlab = "princ comp",ylab="cum proportion of va",ylim = c(0,1),type="b")

summary(Sp_pca)

