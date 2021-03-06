#############################################################
# Obtenci�n de datos y preparaci�n ##########################
#############################################################

# Ruta del archivo
file_path <- "C:\\Users\\diego\\OneDrive\\Documentos\\UNAM\\Cursos\\Data Mining\\R\\Estaciones.txt"

# Obtener los datos del archivo
estaciones <- read.table(file_path, sep = "\t",header = T)

# Obtener las distancias
distancias <- round(dist(estaciones[2:50], method='euclidean'),2)

# Obtener la cantidad de grupos adecuada a trav�s del m�todo del codo
set.seed(123)
SSE <- sapply(2:12,
              function(k) {
                kmeans(distancias[2:50], k, nstart=25)$tot.withinss
              })
plot(2:12, SSE, type = "b", pch = 19, frame = FALSE, 
     xlab = "Numero de grupos", ylab = "Suma total de cuadrados")
abline(v = 5, lty =2)
# Los grupos recomendados de usar fueron 5

#############################################################
# Formaci�n de grupos #######################################
#############################################################

# Jer�rquico
jerarquico <- hclust(distancias)
plot(jerarquico)
rect.hclust(jerarquico, k = 5, border = 2:6)

# Particional
particional <- kmeans(estaciones[2:50], centers = 5, iter.max = 20)
cluster::clusplot(estaciones[2:50], particional$cluster, color=T, 
                  shade=T, main='Estaciones')

# Lista del numero de la columna que pertenecen a las precipitaciones de todos los
# meses
precipitaciones <- c()
for (i in seq_along(sample(12))) {precipitaciones[i] <- 4*(i-1)+3}

#Lista del n�mero de la columna de la temperatura observada de todos los 
# meses
T_observada <- precipitaciones+1

# Lista del n�mero de la columna de la temperatura m�xima de todos los meses
T_max <- T_observada+1

# Lista del n�mero de la columna de la temperatura m�nima de todos los meses
T_min <- T_max+1

# Inicializar el color en una lista de colores que tendr� en la grafica
colores <- c('green')

#############################################################
# Plots iniciales ###########################################
#############################################################

# Ploteo de la gr�fica de las precipitaciones de las estaciones climatologicas
# en todo el a�o
par(mar=c(5, 4, 4,10))
plot(1:12,unlist(estaciones[precipitaciones][1,]),
     type='l',col='green',xlab='Meses',ylab='Precipitacion', 
     ylim = range(0,330),lwd=2)

for (i in 2:nrow(estaciones)){
  lines(1:12,unlist(estaciones[precipitaciones][i,]), col=colors()[i*14+3],lwd=2)
  colores[i]<-colors()[i*10+5]
}
title('Precipitaciones por mes en las estaciones')
legend(13,350,legend=unlist(estaciones[1]), col=colores,
       inset = c(0,0),ncol=3,lwd=2,cex=0.5,xpd = TRUE)



# Ploteo de la gr�fica de la temperatura observada de las estaciones 
# climatologicas en todo el a�o
colores <- c('green')
par(mar=c(5, 4, 4,10))
plot(1:12,unlist(estaciones[T_observada][1,]),
     type='l',col='green',xlab='Meses',ylab='Temp observada', 
     ylim = range(0,30),lwd=2)

for (i in 2:nrow(estaciones)){
  lines(1:12,unlist(estaciones[T_observada][i,]), col=colors()[i*14+3],lwd=2)
  colores[i]<-colors()[i*10+5]
}
title('Temp observada por mes en las estaciones')
legend(13,30,legend=unlist(estaciones[1]), col=colores,
       inset = c(0,0),ncol=3,lwd=2,cex=0.5,xpd = TRUE)



# Ploteo de la gr�fica de la temperatura m�xima de las estaciones climatologicas
# en todo el a�o
colores <- c('green')
par(mar=c(5, 4, 4,10))
plot(1:12,unlist(estaciones[T_max][1,]),
     type='l',col='green',xlab='Meses',ylab='Temp max', 
     ylim = range(15,45),lwd=2)

for (i in 2:nrow(estaciones)){
  lines(1:12,unlist(estaciones[T_max][i,]), col=colors()[i*14+3],lwd=2)
  colores[i]<-colors()[i*10+5]
}
title('Temp max por mes en las estaciones')
legend(13,50,legend=unlist(estaciones[1]), col=colores,
       inset = c(0,0),ncol=3,lwd=2,cex=0.5,xpd = TRUE)



# Ploteo de la gr�fica de la temperatura m�nima de las estaciones climatologicas
# en todo el a�o
colores <- c('green')
par(mar=c(5, 4, 4,10))
plot(1:12,unlist(estaciones[T_min][1,]),
     type='l',col='green',xlab='Meses',ylab='Temp min', 
     ylim = range(0,30),lwd=2)

for (i in 2:nrow(estaciones)){
  lines(1:12,unlist(estaciones[T_min][i,]), col=colors()[i*14+3],lwd=2)
  colores[i]<-colors()[i*10+5]
}
title('Temp min por mes en las estaciones')
legend(13,30,legend=unlist(estaciones[1]), col=colores,
       inset = c(0,0),ncol=3,lwd=2,cex=0.5,xpd = TRUE)


#############################################################
# An�lisis de los clusters ##################################
#############################################################
clusters <- particional$centers
clusters


# Ploteo de la gr�fica de las precipitaciones de los clusters
# en todo el a�o
# Se restar� 1 a todas las listas como "precipitaciones" porque "clusters"
# tiene una columna menos, la de 'Estacion'
plot(1:12,unlist(clusters[1,precipitaciones-1]),
     type='l',col='green',xlab='Meses',ylab='Precipitacion', 
     ylim = range(0,250),lwd=2)
lines(1:12,unlist(clusters[2,precipitaciones-1]), col='black',lwd=2)
lines(1:12,unlist(clusters[3,precipitaciones-1]), col='orange',lwd=2)
lines(1:12,unlist(clusters[4,precipitaciones-1]), col='red',lwd=2)
lines(1:12,unlist(clusters[5,precipitaciones-1]), col='blue',lwd=2)

title('Precipitaciones por mes en los clusters')
legend(13,250,legend=c('cluster1','cluster2','cluster3','cluster4','cluster5'), 
       col=c('green','black','orange','red','blue'),
       inset = c(0,0),lwd=2,xpd = TRUE)


# Ploteo de la gr�fica de las precipitaciones de los clusters
# en todo el a�o
plot(1:12,unlist(clusters[1,T_min-1]),
     type='l',col='green',xlab='Meses',ylab='Temp min', 
     ylim = range(0,30),lwd=2)
lines(1:12,unlist(clusters[2,T_min-1]), col='black',lwd=2)
lines(1:12,unlist(clusters[3,T_min-1]), col='orange',lwd=2)
lines(1:12,unlist(clusters[4,T_min-1]), col='red',lwd=2)
lines(1:12,unlist(clusters[5,T_min-1]), col='blue',lwd=2)

title('Temperatura m�nima en los clusters')
legend(13,30,legend=c('cluster1','cluster2','cluster3','cluster4','cluster5'), 
       col=c('green','black','orange','red','blue'),
       inset = c(0,0),lwd=2,xpd = TRUE)


# Ploteo de la gr�fica de las precipitaciones de los clusters
# en todo el a�o
plot(1:12,unlist(clusters[1,T_max-1]),
     type='l',col='green',xlab='Meses',ylab='Temp max', 
     ylim = range(0,45),lwd=2)
lines(1:12,unlist(clusters[2,T_max-1]), col='black',lwd=2)
lines(1:12,unlist(clusters[3,T_max-1]), col='orange',lwd=2)
lines(1:12,unlist(clusters[4,T_max-1]), col='red',lwd=2)
lines(1:12,unlist(clusters[5,T_max-1]), col='blue',lwd=2)

title('Temperatura m�xima en los clusters')
legend(13,45,legend=c('cluster1','cluster2','cluster3','cluster4','cluster5'), 
       col=c('green','black','orange','red','blue'),
       inset = c(0,0),lwd=2,xpd = TRUE)


# Datos de los elementos de los clusters ######################
for (i in 1:5) {
  estaciones[particional$cluster == i,]
  
  print(paste("Cluster ",i))
  #Precipitaci�n m�nima del cluster i
  print(paste('Precipitaciones minimas del cluster ',i))
  print(min((estaciones[particional$cluster == i,precipitaciones])))
  
  #Precipitaci�n m�xima del cluster i
  print(paste('Precipitaciones m�ximas del cluster ',i))
  print(max((estaciones[particional$cluster == i,precipitaciones])))
  
  #Precipitaci�n media del cluster i
  print(paste('Precipitaciones medias del cluster ',i))
  print(mean(unlist(estaciones[particional$cluster == i,precipitaciones])))
  
  #Temperatura media del cluster i
  print(paste('Temperaturas medias del cluster ',i))
  print(mean(unlist(estaciones[particional$cluster == i,T_observada])))
  
  #Temperatura m�xima media del cluster i
  print(paste('Temperaturas m�ximas del cluster ',i))
  print(mean(unlist(estaciones[particional$cluster == i,T_max])))
  
  #Temperatura m�nima media del cluster i
  print(paste('Temperaturas m�nimas del cluster ',i))
  print(mean(unlist(estaciones[particional$cluster == i,T_min])))
  
  print("")
}
