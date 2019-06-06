rm(list=ls())
setwd("C:/Users/DELL/Downloads/Multivariado proyecto")
library(readr)
Melbourne <- read_csv("Melbourne_housing_FULL.csv", 
                                   na = "NA")

#Limpiamos los datos
Melbourne$Address<-NULL
Melbourne$SellerG<-NULL
Melbourne$Date<-NULL
Melbourne$Postcode<-NULL
Melbourne$CouncilArea<-NULL
Melbourne$YearBuilt<-NULL
Melbourne$Propertycount<-NULL
Melbourne$Bedroom2<-NULL

#Eliminamos registros faltantes de precio de casas
Melbourne<-Melbourne[!is.na(Melbourne$Price),]

#Imputamos Landsize por la mediana por suburbio y descartamos los que no fue posible imputar
for (i in 1:nrow(Melbourne)){
  if(is.na(Melbourne$Landsize[i])){
    Melbourne$Landsize[i]<-median(Melbourne$Landsize[Melbourne$Suburb==Melbourne$Suburb[i]],na.rm=TRUE)
  }
}

Melbourne<-Melbourne[!is.na(Melbourne$Landsize),]

for (i in 1:nrow(Melbourne)){
  if (Melbourne$Landsize[i]<100){
    Melbourne$Landsize[i]=100
  }
}

#Imputamos Numero de baños por la mediana por suburbio
for (i in 1:nrow(Melbourne)){
  if(is.na(Melbourne$Bathroom[i])){
    Melbourne$Bathroom[i]<-median(Melbourne$Bathroom[Melbourne$Suburb==Melbourne$Suburb[i]],na.rm=TRUE)
  }
}


#Imputamos los datos faltantes en building Area usando la mediana por suburbio o una proporcion
#del tamaño del terreno
medians=list()
for (i in unique(Melbourne$Suburb)){
  if (sum(Melbourne$Suburb==i)>1){
  medians[[i]]<-median(Melbourne$BuildingArea[Melbourne$Suburb==i],na.rm=TRUE)
  }
  else{
    if (is.na(Melbourne$BuildingArea[Melbourne$Suburb==i])){
      set.seed(100)
      a<-sample(c(.4,.5,.6,.7,.8,.9),size=1)
      medians[[i]]<-a*Melbourne$Landsize[Melbourne$Suburb==i]
    }
    else{
      medians[[i]]<-Melbourne$BuildingArea[Melbourne$Suburb==i]
    }
  }
}

for (i in names(medians[is.na(medians)])){
  set.seed(100)
  a<-sample(c(.4,.5,.6,.7,.8,.9),size=1)
  medians[[i]]<-a*median(Melbourne$Landsize[Melbourne$Suburb==i])
}



for (i in 1:nrow(Melbourne)){
  if(is.na(Melbourne$BuildingArea[i])){
    Melbourne$BuildingArea[i]<-medians[[Melbourne$Suburb[i]]]
  }
}

#Imputamos los datos faltantes para los automoviles con la mediana por suburbio
for (i in 1:nrow(Melbourne)){
  if(is.na(Melbourne$Car[i])){
    Melbourne$Car[i]<-round(median(Melbourne$Car[Melbourne$Suburb==Melbourne$Suburb[i]],na.rm=TRUE),0)
  }
}


#write.csv(Melbourne, file = "Melbourne.csv")

#Checamos la distribucion de los precios
d1<-density(Melbourne$Price)
plot(d1,main="Distribución de precios",xlab="Precios",ylab="Densidad")
polygon(d1,col="red", border="blue")

d2<-density(log(Melbourne$Price))
plot(d2, main="Distribución de los log-precios",xlab="log-Precios",ylab="Densidad")
polygon(d2,col="red", border="blue")

qqnorm(log(Melbourne$Price),col="red",main="QQ-plot log-precios")
qqline(log(Melbourne$Price))

library("sm")


sm.density.compare(log(Melbourne$Price), as.factor(Melbourne$Regionname), xlab="Precios", ylab="Densidad")
title(main="Log-Precios por región")
cyl.f <- factor(Melbourne$Regionname, levels= unique(Melbourne$Regionname))
colfill<-c(2:(2+length(levels(cyl.f)))) 
plot(1:10,1:10,axes = FALSE, type="n",xlab="",ylab="")
legend(locator(1), levels(cyl.f), fill=colfill,box.lty=0)

#Ventas de casas por metodo de venta y tipo de vivienda
library("colorspace")

barplot(table(Melbourne$Method), main="Total de ventas por método", xlab="Método",ylab="Cantidad",col=rainbow_hcl(5))
barplot(table(Melbourne$Type), main="Ventas por tipo de vivienda", xlab="Tipo",ylab="Cantidad",col=rainbow_hcl(5),names.arg = c("House","Unit","Townhouse"))

#Ventas por tamaño de terreno expresadas en cuartiles centrales
set.seed(100)
a<-quantile(Melbourne$Landsize,probs=seq(0, 1, 0.25))
frecuencias<-rep(1,4)
for (i in 1:length(frecuencias)){
  frecuencias[i]<-sum(Melbourne$Landsize>a[[i]] & Melbourne$Landsize<=a[[i+1]])
}  

barplot(frecuencias, main="Ventas por tamaño de terreno", xlab="Tamaño del terreno en cuartiles centrales",ylab="Cantidad",col=rainbow_hcl(5),names.arg = c("<=1ero","1-2","2-3",">3er"))

#Aplicando regresión
new.df<-cbind(log(Melbourne$Price),Melbourne$Distance,Melbourne$Bathroom,Melbourne$Car, Melbourne$Landsize,Melbourne$BuildingArea,Melbourne$Rooms, model.matrix(~as.factor(Melbourne$Type)-1),model.matrix(~as.factor(Melbourne$Method)-1),model.matrix(~as.factor(Melbourne$Regionname)-1))
new.df<-new.df[,1:23]
new.df<-as.data.frame(new.df)
names(new.df)<-c("logPrice","Distance","Bathrooms", "Cars", "Landsize","BuildArea","Rooms", "TypeH", "TypeT", "TypeU","MethodPI", "MethodS","MethodSA", "MethodSP","MethodVB", "RegionEM", "RegionEV","RegionNM","RegionNV","RegionSEM","RegionSM","RegionWM","RegionWV")

modelo1<-lm(logPrice~.,data=new.df)
summary(modelo1)

modelo2<-lm(logPrice~.-TypeU-MethodSA-RegionWV-MethodVB,data=new.df)      #Este el bueno
summary(modelo2)
mean(modelo2$residuals^2) #Error cuadrado medio
plot(modelo2,1, sub="Modelo2")
d3<-density(modelo2$residuals)
plot(d3, main="Densidad de los residuos", ylab="Densidad")
polygon(d3,col="red", border="blue")

logPrice.pred<-predict(modelo2,new.df[,-1])

#Primeras 100 observaciones log-precios Vs log-precios predecidos
plot(new.df$logPrice[1:100],col="red",main="log-precios Vs predecidos",xlab="Indice",ylab="log-Precios")
points(logPrice.pred[1:100],col="blue")
segments(x0=1:100,y0=new.df$logPrice[1:100], x1=1:100,y1=logPrice.pred[1:100],
         col = par("fg"), lty = par("lty"), lwd = par("lwd"))
min(abs(logPrice.pred-new.df$logPrice))
median(abs(exp(logPrice.pred)-exp(new.df$logPrice)))
max(abs(exp(logPrice.pred)-exp(new.df$logPric)))


points_AirportWest<-Melbourne$Suburb=="Meadow Heights"
plot(main="Meadow Heights",new.df$logPrice[points_AirportWest],col="red", ylim=c(min(min(new.df$logPrice[points_AirportWest],logPrice.pred[points_AirportWest])),max(max(new.df$logPrice[points_AirportWest],logPrice.pred[points_AirportWest]))))
points(logPrice.pred[points_AirportWest],col="blue")
segments(x0=1:sum(points_AirportWest),y0=new.df$logPrice[points_AirportWest], x1=1:sum(points_AirportWest),y1=logPrice.pred[points_AirportWest],
         col = par("fg"), lty = par("lty"), lwd = par("lwd"))
median(abs(exp(logPrice.pred[points_AirportWest])-exp(new.df$logPrice[points_AirportWest])))

library(corrplot) #Correlaciones entre las variables elegidas
corrplot(cor(new.df),method="circle")

######## La version podada

library(readr)
MelbournePodado <- read_csv("C:/Users/DELL/Downloads/Multivariado proyecto/MelbournePodado.csv")

new.df2<-cbind(log(MelbournePodado$Price),MelbournePodado$Distance,MelbournePodado$Bathroom,MelbournePodado$Car, MelbournePodado$Landsize,MelbournePodado$BuildingArea,MelbournePodado$Rooms, model.matrix(~as.factor(MelbournePodado$Type)-1),model.matrix(~as.factor(MelbournePodado$Method)-1),model.matrix(~as.factor(MelbournePodado$Regionname)-1))
new.df2<-new.df2[,1:23]
new.df2<-as.data.frame(new.df2)
names(new.df2)<-c("logPrice","Distance","Bathrooms", "Cars", "Landsize","BuildArea","Rooms", "TypeH", "TypeT", "TypeU","MethodPI", "MethodS","MethodSA", "MethodSP","MethodVB", "RegionEM", "RegionEV","RegionNM","RegionNV","RegionSEM","RegionSM","RegionWM","RegionWV")

modeloPodado<-lm(logPrice~.-TypeU-MethodSA-RegionWV-MethodVB,data=new.df2)      #Este el bueno
summary(modeloPodado)
mean(modeloPodado$residuals^2) #Error cuadrado medio

logPrice.predPodado<-predict(modeloPodado,new.df2[,-1])

min(abs(logPrice.predPodado-new.df2$logPrice))
median(abs(exp(logPrice.predPodado)-exp(new.df2$logPrice)))
max(abs(exp(logPrice.predPodado)-exp(new.df2$logPrice)))
