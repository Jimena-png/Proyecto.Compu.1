#tarea 2 jimena y francini

setwd("C:/Users/jimen/Downloads")

install.packages("readr")
install.packages("labelled")
install.packages("tidyverse")
install.packages("haven")
install.packages("dplyr")
library(labelled)
library(tidyverse)
library(haven)
library(dplyr)
library(readr)


NHANES <- read_csv("NHANES.csv")
#1. 

head(NHANES)
str(NHANES)
View(NHANES)

#2. 
nhanes.reducida<-NHANES%>%select(SEQN,SMQ020,RIAGENDR,RIDAGEYR,DMDEDUC2,
                      BMXWT,BMXHT,BMXBMI)

attach(nhanes.reducida)


#3
colnames(nhanes.reducida)<-c("seqn", "smoking","gender","age", "education", "weight" ,"height", "bmi")
mutate(nhanes.reducida,seqn=SEQN,smoking=SMQ020,gender=RIAGENDR,age=RIDAGEYR,education=DMDEDUC2,weight=BMXWT,height=BMXHT,bmi=BMXBMI)


#4
#numeros de duplicados
anyDuplicated(nhanes.reducida,fromLast = TRUE)

#removiendo NA's
nhanes.reducida<-nhanes.reducida[complete.cases(nhanes.reducida), ]


#valores extremos 
attach(nhanes.reducida)

Q1.age<-quantile(age,0.25)
Q3.age<-quantile(age,0.75)
RIQ.age<-Q3.age - Q1.age

outliers.age<-which(age < (Q1.age - 1.5 * RIQ.age) | age > (Q3.age + 1.5 * RIQ.age))#posiciones de valores extremos

age.values<-age[outliers.age] #valores extremos
age.values

Q1.weight<-quantile(weight,0.25)
Q3.weight<-quantile(weight,0.75)
RIQ.weight<-Q3.weight- Q1.weight

outliers.weight<-which(weight< (Q1.weight - 1.5 * RIQ.weight) | weight > (Q3.weight + 1.5 * RIQ.weight))#posiciones de valores extremos

weight.values<-weight[outliers.weight]#valores extremos
weight.values

Q1.height<-quantile(height,0.25)
Q3.height<-quantile(height,0.75)
RIQ.height<-Q3.height - Q1.height

outliers.height<-which(height< (Q1.height - 1.5 * RIQ.height) | height > (Q3.height + 1.5 * RIQ.height))#posiciones de valores extremos

height.values<-height[outliers.height]#valores extremos
height.values

Q1.bmi<-quantile(bmi,0.25)
Q3.bmi<-quantile(bmi,0.75)
RIQ.bim<-Q3.bmi - Q1.bmi

outliers.bmi<-which((bmi< Q1.bmi - 1.5 * RIQ.bim) | (bmi > Q3.bmi + 1.5 * RIQ.bim))#posiciones de valores extremos

bmi.values<-bmi[outliers.bmi]#valores extremos
bmi.values



#5
hist(age, main="Edad", xlab= "Años", ylab="Frecuencia", xlim= c(0, 100), col= "lightblue")
#R/ el histograma de la edad nos muestra que no hay ningun tipo de distribucion
#ya que esta es una variable que tiene un distribucion muy constante 


hist(weight, main="Peso", xlab= "Kilos",  ylab="Frecuencia", xlim= c(0, 250), col= "lightblue")
#R/ la mayoría de las personas están en el peso de entre 50kg y los 100kg, y que tiene
#una asimetría positiva.

hist(height, main="Estatura", xlab= "Centímetros", ylab="Frecuencia", col= "lightblue")
#R/ la altura de las personas de Estados Unidos está de entre el 1.40mts y 1.90mts, asimismo,
#se puede ver que una distribución más o menos normal.

hist(bmi, main="Indice de Masa Corporal", xlab= "Valor del IMC", xlim= c(0, 90), col= "lightblue")
#R/ el indice de masa muscular de las personas de Estados Unidos está entre los 10kg y 50kg 
#con una moda de más o menos de 27kg, asimismo tiene una asimetría positiva.

#6
boxplot(age, main="Edad", xlab= "Años", ylab="Frecuencia", col="lightpink", border="red",
        horizontal=TRUE, notch=TRUE)
#El diagrama de cajas anterior muestra que la mediana es 49, así como que el percentil 25% 
#aproximadamente es 35, y el percentil 75% es aproximadamente 65, por ende, la mayor concentracion
#de los datos está entre 35 y 65.


boxplot(weight, main="Peso", xlab= "Kilos", ylab= "Frecuencia", col="lightpink", border= "red",
        horizontal=TRUE, notch=TRUE)
#Se demuestra que la mediana es aproximadamente 78,asi como que el percentil 25% es 66 y el 
#percentil 75% es aproximadamente es 93, asimismo, se observa como existen gran cantidad de
#valores atípicos que que se alejan considerablemente de la tendencia central y la variabilidad general


boxplot(height, main="Estatura", xlab= "Centímetros", ylab= "Frecuencia", col="lightpink", border= "red",
        horizontal=TRUE, notch=TRUE)
#Se demuestra que la mediana es aproximadamente 165, qu eel percentil 25% es de más o menos 158 y
#el percentil 75% de aproximadamente 174, asimismo se observa la presencia de cuatro valores
#atipicos, pues se ubican más allá de los valores extremos del diagrama de cajas.


boxplot(bmi,  main="Indice de Masa Corporal", xlab= "Valor del IMC", ylab= "Frecuencia", col="lightpink", border= "red",
        horizontal=TRUE, notch=TRUE)
#La mediana es de aproximadamente 28, el percentil 25% de más o menos 25 y el percentil 75%
#tiene un valor aporximado a 33, asimismo se observa la presencia de gran cantidad de valores extremos
# que se mantienen alejados de manera considerable de la tendencia central de los datos de bmi.
 
#7
age.values
bmi.values
height.values
weight.values

#8
clean.age<-age[-outliers.age]
boxplot(clean.age, main="Edad", xlab= "Años", ylab="Frecuencia", col="lightpink", border="red",
        horizontal=TRUE, notch=TRUE)
#R/   El grafico no se genera por la falta de valores extremos y se confirma en el punto 4 y 7que no hay valores 
#extremos para esta variable. 


clean.weight<-weight[-outliers.weight]
boxplot(clean.weight, main="Peso", xlab= "Kilos", ylab= "Frecuencia", col="lightpink", border= "red",
        horizontal=TRUE, notch=TRUE)
#R/Aqui podemos observar como en los datos podemos suponer normalidad, la mediana se encuentra en 80kg. 

clean.height<-height[-outliers.height]
boxplot(clean.height, main="Estatura", xlab= "Centímetros", ylab= "Frecuencia", col="lightpink", border= "red",
        horizontal=TRUE, notch=TRUE)
#R/ podemos ver como se distribuyen los graficos sin los valores extremos, y estos tienen una distribucion normal.

clean.bmi<-bmi[-outliers.bmi]
boxplot(clean.bmi,  main="Indice de Masa Corporal", xlab= "Valor del IMC", ylab= "Frecuencia", col="lightpink", border= "red",
        horizontal=TRUE, notch=TRUE)
#R/la mediana se encuentra aprox en 28kg y el 25% de los datos son menores a 24y el 75% de los datos son mayores a 32kg.  

#9
nhanes.reducida <- nhanes.reducida %>% 
  mutate(smoking = recode(smoking, 
                          "1" = "yes", 
                          "2" = "no", 
                          "7" = "na", 
                          "9" = "na"))

#10
nhanes.reducida <- nhanes.reducida %>% 
  mutate(gender = recode(gender, 
                          "1" = "male", 
                          "2" = "female"))
#11

# Crear variables "dummy"
nhanes.reducida <- nhanes.reducida %>% 
  mutate(yes_smoking = factor(smoking == "yes", levels = c(FALSE, TRUE), labels = c("No", "Yes")),
         no_smoking = factor(smoking == "no", levels = c(FALSE, TRUE), labels = c("No", "Yes")),
         na_smoking = factor(smoking == "na", levels = c(FALSE, TRUE), labels = c("No", "Yes")))





