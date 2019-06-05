rm(list=ls())
library(tidyverse)
library(readxl)
data <- read_excel("Datos/Cuestionario.xlsx")
summary(data)
head(data)
# Ponemos nombres a las preguntas
nombres=seq(1,34)
nombres=paste("Pregunta", nombres, sep="")
colnames(data)=nombres
rm(nombres)
data=data %>% select(Pregunta1,Pregunta2,Pregunta3)



data=sapply(data, as.factor) %>% as.data.frame()
summary(data)
prueba=length(levels(data$Pregunta5))



library(arules)

rules=apriori(data, parameter=list(support=0.0000,confidence=0.1))

inspect(rules)


ruledf = data.frame(
  lhs = labels(lhs(rules))$elements,
  rhs = labels(rhs(rules))$elements, 
  rules@quality)


str(rules)

lhs=rules@lhs
rhs=rules@rhs
quality=rules@quality

quality
rules@lhs@itemInfo

datafr=as(rules, "data.frame")


reglasapriori[,2]=NULL

reglasapriori= reglasapriori %>% arrange(-lift)
