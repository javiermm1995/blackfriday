# Empezamos limpiando la memoria
rm(list=ls())
library(tidyverse)
data=read_csv(file="Datos/BlackFriday.csv", col_names=TRUE, )
summary(data)

# Convertimos caracter a factor

data=data %>% mutate(Gender=as.factor(Gender), 
                Stay_In_Current_City_Years=as.factor(Stay_In_Current_City_Years),
                City_Category=as.factor(City_Category), 
                Product_ID=as.factor(Product_ID), 
                Age=as.factor(Age))

# Comprobamos datos no repetidos
(nrow(unique(data))==nrow(data))

# Medimos el número de nulos en cada columna
nulos=function(x)  mean(is.na(x))
sapply(data, nulos)
rm(nulos)
# Vemos que falla la categoria 2 y 3

# Vamos a intentar entender la jerarquía de estos datos
jerarquia=data %>% select(Product_Category_1,Product_Category_2,Product_Category_3, Product_ID) %>% 
  unique() %>% arrange(Product_Category_1,Product_Category_2,Product_Category_3) %>% mutate_all(as.character)


jerarquia=jerarquia[which(complete.cases(jerarquia)),]
summary(jerarquia)
jerarquia

# Hacemos una jerarquia
library(data.tree)
library(treemap)
jerarquia$pathString=paste("Todo",jerarquia$Product_Category_1, jerarquia$Product_Category_2,jerarquia$Product_Category_3, sep = "/")
summary(jerarquia)
tree=as.Node(jerarquia[,])
print(tree)
rm(jerarquia,tree)

# Hacemos una segmentación de clientes RFM. 

summary(data)

clientes=data %>% group_by(User_ID, Age, Gender) %>% summarize(M=sum(Purchase)/100, F=n())

clientes %>% ggplot(aes(y=F, x=M, color=Gender))+ geom_point() + theme_classic()

clientes %>% ggplot(aes(x=M, fill=Gender ,color=Gender)) + geom_density(alpha=0.3)+ xlim(0,50000) + scale_x_log10() 

hist(data$Occupation)






