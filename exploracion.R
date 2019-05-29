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
                Age=as.factor(Age),
                Marital_Status=as.factor(Marital_Status))

# Comprobamos datos no repetidos, la línea inferior da como resultado TRUE
#(nrow(unique(data))==nrow(data))

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

# Hacemos una segmentación de clientes RFM. Recency (No disponible), Frequency (nº de productos) y Monetary(sum)

summary(data)

clientes=data %>% group_by(User_ID, Age, Gender) %>% summarize(M=sum(Purchase)/100, F=n()) %>% ungroup()

# Tenemos 6000 clientes, les hacemos un scoring de 0 a 10

clientes=clientes %>% ungroup() %>% mutate(rankM=rank(M)/length(M), rankF=rank(F)/length(F)) %>% 
  arrange(rankM) %>% mutate(cumM=cumsum(M)) %>% arrange(rankF) %>% mutate(cumF=cumsum(F))


# Hacemos algún plot
clientes %>% ggplot(aes(y=F, x=M, color=Gender))+ geom_point() + theme_classic()

clientes %>% ggplot(aes(y=rankF, x=rankM))+ geom_point(alpha=0.05) + theme_classic()

clientes %>% ggplot(aes(x=rankF, y=cumM, color=Age))+ geom_point(alpha=0.2) + theme_classic()

clientes %>% ggplot(aes(x=rankM, y=cumF))+ geom_point(alpha=0.05) + theme_classic()

clientes %>% ggplot(aes(y=M, x=F))+ geom_point(alpha=0.05) + theme_classic()


clientes %>% ggplot(aes(x=M, fill=Gender ,color=Gender)) + geom_density(alpha=0.3)+ xlim(0,50000) + scale_x_log10() 

clientes %>% ggplot(aes(x=M,color=Age)) + geom_density(alpha=0.1, size=1)+ xlim(0,50000) + 
  scale_x_log10() + scale_color_brewer(palette="Spectral") + theme_classic()

clientes %>% ggplot(aes(x=Age,y=M, color=Age)) + geom_boxplot() +theme_classic()+  scale_y_log10()

# Continuamos con un análisis preliminar

summary(data)


# Cuántos productos compra cada cliente? La moda es 25, la media 91, la mediana 53

data %>% group_by(User_ID) %>% summarize(N=n()) %>% ggplot(aes(x=N)) + geom_histogram() + scale_x_log10()

data %>% group_by(User_ID) %>% summarize(N=n()) %>% ggplot(aes(x=N)) + geom_histogram() + xlim(0,100)

data %>% group_by(User_ID) %>% summarize(N=n()) %>% summary()


# Calculamos los productos que más ventas han tenido
prod = data %>% group_by(Product_ID,Precio=Purchase) %>% summarize(N=n()) %>% 
  ungroup()  %>% mutate(Ventas=Precio*N) %>% arrange(N) %>% mutate(N=as.factor(N))
prod

hist(prod$N)

summary(prod)

head(prod)

hist(prod$Ventas)

prod %>% ggplot(aes(x=N, y=Precio)) + geom_boxplot()

prod %>% mutate(N=as.numeric(N)) %>% group_by(N) %>% summarize (total=sum(N*Precio)) %>% 
  ungroup() %>% ggplot(aes(y=total, x=N)) + geom_bar(stat="identity") #+ scale_y_log10()
# A pesar de que cada usuario compra aproximadamente 25 cosas, 

# Vemos que productos se asocian más a otros productos

# Necesitamos convertir los productos en wide format
data %>% select(Product_ID) %>% distinct() %>% mutate(value=1) %>% summarize(sum(value)) # Tenemos 3623 productos distintos 

# Creamos una matriz de transacciones para utilizar con arules
transacciones=data %>% select(User_ID, Product_ID) %>% mutate(value=1) %>% 
  spread(Product_ID,value,fill=0) %>% select(-User_ID) %>% as.matrix()

# Lo incluimos en arules
library(arules)
reglas=apriori(transacciones, parameter=list(support=0.1,confidence=0.5))
reglas=eclat(transacciones, parameter=list(support=0.02, minlen=4, target="maximally frequent itemsets", ext=TRUE))

(reglasoporte=inspect(reglas) %>% mutate(IndicePromo=row_number())) # En esta tabla tenemos 4 o más objetos que se han vendido al mismo cliente más de un 10% de clientes



# Inspeccionamos qué clientes podrían comprar los primeros productos más comunes juntos y no lo han comprado.
# Generamos una tabla de datos con recomendaciones 

tablas=reglasoporte %>% select(items) %>% mutate(items=as.character(items)) %>% .$items %>% 
  str_replace_all("[{}]","") %>% as.data.frame() %>% mutate(items=as.character(.)) %>% 
  select(items) %>% separate(items, sep=",", into=as.character(c(1:4))) %>%
  mutate(IndicePromo=row_number()) %>% gather(key=Indicador, value=Producto, 1:4) %>% 
    select(-Indicador) %>% arrange(IndicePromo) %>% left_join(reglasoporte, by="IndicePromo") %>%
  select(-items,-count)
tablas

# La tabla superior nos dice qué producto participa en qué promoción

# Esto lo hemos analizado con eclat que sirve únicamente para cosas comunes, sirve para hacer "cestas" o paquetes completos

reglas=apriori(transacciones, parameter=list(support=0.01,confidence=0.4, maxlen=5))
inspect(reglas)



