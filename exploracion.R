# Empezamos limpiando la memoria
rm(list=ls())
library(tidyverse)
data=read_csv(file="Datos/BlackFriday.csv", col_names=TRUE, )
head(data)
summary(data)

# Convertimos caracter a factor

data=data %>% mutate(Gender=as.factor(Gender), 
                Stay_In_Current_City_Years=as.factor(Stay_In_Current_City_Years),
                City_Category=as.factor(City_Category), 
                Product_ID=as.factor(Product_ID), 
                Age=as.factor(Age),
                Marital_Status=as.factor(Marital_Status),
                IDticket=cumsum(c(1, diff(User_ID) != 0)))

summary(data)
 # Comprobamos datos no repetidos, la línea inferior da como resultado TRUE
#(nrow(unique(data))==nrow(data))

# Medimos el número de nulos en cada columna
nulos=function(x)  mean(is.na(x))
sapply(data, nulos)
rm(nulos)
# Vemos que falla la categoria 2 y 3, se recomienda utilizar la categoria 1.










# Vamos a intentar entender la jerarquía de estos datos
jerarquia=data %>% select(Product_Category_1,Product_Category_2,Product_Category_3, Product_ID) %>% 
  unique() %>% arrange(Product_Category_1,Product_Category_2,Product_Category_3) %>% mutate_all(as.character)


jerarquia=jerarquia[which(complete.cases(jerarquia)),]
summary(jerarquia)
jerarquia

# Hacemos una jerarquia
library(data.tree)
library(treemap)
jerarquia$pathString=paste("Todo",jerarquia$Product_Category_1, jerarquia$Product_Category_2,sep = "/")
jerarquia$pathString=paste("Todo",jerarquia$Product_Category_1, jerarquia$Product_Category_2,jerarquia$Product_Category_3, sep = "/")
head(jerarquia)
tree=as.Node(jerarquia[,])
print(tree)
plot(tree)
rm(jerarquia,tree)

# Hacemos una segmentación de clientes RFM. Recency (No disponible), Frequency (nº de tickets) y Monetary(sum)




summary(data)

clientes= data %>% group_by(User_ID, Age, Gender, IDticket, Occupation, City_Category,
                            Stay_In_Current_City_Years, Marital_Status) %>% 
  summarize(M=sum(Purchase)/100, F=n()) %>% 
  group_by(User_ID, Age, Gender, Occupation, City_Category,
           Stay_In_Current_City_Years, Marital_Status) %>%
  summarize(M=sum(M), nproductos=sum(F), ntickets=n()) %>% 
  ungroup() %>%
  mutate(gastoticket=M/ntickets, gastoproducto=M/nproductos)

summary(clientes)
dim(clientes)

# M es el gasto total, nproductos el número de productos comprados y ntickets el número de compras que han hecho

# Tenemos 6000 clientes, les hacemos un scoring mediante distintas variables

clientes=clientes %>% ungroup() %>% mutate(rankM=rank(M)/length(M), ranknproductos=rank(nproductos)/length(nproductos)) %>% 
  arrange(rankM) %>% mutate(cumM=cumsum(M)) %>% arrange(ranknproductos) %>% mutate(cumnproductos=cumsum(nproductos))

summary(clientes)

# Hacemos algún plot
clientes %>% ggplot(aes(x=as.factor(ntickets), y=M, color=Gender))+ geom_boxplot()+ scale_y_log10()

clientes %>% ggplot(aes(x=nproductos, y=M, color=Gender))+ geom_point()

clientes %>% ggplot(aes(y=ranknproductos, x=rankM))+ geom_point(alpha=0.05) + theme_classic()

clientes %>% ggplot(aes(x=ranknproductos, y=cumM, color=Age))+ geom_point(alpha=0.2) + theme_classic()

clientes %>% ggplot(aes(x=rankM, y=cumnproductos))+ geom_point(alpha=0.05) + theme_classic()

clientes %>% ggplot(aes(x=M, fill=Gender ,color=Gender)) + geom_density(alpha=0.3)+ xlim(0,50000) + scale_x_log10() 

clientes %>% ggplot(aes(x=M,color=Age)) + geom_density(alpha=0.1, size=1)+ xlim(0,50000) + 
  scale_x_log10() + scale_color_brewer(palette="Spectral") + theme_classic()

clientes %>% ggplot(aes(x=Age,y=M, color=Age)) + geom_boxplot() +theme_classic()

clientes %>% ggplot(aes(y=cumnproductos/max(cumnproductos), x=ranknproductos))+ geom_line()

clientes %>% ggplot(aes(y=cumM/max(cumM), x=rankM))+ geom_line()

clientes %>% ggplot(aes(y=cumM, x=ranknproductos))+ geom_point(alpha=0.1)



# Cuántos productos compra cada cliente? La moda es 25, la media 91, la mediana 53

data %>% group_by(User_ID) %>% summarize(N=n()) %>% ggplot(aes(x=N)) + geom_histogram() + scale_x_log10()

data %>% group_by(User_ID) %>% summarize(N=n()) %>% ggplot(aes(x=N)) + geom_histogram() + xlim(0,100)

data %>% group_by(User_ID) %>% summarize(N=n()) %>% summary()



# Continuamos con un análisis preliminar

summary(data)



# Calculamos los productos que más ventas han tenido
prod = data %>% group_by(Product_ID,Precio=Purchase) %>% summarize(N=n()) %>% 
  ungroup()  %>% mutate(Ventas=Precio*N) %>% arrange(N)

prod
as.factor(cut(prod$Precio,8))

summary(prod)

hist(as.numeric(prod$N))

summary(prod)

head(prod)

hist(prod$Ventas)

prod %>% ggplot(aes(y=Ventas, x=redondeoprecio)) + geom_boxplot()# es necesario discretizar precio y hacer boxplot

prod %>% mutate(N=as.numeric(N)) %>% group_by(N) %>% summarize (total=sum(N*Precio)) %>% 
  ungroup() %>% ggplot(aes(y=total, x=N)) + geom_bar(stat="identity") #+ scale_y_log10()

prod %>% arrange(Precio) %>% mutate(beneficio=cumsum(Ventas)) %>% 
  ggplot(aes(y=beneficio, x=Precio))+ geom_line()
# Más o menos todos los productos generan un beneficio aproximado


# A pesar de que cada usuario compra aproximadamente 25 cosas, 

# Vemos que productos se asocian más a otros productos

# Necesitamos convertir los productos en wide format

data %>% select(Product_ID) %>% distinct() %>% mutate(value=1) %>% summarize(sum(value)) # Tenemos 3623 productos distintos 

# Creamos una matriz de transacciones para utilizar con arules
transacciones=data %>% select(User_ID, Product_ID) %>% mutate(value=1) %>% 
  spread(Product_ID,value,fill=0) %>% select(-User_ID) %>% as.matrix()

# Lo incluimos en arules
library(arules)
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

# De ahora en adelante lo analizamos con apriori. Buscamos qué productos se 
reglas=apriori(transacciones, parameter=list(support=0.01,confidence=0.6, maxlen=4, minlen=2))

reglasapriori=inspect(reglas)

reglasapriori[,2]=NULL

reglasapriori= reglasapriori %>% arrange(-lift)

reglasapriori

reglasapriori %>% ggplot(aes(y=lift, x=confidence)) + geom_point(alpha=0.3)

reglasapriori %>% ggplot(aes(x=lift)) + geom_histogram() + xlim(2,5)


# Proponemos hacer paquetes con eclat y mandar un correo o carta a cada cliente que no haya comprado el producto Y dado X
































# Segmentación de clientes, según los productos que han comprado, habrá que hacer un PCA y quedarnos con los 3 primeros.
pca=prcomp(transacciones)
#13:53
pcaresult=as.data.frame(pca$x)
summary(pcaresult$PC1)
summary(pcaresult$PC2)
summary(pcaresult$PC3)
summary(pcaresult$PC4)
summary(pcaresult$PC5)



dataplot=pcaresult %>% select(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10)

nrow(dataplot)

clientespc=clientes %>% cbind(dataplot)

summary(clientespc)

matriz=pca$rotation[1:1000,1:1000]

pca$rotation[]

library(corrplot)
corrplot(matriz)

# Vamos a ver la matriz de rotacion

hist(abs(pca$rotation[,15]))


hist(as.vector(pca$rotation))

# El pca no sirve para nada

# Tenemos que hacer la segmentación en 3 tipos de clientes. Alto valor, medio valor y bajo valor

plot(as.numeric(data$User_ID))


# Hacemos una segmentación RFM







# Estudiamos otras variables como tipo de ciudad y cosas asi

                