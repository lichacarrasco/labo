# Prueba de sampleo del dataset

#require("data.table")
require("BalancedSampling")
require("arrow")
require(dplyr)
#dataset <- fread("C:/Users/Cohen2/Desktop/eyf/datasets/competencia3_2022.csv.gz")


# Ejemplos de documentacion

cube()


set.seed(912)
N = 1000; # population size
n = 100; # sample size
p = rep(n/N,N);
p
X = cbind(p,runif(N),runif(N)); # matrix of auxiliary variables
X
s = cube(p,X)
s



N = nrow(data)      # Tamaño del dataset
n = 1000               # Tamaño de la muestra
p = rep(n/N,N)         # 
X = cbind(p,seleccion$freq.x, seleccion$freq.y)
X
cube(p,X)

muestra <- dataset[cube(p,X),]


muestra <- sample(seq_len(nrow(dataset)), 100, prob = dataset$clase_ternaria)

unique(dataset$foto_mes)
meses <- dataset %>% 
  filter(foto_mes == 202105)


unique(dataset$clase_ternaria)


data <- arrow::read_csv_arrow("C:/Users/Cohen2/Desktop/eyf/datasets/competencia3_2022.csv.gz")
unique(data$foto_mes)
data <- filter(data, foto_mes != 202107)
data <- filter(data, foto_mes != 202106)
data <- filter(data, foto_mes != 202105)


a <- data %>% 
  group_by(foto_mes) %>% 
  summarise(n = n() ) %>% 
  mutate(freq = n / sum(n))

balance_mes <- a[,c(1,3)]

balance_clase <- data.frame(clase_ternaria = c("BAJA+1", "BAJA+2", "CONTINUA"), freq = c(0.00438, 0.00427, 0.953))
a

colnames(data)

seleccion <- data %>% 
  select(numero_de_cliente, foto_mes, clase_ternaria) %>% 
  left_join(balance_mes, by = "foto_mes") %>% 
  left_join(balance_clase, by = "clase_ternaria")

View(head(seleccion, 1000))


N = nrow(data)      # Tamaño del dataset
n = 1000               # Tamaño de la muestra
p = rep(n/N,N)         # 
X = cbind(p,seleccion$freq.x, seleccion$freq.y)
X
cube(p,X)

muestra <- data[cube(p,X),]
