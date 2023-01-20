setwd("D:/Users/T-Gamer/Documents/Profissional_D/DataScience/Portfolio/DataScience-R/Breast_Cancer_Prediction")

# Carregando os pacotes necessários

# Tratamento de dados
library(dplyr)

# Modelo de classificação knn
library(class)

# Avaliação do modelo
library(gmodels)

#Carregando dataset

col_names <- c("ID","clump_thickness","uniformity_cell_size",
               "uniformity_cell_shape", "marginal_adhesion",
               "single_epithelial_cell_size", "bare_nuclei",
               "bland_chromatin","normal_nucleoli", "mitoses",
               "class")

# Backup dos dados não tratados
dados_bruto <- read.table("breast-cancer-wisconsin.data", header = FALSE, 
                    sep = ",", dec = ".", col.names = col_names)

# Df para tratamento dos dados
dados <- dados_bruto

# Estudando NAs
any(is.na(dados))
qtd_vazios <-  100 * unlist(dados %>% filter(bare_nuclei == "?") %>% count()) / length(dados$bare_nuclei)
qtd_vazios # 2.3% de NAs (16 observações de 699) -> dropar 

# Excluindo linhas com NA
dados <- dados[dados$bare_nuclei != "?",]

str(dados)

# Removendo a coluna ID (irelevante/prejudicial para o modelo preditivo)
dados <- dados [-1]
str(dados)

# Tranformando os tipos de dados para Numérico
dados[c(1:9)] <- sapply(dados[c(1:9)], as.numeric)

# Fatorizando a coluna Class
table(dados$class)
dados$class <- factor(dados$class, levels = c(2,4), 
                      labels = c("Benigno", "Maligno"))
str(dados)

# Verificando a proporção de Class
round(prop.table(table(dados$class)) * 100, digits = 1)

# Divisão dados treino e teste
d_treino <- dados[1:480,1:9]
d_teste <- dados [481:683,1:9]

# Labels
d_treino_labels <- dados[1:480,10]
d_teste_labels <- dados [481:683,10]

# Criando e treinando o modelo
modelo <- knn(train = d_treino, test = d_teste, cl = d_treino_labels, k=21)

# Avaliando modelo
CrossTable(x = d_teste_labels, y = modelo, prop.chisq = FALSE)

