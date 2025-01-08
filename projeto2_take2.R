# Carregar pacotes necessários
library(tidyverse)
library(xtable)
library(reshape2)
library(viridis)
library(ggplot2)


# Carregar os dados (substitua 'dados.csv' pelo nome do arquivo real)
dados <- read.delim("dados.txt",
                          sep = ';', dec = '.')

dados <- dados %>% select(-VisibilidadeMedia,
                          -X,
                          -PressaoNivelMarMedia,
                          -NumDiasPrecipitacao,
                          -Hora,
                          -Estacao)

# remover NAs 
# nrow: 115 -> 106

dados <- na.omit(dados)
colSums(is.na(dados))

dados$Data <- as.Date(dados$Data, format = "%d/%m/%Y")

head(dados)


###########################
# ANALISE DESCRITIVA
###########################

sumario <- dados[, 2:15] %>%
  summarise(across(everything(), 
                   list(min = ~round(min(.x, na.rm = TRUE), 2),
                        q1 = ~round(quantile(.x, 0.25, na.rm = TRUE), 2),
                        median = ~round(median(.x, na.rm = TRUE), 2),
                        mean = ~round(mean(.x, na.rm = TRUE), 2),
                        q3 = ~round(quantile(.x, 0.75, na.rm = TRUE), 2),
                        max = ~round(max(.x, na.rm = TRUE), 2),
                        sd = ~round(sd(.x, na.rm = TRUE), 2)))) %>%
  pivot_longer(cols = everything(),
               names_to = c("Variável", "Estatística"),
               names_sep = "_",
               values_to = "Valor") %>%
  pivot_wider(names_from = Estatística, values_from = Valor)

print((xtable(sumario, 
      caption = "Resumo Estatístico das Variáveis", 
      label = "tab:resumo_estatistico")), type = "latex", comment = FALSE) 
# Imprimir a tabela no formato LaTeX

#series temp
par(mfrow = c(2, 3))

for (col in colnames(dados[, 2:15])){
  plot(dados$Data, dados[[col]], type = 'l',
       ylab = col,
       xlab = 'Tempo')
  } # sazonalidade nenhuma tendencia

#boxplot
colores <- rev(plasma(20))
pastel_colors <- c(
  'white',
  "#F2C6D2",  # Light Rose
  "#F8D5D1",  # Pastel Pink
  "#F4E1D2",  # Pastel Peach
  "#F3D8C1",  # Pale Apricot
  "#D6A7A1",  # Muted Coral
  "#D9E7C3",  # Pastel Olive
  "#D0E6A5",  # Light Sage Green
  "#B1E5D3",  # Light Mint
  "#B1E5D3",   # Light Mint
  "#A8D8D8",  # Pale Teal
  "#C9DAF8",  # Powder Blue
  "#B8C2CC",  # Soft Periwinkle
  "#D8C4E1",  # Light Lavender
  "#E0B0FF"  # Lavender
)
par(mfrow = c(2, 3))

for (n in 2:15){
  boxplot(dados[n],
       ylab = colnames(dados[n]),
       col = pastel_colors[n])
} 

# matriz de correlacao
cor_matrix <- cor(dados[, 2:15], use = "complete.obs", method = "pearson")
cor_matrix <- melt(cor_matrix)
cor_matrix <- cor_matrix %>%
  mutate(row = as.numeric(factor(Var1)),
         col = as.numeric(factor(Var2))) %>%
  filter(row < col)
cor_matrix$Var2 <- factor(cor_matrix$Var2, rev(levels(cor_matrix$Var2)))
ggplot(cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +  
  scale_fill_viridis(name = 'Coef. de\ncorrelação',option = "viridis", direction = 1) +
  geom_text(aes(label = round(value, 2)), color = "white", size = 2.5) +  # Add the correlation values
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "",
       x = "", y = "")

###########################
# MODELAGEM
###########################

# Normalizar os dados (escala 0-1)
variaveis_normalizadas <- scale(dados[,-1])


# Determinar o número ideal de clusters (Método Elbow)
set.seed(123)
sse <- numeric()
for (k in 1:10) {
  kmeans_model <- kmeans(variaveis_normalizadas, centers = k, nstart = 25)
  sse[k] <- kmeans_model$tot.withinss
}

# Plotar o método Elbow
plot(1:10, sse, type = "b", pch = 19, frame = FALSE, 
     xlab = "Número de clusters (k)", ylab = "Soma dos quadrados dos erros (SSE)",
     main = "Método Elbow")
abline(v = 3, col = 'red', type = '--')

# Definir o número de clusters (supondo 3 como exemplo)
k <- 3

# Aplicar o K-Means
kmeans_resultado <- kmeans(variaveis_normalizadas, centers = k, nstart = 25)

# Adicionar os clusters ao dataframe filtrado
dados$Cluster <- as.factor(kmeans_resultado$cluster)

# Visualizar os primeiros resultados
head(dados)

# Analisar as médias de cada variável por cluster
cluster_summary <- dados %>% 
  group_by(Cluster) %>% 
  summarise(across(colnames(dados[, 1:15]), 
                   list(media = mean, sd = sd), na.rm = TRUE))

print(cluster_summary)

# Lista de variáveis numéricas
variaveis <- c("VelocidadeVentoMedia", "VelocidadeVentoMaximaMedia", "EvaporacaoPiche", "InsolacaoTotal", "NebulosidadeMedia")

# Loop para criar boxplots de todas as variáveis numéricas pelos clusters
for (variavel in variaveis) {
  print( ggplot(dados_filtrados, aes_string(x = "Cluster", y = variavel, fill = "Cluster")) +
           geom_boxplot() +
           labs(
             title = paste("Distribuição de", variavel, "por Cluster"),
             x = "Cluster",
             y = variavel
           ) +
           theme_minimal() +
           theme(legend.position = "none") +
           scale_fill_brewer(palette = "Set2") )
}

# Adicionar uma coluna com o mês extraído da data
dados_filtrados <- dados_filtrados %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"),
         Mes = format(Data, "%m"))

# Calcular a frequência de clusters por mês
freq_mes <- dados_filtrados %>%
  group_by(Mes, Cluster) %>%
  summarise(Frequencia = n(), .groups = "drop")

# Gráfico de barras para a distribuição de clusters ao longo dos meses
ggplot(freq_mes, aes(x = Mes, y = Frequencia, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribuição dos Clusters por Mês",
       x = "Mês",
       y = "Frequência") +
  theme_minimal()

dados_filtrados <- dados_filtrados %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"),
         Ano = format(Data, "%Y"))

freq_ano <- dados_filtrados %>%
  group_by(Ano, Cluster) %>%
  summarise(Frequencia = n(), .groups = "drop")

# Gráfico de barras para a distribuição de clusters ao longo dos anos
ggplot(freq_ano, aes(x = Ano, y = Frequencia, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribuição dos Clusters por Ano",
       x = "Ano",
       y = "Frequência") +
  theme_minimal()
