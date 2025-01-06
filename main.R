# Carregar pacotes necessários
library(tidyverse)

# Carregar os dados (substitua 'dados.csv' pelo nome do arquivo real)
dados <- read.table("dados.txt", sep = ";", stringsAsFactors = FALSE, header = TRUE)

dados <- dados[, -c(20, 21)]
# Visualizar as primeiras linhas para entender o formato dos dados
head(dados)

# Selecionar variáveis numéricas para a clusterização
variaveis_numericas <- dados %>% 
  select(VelocidadeVentoMedia, VelocidadeVentoMaximaMedia, EvaporacaoPiche, InsolacaoTotal, NebulosidadeMedia)

# Criar uma versão filtrada do conjunto de dados para evitar discrepâncias
dados_filtrados <- dados[complete.cases(variaveis_numericas), ]  # Somente linhas sem NA nas variáveis numéricas

# Normalizar os dados (escala 0-1)
variaveis_normalizadas <- scale(variaveis_numericas[complete.cases(variaveis_numericas), ])

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

# Definir o número de clusters (supondo 3 como exemplo)
k <- 3

# Aplicar o K-Means
kmeans_resultado <- kmeans(variaveis_normalizadas, centers = k, nstart = 25)

# Adicionar os clusters ao dataframe filtrado
dados_filtrados$Cluster <- as.factor(kmeans_resultado$cluster)

# Visualizar os primeiros resultados
head(dados_filtrados)

# Analisar as médias de cada variável por cluster
cluster_summary <- dados_filtrados %>% 
  group_by(Cluster) %>% 
  summarise(across(c(VelocidadeVentoMedia, VelocidadeVentoMaximaMedia, EvaporacaoPiche, InsolacaoTotal, NebulosidadeMedia), 
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
