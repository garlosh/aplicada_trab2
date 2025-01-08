# Carregar pacotes necessários
library(tidyverse)
library(ggplot2)
library(caret)
# Carregar os dados (substitua 'dados.csv' pelo nome do arquivo real)
dados <- read.table("dados.txt", sep = ";", stringsAsFactors = FALSE, header = TRUE)

dados <- dados[, -c(20, 21)]
# Visualizar as primeiras linhas para entender o formato dos dados
head(dados)

# Selecionar variáveis numéricas para a clusterização
variaveis_numericas <- dados %>% 
  select(DirecaoVento,
         VelocidadeVentoMedia,
         VelocidadeVentoMaximaMedia,
         EvaporacaoPiche,
         EvapoBHPotencial,
         EvapoBHReal,
         InsolacaoTotal,
         NumDiasPrecipitacao,
         PrecipitacaoTotal,
         PressaoNivelMarMedia,
         PressaoMedia,
         TempMaximaMedia,
         NebulosidadeMedia,
         TempMinimaMedia,
         UmidadeRelativaMedia)

cor_matrix <- cor(variaveis_numericas, use = "complete.obs")
selected_vars <- findCorrelation(cor_matrix, cutoff = 0.8, verbose = TRUE)
variaveis_selecionadas <- variaveis_numericas[, -selected_vars]

# Criar uma versão filtrada do conjunto de dados para evitar discrepâncias
dados_filtrados <- dados[complete.cases(variaveis_selecionadas), ]  # Somente linhas sem NA nas variáveis numéricas

# Normalizar os dados (escala 0-1)
variaveis_normalizadas <- scale(variaveis_selecionadas[complete.cases(variaveis_selecionadas), ])

# Determinar o número ideal de clusters (Método Elbow)
set.seed(123)
sse <- numeric()
for (k in 1:10) {
  kmeans_model <- kmeans(variaveis_normalizadas, centers = k, nstart = 25)
  sse[k] <- kmeans_model$tot.withinss
}

# Plotar o método Elbow
plot(1:10, sse, type = "b", pch = 19, frame = FALSE, 
     xlab = "Número de clusters (k)", ylab = "Soma dos quadrados dos erros (SSE)")

# Definir o número de clusters (supondo 3 como exemplo)
k <- 2

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
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(x = "Mês",
       y = "Frequência") +
  theme_minimal()


# Converter a coluna Data para o formato correto
dados_filtrados <- dados_filtrados %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"))


# Variáveis de interesse para analisar nos clusters
lapply(c("PrecipitacaoTotal", "TempCompensadaMedia", "InsolacaoTotal"), function(var) {
  ggplot(dados_filtrados, aes(x = Mes, y = !!sym(var), color = Cluster)) +
    geom_boxplot() +
    labs(
      title = paste("Distribuição de", var, "por Mês"),
      x = "Mês",
      y = var
    ) +
    theme_minimal()
})



# Agregar dados por Data e Cluster
dados_temporais <- dados_filtrados %>%
  group_by(Data, Cluster) %>%
  summarise(
    Frequencia = n(),
    VelocidadeVentoMedia = mean(VelocidadeVentoMedia, na.rm = TRUE),
    VelocidadeVentoMaximaMedia = mean(VelocidadeVentoMaximaMedia, na.rm = TRUE),
    .groups = "drop"
  )


# Detectar anomalias
library(anomalize)

# Variáveis de interesse para análise de anomalias
dados_temporais <- dados_filtrados %>%
  group_by(Data, Cluster) %>%
  summarise(
    Frequencia = n(),
    VelocidadeVentoMedia = mean(VelocidadeVentoMedia, na.rm = TRUE),
    VelocidadeVentoMaximaMedia = mean(VelocidadeVentoMaximaMedia, na.rm = TRUE),
    InsolacaoTotal = mean(InsolacaoTotal, na.rm = TRUE),
    NebulosidadeMedia = mean(NebulosidadeMedia, na.rm = TRUE),
    .groups = "drop"
  )

# Variáveis de interesse (ajustadas se necessário)
variaveis <- c("VelocidadeVentoMedia", "VelocidadeVentoMaximaMedia", "InsolacaoTotal", "NebulosidadeMedia")

# Lista para armazenar resultados de anomalias
anomalias_lista <- list()

# Loop para detectar anomalias por variável e cluster
for (variavel in variaveis) {
  if (variavel %in% colnames(dados_temporais)) {  # Verificar se a variável existe
    anomalias <- dados_temporais %>%
      select(Data, Cluster, all_of(variavel)) %>%
      group_by(Cluster) %>%
      rename(Variavel = all_of(variavel)) %>%
      time_decompose(Variavel, method = "stl", frequency = "auto") %>%
      anomalize(remainder, method = "iqr") %>%
      time_recompose() %>%
      filter(anomaly == "Yes") %>%
      mutate(Variavel = variavel)
    
    # Adicionar à lista de anomalias
    anomalias_lista[[variavel]] <- anomalias
  } else {
    warning(paste("Variável", variavel, "não encontrada no dataset."))
  }
}

# Combinar todas as anomalias em um único dataframe
anomalias_todas <- bind_rows(anomalias_lista)

# Exibir as anomalias detectadas
print(anomalias_todas)

# Resumo de anomalias por cluster e variável
anomalias_resumo <- anomalias_todas %>%
  group_by(Cluster, Variavel) %>%
  summarise(
    Total_Anomalias = n(),
    Datas_Anomalias = paste(Data, collapse = ", "),
    .groups = "drop"
  )

# Exibir o resumo
print(anomalias_resumo)

library(knitr)
library(kableExtra)

anomalias_resumo %>%
  kable("html", caption = "Resumo das Anomalias por Cluster e Variável") %>%
  kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover", "condensed"))
