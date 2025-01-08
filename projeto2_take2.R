# Carregar pacotes necessários
library(tidyverse)
library(xtable)
library(reshape2)
library(viridis)
library(ggplot2)
library(cluster)
library(gridExtra)
library(caret)


# Carregar os dados (substitua 'dados.csv' pelo nome do arquivo real)
dados <- read.delim("C:/Users/luiza/Downloads/Dados2_3.txt",
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


png("seriestemp.png", width = 2000, height = 3000, res = 300) # Adjust dimensions and resolution

par(mfrow = c(5,3))

for (col in colnames(dados[, 2:15])){
  plot(dados$Data, dados[[col]], type = 'l',
       ylab = col,
       xlab = 'Tempo')
}
dev.off()

#boxplot
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

png("boxplots.png", width = 1500, height = 3000, res = 300)
par(mfrow = c(5,3))

for (n in 2:15){
  boxplot(dados[n],
       ylab = colnames(dados[n]),
       col = pastel_colors[n])
} 
dev.off()

# matriz de correlacao
cor_matrix <- cor(dados[, -1], use = "complete.obs", method = "pearson")
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
# Pre-processamento
###########################

dados <- dados %>% select(-PressaoMedia, # corr alta (>0.9) com muitas variaveis
                          -EvapoBHPotencial, # corr alta (>0.9) com muitas variaveis
                          -EvapoBHReal, # corr alta (>0.9) com muitas variaveis
                          -TempCompensadaMedia, # corr de 0,98 com tempcompensadamedia
                          -TempMinimaMedia, # corr de 0,97 com tempcompensadamedia
                          -UmidadeRelativaMedia, # corr alta  ~0.8 com 3 variáveis
                          -NebulosidadeMedia,
                          -EvaporacaoPiche) #corr moderada/alta com quase todas as var restantes
# Normalizar os dados (escala 0-1)
variaveis_normalizadas <- scale(dados[,-1])

###########################
# MODELAGEM - K-MEANS
###########################

# Determinar o número ideal de clusters (Método Elbow)
set.seed(123)
sse <- numeric()
for (k in 1:10) {
  kmeans_model <- kmeans(variaveis_normalizadas, centers = k, nstart = 25)
  sse[k] <- kmeans_model$tot.withinss
}
plot(1:10, sse, type = "b", pch = 19, frame = FALSE, 
     xlab = "Número de clusters (k)", ylab = "Soma dos quadrados dos erros (SSE)",
     main = "Método Elbow")
abline(v = 3, col = 'red', type = '--')

k <- 2

# Aplicar o K-Means
kmeans_resultado <- kmeans(variaveis_normalizadas, centers = k, nstart = 25)

dados_final <- dados
dados_final$Cluster <- as.factor(kmeans_resultado$cluster)
head(dados_final)

#médias
cluster_summary <- dados_final %>% 
  group_by(Cluster) %>% 
  summarise(across(
    .cols = colnames(dados[, -1]), 
    .fns = ~ round(mean(.x, na.rm = TRUE), 2)
  ))
print(xtable(
    t(cluster_summary),
    caption = "Resumo Estatístico das Variáveis por Cluster", 
    label = "tab:resumo_estatistico",
  type = "latex", 
  comment = FALSE, 
  include.rownames = FALSE))


#boxplots
plots <- list()
for (variavel in colnames(dados[,-1])) {
  p <- ggplot(dados_final, aes_string(x = "Cluster", y = variavel, fill = "Cluster")) +
           geom_boxplot() +
           labs(
             title = paste("Distribuição de", variavel, "por Cluster"),
             x = "Cluster",
             y = variavel
           ) +
           theme_bw() +
          theme(plot.title = element_text(hjust = 0.5),
                legend.position = "none") +
           scale_fill_brewer(palette = "Set2")
  plots[[variavel]] <- p
}
grid.arrange(grobs = plots, ncol = 3)

#MES
dados_final <- dados_final %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"),
         Mes = format(Data, "%m"))

freq_mes <- dados_final %>%
  group_by(Mes, Cluster) %>%
  summarise(Frequencia = n(), .groups = "drop")

ggplot(freq_mes, aes(x = Mes, y = Frequencia, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Distribuição dos Clusters por Mês",
    x = "Mês",
    y = "Frequência",
    fill = "Cluster"
  ) +
  scale_fill_brewer(palette = "Set2") + # Cores suaves e harmônicas
  theme_light(base_size = 14) +       # Ajusta o tema para um design limpo
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), # Centraliza e estiliza o título
    axis.text.x = element_text(angle = 45, hjust = 1),               # Rotaciona os rótulos do eixo X
    axis.text = element_text(color = "gray30"),                      # Ajusta a cor dos textos dos eixos
    legend.position = "top",                                         # Move a legenda para o topo
    legend.title = element_text(face = "bold")                       # Destaque para o título da legenda
  )



#ANO
dados_final <- dados_final %>%
  mutate(Data = as.Date(Data, format = "%d/%m/%Y"),
         Ano = format(Data, "%Y"))

freq_ano <- dados_final %>%
  group_by(Ano, Cluster) %>%
  summarise(Frequencia = n(), .groups = "drop")

ggplot(freq_ano, aes(x = Ano, y = Frequencia, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribuição dos Clusters por Ano",
       x = "Ano",
       y = "Frequência") +
  theme_minimal()

###########################
# MODELAGEM - HIERARQUICO
###########################

dist_matrix <- dist(variaveis_normalizadas, method = "euclidean")

# Construir o dendrograma (método de ligação completa)
hc <- hclust(dist_matrix, method = "complete")

# Plotar o dendrograma
plot(hc, main = "Dendrograma - Cluster Hierárquico", xlab = "", sub = "", cex = 0.7)
rect.hclust(hc, k = 3, border = 2:5)

clusters_ <- cutree(hc, k = 2)

# Adicionar os clusters ao dataset original
dados_final <- dados
dados_final$Cluster <- as.factor(clusters)

# Diagnóstico: número de observações por cluster
table(dados_final$Cluster)
cluster_means <- aggregate(dados_final[, -ncol(dados_final)], by = list(Cluster = dados_final$Cluster), FUN = mean)
print(cluster_means)

silhouette_scores <- silhouette(clusters, dist_matrix)
silhouette_scores <- silhouette(kmeans_resultado$cluster, dist(variaveis_normalizadas))


# Visualizar o resultado
summary(silhouette_scores)
plot(silhouette_scores, col = 2:4, border = NA, main = "Gráfico de Silhouette")
