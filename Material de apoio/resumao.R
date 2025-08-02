ordem_crescente <- sort(gastos_dia, decreasing = FALSE)  # Ordena os dados em ordem crescente.
ordem_decrescente <- sort(gastos_dia, decreasing = TRUE) # Ordena os dados em ordem decrescente.

total <- sum(gastos_dia)   # Soma todos os valores.
minimo <- min(gastos_dia)  # Retorna o valor mínimo.
maximo <- max(gastos_dia)  # Retorna o valor máximo.
media <- mean(gastos_dia)  # Calcula a média aritmética.

dim(matriz3)     # Retorna as dimensões da matriz (linhas, colunas).
nrow(matriz3)    # Retorna o número de linhas.
ncol(matriz3)    # Retorna o número de colunas.

# Define nomes para linhas e colunas da matriz.
dimnames(matriz3) <- list(c("Linha1", "Linha2", ...), c("Coluna1", "Coluna2"))

# Operações com DataFrames:
nrow(df1)        # Número de linhas do dataframe.
ncol(df1)        # Número de colunas do dataframe.
dim(df1)         # Dimensões do dataframe.
summary(df1)     # Resumo estatístico do dataframe (média, mediana, quartis, etc.).

mean(datfx)      # Média.
median(datfx)    # Mediana.
table(datfx)     # Tabela de frequência para moda.
max(datfx)       # Valor máximo.
min(datfx)       # Valor mínimo.
range(datfx)     # Retorna o mínimo e o máximo.
diff(range(datfx)) # Amplitude (diferença entre máximo e mínimo).

var(datfx)       # Variância.
sd(datfx)        # Desvio padrão.
CV = sd(datfx) / mean(datfx) * 100  # Coeficiente de variação.

# Cria uma tabela resumo com estatísticas descritivas.
descritiva = rbind(Média = mean(datfx), Mediana = median(datfx), ...)

Freq <- table(Notas)           # Frequência absoluta.
FreqAc <- cumsum(Freq)         # Frequência acumulada.
FreqRel <- prop.table(Freq)    # Frequência relativa.
FreqRelAc <- cumsum(FreqRel)   # Frequência relativa acumulada.

# Combina todas as frequências em uma tabela.
TabResul = cbind(Freq, FreqAc, FreqRel = round(FreqRel*100, 2), ...)

setwd("C:\\P&E 20251")  # Define o diretório de trabalho.
Estadoc <- read.csv("Estadocivil.csv", header = TRUE, sep = ",", dec = ".")

milsa <- read.table("TabelaLivro.csv", header = TRUE, sep = ";", dec = ".")

head(Estadoc)  # Mostra as primeiras linhas do dataframe.
tail(Estadoc)  # Mostra as últimas linhas.
str(Estadoc)   # Exibe a estrutura do dataframe (tipos de dados, etc.).

barplot(Freq, main = "Estado Civil", ylab = "Frequencia", ...)  # Gráfico de barras.
pie(Freq, main = "Estado Civil", col = c("Yellow", "Orange", ...))  # Gráfico de pizza.

h = hist(Producao$Bike, breaks = limitesclas, ...)  # Cria um histograma.
lines(c(min(h$breaks), h$mids, max(h$breaks)), ...) # Adiciona linhas ao histograma.

boxplot(Producao, col = "red")                     # Boxplot simples.
boxplot(Producao, col = "Orange", varwidth = TRUE)  # Boxplot com largura proporcional.

# Tabela de contingência para variáveis qualitativas.
inst.civ.tb <- table(milsa$Instrucao, milsa$Est.civil)  

# Proporções marginais e condicionais.
prop.table(inst.civ.tb)             # Proporções totais.
prop.table(inst.civ.tb, margin = 1) # Proporções por linha.
prop.table(inst.civ.tb, margin = 2) # Proporções por coluna.

cor(milsa$Anos, milsa$Salario)  # Calcula a correlação entre duas variáveis.

plot(milsa$Anos, milsa$Salario)  # Gráfico de dispersão.

# Ajusta um modelo de regressão linear e plota a reta.
modelo <- lm(Salario ~ Anos, data = milsa)
abline(modelo, col = "darkred", lwd = 2)

iris <- na.omit(iris)          # Remove linhas com valores ausentes (NA) do dataset `iris`.
sum(is.na(iris))                # Conta o total de NAs no dataset (deve ser 0 após `na.omit`).
colSums(is.na(iris))           # Conta NAs por coluna.
Iris_clean <- na.omit(iris)     # Cria uma cópia "limpa" do dataset.
table(iris$Species)             # Tabela de frequência da variável categórica `Species`.
iris$Sepal.Area <- NULL         # Remove a coluna `Sepal.Area` (se existir).

# Gráfico de barras com médias de comprimento de sépala por espécie.
barplot(medias$Sepal.Length, 
        names.arg = medias$Species, 
        col = c("skyblue", "lightgreen", "salmon"), 
        main = "Média do Comprimento da Sépala por Espécie", 
        xlab = "Espécie", 
        ylab = "Média do Comprimento da Sépala (cm)", 
        ylim = c(0, max(medias$Sepal.Length) + 0.5))

# Adiciona legenda ao gráfico.
legend(x = 5.75, y = 1.25, 
       legend = levels(iris$Species), 
       col = 1:3, bty = "n", pch = 16, cex = 0.85)

cor(iris[, 1:4])                # Matriz de correlação entre as 4 variáveis numéricas do `iris`.

cov(reg$PesoTotal, reg$PesoPapel)  # Covariância entre duas variáveis.
cor(reg$PesoTotal, reg$PesoPapel)  # Correlação (Pearson) entre duas variáveis.
cor.test(reg$PesoTotal, reg$PesoPapel)  # Teste de significância da correlação.

# Visualização avançada com `corrplot`:
install.packages("corrplot")    # Instala o pacote (se necessário).
library(corrplot)               # Carrega o pacote.
corrplot(mc)                    # Gráfico padrão da matriz de correlação.
corrplot(mc, method = "square", type = "lower")  # Personalizado (quadrados, metade inferior).

# Ajusta um modelo de regressão linear simples.
modelo <- lm(PesoTotal ~ PesoPapel, data = reg)
modelo                         # Mostra os coeficientes (intercepto e inclinação).

# Resumo detalhado do modelo.
summary_modelo <- summary(modelo)
print(summary_modelo)           # Exibe R², p-valores, estatísticas, etc.

# Extrai coeficientes e R² arredondados.
coef_intercepto <- round(coef(modelo)[1], 2)
coef_inclinacao <- round(coef(modelo)[2], 2)
r2 <- round(summary_modelo$r.squared, 3)

# Configura a área de plotagem para 4 gráficos de diagnóstico.
par(mfrow = c(2, 2))  

# Plota os gráficos de diagnóstico do modelo:
plot(modelo, which = 1, pch = 19, col = "blue")  # Resíduos vs Ajustados.
plot(modelo, which = 2, pch = 19, col = "blue")  # Q-Q plot para normalidade.
plot(modelo, which = 3, pch = 19, col = "blue")  # Escala-Local (homocedasticidade).
plot(modelo, which = 5, pch = 19, col = "blue")  # Resíduos vs Leverage (pontos influentes).

rm(list = ls())  # Remove todos os objetos da memória do R.