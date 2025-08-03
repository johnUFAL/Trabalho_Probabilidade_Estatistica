setwd("C:\\P&E 20251")      # Define diretório de trabalho
rm(list = ls())             # Limpa objetos da memória
install.packages("corrplot")# Instala pacotes (se precisar)
library(corrplot)           # Carrega pacotes

Estadoc <- read.csv("Estadocivil.csv", header = TRUE, sep = ",", dec = ".")
milsa <- read.table("TabelaLivro.csv", header = TRUE, sep = ";", dec = ".")

head(Estadoc)   # Primeiras linhas
tail(Estadoc)   # Últimas linhas
str(Estadoc)    # Estrutura
summary(Estadoc)# Estatísticas descritivas

iris <- na.omit(iris)       # Remove NAs
sum(is.na(iris))            # Conta NAs
iris$Sepal.Area <- NULL     # Remove coluna desnecessária

total <- sum(gastos_dia)
minimo <- min(gastos_dia)
maximo <- max(gastos_dia)
media <- mean(gastos_dia)
median(datfx)                # Mediana
var(datfx); sd(datfx)        # Variância e desvio padrão
CV = sd(datfx)/mean(datfx)*100 # Coeficiente de variação

Freq <- table(Notas)        
FreqAc <- cumsum(Freq)      
FreqRel <- prop.table(Freq) 
FreqRelAc <- cumsum(FreqRel)
TabResul = cbind(Freq, FreqAc, FreqRel = round(FreqRel*100,2))

dim(matriz3)
nrow(df1); ncol(df1); dim(df1)
dimnames(matriz3) <- list(c("Linha1","Linha2"), c("Coluna1","Coluna2"))

ordem_crescente <- sort(gastos_dia, decreasing = FALSE)
ordem_decrescente <- sort(gastos_dia, decreasing = TRUE)

barplot(Freq, main = "Estado Civil", ylab = "Frequência")
pie(Freq, main = "Estado Civil", col = c("Yellow", "Orange"))

hist(Producao$Bike, breaks = limitesclas)
boxplot(Producao, col = "red")

plot(milsa$Anos, milsa$Salario) # Dispersão
corrplot(cor(iris[,1:4]))       # Correlação

modelo <- lm(Salario ~ Anos, data = milsa)
summary(modelo)                 # Resumo do modelo
abline(modelo, col = "darkred")

# Coeficientes
coef_intercepto <- round(coef(modelo)[1],2)
coef_inclinacao <- round(coef(modelo)[2],2)
r2 <- round(summary(modelo)$r.squared,3)

# Gráficos de diagnóstico
par(mfrow = c(2,2))
plot(modelo, which = 1:2)
