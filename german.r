## Trabalho de P&E - German Credit Data

rm(list = ls()) # Limpeza
dev.off() # Erros gráficos

# Carregando pacotes
library(ggplot2)
library(corrplot)
library(hexbin)
library(readxl)

# imporntando, lendO e preparando o dataframe
setwd("C:/Users/Joao Duarte/Desktop/Trabalho R/Trabalho_Probabilidade_Estatistica")

df <- read_excel("german.xlsx", col_names = FALSE)

colnames(df) <- c(
  "Status_conta", "Meses_existencia", "Historico_credito", "Proposito",
  "Valor_credito", "Saldo_poupanca", "Tempo_emprego", "Taxa_parcela_renda",
  "Status_pessoal_sexo", "Fiador", "Residencia_atual", "Bens",
  "Idade", "Planos_parcelamento", "Moradia", "Credito_existente", 
  "Emprego", "Dependentes", "Telefone", "Trabalhador_estrangeiro", "Classe"
)

# Convertentdo valores para numericos
df$Meses_existencia <- as.numeric(df$Meses_existencia)
df$Valor_credito <- as.numeric(df$Valor_credito)
df$Taxa_parcela_renda <- as.numeric(df$Taxa_parcela_renda)
df$Residencia_atual <- as.numeric(df$Residencia_atual)
df$Idade <- as.numeric(df$Idade)
df$Credito_existente <- as.numeric(df$Credito_existente)
df$Dependentes <- as.numeric(df$Dependentes)


# Comfiguração gráfica
par(mar = c(4, 4, 2, 1))
theme_set(theme_minimal()) #padrão ggplot

# Analise bruta
head(df) # Primeiras linhas
tail(df) # Ultimas linhas
str(df) # Estrutura do df
summary(df) # Est. Descritiva

# Tratamento
print(colSums(is.na(df))) # Verifica dados faltantes
print(sapply(df, class)) 

par(mfrow = c(2, 4))
var_numericas <- c("Meses_existencia", "Valor_credito", "Taxa_parcela_renda",
                   "Residencia_atual", "Idade", "Credito_existente", "Dependentes")

for (var in var_numericas) {
  boxplot(df[[var]], main = paste("Outliers em", var), ylab = var)
}
par(mfrow = c(1, 1))

# Usando valor crédito: valor min, valor max, média, mediana. desvio padrão, os quartis, histograma, boxplot e interpretação
min(df$Valor_credito)
max(df$Valor_credito)
mean(df$Valor_credito)
median(df$Valor_credito)
sd(df$Valor_credito)
quantile(df$Valor_credito, probs = c(0.25, 0.50, 0.75))

hist(df$Valor_credito,
     breaks = 30,
     main = "Dsitribuição de Frequência",
     xlab = "Valor Crédito (DM)", 
     col = "lightblue")

boxplot(df$Valor_credito, 
        ylim = c(0, 20000),
        main = "Distribuição Valor Crédito",
        ylab = "Valores (DM)", 
        col = "lightgreen")

# Analise bivariada:  "Saldo_poupanca" e "Tempo_emprego"
str(df[c("Saldo_poupanca", "Tempo_emprego")])

tbCont <- table(df$Saldo_poupanca, df$Tempo_emprego)
print(tbCont)
print(addmargins(tbCont)) #soma total

prop.table(tbCont, margin = 2)

tst <- chisq.test(tbCont) # Associação significativa
print(tst) # Associação sig == p < 0.05

ggplot(as.data.frame(tbCont), 
       aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Saldo Poupança", y = "Frequência", 
       fill = "Tempo Emprego", 
       title = "Relação Saldo Poupança x Tempo Emprego") +
  theme_minimal()


# Analise em boxplot das variáveis Idade e saldo_poupanca
df$Saldo_poupanca <- factor(
  df$Saldo_poupanca,
  levels = c("A61", "A62", "A63", "A64", "A65"),
  labels = c("<100 DM", "100-500 DM", "500-1000 DM", "≥1000 DM", "Desconhecido"),
  exclude = NULL
)

ggplot(df, aes(x = Saldo_poupanca, y = Idade, fill = Saldo_poupanca)) +
  geom_boxplot() + labs(title = "Idade por Saldo em poupança",
                        x = "Saldo em Poupança",
                        y = "Idade (anos",
                        fill = "Saldo") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Matriz de correlação
var_cor <- df[, c("Meses_existencia", "Valor_credito", "Taxa_parcela_renda",
                        "Residencia_atual", "Idade", "Credito_existente", "Dependentes")]

mat <- cor(var_cor)

corrplot(mat, 
         method = "color", 
         type = "upper", 
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.5,
         diag = FALSE,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         mar = c(0, 0 , 2, 0),
         title = "Matriz de Correlação",
         cl.ratio  = 0.2)

# Analise bivariada:  "Meses_existencia" e "Residencia_atual" disperssão com linha de tendência
ggplot(df, aes(x = Meses_existencia, y = Residencia_atual)) +
  geom_point(alpha = 0.6, color = "blue") +  #scatterplot
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink") +  #regressão
  labs(title = "Relação entre Meses de Existência e Tempo na Residência Atual",
       x = "Meses de Existência",
       y = "Tempo na Residência Atual (mês)",
       caption = paste("Correlação =", round(cor(df$Meses_existencia, df$Residencia_atual), 2))) +
  theme_minimal()

modelo <- lm(Residencia_atual ~ Meses_existencia, data = df)
summary(modelo) # Regressão linear

# Correlação entre Meses_existencia x Valor_credito
ggplot(df, aes(x=Meses_existencia, y=Valor_credito)) +
  geom_hex(bins=30) +
  geom_smooth(method="lm", color="red") +
  scale_fill_gradient(low="lightblue", high="darkblue") +
  labs(title="Relação entre Tempo de Histórico e Valorvde Crédito",
       x="Meses de Existencia",
       y="Valor do credito")

# Gráfico de barra sobre Saldo em poupança x tempo de emprego
rotulo_temp <- c(
  "A71" = "A71 - desempregado",
  "A72" = "A72 - < 1 ano",
  "A73" = "A73 - 1 ≤ anos < 4",
  "A74" = "A74 - 4 ≤ anos < 7",
  "A75" = "A75 - ≥ 7 anos"
)

ggplot(as.data.frame(tbCont), aes(x=Var1, y=Freq, fill=Var2)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_discrete(
    name = "Tempo emprego",
    labels = rotulo_temp) +
  labs(title="Saldo na conta por Tempo empregado",
      x="Saldo (DM)",
      y="Frequencia") +
  theme_minimal()

# Boxplot faixa etária x valor do crédito
df$Faixa_etaria <- cut(df$Idade, breaks=c(20,30,40,50,60,70))

ggplot(df, aes(x=Faixa_etaria, y=Valor_credito)) +
  geom_boxplot(fill="lightyellow") +
  stat_summary(fun=mean, geom="point", color="orange", size=3) +
  labs(title="Distribuição do valor crédito por Faixa de idade",
       x="Faixa etária",
       y="Valor do crédito (DM)")

