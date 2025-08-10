# imporntando, lende e preparando o dataframe
setwd("C:/Users/Joao Duarte/Desktop/Trabalho R/Trabalho_Probabilidade_Estatistica")

df <- read.table("german.data", header = FALSE, sep = " ")

colnames(df) <- c(  "Status_conta", "Meses_existencia", "Historico_credito", "Proposito",
                    "Valor_credito", "Saldo_poupanca", "Tempo_emprego", "Taxa_parcela_renda",
                    "Status_pessoal_sexo", "Fiador", "Residencia_atual", "Bens",
                    "Idade", "Planos de parcelamento", "Moradia", "Credito_existente", "Emprego", "Dependentes",
                    "Telefone", "Trabalhador_estrangeiro", "Classe")

# Analise bruta
head(df) # Primeiras linhas
tail(df) # Ultimas linhas
str(df) # Estrutura do df
summary(df) # Est. Descritiva

# Tratamento
colSums(is.na(df)) # Verifica dados faltantes

par(mar = c(4, 4, 2, 1))

# Verificando outliers 
boxplot(df$Meses_existencia, main =  "Outliers em Meses Existencia")
boxplot(df$Valor_credito, main =  "Outliers em Valor_credito")
boxplot(df$Taxa_parcela_renda, main =  "Outliers em Taxa_parcela_renda")
boxplot(df$Residencia_atual, main =  "Outliers em Residencia_atual")
boxplot(df$Idade, main =  "Outliers em Idade")
boxplot(df$Credito_existente, main =  "Outliers em Credito_existente")
boxplot(df$Dependentes, main =  "Outliers em Dependentes") 

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
        ylin = c(0, 20000),
        main = "Distribuição Valor Crédito",
        ylab = "Valores (DM)", 
        col = "lightgreen")

# Analise bivariada:  "Saldo_poupanca" e "Tempo_emprego"
str(df[c("Saldo_poupanca", "Tempo_emprego")])

tbCont <- table(df$Saldo_poupanca, df$Tempo_emprego)
print(tbCont)
addmargins((tbCont)) #soma total

prop.table(tbCont)
prop.table(tbCont, margin = 1)
prop.table(tbCont, margin = 2)

tst <- chisq.test(tbCont) # Associação significativa
print(tst) # Associação sig == p < 0.05
tst$expected # Esperados

ggplot(as.data.frame(tbCont), 
       aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Saldo Poupança", y = "Frequência", 
       fill = "Tempo Emprego", 
       title = "Relação Saldo Poupança x Tempo Emprego") +
  theme_minimal()
