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

  # Verificando outliers 
boxplot(df$Meses_existencia, main =  "Outliers em Meses Existencia")
boxplot(df$Valor_credito, main =  "Outliers em Valor_credito")
boxplot(df$Taxa_parcela_renda, main =  "Outliers em Taxa_parcela_renda")
boxplot(df$Residencia_atual, main =  "Outliers em Residencia_atual")
boxplot(df$Idade, main =  "Outliers em Idade")
boxplot(df$Credito_existente, main =  "Outliers em Credito_existente")
boxplot(df$Dependes, main =  "Outliers em Dependes")
