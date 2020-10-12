# Bar Plots

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/FCD/BigDataRAzure/Cap04")
getwd()

?barplot

# Preparando os dados - número de casamentos em uma igreja de SP
dados <- matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67), nrow = 3, byrow = T)
dados

# Nomeando linhas e colunas na matriz
colnames(dados) <- c("0","1-150","151-300",">300")
rownames(dados) <- c("Jovem","Adulto","Idoso")
dados

# Construindo o Barplot
barplot(dados, beside = T)
barplot(dados)

# Construindo o plot - Stacked Bar Plot
# As 3 faixas de idade são representadas na mesma coluna para as diferentes quantidades
barplot(dados, col = c("steelblue1", "tan3", "seagreen3"))

# Crie uma legenda para o gráfico anterior
colors()
legend("topright", pch = 1, col = c("steelblue1", "tan3", "seagreen3"), legend = c("Jovem","Adulto","Idoso"))

# Agora temos uma coluna para cada faixa etária, mas na mesma faixa de quantidade
barplot(dados, beside = T, col = c("steelblue1", "tan3", "seagreen3"))
legend("topright", pch = 1, col = c("steelblue1", "tan3", "seagreen3"), legend = c("Jovem","Adulto","Idoso"))

# Com a Transposta da matriz, invertemos as posições entre faixa etária e faixa de quantidade
t(dados)
barplot(t(dados), beside = T, col = c("steelblue1", "tan3", "seagreen3", "peachpuff1"))
legend("topright", pch = 1, col = c("steelblue1", "tan3", "seagreen3", "peachpuff1"), legend = c("0","1-150","151-300",">300"))



