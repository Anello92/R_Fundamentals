# Split-Apply-Combine - plyr

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/FCD/BigDataRAzure/Cap07")
getwd()

# Instalando os pacotes
install.packages("plyr")
install.packages("gapminder")
# https://www.gapminder.org

library(plyr)
library(gapminder)
?gapminder

# Split-Apply-Combine
?ddply
df <- ddply(gapminder, ~ continent, 
            summarize, 
            max_le = max(lifeExp))
str(df)
head(df)
View(df)
levels(df$continent)


# Split-Apply-Combine
ddply(gapminder, ~ continent, 
      summarize, 
      n_uniq_countries = length(unique(country)))


ddply(gapminder, ~ continent, 
      function(x) c(n_uniq_countries = length(unique(x$country))))


ddply(gapminder, ~ continent, 
      summarize,
      min = min(lifeExp), 
      max = max(lifeExp),
      median = median(gdpPercap))


# Usando um dataset do ggplot
library(ggplot2)
data(mpg)
str(mpg)
?mpg


# Trabalhando com um subset do dataset
data <- mpg[,c(1,7:9)]
str(data)
View(data)


# Sumarizando e Agregando Dados
ddply(data, .(manufacturer), 
      summarize, 
      avgcty = mean(cty))


# Várias funções em uma única chamada
ddply(data, .(manufacturer), 
      summarize, 
      avgcty = mean(cty), 
      sdcty = sd(cty), 
      maxhwy = max(hwy))


# Sumarizando os dados pela combinação de variáveis/fatores
ddply(data, .(manufacturer, drv), 
      summarize, 
      avgcty = mean(cty), 
      sdcty = sd(cty),
      maxhwy = max(hwy))





