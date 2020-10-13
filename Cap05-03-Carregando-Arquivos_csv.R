# Trabalhando com arquivos csv

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/FCD/BigDataRAzure/Cap05")
getwd()

# Usando o pacote readr 
install.packages("readr")
library(readr)

# Abre o promt para escolher o arquivo
meu_arquivo <- read_csv(file.choose())


# Importando arquivos
df1 <- read_table("temperaturas.txt", 
                  col_names = c("DAY","MONTH","YEAR","TEMP"))

head(df1)
View(df1)
str(df1)

read_lines("temperaturas.txt", skip = 0, n_max = -1L)
read_file("temperaturas.txt")

# Exportando e Importando
write_csv(iris, "iris.csv")
dir()

# col_integer(): 
# col_double(): 
# col_logical(): 
# col_character(): 
# col_factor(): 
# col_skip(): 
# col_date() (alias = “D”), col_datetime() (alias = “T”), col_time() (“t”) 

df_iris <- read_csv("iris.csv", col_types = list(
  Sepal.Length = col_double(),
  Sepal.Width = col_double(),
  Petal.Length = col_double(),
  Petal.Width = col_double(),
  Species = col_factor(c("setosa", "versicolor", "virginica"))
))

dim(df_iris)
str(df_iris)


# Manipulando arquivos csv
df_cad <- read_csv("cadastro.csv")
View(df_cad)
class(df_cad)


install.packages("dplyr")
library(dplyr)
options(warn=-1)

df_cad <- tbl_df(df_cad)
head(df_cad)
View(df_cad)

write.csv(df_cad, "df_cad_bkp.csv")

# Importando vários arquivos simultaneamente
list.files()
lista_arquivos <- list.files('C:/FCD/BigDataRAzure/Cap05/arquivos', full.names = TRUE)
class(lista_arquivos)
lista_arquivos

lista_arquivos2 <- lapply(lista_arquivos, read_csv)
class(lista_arquivos2)
View(lista_arquivos2)


# Parsing
parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/34", "%y/%m/%d")


locale("en")
locale("fr")
locale("pt")



                   
