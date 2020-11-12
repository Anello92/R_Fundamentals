# Projeto - Prevendo Reclamações de Clientes

# Este projeto tem como objetivo analisar reclamações de clientes e então:

# Aprendizagem Supervisionada

# 1- Prever o Número de Reclamações (Regressão)
# 2- Prever se o Cliente Vai Responder ao Feedback da Empresa ou Não (Classificação Binária)
# 3- Prever o Meio Que o Cliente Usará Para a Reclamação (Classificação Multiclasse)
# 4- Prever a Resposta Para um Tipo de Reclamação (Classificação Multiclasse - Probabilidades)
# 5- Prever a Região Geográfica de Onde Originará a Reclamação (Classificação Multiclasse)

# Aprendizagem Não Supervisionada

# 6- Segmentar Clientes Por Tipo de Reclamação (Clusterização)

######################################################################################################## 

#####  Fase 1 - Análise Exploratória, Limpeza e Transformação nos Dados #####  


##### Fase 1 - Tarefa 1: Definindo o Problema de Negócio #####

# Neste projeto, vamos trabalhar com Machine Learning em R para prever possíveis reclamações de clientes.

# Mas faremos isso de 3 formas diferentes:

# Usaremos Regressão para prever a quantidade de possíveis reclamações.
# Usaremos Classificação para prever se um novo cliente pode ou não vir a fazer uma reclamação.
# Usaremos Clusterização para agrupar os clientes por similaridade e encontrar padrões que podem 
# levar à reclamação.

# Nosso conjunto de dados contém a lista de reclamações de consumidores recebidas pelo 
# Consumer Financial Protection Bureau. 

# São reclamações sobre produtos e serviços financeiros recebidos pela agência de dezembro de 2011 
# a janeiro de 2020. Os dados estão disponíveis em formato csv e o download foi feito no link abaixo:

# Fonte de Dados: https://catalog.data.gov/dataset/consumer-complaint-database


# Definindo o diretório de trabalho
setwd("~/Dropbox/DSA/AnaliseEstatisticaMachineLearning/Cap12")
getwd()

# Pacotes
library(readr)
library(ggplot2)


##### Fase 1 - Tarefa 2: Coletando e Carregando os Dados ##### 


# Carregando o dataset
reclamacoes_clientes <- read.csv('dados/complaints.csv')
dim(reclamacoes_clientes)
str(reclamacoes_clientes)
View(reclamacoes_clientes)


##### Fase 1 - Tarefa 3: Limpeza do Dataset com 1,5 Milhão de Registros ##### 

# Atualizando os nomes das colunas
nomes_colunas <- c("data_reclamacao", "produto", "sub_produto", "problema", "sub_problema", 
                   "narrativa_reclamacao", "resposta_publica_empresa", "empresa", "estado", 
                   "postal_code", "tags", "consentimento_consumidor", "meio_envio_reclamacao", 
                   "data_enviada_para_empresa", "resposta_empresa_para_consumidor", "resposta_rapida", 
                   "disputa_cliente", "id_reclamacao")


colnames(reclamacoes_clientes) <- nomes_colunas
View(reclamacoes_clientes)


# Criando colunas adicionais no dataset, a partir da coluna de data

# Tipo da variável
str(reclamacoes_clientes$data_reclamacao)

# Converte para string
chardates <- as.character(reclamacoes_clientes$data_reclamacao)
str(chardates)

# Se tiver, substitui / por -
chardates <- gsub("/", "-", chardates)

# Converte para tipo date
z <- as.Date(chardates,"%Y-%m-%d") 

# Cria as novas colunas
reclamacoes_clientes$AnoReclamacao <- format(z, "%Y") 
reclamacoes_clientes$MesReclamacao <- format(z, "%m") 
reclamacoes_clientes$DiaReclamacao <- format(z, "%d") 


# Visualizando o dataset
View(reclamacoes_clientes)


# Verificando o range das reclamações
range(reclamacoes_clientes$AnoReclamacao)
range(reclamacoes_clientes$MesReclamacao)
range(reclamacoes_clientes$DiaReclamacao)




##### Fase 1 - Tarefa 4: Análise Exploratória ##### 


# Distribuição de Reclamações Por Estado (Expressa em %)

# Agregando as reclamações
?aggregate
agrega_reclamacoes_estado = aggregate(id_reclamacao ~ estado, 
                                      data = reclamacoes_clientes, 
                                      FUN = length)
View(agrega_reclamacoes_estado)

# Gráfico 1
?ggplot
ggplot(agrega_reclamacoes_estado, aes(x = estado, width = 0.5)) + 
  geom_bar(stat = "identity", 
           aes(y = id_reclamacao / sum(id_reclamacao) * 100)) + 
  geom_text(aes(label = paste(round(id_reclamacao / sum(id_reclamacao) * 100, 2),"%"),
                y = id_reclamacao / sum(id_reclamacao) * 100), 
            hjust = -0.2, 
            check_overlap = TRUE, 
            size = 3) + 
  ylim(0, round(max(agrega_reclamacoes_estado$id_reclamacao) / sum(agrega_reclamacoes_estado$id_reclamacao), 2) * 100) + 
  coord_flip() + 
  xlab(label = "Estados") + 
  ylab(label = "Percentual de Reclamações Por Estado")

# Distribuição de Reclamações Por Produto (Expressa em %)

# Agregando as reclamações
str(reclamacoes_clientes$produto)
reclamacoes_clientes$produto <- as.character(reclamacoes_clientes$produto)
str(reclamacoes_clientes$produto)
agrega_reclamacoes_produto = aggregate(id_reclamacao ~ produto, 
                                       data = reclamacoes_clientes, 
                                       FUN = length)
View(agrega_reclamacoes_produto)

# Gráfico 2
ggplot(agrega_reclamacoes_produto, aes(x = produto, width = 0.5)) + 
  geom_bar(stat = "identity", 
           aes(y = id_reclamacao / sum(id_reclamacao) * 100)) + 
  geom_text(aes(label = paste(round(id_reclamacao / sum(id_reclamacao) * 100, 2), "%"),
                y = id_reclamacao / sum(id_reclamacao) * 100),
            hjust = -0.2,
            check_overlap = TRUE,
            size = 3) + 
  ylim(0, round(max(agrega_reclamacoes_produto$id_reclamacao) / sum(agrega_reclamacoes_produto$id_reclamacao), 2) * 100) + 
  coord_flip() + 
  xlab(label = "Produtos") + 
  ylab(label = "Percentual de Reclamações Por Produto")

# Distribuição de Reclamações Por Tipo de Resposta da Empresa (Expressa em %)

# Agregando as reclamações
str(reclamacoes_clientes$resposta_empresa_para_consumidor)
reclamacoes_clientes$resposta_empresa_para_consumidor = as.character(reclamacoes_clientes$resposta_empresa_para_consumidor)
agrega_reclamacoes_resposta = aggregate(id_reclamacao ~ resposta_empresa_para_consumidor, 
                                        data = reclamacoes_clientes, 
                                        FUN = length)
View(agrega_reclamacoes_resposta)

# Gráfico 3
ggplot(agrega_reclamacoes_resposta, aes(x = resposta_empresa_para_consumidor, width = 0.5)) + 
  geom_bar(stat = "identity", 
           aes(y = id_reclamacao / sum(id_reclamacao) * 100)) + 
  geom_text(aes(label = paste(round(id_reclamacao / sum(id_reclamacao) * 100, 2),"%"),
                y = id_reclamacao/sum(id_reclamacao) * 100),
            hjust = -0.2,
            check_overlap = TRUE,
            size = 3) + 
  coord_flip() + 
  xlab(label = "Resposta das Empresas") + 
  ylab(label = "Percentual de Reclamações") 

# Distribuição de Reclamações Por Disputa Judicial (Expressa em %)

# Agregando as reclamações
str(reclamacoes_clientes$disputa_cliente)
reclamacoes_clientes$disputa_cliente = as.character(reclamacoes_clientes$disputa_cliente)
agrega_reclamacoes_disputa = aggregate(id_reclamacao ~ disputa_cliente,
                                       data = reclamacoes_clientes,
                                       FUN = length)
View(agrega_reclamacoes_disputa)

# Gráfico 4
ggplot(agrega_reclamacoes_disputa, aes(x = disputa_cliente, width = 0.5)) + 
  geom_bar(stat = "identity",
           aes(y = id_reclamacao / sum(id_reclamacao) * 100)) + 
  geom_text(aes(label = paste(round(id_reclamacao / sum(id_reclamacao) * 100, 2),"%"),
                y = id_reclamacao / sum(id_reclamacao) * 100),
            vjust = -0.5,
            check_overlap = TRUE,
            size = 3) + 
  xlab(label = "O Consumidor Foi Para Disputa Judicial") + 
  ylab(label = "Percentual de Reclamações")

# Meio de Comunicação Usado Para Envio da Reclamação

# Agregando as reclamações
str(reclamacoes_clientes$meio_envio_reclamacao)
reclamacoes_clientes$meio_envio_reclamacao = as.character(reclamacoes_clientes$meio_envio_reclamacao)
agrega_reclamacoes_meio = aggregate(id_reclamacao ~ meio_envio_reclamacao,
                                    data = reclamacoes_clientes,
                                    FUN = length)

View(agrega_reclamacoes_meio)

# Gráfico 5
ggplot(agrega_reclamacoes_meio, aes(x = meio_envio_reclamacao, width = 0.5)) + 
  geom_bar(stat = "identity", aes(y = id_reclamacao / sum(id_reclamacao) * 100)) + 
  geom_text(aes(label = paste(round(id_reclamacao / sum(id_reclamacao) * 100, 2),"%"),
                y = id_reclamacao / sum(id_reclamacao) * 100),
            vjust  = -0.5,
            check_overlap = TRUE,
            size = 3) +  
  xlab(label = "Meio de Envio da Reclamação") + 
  ylab(label = "Percentual de Reclamações") 

# Reclamações Por Ano

# Gráfico 6 (2 variáveis)
ggplot(reclamacoes_clientes, aes(AnoReclamacao)) + 
  geom_bar(position = "dodge") + 
  xlab(label = "Ano da Reclamação") + 
  ylab(labe = "Número de Reclamações")

# Respostas das Empresas Por Ano

# Gráfico 7 (3 variáveis)
ggplot(reclamacoes_clientes, aes(AnoReclamacao, fill = resposta_empresa_para_consumidor)) + 
  geom_bar(position = "dodge") + 
  xlab(label = "Ano da Reclamação") + 
  ylab(label = "Número de Reclamações")

# Reclamações Por Produto Por Ano

# Gráfico 8 (3 variáveis)
ggplot(reclamacoes_clientes, aes(AnoReclamacao, fill = produto)) + 
  geom_bar(position = "dodge") + 
  xlab(label = "Ano da Reclamação") + 
  ylab(label = "Número de Reclamações")

# Velocidade da Resposta da Empresa Por Ano

# Gráfico 9
ggplot(reclamacoes_clientes, aes(AnoReclamacao, fill = resposta_rapida)) + 
  geom_bar(position = "dodge") + 
  xlab(label = "Ano da Reclamação") + 
  ylab(label = "Número de Reclamações")

# Top 10 Empresas Que Mais Receberam Reclamações

# Agregando as reclamações e Gráfico 10 no mesmo bloco de código
library(dplyr)

reclamacoes_clientes %>%
  group_by(empresa) %>%
  summarise(id_reclamacao = n()) %>%
  arrange(desc(id_reclamacao)) %>%
  mutate(empresa = factor(empresa, empresa, ordered = T)) %>% 
  top_n(10)  %>%
  ggplot(., aes(x = empresa, y = id_reclamacao)) +
  geom_bar(stat = "identity", aes(y = id_reclamacao / sum(id_reclamacao) * 100)) + 
  geom_text(aes(label = paste(round(id_reclamacao / sum(id_reclamacao) * 100, 2),"%"),
                y = id_reclamacao / sum(id_reclamacao) * 100),
            hjust = -0.2,
            check_overlap = TRUE,
            size = 3) + 
  coord_flip() + 
  xlab(label = "Empresas Campeãs de Reclamações") + 
  ylab(label = "Percentual de Reclamações") 


# Fim da Fase 1

######################################################################################################## 

#####  Fase 2 - Pré-Processamento dos Dados Para Modelagem Preditiva #####  


# Carregando o dataset
reclamacoes_clientes <- read.csv('dados/complaints.csv')
dim(reclamacoes_clientes)
str(reclamacoes_clientes)
View(reclamacoes_clientes)

# Atualizando os nomes das colunas
nomes_colunas <- c("data_reclamacao", "produto", "sub_produto", "problema", "sub_problema", 
                   "narrativa_reclamacao", "resposta_publica_empresa", "empresa", "estado", 
                   "postal_code", "tags", "consentimento_consumidor", "meio_envio_reclamacao", 
                   "data_enviada_para_empresa", "resposta_empresa_para_consumidor", "resposta_rapida", 
                   "disputa_cliente", "id_reclamacao")

colnames(reclamacoes_clientes) <- nomes_colunas
View(reclamacoes_clientes)

# Criando colunas adicionais no dataset, a partir da coluna de data

# Converte para string
chardates <- as.character(reclamacoes_clientes$data_reclamacao)
str(chardates)

# Se tiver, substitui / por -
chardates <- gsub("/", "-", chardates)

# Converte para tipo date
z <- as.Date(chardates,"%Y-%m-%d") 

# Cria as novas colunas
reclamacoes_clientes$AnoReclamacao <- format(z, "%Y") 
reclamacoes_clientes$MesReclamacao <- format(z, "%m") 
reclamacoes_clientes$DiaReclamacao <- format(z, "%d") 

# Visualizando o dataset
View(reclamacoes_clientes)


# 1 - Modelo de Regressão Linear - Prevendo o Número de Reclamações

# Carrega dados de total de assets sobre as empresas ao longo dos anos
# Usaremos essa variável para prever o total de reclamações
df_assets = read.csv('dados/assets.csv')
View(df_assets)

# Remove a terceira coluna
df_assets = df_assets[-3]
View(df_assets)

# Vamos agregar os dados e encontrar os totais por ano
df_total_reclamacoes <- aggregate(cbind(count = id_reclamacao) ~ AnoReclamacao, 
                                  data = reclamacoes_clientes, 
                                  FUN = function(x){NROW(x)})

View(df_total_reclamacoes)

# Agora concatenamos os totais
df_final <- cbind(df_assets, df_total_reclamacoes)
View(df_final)
df_final <- df_final[, -1]
View(df_final)
df_final <- df_final[, -2]
View(df_final)
names(df_final) <- c("Total_Assets", "Total_Reclamacoes")
View(df_final)

# Plot Assets x Reclamações
?plot
plot(df_final$Total_Assets, 
     df_final$Total_Reclamacoes, 
     xlab = "Total Assets",
     ylab = "Total Reclamações",
     type = "p")


# Machine Learning - Regressão Linear

# Regressão Linear Simples - Nosso objetivo é resolver esta equação:

# y = a + bx

# y = variável de saída
# x = variável de entrada
# a e b = coeficientes

# Regressão Linear Múltipla - Nosso objetivo é resolver esta equação:

# y = a + b1x1 + b2x2 + bnxn

# Construindo o Modelo de Regressão Linear (cuidado com os tipos de dados)
?lm
# Fórmula: lm(y ~ x)

# Define as variáveis
y <- df_final$Total_Reclamacoes
x <- df_final$Total_Assets

# Cria o modelo
modelo_reg_v1 = lm(y ~ x)
summary(modelo_reg_v1)

# Visualizando os tipos de dados
str(df_final)

# Transforma de fator para tipo caracter
install.packages("varhandle")
library(varhandle)
df_final$Total_Assets <- unfactor(df_final$Total_Assets)
str(df_final)

# Transforma de tipo caracter para numérico
df_final$Total_Assets <- as.numeric(as.character(df_final$Total_Assets))
str(df_final)
View(df_final)

# Como valores NA foram gerados, é preciso carregar o dataframe original novamente!
# Execute o comando de concatenação dos dataframes novamente!

# Agora concatenamos os totais
df_final <- cbind(df_assets, df_total_reclamacoes)
View(df_final)
df_final <- df_final[, -1]
View(df_final)
df_final <- df_final[, -2]
View(df_final)
names(df_final) <- c("Total_Assets", "Total_Reclamacoes")
View(df_final)

# Transforma de tipo caracter para numérico, removendo as vírgulas com gsub()
df_final$Total_Assets <- as.numeric(gsub(",","", as.character(df_final$Total_Assets)))
str(df_final)
View(df_final)

# Agora sim, criamos o modelo

# Define as variáveis
y <- df_final$Total_Reclamacoes
x <- df_final$Total_Assets

# Cria o modelo
modelo_reg_v2 = lm(y ~ x)
summary(modelo_reg_v2)

# Vamos remover o registro de 2020, pois ele está muito fora do padrão dos dados.
# Antes, vamos analisar como fazemos isso.
df_final
df_final[-1]
df_final[-1,]
df_final[1:9,]
dim(df_final)
dim(df_final)[1]
n <- dim(df_final)[1]
df_final[1:9,]
df_final[1:(n-1),]

# Removemos a última linha do dataframe
df_final <- df_final[1:(n-1),]
View(df_final)

# Agora sim, criamos a terceira versão do modelo

# Define as variáveis
y <- df_final$Total_Reclamacoes
Total_Assets <- df_final$Total_Assets

# Cria o modelo
modelo_reg_v3 = lm(y ~ Total_Assets)
summary(modelo_reg_v3)

# Interpretação do Modelo de Regressão em R

# ****************************************************
# *** Estas informações abaixo é que farão de você ***
# *** um verdadeiro conhecedor de Machine Learning ***
# ****************************************************

# Equação de Regressão
# y = a + bx (simples)
# y = a + b0 x v0 + b1 x v1 (múltipla)

# Resíduos
# Diferença entre os valores observados de uma variável e seus valores previstos
# Seus resíduos devem se parecer com uma distribuição normal, o que indica
# que a média entre os valores previstos e os valores observados é próximo de 0 (o que é bom)

# Coeficiente - Intercept - a (alfa)
# Valor de a na equação de regressão

# Coeficientes - Nomes das variáveis - b (beta)
# Valor de b na equação de regressão

# Obs: A questão é que lm() ou summary() têm diferentes convenções de 
# rotulagem para cada variável explicativa. 
# Em vez de escrever slope_1, slope_2, .... 
# Eles simplesmente usam o nome da variável em qualquer saída para 
# indicar quais coeficientes pertencem a qual variável.

# Erro Padrão
# Medida de variabilidade na estimativa do coeficiente a (alfa) e b (beta). 
# O ideal é que este valor seja menor que o valor do coeficiente, mas nem sempre 
# isso irá ocorrer.

# Asteriscos 
# Os asteriscos representam os níveis de significância de acordo com o p-value.
# Quanto mais estrelas, maior a significância.
# Atenção --> Muitos astericos indicam que é improvável que não exista 
# relacionamento entre as variáveis.

# Valor t
# Define se coeficiente da variável é significativo ou não para o modelo. 
# Ele é usado para calcular o p-value e os níveis de significância.

# p-value
# O p-value representa a probabilidade que a variável não seja relevante. 
# Deve ser o menor valor possível. 
# Se este valor for realmente pequeno, o R irá mostrar o valor 
# como notação científica

# Em regressão linear, o valor-p define o resultado para o teste de hipótese:

# H0: Não há relacionamento significante entre a variável preditora x e a variável target y.
# H1: Há relacionamento significante entre a variável preditora x e a variável target y.

# Se valor-p > 0.05 indica que não há evidência estatística para rejeitar H0. Falhamos em rejeitar a H0.
# Se valor-p < 0.05 indica que há evidência estatística para rejeitar H0.

# Significância
# São aquelas legendas próximas as suas variáveis
# Espaço em branco - ruim
# Pontos - razoável
# Asteriscos - bom
# Muitos asteriscos - muito bom

# Residual Standard Error
# Este valor representa o desvio padrão dos resíduos

# Degrees of Freedom
# É a diferença entre o número de observações na amostra de treinamento 
# e o número de variáveis no seu modelo.

# R-squared (coeficiente de determinação - R^2)
# Ajuda a avaliar o nível de precisão do nosso modelo. 
# Quanto maior, melhor, sendo 1 o valor ideal.

# F-statistics
# É o teste F do modelo. Esse teste obtém os parâmetros do nosso modelo 
# e compara com um modelo que tenha menos parâmetros.
# Em teoria, um modelo com mais parâmetros tem um desempenho melhor. 

# Se o seu modelo com mais parâmetros NÃO tiver perfomance
# melhor que um modelo com menos parâmetros, o valor do p-value será bem alto. 

# Se o modelo com mais parâmetros tiver performance
# melhor que um modelo com menos parâmetros, o valor do p-value será mais baixo.

# Lembre-se que correlação não implica causalidade

# Checando se os resíduos são normalmente distribuídos
View(df_final)
?qqnorm
resid(modelo_reg_v3)
qqnorm(resid(modelo_reg_v3))
qqline(resid(modelo_reg_v3))
hist(resid(modelo_reg_v3))

# Vamos treinar o modelo com 7 registros e fazer a avaliação do modelo com 2 registros
df_final_treino = df_final[1:7,]
df_final_teste = df_final[-1:-7,]
View(df_final_treino)
View(df_final_teste)

# Cria o modelo
# Observe que estamos usando notação diferente agora, mas a ideia é a mesma
modelo_reg_v4 = lm(Total_Reclamacoes ~ Total_Assets, df_final_treino)
summary(modelo_reg_v4)
qqnorm(resid(modelo_reg_v4))
qqline(resid(modelo_reg_v4))
hist(resid(modelo_reg_v4))

# Extraindo as previsões do modelo com intervalo de confiança
?predict
previsoes_treino <- predict(modelo_reg_v4, newdata = df_final_treino, interval = "confidence")
previsoes_reais_treino <- cbind(df_final_treino, previsoes_treino)
View(previsoes_reais_treino)
modelo_reg_v4$residuals

# Métricas de erro do modelo (quanto menor, melhor)

# Residual Sum of Squares
RSS <- c(crossprod(modelo_reg_v4$residuals))
print(RSS)

# Mean Squared Error
MSE <- RSS / length(modelo_reg_v4$residuals)
print(MSE)

# Root Mean Squared Error
RMSE <- sqrt(MSE)
print(RMSE)

# Comparando os modelos
RMSE_modelo_v2 <- sqrt(c(crossprod(modelo_reg_v2$residuals)) / length(modelo_reg_v2$residuals))
print(RMSE_modelo_v2)
RMSE_modelo_v3 <- sqrt(c(crossprod(modelo_reg_v3$residuals)) / length(modelo_reg_v3$residuals))
print(RMSE_modelo_v3)
RMSE_modelo_v4 <- sqrt(c(crossprod(modelo_reg_v4$residuals)) / length(modelo_reg_v4$residuals))
print(RMSE_modelo_v4)

# Plot
library("ggplot2")
p <- ggplot(previsoes_reais_treino, aes(Total_Reclamacoes, fit)) +
  geom_point() +
  stat_smooth(method = lm)

p + geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

# Previsões com o modelo treinado

# Define as variáveis (dados de teste)
y_teste <- df_final_teste$Total_Reclamacoes
x_teste <- df_final_teste$Total_Assets
class(x_teste)

# Ajustamos x pois predict() espera receber um dataframe
x_teste_df = as.data.frame(x_teste)
class(x_teste_df)
colnames(x_teste_df) <- 'Total_Assets'
View(x_teste_df)

# Previsões
?predict
previsoes_teste = predict(modelo_reg_v4, newdata = x_teste_df)
View(previsoes_teste)

# Comparando valor real x valor previsto
previsoes_reais_teste <- cbind(df_final_teste, previsoes_teste)
View(previsoes_reais_teste)

# Erro do modelo ao prever dados de teste - Mean Squared Prediction Error (MSPE)
mspe <- mean((df_final_teste$Total_Assets - previsoes_teste) ^ 2)
print(mspe)

# Gerando novos dados
print(df_assets)

x <- 13044187413
x_novo = as.data.frame(x)
class(x_novo)
colnames(x_novo) <- 'Total_Assets'
View(x_novo)

# Previsão para o ano corrente com base no patromônio total da empresa
previsoes_novo = predict(modelo_reg_v4, newdata = x_novo)
print(previsoes_novo)

# Resposta: Esperamos ter aproximadamente 360.792 reclamações para este ano!