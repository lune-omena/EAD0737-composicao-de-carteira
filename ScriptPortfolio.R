  
# ------------------------------------------------
# FEAUSP
# EAD737 - Topicos Avancados de Financas
# Prof. Leandro Maciel (leandromaciel@usp.br)
# install.packages(c("PerformanceAnalytics", "PortfolioAnalytics", "readxl"))
# ------------------------------------------------

# Script Aula 8 - Composicao de Carteiras - II
#PortfolioAnalytics
# ------------------------------------------------

install.packages("ROI")

install.packages('fPortfolio')
install.packages("Rglpk")
install.packages("ROI.plugin.glpk")
install.packages("ROI.plugin.quadprog")


# Carregar os pacotes necessarios:

library(readxl)
library(PortfolioAnalytics)
library(PerformanceAnalytics)

# ------------------------------------------------

cat("\f") # Limpar o console

rm(list = ls()) # Limpar todas as variaveis

# ------------------------------------------------

# Carregar os dados dos retornos das acoes:

setwd("~/Documentos/ano_3/finanças") # definir diretorio

Acoes = read_excel("dados_lu_ei.xlsx",sheet = "retornos")

# ------------------------------------------------

# Avaliar as correlacoes dos retornos:

noAcoes = ncol(Acoes) - 1 # numero de acoes (primeira coluna datas)

matrizCorrelacao = round(cor(as.matrix(Acoes[,2:(noAcoes+1)])),3)*100

# Dividir amostra antes e depois da formacao de carteiras...

# Antes, periodo de 31/10/17 a 29/10/2021:

Retornos = Acoes[1:968,] # divisao com base nos dados

# Depois, periodo de 1/11/21 a 28/10/2022:
RetornosFora = Acoes[969:1210,]

summary(Retornos) # estatisticas descritivas retornos

# ------------------------------------------------

# Transformar dados em tipo series temporais (pacote exige):

Retornos = xts(Retornos[,2:(noAcoes+1)], as.Date(Retornos$Date, format = "%Y-%m-%d"))
RetornosFora = xts(RetornosFora[,2:(noAcoes+1)], as.Date(RetornosFora$Date, format = "%Y-%m-%d"))

# ------------------------------------------------

# Especificacoes da carteira:

fund.names = colnames(Retornos) # nome dos ativos

carteira = portfolio.spec(assets = fund.names) # criando a carteira

# Restricao 1 - carteira totalmente investida:

carteira = add.constraint(portfolio = carteira, type = "full_investment")

# Restricao 2 - apenas posicoes compradas:

carteira = add.constraint(portfolio = carteira, type = "long_only")

# Restricao 3 - para os pesos:

# carteira = add.constraint(portfolio = carteira, type = "box", min = 0, max = 0.15)

# ------------------------------------------------

# Fronteira Eficiente:

FE = meanvar.efficient.frontier(portfolio = carteira, Retornos, n.portfolios = 20)
plot(100*FE[,2],100*FE[,1],xlab="Risco (%)",ylab = "Retorno (%)",col="blue",main = "Fronteira Eficiente")

# ------------------------------------------------

# Processo de otimizacao... 

# Definindo o objetivo do investidor:

# 1. Carteira de variancia minima (CVM) - eficiente e com menor risco...

carteira = add.objective(portfolio = carteira, type = "risk", name = "StdDev")

# 2. Carteira de retorno pre definido - eficiente e com menor risco para o retorno desejado...

carteira = add.constraint(portfolio = carteira, type = "return",return_target = 0.0008)

# Otimizando a carteira...

MinhaCarteira = optimize.portfolio(R = Retornos,portfolio = carteira,optimize_method = "ROI",trace = TRUE)

MinhaCarteira # informacoes da carteira

# Calcular o retorno medio (%) da carteira:

mean(Return.portfolio(Retornos,weights = extractWeights(MinhaCarteira)))*100

# Verifique a carteira na fronteira eficiente!!!

100*round(MinhaCarteira[["weights"]],4) # pesos em cada ativo (%)

# Alocacoes:

plot(MinhaCarteira)

# Vamos verificar o desempenho dela fora da amostra 2017 a 2019...

# Calcular os retornos nesse periodo:

RetornoMC = Return.portfolio(RetornosFora,weights = extractWeights(MinhaCarteira))

# Retorno medio (%) fora da amostra:

mean(RetornoMC)*100

# Desvio-padrao (%) fora da amostra:

sd(RetornoMC)*100

# Vizualizacao:

plot(RetornoMC)

# Visualizar retornos acumulados (soma geometrica dos retornos dia a dia): 

chart.CumReturns(RetornoMC)

# ------------------------------------------------