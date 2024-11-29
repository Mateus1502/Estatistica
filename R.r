# Dados
IDH_alto <- c(494, 506, 504, 489, 498, 523, 491)
IDH_medio <- c(401, 397, 402, 396, 407, 367, 376)
IDH_baixo <- c(356, 360, 364, 360, 361, 352)

# Criar um dataframe com os dados
dados <- data.frame(
  valor = c(IDH_alto, IDH_medio, IDH_baixo),
  grupo = factor(rep(c("alto", "medio", "baixo"), times = c(length(IDH_alto), length(IDH_medio), length(IDH_baixo))))
)

# Teste de normalidade de Shapiro-Wilk para cada grupo
shapiro_result <- by(dados$valor, dados$grupo, shapiro.test)
print("Resultados do Teste de Shapiro-Wilk:")
print(shapiro_result)

# Teste de homogeneidade de variância de Levene (usando a função native)
# Calcula a variância de cada grupo e compara
variancia_alto <- var(IDH_alto)
variancia_medio <- var(IDH_medio)
variancia_baixo <- var(IDH_baixo)

print("Variâncias dos grupos:")
print(c(variancia_alto, variancia_medio, variancia_baixo))

# Realizar a ANOVA
resultado_anova <- aov(valor ~ grupo, data = dados)
print("Resumo da ANOVA:")
summary(resultado_anova)

# Realizar o teste de Tukey para comparações múltiplas
resultado_tukey <- TukeyHSD(resultado_anova)
plot(resultado_tukey , las = 1)

print("Resultados do teste de Tukey:")
print(resultado_tukey)

# Gráfico de boxplot (usando funções básicas do R)
boxplot(valor ~ grupo, data = dados,
        main = "Distribuição dos Resultados de Prova por Grupo de IDH",
        xlab = "Grupo de IDH",
        ylab = "Resultados do PISA",
        col = c("lightblue", "lightgreen", "lightpink"))
# Gráfico de boxplot (adaptado para os dados fornecidos)
boxplot(valor ~ grupo, data = dados,
        xlab = "Grupo de IDH",
        ylab = "Resultados do PISA",
        main = "Distribuição dos Resultados de Prova por Grupo de IDH",
        col = c("skyblue", "pink", "lightgreen"))

# Adicionando jitter aos pontos para visualizar melhor os valores individuais
stripchart(valor ~ grupo, data = dados,
           vertical = TRUE, method = "jitter", add = TRUE,
           pch = 21, col = "black", bg = "white")
