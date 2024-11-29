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

# Teste de homogeneidade de variância de Levene
resultado_levene <- leveneTest(valor - grupo, data = dados)
print
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

# Realizar o teste de Tukey 
resultado_tukey <- TukeyHSD(resultado_anova)
plot(resultado_tukey , las = 1)

print("Resultados do teste de Tukey:")
print(resultado_tukey)

# Gráfico de boxplot
boxplot(valor ~ grupo, data = dados,
        main = "Distribuição dos Resultados de Prova por Grupo de IDH",
        xlab = "Grupo de IDH",
        ylab = "Resultados do PISA",
        col = c("lightblue", "lightgreen", "lightpink"))
#Murilo rode isso aqui
par(mfrow = c(1, 3))  # Organizar a área de plotagem em 3 colunas
hist(IDH_alto, main = "Histograma IDH Alto", xlab = "Resultados do PISA", col = "lightblue")
hist(IDH_medio, main = "Histograma IDH Médio", xlab = "Resultados do PISA", col = "lightgreen")
hist(IDH_baixo, main = "Histograma IDH Baixo", xlab = "Resultados do PISA", col = "lightpink")
par(mfrow = c(1, 1))  # Restaurar a área de plotagem para uma única janela
