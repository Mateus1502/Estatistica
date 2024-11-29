> # Dados
> IDH_alto <- c(494, 506, 504, 489, 498, 523, 491)
> IDH_medio <- c(401, 397, 402, 396, 407, 367, 376)
> IDH_baixo <- c(356, 360, 364, 360, 361, 352)
> 
> # Criar um dataframe com os dados
> dados <- data.frame(
+     valor = c(IDH_alto, IDH_medio, IDH_baixo),
+     grupo = factor(rep(c("alto", "medio", "baixo"), times = c(length(IDH_alto), length(IDH_medio), length(IDH_baixo))))
+ )
> 
> # Teste de normalidade de Shapiro-Wilk para cada grupo
> shapiro_result <- by(dados$valor, dados$grupo, shapiro.test)
> print("Resultados do Teste de Shapiro-Wilk:")
> 
> # Teste de homogeneidade de variância de Levene (usando a função native)
> # Calcula a variância de cada grupo e compara
> variancia_alto <- var(IDH_alto)
> variancia_medio <- var(IDH_medio)
> variancia_baixo <- var(IDH_baixo)
> 
> print("Variâncias dos grupos:")
> print(c(variancia_alto, variancia_medio, variancia_baixo))
> 
> # Realizar a ANOVA
> resultado_anova <- aov(valor ~ grupo, data = dados)
> print("Resumo da ANOVA:")
> summary(resultado_anova)
> 
> # Gráfico de boxplot (usando funções básicas do R)
> boxplot(valor ~ grupo, data = dados,
+         main = "Distribuição dos Resultados de Prova por Grupo de IDH",
+         xlab = "Grupo de IDH",
+         ylab = "Resultados do PISA",
+         col = c("lightblue", "lightgreen", "lightpink"))> # Dados
> IDH_alto <- c(494, 506, 504, 489, 498, 523, 491)
> IDH_medio <- c(401, 397, 402, 396, 407, 367, 376)
> IDH_baixo <- c(356, 360, 364, 360, 361, 352)
> 
> 
> # Teste de normalidade de Shapiro-Wilk para cada grupo
> shapiro_result <- by(dados$valor, dados$grupo, shapiro.test)
> print("Resultados do Teste de Shapiro-Wilk:")
[1] "Resultados do Teste de Shapiro-Wilk:"
> print(shapiro_result)
dados$grupo: alto

	Shapiro-Wilk normality test

data:  dd[x, ]
W = 0.89904, p-value = 0.3252

--------------------------------------------------------------- 
dados$grupo: baixo

	Shapiro-Wilk normality test

data:  dd[x, ]
W = 0.93614, p-value = 0.6282

--------------------------------------------------------------- 
dados$grupo: medio

	Shapiro-Wilk normality test

data:  dd[x, ]
W = 0.84586, p-value = 0.1126

> 
> # Teste de homogeneidade de variância de Levene (usando a função native)
> # Calcula a variância de cada grupo e compara
> variancia_alto <- var(IDH_alto)
> variancia_medio <- var(IDH_medio)
> variancia_baixo <- var(IDH_baixo)
> 
> print("Variâncias dos grupos:")
[1] "Variâncias dos grupos:"
> print(c(variancia_alto, variancia_medio, variancia_baixo))
[1] 136.57143 221.23810  17.76667
> 
> # Realizar a ANOVA
> resultado_anova <- aov(valor ~ grupo, data = dados)
> print("Resumo da ANOVA:")
[1] "Resumo da ANOVA:"
> summary(resultado_anova)
            Df Sum Sq Mean Sq F value   Pr(>F)    
grupo        2  73428   36714   279.2 9.99e-14 ***
Residuals   17   2236     132                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> # Realizar o teste de Tukey para comparações múltiplas
> resultado_tukey <- TukeyHSD(resultado_anova)
> 
> print("Resultados do teste de Tukey:")
[1] "Resultados do teste de Tukey:"
> print(resultado_tukey)
  Tukey multiple comparisons of means
    95% Nível de confiança

Fit: aov(formula = valor ~ grupo, data = dados)

$grupo
                  diff        lwr        upr     p adj
baixo-alto  -141.88095 -158.24822 -125.51368 0.0000000
medio-alto  -108.42857 -124.15373  -92.70341 0.0000000
medio-baixo   33.45238   17.08511   49.81965 0.0001858

> 
> # Gráfico de boxplot (usando funções básicas do R)
> boxplot(valor ~ grupo, data = dados,
+         main = "Distribuição dos Resultados de Prova por Grupo de IDH",
+         xlab = "Grupo de IDH",
+         ylab = "Resultados do PISA",
+         col = c("lightblue", "lightgreen", "lightpink"))
