# Carregue os pacotes no seu script:
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggpubr)
library(readxl)
library(janitor)
library(cowplot)
library(patchwork)

# Importando os dados 

dados <- read_excel(path = "dados/Dados artigo Alho - Evaldo.xlsx", sheet = 2) |> 
  clean_names()

# Visualize a dispersão dos pontos:
ggplot(dados, aes(x = niveis , y = gli)) + geom_point()

# Glic -------------------------------------------
# Ajustando o modelo de regressão linear simples
modelo_lin <- lm(gli ~ niveis, data = dados)

# Extraia os coeficientes e o valor de R² e p-valor:
coeficientes <- coef(modelo_lin)
a <- coeficientes["(Intercept)"]
b <- coeficientes["niveis"]
r2 <- summary(modelo_lin)$r.squared
p_valor <- summary(modelo_lin)$fstatistic
p_valor <- pf(p_valor[1], p_valor[2], p_valor[3], lower.tail = FALSE)

# Calcular a média, o erro padrão e o desvio padrão para cada nível de niveis
dados_sumario <- dados %>%
  group_by(niveis) %>%
  summarise(
    Media = mean(gli),
    ErroPadrao = sd(gli) / sqrt(n()),
    DesvioPadrao = sd(gli),
    .groups = 'drop'  # Removendo o agrupamento após o summarise
  )

# Gráfico com média e barra de erro padrão
g1 <- ggplot(dados, aes(x = niveis, y = gli)) +
  geom_point(alpha=0.9, color = "black") +  # Pontos semi-transparentes para visualizar a sobreposição
  geom_point(data = dados_sumario, aes(y = Media), color = "blue", 
             size = 3, shape = 18) +  # Pontos da média
  geom_errorbar(data = dados_sumario, 
                aes(ymin = Media - ErroPadrao, 
                    ymax = Media + ErroPadrao, 
                    x = niveis,
                    y = Media), 
                linewidth = 0.8,
                width = 0.05, 
                color = "red") +
  geom_text(data = dados_sumario, aes(label = sprintf("%.2f ± %.2f", Media, DesvioPadrao), y = Media + ErroPadrao + 0.2), vjust = 0) +
  stat_smooth(data = dados, aes(x = niveis, y = gli), method = "lm", formula = y ~ x, se = TRUE, 
              color = "black", linetype = "solid", linewidth = 0.8) +
  labs(x = expression("Garlic essential oil (g.kg"^"-1" * ")"),
       y = expression("Serum glucose (mg.dL"^-1 * ")"))+
  annotate("text", x = min(dados$niveis)+0.4, y = min(dados$gli) +0.4 , hjust = 0.0, vjust = 0.1,
           label = sprintf("y = %.2f + %.2f*x\nR² = %.2f, p = %.4f", a, b, r2, p_valor),
           size = 4, color = "black") +
  # Adiciona letras identificadoras de gráficos:
  annotate("text", x = max(dados$niveis) +0 , y = max(dados$gli) -1, label = "A", 
           hjust = 1.1, vjust = -0.1, size = 5) +
  scale_x_continuous(limits = c(-0.05,1.55)) +
  theme_classic2() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        axis.text = element_text(size = 12))
g1

# Eritrócitos -----------------------------------------------------------------------
# Ajustando o modelo de regressão linear simples
modelo_lin <- lm(er ~ niveis, data = dados)

# Extraia os coeficientes e o valor de R² e p-valor:
coeficientes <- coef(modelo_lin)
a <- coeficientes["(Intercept)"]
b <- coeficientes["niveis"]
r2 <- summary(modelo_lin)$r.squared
p_valor <- summary(modelo_lin)$fstatistic
p_valor <- pf(p_valor[1], p_valor[2], p_valor[3], lower.tail = FALSE)

# Calcular a média, o erro padrão e o desvio padrão para cada nível de niveis
dados_sumario <- dados %>%
  group_by(niveis) %>%
  summarise(
    Media = mean(er),
    ErroPadrao = sd(er) / sqrt(n()),
    DesvioPadrao = sd(er),
    .groups = 'drop'  # Removendo o agrupamento após o summarise
  )

# Gráfico com média e barra de erro padrão
g2 <- ggplot(dados, aes(x = niveis, y = er)) +
  geom_point(alpha=0.9, color = "black") +  # Pontos semi-transparentes para visualizar a sobreposição
  geom_point(data = dados_sumario, aes(y = Media), color = "blue", 
             size = 3, shape = 18) +  # Pontos da média
  geom_errorbar(data = dados_sumario, 
                aes(ymin = Media - ErroPadrao, 
                    ymax = Media + ErroPadrao, 
                    x = niveis,
                    y = Media), 
                linewidth = 0.8,
                width = 0.05, 
                color = "red") +
  geom_text(data = dados_sumario, aes(label = sprintf("%.2f ± %.2f", Media, DesvioPadrao), y = Media + ErroPadrao + 0.02), vjust = 0) +
  stat_smooth(data = dados, aes(x = niveis, y = er), method = "lm", formula = y ~ x, se = TRUE, 
              color = "black", linetype = "solid", linewidth = 0.8) +
  labs(x = expression("Garlic essential oil (g.kg"^"-1" * ")"),
       y = expression("Erythrocyte (x 10"^"-6" * " " * mu * "L)")) +
  annotate("text", x = min(dados$niveis)+0.6, y = min(dados$er) -0.05 , hjust = 0.0, vjust = 0.1,
           label = sprintf("y = %.2f + %.2f*x\nR² = %.2f, p = %.4f", a, b, r2, p_valor),
           size = 4, color = "black") +
  annotate("text", x = max(dados$niveis) + 0.05, y = max(dados$er), label = "B", 
           hjust = 1.1, vjust = -0.1, size = 5)+
  scale_x_continuous(limits = c(-0.05,1.55)) +
  theme_classic2() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        axis.text = element_text(size = 12))
g2

# MCV ---------------------------------------
# Ajustando o modelo de regressão linear simples
modelo_lin <- lm(vcm ~ niveis, data = dados)

# Extraia os coeficientes e o valor de R² e p-valor:
coeficientes <- coef(modelo_lin)
a <- coeficientes["(Intercept)"]
b <- coeficientes["niveis"]
r2 <- summary(modelo_lin)$r.squared
p_valor <- summary(modelo_lin)$fstatistic
p_valor <- pf(p_valor[1], p_valor[2], p_valor[3], lower.tail = FALSE)

# Calcular a média, o erro padrão e o desvio padrão para cada nível de niveis
dados_sumario <- dados %>%
  group_by(niveis) %>%
  summarise(
    Media = mean(vcm),
    ErroPadrao = sd(vcm) / sqrt(n()),
    DesvioPadrao = sd(vcm),
    .groups = 'drop'  # Removendo o agrupamento após o summarise
  )

# Gráfico com média e barra de erro padrão

g3 <- ggplot(dados, aes(x = niveis, y = vcm)) +
  geom_point(alpha=0.9, color = "black") +  # Pontos semi-transparentes para visualizar a sobreposição
  geom_point(data = dados_sumario, aes(y = Media), color = "blue", 
             size = 3, shape = 18) +  # Pontos da média
  geom_errorbar(data = dados_sumario, 
                aes(ymin = Media - ErroPadrao, 
                    ymax = Media + ErroPadrao, 
                    x = niveis,
                    y = Media), 
                linewidth = 0.8,
                width = 0.05, 
                color = "red") +
  geom_text(data = dados_sumario, aes(label = sprintf("%.2f ± %.2f", Media, DesvioPadrao), y = Media + ErroPadrao + 6), vjust = 0) +
  stat_smooth(data = dados, aes(x = niveis, y = vcm), method = "lm", formula = y ~ x, se = TRUE, 
              color = "black", linetype = "solid", linewidth = 0.8) +
  labs(x = expression("Garlic essential oil (g.kg"^"-1" * ")"),
       y = expression("Mean Corpuscular volume (fL)"))+
  annotate("text", x = min(dados$niveis)+0.6, y = min(dados$vcm) + 20 , hjust = 0.0, vjust = 0.1,
           label = sprintf("y = %.2f + %.2f*x\nR² = %.2f, p = %.4f", a, b, r2, p_valor),
           size = 4, color = "black") +
  scale_x_continuous(limits = c(-0.06,1.55)) +
  scale_y_continuous(breaks = seq(340, 500, by = 30)) + 
  annotate("text", x = max(dados$niveis) + 0.05, y = min(dados$vcm)+ 5, label = "C", 
           hjust = 1.1, vjust = -0.1, size = 5)+
  theme_classic2() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        axis.text = element_text(size = 12))
g3

# MCH -------------------------------------------------------
# Ajustando o modelo de regressão linear simples
modelo_lin <- lm(hcm ~ niveis, data = dados)

# Extraia os coeficientes e o valor de R² e p-valor:
coeficientes <- coef(modelo_lin)
a <- coeficientes["(Intercept)"]
b <- coeficientes["niveis"]
r2 <- summary(modelo_lin)$r.squared
p_valor <- summary(modelo_lin)$fstatistic
p_valor <- pf(p_valor[1], p_valor[2], p_valor[3], lower.tail = FALSE)

# Calcular a média, o erro padrão e o desvio padrão para cada nível de niveis
dados_sumario <- dados %>%
  group_by(niveis) %>%
  summarise(
    Media = mean(hcm),
    ErroPadrao = sd(hcm) / sqrt(n()),
    DesvioPadrao = sd(hcm),
    .groups = 'drop'  # Removendo o agrupamento após o summarise
  )

# Gráfico com média e barra de erro padrão
g4 <- ggplot(dados, aes(x = niveis, y = hcm)) +
  geom_point(alpha=0.9, color = "black") +  # Pontos semi-transparentes para visualizar a sobreposição
  geom_point(data = dados_sumario, aes(y = Media), color = "blue", 
             size = 3, shape = 18) +  # Pontos da média
  geom_errorbar(data = dados_sumario, 
                aes(ymin = Media - ErroPadrao, 
                    ymax = Media + ErroPadrao, 
                    x = niveis,
                    y = Media), 
                linewidth = 0.8,
                width = 0.05, 
                color = "red") +
  geom_text(data = dados_sumario, aes(label = sprintf("%.2f ± %.2f", Media, DesvioPadrao), y = Media + ErroPadrao + 2), vjust = 0) +
  stat_smooth(data = dados, aes(x = niveis, y = hcm), method = "lm", formula = y ~ x, se = TRUE, 
              color = "black", linetype = "solid", linewidth = 0.8) +
  labs(x = expression("Garlic essential oil (g.kg"^"-1" * ")"),
       y = expression("Mean corpuscular hemoglobin (pg)"))+
  annotate("text", x = min(dados$niveis)+0.6, y = min(dados$hcm) + 2 , hjust = 0.0, vjust = 0.1,
           label = sprintf("y = %.2f + %.2f*x\nR² = %.2f, p = %.4f", a, b, r2, p_valor),
           size = 4, color = "black") +
  scale_x_continuous(limits = c(-0.05,1.55)) +
  scale_y_continuous(breaks = seq(50, 95, by =5)) +
  annotate("text", x = max(dados$niveis) + 0.05, y = min(dados$hcm), label = "D", 
           hjust = 1.1, vjust = -0.1, size = 5)+
  theme_classic2() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        axis.text = element_text(size = 12))
g4

# Combinando plots
combined_plot <- plot_grid(g1, g2, g3, g4, ncol = 2)

(g1+g2)/(g3+g4) + plot_layout()

# Exibir o gráfico combinado
print(combined_plot)

ggsave("Fig3.png", plot = combined_plot, 
       width = 36, height = 16, dpi = 300, units = "cm")

