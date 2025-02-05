# Trabalho Modelos Lineares Generalizados
# Fatores determinantes na frequência de acesso a cuidados hospitalares
# Gonçalo Araújo 15-01-2025

# packages necessários

library(AER)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(faraway)
library(MASS)

# importar o dataset do package AER

data("NMES1988")
View(NMES1988)

head(NMES1988)
str(NMES1988)

datase <- NMES1988[, !(names(NMES1988) %in% c("nvisits", "ovisits","novisits",
                                                   "emergency","hospital","adl",
                                                   "region","afam","married","school",
                                                   "employed","medicaid"))]
View(datase)
names(datase)

total_na_count <- sum(is.na(datase))
print(total_na_count)

datase$age <- datase$age*10

dim(datase)



str(datase)

datatype <- sapply(datase, class)
datatype_count <- table(datatype)
print(datatype_count)


summary_df <- data.frame(
  Data_Type = sapply(datase, class),
  Unique_Count = sapply(datase, function(x) length(unique(x))), 
  row.names = names(datase)
)
print(summary_df)

# EDA
# Análises Univariadas: variáveis contínuas

## Idas ao hospital (variável resposta)

histogram <- ggplot(datase, aes(x = visits)) + 
  geom_histogram(binwidth = 4, fill = "steelblue3", color = "black") +
  labs(title = NULL,
       x = "Número de idas ao hospital", y = "Frequência") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot <- ggplot(datase, aes(x = NULL, y = visits)) +
  geom_boxplot(fill = "steelblue3") +
  labs(title = NULL, x = "", 
       y = "Número de idas ao hospital" ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram, boxplot,
  ncol = 2,
  top = textGrob(
    "Número de idas ao hospital dos pacientes",
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

median_visits <- median(datase$visits, na.rm = T)
range_visits <- range(datase$visits, na.rm = T)

visits_stats <- data.frame(Statistic = c("Median","Min","Max"),
                          Value = round(c(median_visits,range_visits[1],
                                          range_visits[2]),3))

print(visits_stats)

## Idade

histogram2 <- ggplot(datase, aes(x = age)) +
  geom_histogram(binwidth = 3, fill = "pink3", color = "black") +
  labs(title = NULL, x = "Idade", y = "Frequencia") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot2 <- ggplot(datase, aes(x="", y = age)) +
  geom_boxplot(fill = "pink3") +
  labs(title = NULL, x = "" , y = "Idade") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram2, boxplot2, 
  ncol = 2,
  top = textGrob("Idade dos pacientes",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)


median_age <- median(datase$age)
range_age <- range(datase$age)

age_stats <- data.frame(Statistic = c("Median", "Min", "Max"),
                        Value = round(c(median_age, range_age[1], range_age[2]), 4))
print(age_stats)

## Rendimento

histogram3 <- ggplot(datase, aes(x = income)) +
  geom_histogram(binwidth = 3, fill = "khaki3", color = "black") +
  labs(title = NULL, x = "Rendimento", y = "Frequencia") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot3 <- ggplot(datase, aes(x="", y = income)) +
  geom_boxplot(fill = "khaki3") +
  labs(title = NULL, x = "" , y = "Rendimento") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram3, boxplot3, 
  ncol = 2,
  top = textGrob("Rendimento dos pacientes",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)


median_income <- median(datase$income, na.rm = T)
range_income <- range(datase$income)

income_stats <- data.frame(Statistic = c("Median","Min","Max"),
                              Value = round(c(median_income,range_income[1],
                                              range_income[2]),4))
print(income_stats)

## Número de condições crónicas

histogram4 <- ggplot(datase, aes(x = chronic)) +
  geom_histogram(binwidth = 2, fill = "firebrick3", color = "black") +
  labs(title = NULL, x = "Número de doenças crónicas", y = "Frequencia") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

boxplot4 <- ggplot(datase, aes(x="", y = income)) +
  geom_boxplot(fill = "firebrick3") +
  labs(title = NULL, x = "" , y = "Número de doenças crónicas") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(
  histogram4, boxplot4, 
  ncol = 2,
  top = textGrob("Número de doenças crónicas dos pacientes",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)


median_chronic <- median(datase$chronic, na.rm = T)
range_chronic <- range(datase$chronic)

chronic_stats <- data.frame(Statistic = c("Median","Min","Max"),
                           Value = round(c(median_chronic,range_chronic[1],
                                           range_chronic[2]),4))
print(chronic_stats)

# Análises Univariadas: variáveis categóricas

## Sexo

freq_dist <- table(datase$gender)
relative_freq <- prop.table(freq_dist)

freq_table <- data.frame(
  Gender = names(freq_dist),
  Frequency = as.integer(freq_dist),
  Relative_Frequency = round(as.numeric(relative_freq), 4))

print(freq_table)

sexo1 <- ggplot(freq_table, aes(x = Gender, y = Relative_Frequency, fill = Gender)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("male" = "darkgreen", "female" = "darkgray")) +
  labs(title = NULL, x = "Sexo", y = "Frequência Relativa") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))


sexo2 <- ggplot(freq_table, aes(x = "", y = Relative_Frequency, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("male" = "darkgreen", "female" = "darkgray")) +
  labs(title = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  geom_text(aes(label = paste0(round(Relative_Frequency * 100, 2), "%")), 
            position = position_stack(vjust = 0.5), size = 4)

grid.arrange(
 sexo1, sexo2, 
  ncol = 2,
  top = textGrob("Sexo dos pacientes",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)

## Estado de saúde

freq_dist2 <- table(datase$health)
relative_freq2 <- prop.table(freq_dist2)

freq_table2 <- data.frame(
  Health = names(freq_dist2),
  Frequency = as.integer(freq_dist2),
  Relative_Frequency = round(as.numeric(relative_freq2), 4))

print(freq_table2)

saude1 <- ggplot(freq_table2, aes(x = Health, y = Relative_Frequency, fill = Health)) +
  geom_bar(stat = "identity") +
  labs(title = NULL, x = "Estado de saúde", y = "Frequência Relativa") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

saude2 <- ggplot(freq_table2, aes(x = "", y = Relative_Frequency, fill = Health)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = NULL) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  geom_text(aes(label = paste0(round(Relative_Frequency * 100, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3)

grid.arrange(
  saude1, saude2, 
  ncol = 2,
  top = grid.text(
    "Estado de saúde dos pacientes", 
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

## Seguro de saúde

freq_dist3 <- table(datase$insurance)
relative_freq3 <- prop.table(freq_dist3)

freq_table3 <- data.frame(
  Insurance = names(freq_dist3),
  Frequency = as.integer(freq_dist3),
  Relative_Frequency = round(as.numeric(relative_freq3), 4))

print(freq_table3)

seguro1 <- ggplot(freq_table3, aes(x = Insurance, y = Relative_Frequency, 
                                  fill = Insurance)) +
  geom_bar(stat = "identity") +
  labs(title = NULL, x = "Seguro de saúde", y = "Frequência Relativa") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

seguro2 <- ggplot(freq_table3, aes(x = "", y = Relative_Frequency, fill = Insurance)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = NULL) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  ) +
  geom_text(aes(label = paste0(round(Relative_Frequency * 100, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3)

grid.arrange(
  seguro1, seguro2, 
  ncol = 2,
  top = grid.text(
    "Seguro de saúde", 
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

# Análises Bivariadas

# Variável resposta contra variaveis continuas

## visitas hospitalares x idade

biv_plot1 <- ggplot(datase, aes(x = age, y = visits)) +
  geom_point(color = "pink3", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Número de idas ao hospital vs Idade dos pacientes",
       x = "Idade",
       y = "Número de idas") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
print(biv_plot1)

biv_plot1b <- ggplot(datase, aes(x = cut(age, breaks = seq(65, 110, 5)), y = visits)) +
  geom_boxplot(fill = "pink3", alpha = 0.7) +
  labs(title = "Número de idas ao hospital vs idade",
       x = "Idade",
       y = "Número de idas") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
print(biv_plot1b)
  
## visitas hospitalares x rendimento

biv_plot2 <- ggplot(datase, aes(x = income, y = visits)) +
  geom_point(color = "khaki3", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Número de idas ao hospital vs Rendimento dos pacientes",
       x = "Rendimento",
       y = "Número de idas") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
print(biv_plot2)

## visitas hospitalres x condições crónicas

biv_plot3 <- ggplot(datase, aes(x = chronic, y = visits)) +
  geom_point(color = "firebrick3", alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Número de idas ao hospital vs Número de condições crónicas",
       x = "Número de condições crónicas",
       y = "Número de idas") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
print(biv_plot3)

biv_plot3b <- ggplot(datase, aes(x = factor(chronic), y = visits)) +
  geom_boxplot(fill = "firebrick3", alpha = 0.7) +
  labs(title = "Número de idas ao hospital vs Número de condições crônicas",
       x = "Número de condições crônicas",
       y = "Número de idas") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
print(biv_plot3b)


# Variável resposta contra variaveis categoricas

## visitas hospitalares x estado de saúde

stats_health <- datase %>%
  group_by(health) %>%
  summarise(
    median_visits = median(visits, na.rm = TRUE),
    min_visits = min(visits, na.rm = TRUE),
    max_visits = max(visits, na.rm = TRUE)
  )

boxplot_saude <- ggplot(datase, aes(x = health, y = visits, fill = health)) +
  geom_boxplot() +
  scale_fill_manual(values = c("excellent" = "#4CAF50", 
                               "poor" = "#2196F3",        
                               "average" = "#FF69B4")) +  
  geom_text(data = stats_health, 
            aes(x = health, 
                y = max(datase$visits) - 50, 
                label = paste0("Med: ", median_visits, 
                               "\nMin: ", min_visits, 
                               "\nMax: ", max_visits)), 
            hjust = 0.6, 
            size = 3, 
            color = "black", 
            nudge_x = 0.3) +
  labs(
    title = "Distribuição de idas ao hospital por estado de saúde",
    x = "Estado de saúde",
    y = "Número de idas ao hospital",
    fill = "Estado de saúde"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )

print(boxplot_saude)

## visitas hospitalares x seguro de saúde

stats_insurance <- datase %>%
  group_by(insurance) %>%
  summarise(
    median_visits2 = median(visits, na.rm = TRUE),
    min_visits2 = min(visits, na.rm = TRUE),
    max_visits2 = max(visits, na.rm = TRUE)
  )

boxplot_insurance <- ggplot(datase, aes(x = insurance, y = visits, fill = insurance)) +
  geom_boxplot() +
  geom_text(data = stats_insurance, aes(x = insurance, y = max(datase$visits) - 50, 
                                     label = paste0("Med: ", median_visits2, "\nMin: ", min_visits2, "\nMax: ", max_visits2)), 
            hjust = 0.6, size = 3, color = "black", nudge_x = 0.3) +
  labs(
    title = "Distribuição de idas ao hospital por posse de seguro de saúde",
    x = "Seguro de saúde",
    y = "Número de idas ao hospital",
    fill = "Seguro de saúde"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )

print(boxplot_insurance)

## visitas hospitalares x género

stats_gender <- datase %>%
  group_by(gender) %>%
  summarise(
    max_visits3 = max(visits, na.rm = TRUE),
    median_visits3 = median(visits, na.rm = TRUE),
    min_visits3 = min(visits, na.rm = TRUE)
  )

boxplot_gender <- ggplot(datase, aes(x = gender, y = visits, fill = gender)) +
  geom_boxplot() +
  geom_text(data = stats_gender, aes(
    x = gender, 
    y = median_visits3 + 20,  
    label = paste0("Med: ", median_visits3, "\nMin: ", min_visits3, "\nMax: ", max_visits3)
  ), hjust = 0.6, size = 3, color = "black", nudge_x = 0.3) +
  scale_fill_manual(values = c("male" = "darkgreen", "female" = "darkgray")) +
  labs(
    title = "Distribuição de idas ao hospital por sexo dos pacientes",
    x = "Sexo",
    y = "Número de idas ao hospital",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )

print(boxplot_gender)

# Análises Multivariadas

plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )


breaks_chronic <- c(0, 2, 4, 6, 8)
labels_chronic <- c("0-2", "3-4", "5-6", "7-8")

breaks_age <- c(65, 71, 76, 81, 86, 91, 96, 101, 106, 111)
labels_age <- c("65 to 70", "71 to 75", "76 to 80", "81 to 85", 
                "86 to 90", "91 to 95", "96 to 100", "101 to 105", "106 to 110")

breaks_income <- c(-5, 6, 16, 26, 36, 46, 56, 61)
labels_income <- c("-5 to 5", "6 to 15", "16 to 25", "26 to 35", 
                   "36 to 45", "46 to 55", "56 to 60")


create_plot <- function(data, x_var, fill_var, title, breaks_x, labels_x) {
  data %>%
    mutate(
      x_category = cut(.data[[x_var]], breaks = breaks_x, labels = labels_x, right = TRUE, include.lowest = TRUE)
    ) %>%
    ggplot(aes(x = x_category, y = visits, fill = .data[[fill_var]])) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = title,
      x = paste(tools::toTitleCase(gsub("_", " ", x_var))),
      y = "Idas ao hospital",
      fill = tools::toTitleCase(gsub("_", " ", fill_var))
    ) +
    plot_theme
}

# 1. Rendimento por Estado de saúde

mult1 <- create_plot(datase, "income", "health", "Idas ao hospital por rendimento e estado de saúde", 
                     breaks_income, labels_income) +
  scale_fill_manual(values = c("excellent" = "#4CAF50", 
                               "poor" = "#2196F3",        
                               "average" = "#FF69B4"))   
print(mult1)

# 2. Rendimento por seguro de saúde

mult2 <- create_plot(datase, "income", "insurance", "Idas ao hospital por rendimento e seguro de saúde", 
                     breaks_income, labels_income) 

print(mult2)

# 3. Rendimento por género

mult3 <- create_plot(datase, "income", "gender", "Idas ao hospital por rendimento e género", 
                     breaks_income, labels_income) +
  scale_fill_manual(values = c("male" = "darkgreen", "female" = "darkgray"))

print(mult3)

# 4. Doenças crónicas por estado de saúde

mult4 <- create_plot(datase, "chronic", "health", "Idas ao hospital por condições crónicas e saúde", 
                     breaks_chronic, labels_chronic) +
  scale_fill_manual(values = c("excellent" = "#4CAF50", 
                               "poor" = "#2196F3",        
                               "average" = "#FF69B4"))  
  
print(mult4)

# 5. Doenças crónicas por seguro de saúde

mult5 <- create_plot(datase, "chronic", "insurance", "Idas ao hospital por nº condições crónicas e seguro", breaks_chronic, labels_chronic)

print(mult5)

# 6. Doenças crónicas por género

mult6 <- create_plot(datase, "chronic", "gender", "Idas ao hospital por nº condições crónicas e género", 
                     breaks_chronic, labels_chronic) +
  scale_fill_manual(values = c("male" = "darkgreen", "female" = "darkgray"))

print(mult6)

# 7. Idade por estado de saúde

mult7 <- create_plot(datase, "age", "health", "Idas ao hospital por idade e estado de saúde", 
                     breaks_age, labels_age) +
  scale_fill_manual(values = c("excellent" = "#4CAF50", 
                               "poor" = "#2196F3",        
                               "average" = "#FF69B4")) 

print(mult7)

# 8. Idade por seguro de saúde

mult8 <- create_plot(datase, "age", "insurance", "Idas ao hospital por idade e seguro de saúde", 
                     breaks_age, labels_age)

print(mult8)

# 9. Idade por género

mult9 <- create_plot(datase, "age", "gender", "Idas ao hospital por idade e género", 
                     breaks_age, labels_age) +
  scale_fill_manual(values = c("male" = "darkgreen", "female" = "darkgray"))

print(mult9)


# Matriz de correlação heatmaps variaveis continuas

correlation_data <- datase[, c("visits","age","chronic", "income")]


correlation_matrix <- round(cor(correlation_data, use = "complete.obs", method = "spearman"), 2)


correlation_matrix_df <- as.data.frame(as.table(correlation_matrix))


correlation_matrix_df$fill_value <- correlation_matrix_df$Freq
diag_indices <- which(correlation_matrix_df$Var1 == correlation_matrix_df$Var2)
correlation_matrix_df$fill_value[diag_indices] <- NA
correlation_matrix_df$label <- as.character(correlation_matrix_df$Freq)
correlation_matrix_df$label[diag_indices] <- "--"

print(correlation_matrix)


heatmap_plot <- ggplot(correlation_matrix_df, aes(x = Var1, y = Var2, fill = fill_value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), color = "black", size = 4) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0, 
    limit = c(-1, 1), space = "Lab", name = "Pearson",
    na.value = "grey90"  
  ) +
  labs(title = "Correlation Matrix vs continuous variables", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)
  )

print(heatmap_plot)


# Matriz de correlação heatmaps variaveis categoricas

categorical_vars <- datase[, c("gender", "health", "insurance")]

categorical_encoded <- categorical_vars %>% 
  mutate(across(everything(), as.factor)) %>% 
  mutate(across(everything(), as.numeric))


all_vars <- cbind(categorical_encoded, visits = datase$visits)


correlation_matrix2 <- cor(all_vars, method = "spearman", use = "complete.obs")

correlation_matrix_df <- as.data.frame(as.table(correlation_matrix2))

diag_indices <- which(correlation_matrix_df$Var1 == correlation_matrix_df$Var2)
correlation_matrix_df$Freq[diag_indices] <- NA  
correlation_matrix_df$label <- as.character(correlation_matrix_df$Freq)
correlation_matrix_df$label[diag_indices] <- "--"
correlation_matrix_melted <- as.data.frame(as.table(correlation_matrix2))

heatmap_plot2 <- ggplot(correlation_matrix_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 3.5) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", midpoint = 0, 
    limit = c(-1, 1), space = "Lab", name = "Spearman"
  ) +
  labs(
    title = "Correlation Matrix vs categorical variables",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  coord_fixed()

print(heatmap_plot2)


# Análise modelos de Poisson

# modelo com todas as variáveis
mod_full <- glm(visits~.,data = datase, family = "poisson")
summary(mod_full)
drop1(mod_full,test="Chisq")

# dado que income é a variável com o p-value mais alto, vou fazer um modelo sem esta
# variavel e comparar os 2 modelos
mod_noincome <- update(mod_full,.~.-income)
summary(mod_noincome)

anova(mod_noincome,mod_full,test="Chisq") # não é significativo, não se rejeita H0 e escolhe-se o modelo mais pequeno (sem income)

deviance(mod_full)
deviance(mod_noincome) 

AIC(mod_full, mod_noincome) # o AIC é ligeiramente maior no modelo sem a variavel income

1 - (mod_full$deviance / mod_full$null.deviance)
1 - (mod_noincome$deviance / mod_noincome$null.deviance) #pseudo r^2 ligeramente menor no modelo sem a variavel income

drop1(mod_noincome,test="Chisq")

#  No entanto, apesar de AIC (maior), R^2 menor, a partir do likelihood test, não se rejeita H0
# e vamos escolher o modelo menor (sem income)

# Vou considerar outros modelos, dado que todas as variaveis são extremamente significativas,
# vou analisar possíveis interações

mod_inter1 <- update(mod_noincome,.~.+insurance*age)
summary(mod_inter1) # interação não é significativa na resposta

anova(mod_noincome, mod_inter1,test="Chisq") # não se rejeita H0 e mantém-se o modelo sem a interação

AIC(mod_inter1, mod_noincome) # AIC menor no modelo sem interação

deviance(mod_inter1)
deviance(mod_noincome)

1 - (mod_inter1$deviance / mod_inter1$null.deviance)
1 - (mod_noincome$deviance / mod_noincome$null.deviance) # r^2 semelhantes

# Portanto, rejeita-se o modelo com a interação e mantém-se o modelo apenas sem income

# Vou considerar outras interações

mod_inter2 <- update(mod_noincome,.~.+health*age)
summary(mod_inter2)

anova(mod_noincome,mod_inter2, test="Chisq") # rejeita-se H0 e escolhe-se o modelo com a interação

AIC(mod_inter2, mod_noincome) # AIC menor no modelo sem interação

deviance(mod_inter2)
deviance(mod_noincome)

1 - (mod_inter2$deviance / mod_inter2$null.deviance)
1 - (mod_noincome$deviance / mod_noincome$null.deviance) # r^2 ligeramente maior no modelo com interação

# Eu tinha escolhido este modelo, contudo após ter analisado a multicolinearidade deste modelo,  verifiquei
# a existência de valores muito altos

car::vif(mod_inter2) # health:age 19386.979279

# Por isso considerei outros modelos com outras interações

mod_inter3 <- update(mod_noincome, .~. +gender*chronic)
summary(mod_inter3)

anova(mod_noincome,mod_inter3, test="Chisq") # não se rejeita H0, mantém se o modelo mais pequeno (sem interação)

AIC(mod_noincome, mod_inter3) # AIC menor no modelo sem interação

deviance(mod_noincome)
deviance(mod_inter3)

1 - (mod_noincome$deviance / mod_noincome$null.deviance)
1 - (mod_inter3$deviance / mod_inter3$null.deviance) # r^2 semelhantes

# entre estes 2 modelos, o melhor é o modelo sem a interação (mod_noincome)

mod_inter4 <- update(mod_noincome, .~. +gender*age)
summary(mod_inter4)

anova(mod_noincome,mod_inter4, test="Chisq") # não se rejeita H0, mantém se o modelo mais pequeno (sem interação)

AIC(mod_noincome, mod_inter4) # AIC menor no modelo sem interação

deviance(mod_noincome)
deviance(mod_inter4)

1 - (mod_noincome$deviance / mod_noincome$null.deviance)
1 - (mod_inter4$deviance / mod_inter4$null.deviance) #r^2 semelhantes

# Por isso rejeita-se o modelo com a interação gender*age

mod_inter5 <- update(mod_noincome, .~. +insurance*chronic)
summary(mod_inter5)

anova(mod_inter5,mod_noincome, test="Chisq") # rejeita-se H0, e opta-se pelo modelo maior (com interação)

AIC(mod_noincome, mod_inter5) # AIC menor no modelo com interação

deviance(mod_noincome)
deviance(mod_inter5)

1 - (mod_noincome$deviance / mod_noincome$null.deviance)
1 - (mod_inter5$deviance / mod_inter5$null.deviance) # r^2 maior no modelo com interação

# Por isso, conclui-se que este ultimo modelo, mod_inter5 é o melhor dos demais.

# Qualidade do ajustamento do mod_inter5

#residuals

plot(fitted(mod_inter5), residuals(mod_inter5),
     xlab="Fitted Values", ylab="Deviance Residuals", 
     main="Deviance Residuals vs Fitted Values")
abline(h=0, col="red",lwd=3)

hist(residuals(mod_inter5, type = "deviance"), main="Histogram of Deviance Residuals", 
     xlab="Deviance Residuals", breaks=30, col = "lightblue")

#qq-plot

qqnorm(residuals(mod_inter5, type = "deviance"))
qqline(residuals(mod_inter5, type = "deviance"), col="red", lwd=3)

#serial plot for residuals

plot(residuals(mod_inter5,type="deviance"),xlab = "Indice",
     ylab = "Residuals",type="o")


#outliers

library(faraway)
halfnorm(residuals(mod_inter5, type = "deviance"), main = "Half-normal plot of residuals")

cooks.distance <- cooks.distance(mod_inter5)
plot(cooks.distance, type = "h", main = "Cook's Distance")
abline(h = 4/(nrow(data)-length(mod_inter5$coefficients)), col = "red")

# Há alguns outliers, vou ver se o modelo tem uma melhor performance sem estes outliers

cutoff <- 4/(nrow(data)-length(mod_inter5$coefficients))

cooks_d <- cooks.distance(mod_inter5)
outliers <- which(cooks_d > cutoff)

datase_nooutliers <- datase[-outliers, ]

mod_outliers <- glm(visits ~ health + chronic + age + gender + insurance + chronic:insurance, 
                 family = poisson, 
                 data = datase_nooutliers)

summary(mod_outliers)

AIC(mod_inter5)
AIC(mod_outliers)

1 - (mod_inter5$deviance / mod_inter5$null.deviance)
1 - (mod_outliers$deviance / mod_outliers$null.deviance)


#multicolinearity

car::vif(mod_inter5)

#overdispersion

phihat  = sum(residuals(mod_inter5,type="pearson")^2)/mod_inter5$df.residual
print(phihat)

# o valor de phihat é superior a 1 (7.068), por isso vou considerar a transformação do modelo
# para negative binomial

summary(mod_inter5, dispersion=phihat)

library(MASS)

mod_nb <- glm.nb(visits ~ health + chronic + age + gender + insurance 
                 + insurance:chronic, data = datase)
summary(mod_nb) # age deixa de ser significativa

AIC(mod_inter5, mod_nb)

mod_nb2 <- update(mod_nb,.~.-age) # criar novo modelo sem age
summary(mod_nb2)

AIC(mod_nb2, mod_nb) # modelo sem age é melhor (AIC mais baixo)

#residuals

plot(fitted(mod_nb2), residuals(mod_nb2),
     xlab="Fitted Values", ylab="Deviance Residuals", 
     main="Deviance Residuals vs Fitted Values")
abline(h=0, col="red",lwd=3)

hist(residuals(mod_nb2, type = "deviance"), main="Histogram of Deviance Residuals", 
     xlab="Deviance Residuals", breaks=30, col = "lightgreen")

#qq-plot

qqnorm(residuals(mod_nb2, type = "deviance"))
qqline(residuals(mod_nb2, type = "deviance"), col="red", lwd=3)

#serial plot for residuals

plot(residuals(mod_nb2,type="deviance"),xlab = "Indice",
     ylab = "Residuals",type="o")


#outliers

halfnorm(residuals(mod_nb2, type = "deviance"), main = "Half-normal plot of residuals")

cooks.distance2 <- cooks.distance(mod_nb2)
plot(cooks.distance2, type = "h", main = "Cook's Distance")
abline(h = 4/(nrow(data)-length(mod_nb2$coefficients)), col = "red")

# ver se o modelo é melhor sem outliers

cutoff2 <- 4/(nrow(data)-length(mod_nb2$coefficients))

cooks_d2 <- cooks.distance(mod_nb2)
outliers2 <- which(cooks_d2 > cutoff2)

datase_nooutliers2 <- datase[-outliers, ]


modnb_outliers <- glm.nb(visits ~ health + chronic + gender + insurance + chronic:insurance,
                    data = datase_nooutliers2)

summary(modnb_outliers)

AIC(mod_nb2)
AIC(modnb_outliers) #modelo sem outliers é melhor


1 - (mod_nb2$deviance / mod_nb2$null.deviance)
1 - (modnb_outliers$deviance / modnb_outliers$null.deviance) #melhor

# mas vou manter o modelo com outliers, dado que cada observação são pacientes, ou seja
# casos reais, isto indica que pode acontecer e não deve ser descartado.
# torna o modelo melhor para dados reais.


#multicolinearity

car::vif(mod_nb2)


# cross-validation
for (i in 1:nrow(datase)) {
  
  mod_cv <- glm.nb(formula = visits ~ health + chronic + gender + insurance + 
                     chronic:insurance + health:chronic, 
                   data = datase, subset = -i, init.theta = 1.165076166, link = log)
  

  pred.cv[i] <- predict(mod_cv, newdata = datase[i, ], type = "response")
}


actual <- datase$visits 
rmse <- sqrt(mean((actual - pred.cv)^2))

print(rmse) # aprox 6,47



#(d) qual é a previsão do modelo para a situação em que as variáveis contínuas tomam o valor das
# suas medianas e as variáveis categóricas estão todas na segunda categoria?

summary(mod_nb2)

#median_chronic
#health_poor (2ºcategoria)
#gender_male (2ºcategoria)
#insurance_yes (2ºcategoria)
#chronic:insuranceyes

modelo <- 1.09245 - 0.32870 + (0.23473 * median_chronic) -0.10812 + 0.42500 -(0.05471* median_chronic)

# modelo = 1.26065

predicted_visits <- exp(1.26065) # aprox 4 visitas.

#Considerando o valor da mediana da variável chronic e assumindo que o indivíduo 
#é do sexo masculino e tem seguro, contabilizando também a interação entre chronic*insurance, 
#o número esperado de visitas hospitalares é de aprox 4.


# (e) no modelo final, designe por X1 a primeira variável contínua que consta no preditor linear e por
# X2 a primeira variável categórica com mais de 2 categorias.

#X1: datase$chronic
#X2: datase$health

# (e.1) Interprete os efeitos brutos e ajustados de X1 e X2.

summary(mod_nb2)

# efeitos ajustados

# chronic: 0.23473
# healthpoor: 0.32870
# healthexcellent: -0.35096

# efeitos brutos

mod_chronicbruto <- glm.nb(visits ~ chronic, data = datase)
summary(mod_chronicbruto) # 0.22309

mod_healthbruto <- glm.nb(visits ~ health, data = datase)
summary(mod_healthbruto)# healthpoor: 0.47904; healthexcellent: -0.47455 

# (e.2) Interprete o efeito provocado na resposta por uma mudança da segunda categoria de X2 para
# a terceira, e indique um intervalo de confiança a 95% para esse efeito.

efeito <- coef(mod_nb2)["healthexcellent"] - coef(mod_nb2)["healthpoor"]
print(efeito) #-0.679662

vcov_matrix <- vcov(mod_nb2)

var_diff <- vcov_matrix["healthexcellent","healthexcellent"] + 
  vcov_matrix["healthpoor","healthpoor"] - 
  2 * vcov_matrix["healthexcellent","healthpoor"]
se_diff <- sqrt(var_diff)

z_value <- qnorm(0.975)  
lower_ci <- efeito - z_value * se_diff
upper_ci <- efeito + z_value * se_diff

exp(c(lower_ci, upper_ci)) # 0.4362317 0.5887567 

# (e.3) Interprete o efeito provocado por um aumento em X1 correspondente a 15% da amplitude
# dos seus valores.

amplitude <- max(datase$chronic) - min(datase$chronic)
delta_x <- 0.15 * amplitude # 1.2

#(e.4) Averigue a significância estatística da interação entre X1 e X2 no modelo final. Interprete os
# coeficientes obtidos, mesmo que não sejam estatisticamente significativos.

mod_nb2inter <- update(mod_nb2,.~.+chronic*health)
summary(mod_nb2inter)

anova(mod_nb2,mod_nb2inter,test="Chisq") 
drop1(mod_nb2inter,test="Chisq") #2.985e-08 ***

# interaction is significant

# fim



