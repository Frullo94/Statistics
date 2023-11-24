install.packages("tidyverse")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("flextable")
install.packages("webshot")
install.packages("leaps")
install.packages("effects")
install.packages("caret")

library(tidyverse)
library(ggplot2)
library(reshape2)
library(flextable)
library(webshot)
library(broom)
library(leaps)
library(dplyr)
library(MASS)
library(effects)
library(caret)

neonati <- read_csv("neonati.csv")
colnames(neonati)
head(neonati)

# realizzo il summary, lo converto in dataframe e creo un file csv
# per avere una tabella chiara da importare nel pdf dove descrivo lo svolgimento dell esame
sum_neo <- summary(neonati)
sum_dataframe <- as.data.frame(as.table(sum_neo))
write.csv(sum_dataframe, "summary_neonati.csv")

#valuto la frequenza delle variabili categoriche, non ho bisogno del file, le copio dalla console direttamente
table(neonati$Tipo.parto)
table(neonati$Ospedale)
table(neonati$Sesso)

# distribuzione età della madre
ggplot(neonati, aes(x = Anni.madre)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuzione delle Età delle Madri",
       x = "Età della Madre",
       y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# distribuzione del peso
ggplot(neonati, aes(x = Peso)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuzione del peso del neonato",
       x = "Peso",
       y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# distribuzione gravidanze
ggplot(neonati, aes(x = N.gravidanze)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuzione delle gravidanze",
       x = "Gravidanze",
       y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# distribuzione gestazione
ggplot(neonati, aes(x = Gestazione)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuzione della gestazione",
       x = "Grestazione",
       y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# distribuzione Lunghezza
ggplot(neonati, aes(x = Lunghezza)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuzione della lunghezza",
       x = "Lunghezza",
       y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# distribuzione Cranio
ggplot(neonati, aes(x = Cranio)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribuzione del cranio",
       x = "Cranio",
       y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# RICHIESTA 4
# confronto media popolazione e valori del file
# test di Shapiro per verificare la normalità dei dati
# t test 
media_peso_popolazione <- 3300  
media_lunghezza_popolazione <- 500

shapiro_test_peso <- shapiro.test(neonati$Peso)
print(shapiro_test_peso)

shapiro_test_lunghezza <- shapiro.test(neonati$Lunghezza)
print(shapiro_test_lunghezza)

t_test_peso <- t.test(neonati$Peso, mu = media_peso_popolazione)
print(t_test_peso)

t_test_lunghezza <- t.test(neonati$Lunghezza, mu = media_lunghezza_popolazione)
print(t_test_lunghezza)

# creo una tabella e la salvo, per il report
risultati <- data.frame(
  Variabile = c("Peso", "Lunghezza"),
  t_statistic = c(t_test_peso$statistic, t_test_lunghezza$statistic),
  p_value = c(t_test_peso$p.value, t_test_lunghezza$p.value),
  conf_int_low = c(t_test_peso$conf.int[1], t_test_lunghezza$conf.int[1]),
  conf_int_high = c(t_test_peso$conf.int[2], t_test_lunghezza$conf.int[2])
)

tabella <- flextable(risultati)
print(tabella)

# RICHIESTA 5

# voglio indagare la distribuzione delle settimane di gestazione per M e F
ggplot(neonati, aes(x = Gestazione)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~Sesso) +
  labs(title = "Distribuzione delle Settimane di Gestazione per Sesso del Neonato",
       x = "Settimane di Gestazione",
       y = "Conteggio") +
  theme_minimal()

# voglio indagare la distribuzione di peso per M e F
ggplot(neonati, aes(x = Peso)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~Sesso) +
  labs(title = "Distribuzione del Peso per Sesso del Neonato",
       x = "Peso",
       y = "Conteggio") +
  theme_minimal()

# voglio indagare la distribuzione di lunghezza per M e F
ggplot(neonati, aes(x = Lunghezza)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~Sesso) +
  labs(title = "Distribuzione della Lunghezza per Sesso del Neonato",
       x = "Lunghezza",
       y = "Conteggio") +
  theme_minimal()

# voglio indagare la distribuzione del Cranio per M e F
ggplot(neonati, aes(x = Cranio)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~Sesso) +
  labs(title = "Distribuzione del Cranio per Sesso del Neonato",
       x = "Cranio",
       y = "Conteggio") +
  theme_minimal()

# RICHIESTA 6
conteggio_cesarei <- neonati %>%
  filter(Tipo.parto == "Ces") %>%
  group_by(Ospedale) %>%
  summarise(n_cesarei = n())

print(conteggio_cesarei)

# ANALISI MULTIDIMENSIONALE

# voglio indafare il tipo di parto con il peso
ggplot(neonati, aes(x = Tipo.parto, y = Peso)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  labs(title = "Distribuzione del Peso per Tipo di Parto",
       x = "Tipo di Parto",
       y = "Peso") +
  theme_minimal()

# voglio indagare il peso in relazione al cranio
ggplot(neonati, aes(x = Cranio, y = Peso)) +
  geom_point(alpha = 0.5) +  # alpha = 0.5 rende i punti semi-trasparenti, utile in caso di sovrapposizione
  labs(title = "Relazione tra Diametro del Cranio e Peso",
       x = "Diametro del Cranio (mm)",
       y = "Peso (grammi)") +
  theme_minimal()

# voglio indagare il peso in relazione alla lunghezza
ggplot(neonati, aes(x = Lunghezza, y = Peso)) +
  geom_point(alpha = 0.5) +  # alpha = 0.5 rende i punti semi-trasparenti, utile in caso di sovrapposizione
  labs(title = "Relazione tra lunghezza e Peso",
       x = "Lunghezza (mm)",
       y = "Peso (grammi)") +
  theme_minimal()

# voglio indagare il peso in relazione alla gestazione
ggplot(neonati, aes(x = Gestazione, y = Peso)) +
  geom_point(alpha = 0.5) +  # alpha = 0.5 rende i punti semi-trasparenti, utile in caso di sovrapposizione
  labs(title = "Relazione tra gestazione e Peso",
       x = "Gestazione (settimane)",
       y = "Peso (grammi)") +
  theme_minimal()

# correlazione tra gestazione, peso, lunghezza, cranio
# utilizzo una heat map
cor_matrix <- cor(neonati[, c("Gestazione", "Lunghezza", "Peso", "Cranio")], use = "pairwise.complete.obs")

melted_cor_matrix <- melt(cor_matrix)
ggplot(data = melted_cor_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +
  coord_fixed()

# MODELLO DI REGRESSIONE LINEARE MULTIPLA

# creo variabili dummy per le variabili categoriche
neonati <- neonati %>%
  mutate(
    parto_cesareo = ifelse(Tipo.parto == "Ces", 1, 0),
    sesso_F = ifelse(Sesso == "F", 1, 0),
    ospedale = as.factor(Ospedale)
  )

modello <- lm(Peso ~ Anni.madre + N.gravidanze + Fumatrici +
                Gestazione + Lunghezza + Cranio +
                parto_cesareo + ospedale + sesso_F, data = neonati)

summary(modello)

# modifico il modello togliendo le variabili che non risultano significiative
neonati <- neonati %>%
  mutate(sesso_F = ifelse(Sesso == "F", 1, 0))

modello2 <- lm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio +
               sesso_F, data = neonati)

summary(modello2)

# Scrivo un codice che simuli i vari modelli possibili e scelga quello migliore
# method = exhaustive
best_subsets <- regsubsets(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio +
                             sesso_F, data = neonati, nvmax = 5, method = "exhaustive")
summary_subsets <- summary(best_subsets)
best_models <- summary_subsets$which
best_3_var_model <- best_models[3, ]
print(names(best_3_var_model)[best_3_var_model])

# method = forward
best_subsets <- regsubsets(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio +
                             sesso_F, data = neonati, nvmax = 5, method = "forward")
summary_subsets <- summary(best_subsets)
best_models <- summary_subsets$which
best_3_var_model <- best_models[3, ]
print(names(best_3_var_model)[best_3_var_model])

# method = backward
best_subsets <- regsubsets(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio +
                             sesso_F, data = neonati, nvmax = 5, method = "backward")
summary_subsets <- summary(best_subsets)
best_models <- summary_subsets$which
best_3_var_model <- best_models[3, ]
print(names(best_3_var_model)[best_3_var_model])

# method seqrep
best_subsets <- regsubsets(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio +
                             sesso_F, data = neonati, nvmax = 5, method = "seqrep")
summary_subsets <- summary(best_subsets)
best_models <- summary_subsets$which
best_3_var_model <- best_models[3, ]
print(names(best_3_var_model)[best_3_var_model])

# modello con solo Gestazione Lunghezza e Cranio
neonati <- neonati %>%
  mutate(sesso_F = ifelse(Sesso == "F", 1, 0))

modello3 <- lm(Peso ~ Gestazione + Lunghezza + Cranio, data = neonati)

summary(modello3)

# grafici di come una variabile influisca sulla previsione, mantenendo costanti le altre
termplot(modello3, partial.resid = TRUE, ask = FALSE)
effetti <- allEffects(modello3)
plot(effetti)

# interazioni ed effetti non lineari, termini polinomiali
modello3_poly <- lm(Peso ~ Gestazione + I(Gestazione^2) + Lunghezza + I(Lunghezza^2) + Cranio + I(Cranio^2), data = neonati)
summary(modello3_poly)

# interazioni ed effetti non lineari
modello3_inter <- lm(Peso ~ Gestazione * Lunghezza * Cranio, data = neonati)
summary(modello3_inter)

# convalida incrociata
set.seed(123) 
control <- trainControl(method = "cv", number = 10)

result <- train(Peso ~ Gestazione + Lunghezza + Cranio, 
                data = neonati, 
                method = "lm", 
                trControl = control)

print(result)
# analisi dei residui
plot(predict(modello3), residuals(modello3),
     xlab = "Valori Predetti", ylab = "Residui",
     main = "Grafico dei Residui",
     pch = 20, col = rgb(0.2, 0.4, 0.6, 0.7))
abline(h = 0, lty = 2, col = "red")

# analisi dei residui 2
qqnorm(residuals(modello3), col = rgb(0.2, 0.4, 0.6, 0.7))
qqline(residuals(modello3), col = "red", lty = 2)

# modello robusto che riduce l impatto degli outliers
modello_robusto <- rlm(Peso ~ Gestazione + Lunghezza + Cranio, data = neonati)
summary(modello_robusto)

# previsione richiesta punto 7
neonati <- neonati %>%
  mutate(sesso_F = ifelse(Sesso == "F", 1, 0))
modello_semplificato <- lm(Peso ~ Gestazione + N.gravidanze + sesso_F, data = neonati)
nuovo_dataframe <- data.frame(
  Gestazione = 39,
  N.gravidanze = 3,
  sesso_F = 1
)
previsione <- predict(modello_semplificato, newdata = nuovo_dataframe, interval = "predict")
previsione

# grafici parziali per la richiesta 7
neonati_femmine <- neonati %>% filter(sesso_F == 1)
p <- ggplot(neonati_femmine, aes(x = Gestazione, y = Peso)) +
  geom_point(alpha = 0.5) +  # Punti originali
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linea di regressione
  labs(title = "Effetto della Gestazione sul Peso (femmina)",
       x = "Settimane di Gestazione",
       y = "Peso alla nascita (g)") +
  theme_minimal()
print(p)

g <- ggplot(neonati_femmine, aes(x = N.gravidanze, y = Peso)) +
  geom_point(alpha = 0.5) +  # Punti originali
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linea di regressione
  labs(title = "Effetto delle gravidanze sul Peso (femmina)",
       x = "Gravidanze",
       y = "Peso alla nascita (g)") +
  theme_minimal()
print(g)
