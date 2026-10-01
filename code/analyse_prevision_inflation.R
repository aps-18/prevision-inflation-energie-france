# Packages ----

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(janitor)
library(ggplot2)
library(forecast)
library(randomForest)
library(tseries)
library(tsoutliers)
library(moments)
library(leaps)
library(gets)
library(broom)
library(knitr)
library(kableExtra)

# Création de la base finale ----

convertir_date <- function(x) {
  x <- str_replace(x, "M", "-")
  ym(x)
}

convertir_nombre <- function(x) {
  x <- str_replace_all(x, "\\(r\\)", "")
  x <- str_replace_all(x, "\\s", "")
  x <- str_replace(x, ",", ".")
  as.numeric(x)
}

ipc <- read_csv2("data/raw/ipc_energie.csv", col_types = cols(.default = col_character())) |>
  clean_names()

gaz <- read_csv2("data/raw/gaz_ttf.csv", col_types = cols(.default = col_character())) |>
  clean_names()

brent <- read_csv2("data/raw/brentE.csv", col_types = cols(.default = col_character())) |>
  clean_names()

mp <- read_csv2("data/raw/mp_hors_energie_eur.csv", col_types = cols(.default = col_character())) |>
  clean_names()

ipc_clean <- ipc |>
  rename(
    date = libelle,
    ipc_energie = indice_des_prix_a_la_consommation_base_2015_ensemble_des_menages_france_energie_series_arretees
  ) |>
  mutate(
    date = convertir_date(date),
    ipc_energie = convertir_nombre(ipc_energie)
  ) |>
  select(date, ipc_energie)

gaz_clean <- gaz |>
  rename(
    date = libelle,
    gaz_ttf = gaz_naturel_ttf_contrats_a_terme_a_la_premiere_echeance_aux_pays_bas_ice_futures_europe_prix_en_euros_par_megawattheure
  ) |>
  mutate(
    date = convertir_date(date),
    gaz_ttf = convertir_nombre(gaz_ttf)
  ) |>
  select(date, gaz_ttf)

brent_clean <- brent |>
  rename(
    date = month,
    brent_eur = brent
  ) |>
  mutate(
    date = convertir_date(date),
    brent_eur = convertir_nombre(brent_eur)
  ) |>
  select(date, brent_eur)

mp_clean <- mp |>
  rename(
    date = mois,
    mp_hors_energie_eur = indice_densemble
  ) |>
  mutate(
    date = convertir_date(date),
    mp_hors_energie_eur = convertir_nombre(mp_hors_energie_eur)
  ) |>
  select(date, mp_hors_energie_eur)

base <- ipc_clean |>
  left_join(brent_clean, by = "date") |>
  left_join(gaz_clean, by = "date") |>
  left_join(mp_clean, by = "date") |>
  arrange(date) |>
  filter(date >= ym("2005-01"), date <= ym("2025-12")) |>
  mutate(
    inflation_energie = ((ipc_energie / lag(ipc_energie, 12)) - 1) * 100,
    inflation_lag1 = lag(inflation_energie, 1),
    inflation_lag3 = lag(inflation_energie, 3),
    inflation_lag6 = lag(inflation_energie, 6),
    inflation_lag12 = lag(inflation_energie, 12),
    brent_lag1 = lag(brent_eur, 1),
    brent_lag3 = lag(brent_eur, 3),
    gaz_ttf_lag1 = lag(gaz_ttf, 1),
    gaz_ttf_lag3 = lag(gaz_ttf, 3),
    mp_hors_energie_lag1 = lag(mp_hors_energie_eur, 1),
    mp_hors_energie_lag3 = lag(mp_hors_energie_eur, 3)
  ) |>
  drop_na()

base_finale <- base

write_csv(base_finale, "data/processed/base_memoire.csv")

# Préparation de la base de modélisation ----

base_model <- base_finale |>
  select(
    date,
    inflation_energie,
    inflation_lag1,
    inflation_lag3,
    inflation_lag6,
    inflation_lag12,
    brent_lag1,
    brent_lag3,
    gaz_ttf_lag1,
    gaz_ttf_lag3,
    mp_hors_energie_lag1,
    mp_hors_energie_lag3
  ) |>
  na.omit()

train <- base_model |>
  filter(date <= as.Date("2021-12-01"))

test <- base_model |>
  filter(date >= as.Date("2022-01-01"))

y_train <- train$inflation_energie
y_test <- test$inflation_energie

calcul_perf <- function(observe, prevision, nom_modele) {
  data.frame(
    Modele = nom_modele,
    RMSE = sqrt(mean((observe - prevision)^2, na.rm = TRUE)),
    MAE = mean(abs(observe - prevision), na.rm = TRUE),
    R2 = 1 - sum((observe - prevision)^2, na.rm = TRUE) / sum((observe - mean(observe, na.rm = TRUE))^2, na.rm = TRUE),
    Correlation = cor(observe, prevision, use = "complete.obs"),
    Biais = mean(prevision - observe, na.rm = TRUE),
    sMAPE = mean(200 * abs(prevision - observe) / (abs(observe) + abs(prevision)), na.rm = TRUE),
    MASE = mean(abs(observe - prevision), na.rm = TRUE) / mean(abs(diff(observe)), na.rm = TRUE)
  )
}

# Modèle naïf ----

pred_naif <- lag(base_model$inflation_energie, 1)[base_model$date >= as.Date("2022-01-01")]

perf_naif <- calcul_perf(
  y_test,
  pred_naif,
  "Naïf"
)

# Modèle ARIMA ----

modele_arima <- auto.arima(
  y_train,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

prev_arima <- forecast(
  modele_arima,
  h = nrow(test)
)

pred_arima <- as.numeric(prev_arima$mean)

perf_arima <- calcul_perf(
  y_test,
  pred_arima,
  "ARIMA"
)

# Régression linéaire ----

modele_lm <- lm(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3 +
    gaz_ttf_lag1 +
    gaz_ttf_lag3 +
    mp_hors_energie_lag1 +
    mp_hors_energie_lag3,
  data = train
)

pred_lm <- predict(
  modele_lm,
  newdata = test
)

perf_lm <- calcul_perf(
  y_test,
  pred_lm,
  "Régression linéaire"
)

# Modèle ARX ----

modele_arx <- lm(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3 +
    gaz_ttf_lag1 +
    gaz_ttf_lag3 +
    mp_hors_energie_lag1 +
    mp_hors_energie_lag3,
  data = train
)

pred_arx <- predict(
  modele_arx,
  newdata = test
)

perf_arx <- calcul_perf(
  y_test,
  pred_arx,
  "ARX"
)

res_arx <- residuals(modele_arx)

ljung_box_arx <- data.frame(
  Modele = "ARX",
  Retard = c(6, 12, 18),
  p_value = c(
    Box.test(res_arx, lag = 6, type = "Ljung-Box")$p.value,
    Box.test(res_arx, lag = 12, type = "Ljung-Box")$p.value,
    Box.test(res_arx, lag = 18, type = "Ljung-Box")$p.value
  )
)

# Modèle ARMAX ----

xreg_train <- train |>
  select(
    inflation_lag1,
    inflation_lag3,
    inflation_lag6,
    inflation_lag12,
    brent_lag1,
    brent_lag3,
    gaz_ttf_lag1,
    gaz_ttf_lag3,
    mp_hors_energie_lag1,
    mp_hors_energie_lag3
  ) |>
  as.matrix()

xreg_test <- test |>
  select(
    inflation_lag1,
    inflation_lag3,
    inflation_lag6,
    inflation_lag12,
    brent_lag1,
    brent_lag3,
    gaz_ttf_lag1,
    gaz_ttf_lag3,
    mp_hors_energie_lag1,
    mp_hors_energie_lag3
  ) |>
  as.matrix()

modele_armax <- auto.arima(
  y_train,
  xreg = xreg_train,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

prev_armax <- forecast(
  modele_armax,
  xreg = xreg_test,
  h = nrow(test)
)

pred_armax <- as.numeric(prev_armax$mean)

perf_armax <- calcul_perf(
  y_test,
  pred_armax,
  "ARMAX"
)

ljung_box_armax <- data.frame(
  Modele = "ARMAX",
  Retard = c(6, 12, 18),
  p_value = c(
    Box.test(residuals(modele_armax), lag = 6, type = "Ljung-Box")$p.value,
    Box.test(residuals(modele_armax), lag = 12, type = "Ljung-Box")$p.value,
    Box.test(residuals(modele_armax), lag = 18, type = "Ljung-Box")$p.value
  )
)

table_ljung_box <- bind_rows(
  ljung_box_arx,
  ljung_box_armax
)

# Random Forest 1 à 4 ----

set.seed(123)

rf1 <- randomForest(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12,
  data = train,
  ntree = 500,
  importance = TRUE
)

rf2 <- randomForest(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3,
  data = train,
  ntree = 500,
  importance = TRUE
)

rf3 <- randomForest(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3 +
    gaz_ttf_lag1 +
    gaz_ttf_lag3,
  data = train,
  ntree = 500,
  importance = TRUE
)

rf4 <- randomForest(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3 +
    gaz_ttf_lag1 +
    gaz_ttf_lag3 +
    mp_hors_energie_lag1 +
    mp_hors_energie_lag3,
  data = train,
  ntree = 500,
  importance = TRUE
)

pred_rf1 <- predict(rf1, newdata = test)
pred_rf2 <- predict(rf2, newdata = test)
pred_rf3 <- predict(rf3, newdata = test)
pred_rf4 <- predict(rf4, newdata = test)

perf_rf1 <- calcul_perf(y_test, pred_rf1, "RF1")
perf_rf2 <- calcul_perf(y_test, pred_rf2, "RF2")
perf_rf3 <- calcul_perf(y_test, pred_rf3, "RF3")
perf_rf4 <- calcul_perf(y_test, pred_rf4, "RF4")

comparaison_rf <- bind_rows(
  perf_rf1,
  perf_rf2,
  perf_rf3,
  perf_rf4
)

# Random Forest pondérée ----

poids_train <- seq(
  1,
  2,
  length.out = nrow(train)
)

set.seed(123)

rf2_pondere <- randomForest(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3,
  data = train,
  ntree = 500,
  importance = TRUE,
  weights = poids_train
)

pred_rf2_pondere <- predict(
  rf2_pondere,
  newdata = test
)

perf_rf2_pondere <- calcul_perf(
  y_test,
  pred_rf2_pondere,
  "RF2 pondéré"
)

# Random Forest rolling window ----

taille_fenetre <- 60

resultats_rolling <- lapply(seq_len(nrow(test)), function(i) {
  
  date_test <- test$date[i]
  
  base_avant_date <- base_model |>
    filter(date < date_test)
  
  train_rolling <- tail(
    base_avant_date,
    taille_fenetre
  )
  
  test_rolling <- test[i, ]
  
  set.seed(123)
  
  modele_rolling <- randomForest(
    inflation_energie ~
      inflation_lag1 +
      inflation_lag3 +
      inflation_lag6 +
      inflation_lag12 +
      brent_lag1 +
      brent_lag3,
    data = train_rolling,
    ntree = 500,
    importance = TRUE
  )
  
  prediction_rolling <- predict(
    modele_rolling,
    newdata = test_rolling
  )
  
  data.frame(
    date = date_test,
    observe = test_rolling$inflation_energie,
    prediction = as.numeric(prediction_rolling)
  )
}) |>
  bind_rows()

pred_rf2_rolling <- resultats_rolling$prediction

perf_rf2_rolling <- calcul_perf(
  y_test,
  pred_rf2_rolling,
  "RF2 rolling window"
)

# Comparaison globale des modèles ----

comparaison_globale <- bind_rows(
  perf_naif,
  perf_rf2_rolling,
  perf_rf2,
  perf_rf2_pondere,
  perf_armax,
  perf_lm,
  perf_rf1,
  perf_rf3,
  perf_rf4,
  perf_arima
) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  ) |>
  arrange(RMSE)

print(comparaison_globale)

write_csv(
  comparaison_globale,
  "data/processed/resultats_comparaison_globale.csv"
)

# Prévisions des modèles ----

resultats_modeles <- data.frame(
  date = test$date,
  inflation_energie = y_test,
  pred_naif = pred_naif,
  pred_arima = pred_arima,
  pred_lm = pred_lm,
  pred_armax = pred_armax,
  pred_rf1 = pred_rf1,
  pred_rf2 = pred_rf2,
  pred_rf3 = pred_rf3,
  pred_rf4 = pred_rf4,
  pred_rf2_pondere = pred_rf2_pondere,
  pred_rf2_rolling = pred_rf2_rolling
)

write_csv(
  resultats_modeles,
  "data/processed/previsions_modeles.csv"
)

# Graphique des prévisions ----

graph_synthese_modeles <- resultats_modeles |>
  select(
    date,
    inflation_energie,
    pred_naif,
    pred_armax,
    pred_rf2,
    pred_rf2_rolling,
    pred_arima
  ) |>
  pivot_longer(
    cols = -date,
    names_to = "serie",
    values_to = "valeur"
  ) |>
  mutate(
    serie = recode(
      serie,
      inflation_energie = "Observé",
      pred_naif = "Naïf",
      pred_armax = "ARMAX",
      pred_rf2 = "RF2",
      pred_rf2_rolling = "RF2 rolling window",
      pred_arima = "ARIMA"
    )
  )

fig_previsions <- ggplot(
  graph_synthese_modeles,
  aes(x = date, y = valeur, color = serie)
) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "Observé" = "black",
      "Naïf" = "#1f77b4",
      "ARMAX" = "#ff7f0e",
      "RF2" = "#2ca02c",
      "RF2 rolling window" = "#d62728",
      "ARIMA" = "#9467bd"
    )
  ) +
  labs(
    title = "Prévisions de l'inflation énergétique sur la période de test",
    x = "Date",
    y = "Inflation énergétique, en %",
    color = "Série"
  ) +
  theme_minimal()

print(fig_previsions)

ggsave(
  "figures/figure_previsions_modeles.png",
  fig_previsions,
  width = 7,
  height = 4
)

# Importance des variables du modèle RF2 ----

importance_rf2 <- importance(rf2)

importance_table_rf2 <- data.frame(
  Variable = rownames(importance_rf2),
  Importance = importance_rf2[, "%IncMSE"]
) |>
  arrange(desc(Importance))

print(importance_table_rf2)

write_csv(
  importance_table_rf2,
  "data/processed/importance_variables_rf2.csv"
)

fig_importance_rf2 <- ggplot(
  importance_table_rf2,
  aes(x = reorder(Variable, Importance), y = Importance)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Importance des variables dans le modèle Random Forest 2",
    x = "Variable",
    y = "Importance (%IncMSE)"
  ) +
  theme_minimal()

print(fig_importance_rf2)

ggsave(
  "figures/figure_importance_rf2.png",
  fig_importance_rf2,
  width = 7,
  height = 4
)

# Tests de Diebold-Mariano ----

dm_test_safe <- function(e1, e2, nom) {
  test <- tryCatch(
    forecast::dm.test(
      e1,
      e2,
      alternative = "two.sided",
      h = 1,
      power = 2
    ),
    error = function(e) NULL
  )
  
  if (is.null(test)) {
    data.frame(
      Comparaison = nom,
      Statistique_DM = NA,
      p_value = NA,
      Conclusion = "Test impossible"
    )
  } else {
    data.frame(
      Comparaison = nom,
      Statistique_DM = as.numeric(test$statistic),
      p_value = as.numeric(test$p.value),
      Conclusion = ifelse(
        test$p.value < 0.05,
        "Différence significative",
        "Différence non significative"
      )
    )
  }
}

erreur_rf2 <- y_test - pred_rf2
erreur_naif <- y_test - pred_naif
erreur_arima <- y_test - pred_arima
erreur_armax <- y_test - pred_armax
erreur_lm <- y_test - pred_lm
erreur_rf2_pondere <- y_test - pred_rf2_pondere
erreur_rf2_rolling <- y_test - pred_rf2_rolling

tests_dm <- bind_rows(
  dm_test_safe(erreur_rf2, erreur_naif, "RF2 vs Naïf"),
  dm_test_safe(erreur_rf2, erreur_arima, "RF2 vs ARIMA"),
  dm_test_safe(erreur_rf2, erreur_armax, "RF2 vs ARMAX"),
  dm_test_safe(erreur_rf2, erreur_lm, "RF2 vs Régression linéaire"),
  dm_test_safe(erreur_rf2, erreur_rf2_pondere, "RF2 vs RF2 pondéré"),
  dm_test_safe(erreur_rf2, erreur_rf2_rolling, "RF2 vs RF2 rolling window"),
  dm_test_safe(erreur_rf2_pondere, erreur_naif, "RF2 pondéré vs Naïf"),
  dm_test_safe(erreur_rf2_rolling, erreur_naif, "RF2 rolling window vs Naïf"),
  dm_test_safe(erreur_armax, erreur_naif, "ARMAX vs Naïf")
) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )

print(tests_dm)

write_csv(
  tests_dm,
  "data/processed/tests_diebold_mariano.csv"
)

# Analyse des erreurs ----

erreurs_rf2 <- resultats_modeles |>
  transmute(
    date,
    observe = inflation_energie,
    prevision = pred_rf2,
    erreur = observe - prevision,
    erreur_absolue = abs(erreur)
  ) |>
  arrange(desc(erreur_absolue))

erreurs_rf2_rolling <- resultats_modeles |>
  transmute(
    date,
    observe = inflation_energie,
    prevision = pred_rf2_rolling,
    erreur = observe - prevision,
    erreur_absolue = abs(erreur)
  ) |>
  arrange(desc(erreur_absolue))

top_erreurs_rf2 <- erreurs_rf2 |>
  slice(1:10) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

top_erreurs_rf2_rolling <- erreurs_rf2_rolling |>
  slice(1:10) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

resume_erreurs <- bind_rows(
  data.frame(
    Modele = "RF2 classique",
    Erreur_moyenne = mean(erreurs_rf2$erreur),
    Erreur_absolue_moyenne = mean(erreurs_rf2$erreur_absolue),
    Erreur_minimale = min(erreurs_rf2$erreur),
    Erreur_maximale = max(erreurs_rf2$erreur),
    Ecart_type_erreurs = sd(erreurs_rf2$erreur)
  ),
  data.frame(
    Modele = "RF2 rolling window",
    Erreur_moyenne = mean(erreurs_rf2_rolling$erreur),
    Erreur_absolue_moyenne = mean(erreurs_rf2_rolling$erreur_absolue),
    Erreur_minimale = min(erreurs_rf2_rolling$erreur),
    Erreur_maximale = max(erreurs_rf2_rolling$erreur),
    Ecart_type_erreurs = sd(erreurs_rf2_rolling$erreur)
  )
) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

print(top_erreurs_rf2)
print(top_erreurs_rf2_rolling)
print(resume_erreurs)

write_csv(top_erreurs_rf2, "data/processed/top_erreurs_rf2.csv")
write_csv(top_erreurs_rf2_rolling, "data/processed/top_erreurs_rf2_rolling.csv")
write_csv(resume_erreurs, "data/processed/resume_erreurs_rf2.csv")

# Robustesse sur 100 graines aléatoires ----

resultats_seeds_rf2 <- lapply(1:100, function(seed) {
  
  set.seed(seed)
  
  modele_rf2_seed <- randomForest(
    inflation_energie ~
      inflation_lag1 +
      inflation_lag3 +
      inflation_lag6 +
      inflation_lag12 +
      brent_lag1 +
      brent_lag3,
    data = train,
    ntree = 500,
    importance = TRUE
  )
  
  pred_rf2_seed <- predict(
    modele_rf2_seed,
    newdata = test
  )
  
  data.frame(
    seed = seed,
    RMSE = sqrt(mean((y_test - pred_rf2_seed)^2)),
    MAE = mean(abs(y_test - pred_rf2_seed))
  )
}) |>
  bind_rows()

table_robustesse_seeds <- data.frame(
  Indicateur = c("RMSE", "MAE"),
  Moyenne = c(
    mean(resultats_seeds_rf2$RMSE),
    mean(resultats_seeds_rf2$MAE)
  ),
  Minimum = c(
    min(resultats_seeds_rf2$RMSE),
    min(resultats_seeds_rf2$MAE)
  ),
  Maximum = c(
    max(resultats_seeds_rf2$RMSE),
    max(resultats_seeds_rf2$MAE)
  ),
  Ecart_type = c(
    sd(resultats_seeds_rf2$RMSE),
    sd(resultats_seeds_rf2$MAE)
  )
) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

print(table_robustesse_seeds)

write_csv(
  resultats_seeds_rf2,
  "data/processed/robustesse_rf2_100_graines_detail.csv"
)

write_csv(
  table_robustesse_seeds,
  "data/processed/robustesse_rf2_100_graines_resume.csv"
)

# Robustesse avec période de test à partir de 2023 ----

train_robust <- base_model |>
  filter(date <= as.Date("2022-12-01"))

test_robust <- base_model |>
  filter(date >= as.Date("2023-01-01"))

y_train_robust <- train_robust$inflation_energie
y_test_robust <- test_robust$inflation_energie

xreg_train_robust <- train_robust |>
  select(
    inflation_lag1,
    inflation_lag3,
    inflation_lag6,
    inflation_lag12,
    brent_lag1,
    brent_lag3,
    gaz_ttf_lag1,
    gaz_ttf_lag3,
    mp_hors_energie_lag1,
    mp_hors_energie_lag3
  ) |>
  as.matrix()

xreg_test_robust <- test_robust |>
  select(
    inflation_lag1,
    inflation_lag3,
    inflation_lag6,
    inflation_lag12,
    brent_lag1,
    brent_lag3,
    gaz_ttf_lag1,
    gaz_ttf_lag3,
    mp_hors_energie_lag1,
    mp_hors_energie_lag3
  ) |>
  as.matrix()

modele_armax_robust <- auto.arima(
  y_train_robust,
  xreg = xreg_train_robust,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

pred_armax_robust <- as.numeric(
  forecast(
    modele_armax_robust,
    xreg = xreg_test_robust,
    h = nrow(test_robust)
  )$mean
)

set.seed(123)

rf1_robust <- randomForest(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12,
  data = train_robust,
  ntree = 500,
  importance = TRUE
)

rf2_robust <- randomForest(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3,
  data = train_robust,
  ntree = 500,
  importance = TRUE
)

rf3_robust <- randomForest(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3 +
    gaz_ttf_lag1 +
    gaz_ttf_lag3,
  data = train_robust,
  ntree = 500,
  importance = TRUE
)

rf4_robust <- randomForest(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3 +
    gaz_ttf_lag1 +
    gaz_ttf_lag3 +
    mp_hors_energie_lag1 +
    mp_hors_energie_lag3,
  data = train_robust,
  ntree = 500,
  importance = TRUE
)

pred_rf1_robust <- predict(rf1_robust, newdata = test_robust)
pred_rf2_robust <- predict(rf2_robust, newdata = test_robust)
pred_rf3_robust <- predict(rf3_robust, newdata = test_robust)
pred_rf4_robust <- predict(rf4_robust, newdata = test_robust)

comparaison_robustesse_2023 <- bind_rows(
  calcul_perf(y_test_robust, pred_armax_robust, "ARMAX"),
  calcul_perf(y_test_robust, pred_rf1_robust, "RF1 : retards inflation"),
  calcul_perf(y_test_robust, pred_rf2_robust, "RF2 : retards inflation + Brent"),
  calcul_perf(y_test_robust, pred_rf3_robust, "RF3 : retards inflation + Brent + gaz"),
  calcul_perf(y_test_robust, pred_rf4_robust, "RF4 : retards inflation + Brent + gaz + matières premières")
) |>
  select(
    Modele,
    RMSE,
    MAE,
    R2
  ) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

print(comparaison_robustesse_2023)

write_csv(
  comparaison_robustesse_2023,
  "data/processed/resultats_robustesse_2023.csv"
)

# Statistiques descriptives détaillées ----

stats_descriptives <- base_finale |>
  select(
    inflation_energie,
    brent_eur,
    gaz_ttf,
    mp_hors_energie_eur
  ) |>
  summarise(
    across(
      everything(),
      list(
        moyenne = ~ mean(.x, na.rm = TRUE),
        ecart_type = ~ sd(.x, na.rm = TRUE),
        minimum = ~ min(.x, na.rm = TRUE),
        maximum = ~ max(.x, na.rm = TRUE),
        skewness = ~ moments::skewness(.x, na.rm = TRUE),
        kurtosis = ~ moments::kurtosis(.x, na.rm = TRUE)
      )
    )
  )

stats_descriptives_long <- stats_descriptives |>
  pivot_longer(
    cols = everything(),
    names_to = "nom",
    values_to = "valeur"
  ) |>
  separate(
    nom,
    into = c("variable", "indicateur"),
    sep = "_(?=moyenne|ecart_type|minimum|maximum|skewness|kurtosis)"
  ) |>
  pivot_wider(
    names_from = indicateur,
    values_from = valeur
  ) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

print(stats_descriptives_long)

write_csv(
  stats_descriptives_long,
  "data/processed/stats_descriptives.csv"
)

# Points atypiques ----

serie_inflation <- ts(
  base_finale$inflation_energie,
  start = c(
    year(min(base_finale$date)),
    month(min(base_finale$date))
  ),
  frequency = 12
)

outliers_inflation <- tsoutliers::tso(
  serie_inflation
)

table_outliers <- outliers_inflation$outliers |>
  as.data.frame()

table_outliers_clean <- table_outliers |>
  mutate(
    date = as.Date(base_finale$date[ind]),
    date = format(date, "%Y-%m"),
    interpretation = case_when(
      grepl("2020", date) ~ "Crise sanitaire et chute de la demande d'énergie",
      grepl("2021", date) ~ "Reprise post-Covid et tensions sur les marchés de l'énergie",
      grepl("2022-04", date) ~ "Choc énergétique lié à la crise européenne et à la guerre en Ukraine",
      grepl("2022-08", date) ~ "Ajustement après le pic du choc énergétique",
      grepl("2023", date) ~ "Phase de désinflation énergétique après le pic de 2022",
      TRUE ~ "Point atypique lié à une variation marquée de l'inflation énergétique"
    )
  ) |>
  select(
    date,
    type,
    coefhat,
    tstat,
    interpretation
  ) |>
  arrange(desc(abs(tstat))) |>
  mutate(
    coefhat = round(coefhat, 2),
    tstat = round(tstat, 2)
  )

print(table_outliers_clean)

write_csv(
  table_outliers_clean,
  "data/processed/points_atypiques_inflation.csv"
)

serie_corrigee <- outliers_inflation$yadj

base_finale_outliers <- base_finale |>
  mutate(
    inflation_corrigee = as.numeric(serie_corrigee)
  )

# Tests de stationnarité ----

adf_result <- tseries::adf.test(
  base_finale$inflation_energie
)

kpss_result <- tseries::kpss.test(
  base_finale$inflation_energie
)

stationnarite_table <- data.frame(
  Test = c("ADF", "KPSS"),
  Hypothese_nulle = c(
    "Présence d'une racine unitaire",
    "Stationnarité de la série"
  ),
  Statistique = c(
    round(as.numeric(adf_result$statistic), 3),
    round(as.numeric(kpss_result$statistic), 3)
  ),
  p_value = c(
    round(adf_result$p.value, 3),
    round(kpss_result$p.value, 3)
  )
)

print(stationnarite_table)

write_csv(
  stationnarite_table,
  "data/processed/tests_stationnarite.csv"
)

# Décomposition STL, ACF et PACF ----

decomp_stl <- stl(
  serie_inflation,
  s.window = "periodic",
  robust = TRUE
)

fig_stl <- forecast::autoplot(decomp_stl) +
  labs(
    title = "Décomposition STL de l'inflation énergétique",
    x = "Date"
  ) +
  theme_minimal()

fig_acf <- forecast::ggAcf(
  serie_inflation,
  lag.max = 36
) +
  labs(
    title = "ACF de l'inflation énergétique"
  ) +
  theme_minimal()

fig_pacf <- forecast::ggPacf(
  serie_inflation,
  lag.max = 36
) +
  labs(
    title = "PACF de l'inflation énergétique"
  ) +
  theme_minimal()

print(fig_stl)
print(fig_acf)
print(fig_pacf)

ggsave("figures/figure_stl.png", fig_stl, width = 7, height = 4)
ggsave("figures/figure_acf.png", fig_acf, width = 7, height = 4)
ggsave("figures/figure_pacf.png", fig_pacf, width = 7, height = 4)

# Best Subset ----

base_selection <- base_finale |>
  select(
    inflation_energie,
    inflation_lag1,
    inflation_lag3,
    inflation_lag6,
    inflation_lag12,
    brent_lag1,
    brent_lag3,
    gaz_ttf_lag1,
    gaz_ttf_lag3,
    mp_hors_energie_lag1,
    mp_hors_energie_lag3
  ) |>
  na.omit()

best_subset <- leaps::regsubsets(
  inflation_energie ~ .,
  data = base_selection,
  nvmax = 10
)

summary_best <- summary(best_subset)

best_subset_table <- data.frame(
  nb_variables = 1:10,
  adjr2 = round(summary_best$adjr2, 3),
  bic = round(summary_best$bic, 3),
  cp = round(summary_best$cp, 3)
)

print(best_subset_table)

variables_best_bic <- coef(
  best_subset,
  which.min(summary_best$bic)
)

print(variables_best_bic)

write_csv(
  best_subset_table,
  "data/processed/best_subset.csv"
)

# GETS ----

modele_general <- lm(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag3 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3 +
    gaz_ttf_lag1 +
    gaz_ttf_lag3 +
    mp_hors_energie_lag1 +
    mp_hors_energie_lag3,
  data = base_selection
)

gets_result <- gets::gets(
  modele_general,
  t.pval = 0.10
)

modele_gets_final <- lm(
  inflation_energie ~
    inflation_lag1 +
    inflation_lag6 +
    inflation_lag12 +
    brent_lag1 +
    brent_lag3 +
    gaz_ttf_lag1 +
    gaz_ttf_lag3 +
    mp_hors_energie_lag1 +
    mp_hors_energie_lag3,
  data = base_selection
)

gets_coef_table <- broom::tidy(
  modele_gets_final
) |>
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = round(p.value, 3)
  )

print(gets_coef_table)

write_csv(
  gets_coef_table,
  "data/processed/gets_selection.csv"
)

# Graphiques descriptifs ----

fig_ipc <- ggplot(
  base_finale,
  aes(x = date, y = ipc_energie)
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Évolution de l'indice des prix à la consommation de l'énergie",
    x = "Date",
    y = "IPC énergie, base 2015",
    caption = "Source : INSEE, avec calculs."
  ) +
  theme_minimal()

fig_inflation <- ggplot(
  base_finale,
  aes(x = date, y = inflation_energie)
) +
  geom_line(linewidth = 0.8) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Inflation énergétique en France",
    subtitle = "Variation en glissement annuel de l'IPC énergie",
    x = "Date",
    y = "Inflation énergétique, en %",
    caption = "Source : INSEE, avec calculs."
  ) +
  theme_minimal()

fig_brent <- ggplot(
  base_finale,
  aes(x = date, y = brent_eur)
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Évolution du prix du pétrole Brent",
    x = "Date",
    y = "Brent, euros par baril",
    caption = "Source : INSEE."
  ) +
  theme_minimal()

fig_gaz <- ggplot(
  base_finale,
  aes(x = date, y = gaz_ttf)
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Évolution du prix du gaz naturel TTF",
    x = "Date",
    y = "Gaz TTF, euros par mégawattheure",
    caption = "Source : INSEE."
  ) +
  theme_minimal()

fig_mp <- ggplot(
  base_finale,
  aes(x = date, y = mp_hors_energie_eur)
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Évolution de l'indice des matières premières\nimportées hors énergie",
    x = "Date",
    y = "Indice en euros",
    caption = "Source : INSEE."
  ) +
  theme_minimal()

print(fig_ipc)
print(fig_inflation)
print(fig_brent)
print(fig_gaz)
print(fig_mp)

ggsave("figures/figure_ipc_energie.png", fig_ipc, width = 7, height = 4)
ggsave("figures/figure_inflation_energie.png", fig_inflation, width = 7, height = 4)
ggsave("figures/figure_brent.png", fig_brent, width = 7, height = 4)
ggsave("figures/figure_gaz_ttf.png", fig_gaz, width = 7, height = 4)
ggsave("figures/figure_matieres_premieres.png", fig_mp, width = 7, height = 4)
