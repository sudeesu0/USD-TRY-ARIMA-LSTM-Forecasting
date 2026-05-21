# =============================================================================
# USD/TRY LOG RETURN TAHMINI
# LSTM ve ARIMA Model Karsilastirmasi
# Veri Kaynagi: Yahoo Finance
# Sembol: TRY=X
# =============================================================================


# =============================================================================
# 1. GEREKLI PAKETLERIN KURULMASI
# =============================================================================

install.packages("quantmod")
install.packages("xts")
install.packages("zoo")
install.packages("forecast")
install.packages("tseries")
install.packages("Metrics")
install.packages("ggplot2")
install.packages("keras3")
install.packages("tensorflow")
install.packages("reticulate")
install.packages("openxlsx")
install.packages("tidyr")



# =============================================================================
# 2. PAKETLERIN CAGIRILMASI
# =============================================================================

library(quantmod)     # Finansal veri cekme
library(xts)          # Zaman serisi nesneleri
library(zoo)          # Eksik veri doldurma (na.approx)
library(forecast)     # auto.arima, forecast fonksiyonlari
library(tseries)      # adf.test (duraganlik testi)
library(Metrics)      # rmse, mae, mase, mape gibi hata metrikleri
library(ggplot2)      # Gorsellestirme
library(keras3)       # LSTM modelleme
library(tensorflow)   # Keras backend
library(reticulate)   # Python entegrasyonu
library(openxlsx)     # Excel cikti
library(tidyr)  # pivot_longer icin

# =============================================================================
# 3. TARIH ARALIKLARININ BELIRLENMESI
# =============================================================================

symbol <- "TRY=X"

# Veri 2023 sonundan baslatilir.
# Cunku 2024-01-01 log return hesabi icin onceki kapanis fiyati gerekir.
download_start_date <- "2023-12-25"

# Asil analiz baslangici
analysis_start_date <- "2024-01-01"

# Bugune kadar veri cekilir
end_date <- "2026-05-04"


# =============================================================================
# 4. YAHOO FINANCE'DEN USD/TRY VERISININ CEKILMESI
# =============================================================================

data_raw <- getSymbols(symbol,
                       src = "yahoo",
                       from = download_start_date,
                       to = end_date,
                       auto.assign = FALSE)

head(data_raw)
tail(data_raw)


# =============================================================================
# 5. ADJUSTED CLOSE DEGERININ SECILMESI
# =============================================================================

price_data <- Ad(data_raw)
colnames(price_data) <- "Adjusted_Close"

head(price_data)
tail(price_data)


# =============================================================================
# 6. HAFTA ICI GUN TAKVIMININ OLUSTURULMASI
# =============================================================================

# Tum takvim gunlerini olustur
all_days <- seq.Date(from = as.Date(download_start_date),
                     to = as.Date(end_date),
                     by = "day")
head(all_days)

# Hafta ici gunleri filtrele (1 = Pazartesi, 2 = Sali, ..., 5 = Cuma)
business_days <- all_days[as.POSIXlt(all_days)$wday %in% 1:5]

# Bos xts nesnesi (sadece tarih indeksi icin)
business_xts <- xts(order.by = business_days)


# =============================================================================
# 7. VERININ HAFTA ICI TAKVIME OTURTULMASI
# =============================================================================

# Hafta ici takvimle birlestir (sol birlesim - eksik gunler NA olur)
price_business <- merge(business_xts, price_data, join = "left")
head(price_business, 20)
tail(price_business, 20)

# Eksik gun kontrolu
toplam <- nrow(price_business)
eksik  <- sum(is.na(price_business$Adjusted_Close))
cat("toplam gunu", toplam)
cat("eksik gunu", eksik)

# Hangi tarihler eksik?
eksik_tarihler <- index(price_business)[is.na(price_business$Adjusted_Close)]
eksik_tarihler <- data.frame(eksik_tarihler, weekdays(eksik_tarihler))
print(eksik_tarihler)

# Eksik degerleri lineer interpolasyon ile doldur
price_business$Adjusted_Close <- na.approx(price_business$Adjusted_Close,
                                           rule = 2)

# Doldurma sonrasi kontrol
eksik_sonra <- sum(is.na(price_business$Adjusted_Close))
cat("\nDoldurma sonrasi eksik:", eksik_sonra, "\n")
tail(price_business, 20)


# =============================================================================
# 8. LOG RETURN HESAPLAMA VE ANALIZ DONEMINE FILTRELEME
# =============================================================================

# Log return: log(P_t) - log(P_{t-1})
log_return <- diff(log(price_business$Adjusted_Close))
colnames(log_return) <- "Log_Return"

# Asil analiz baslangicindan itibaren kes (oncesini at)
log_return     <- log_return[paste0(analysis_start_date, "/")]
price_business <- price_business[paste0(analysis_start_date, "/")]

cat("gozlem sayisi", nrow(log_return))
cat("aralik", as.character(start(log_return)), "-", as.character(end(log_return)))
cat("volatilite (std)", round(sd(log_return), 6))

# Excel'e veri yaz
write.xlsx(
  data.frame(date = index(log_return),
             Adjusted_Close = as.numeric(price_business$Adjusted_Close),
             Log_Return = as.numeric(log_return)),
  "sude.xlsx"
)


# =============================================================================
# 9. EGITIM / TEST AYRIMI (%80 / %20)
# =============================================================================

n       <- nrow(log_return)
train_n <- floor(n * 0.8)

# Log return egitim ve test
train_lr <- log_return[1:train_n]
test_lr  <- log_return[(train_n + 1):n]

# Fiyat egitim ve test
train_p  <- price_business[1:train_n]
test_p   <- price_business[(train_n + 1):n]

cat("egitim", nrow(train_lr), "test", nrow(test_lr))


# =============================================================================
# 10. DURAGANLIK TESTI (ADF)
# =============================================================================

cat("adf testi")
print(adf.test(as.numeric(train_lr)))


# =============================================================================
# 11. ARIMA MODEL EGITIMI VE TEST TAHMINI
# =============================================================================

# auto.arima ile en iyi (p,d,q) sec
arima_model <- auto.arima(as.numeric(train_lr),
                          seasonal = FALSE,
                          stepwise = FALSE,
                          approximation = FALSE)
print(summary(arima_model))

# Test seti uzunlugu kadar ileri tahmin (multi-step ahead)
arima_fc      <- forecast(arima_model, h = nrow(test_lr))
arima_pred_lr <- as.numeric(arima_fc$mean)

# Log return tahminlerini fiyat olcegine cevir
# fiyat[t] = son_egitim_fiyati * exp(kumulatif_log_return)
son_egitim   <- as.numeric(tail(train_p$Adjusted_Close, 1))
arima_pred_p <- son_egitim * exp(cumsum(arima_pred_lr))

# =============================================================================
# 12. ARIMA - EGITIM SETI UYUM (FITTED VALUES, ONE-STEP-AHEAD)
# =============================================================================

# Egitim setindeki tarihler
train_tarih <- index(train_p)

# ARIMA'nin egitim setindeki fitted log return degerleri
arima_fitted_lr <- as.numeric(fitted(arima_model))

# ARIMA one-step-ahead training fit
# fiyat[t] = gercek_onceki_fiyat * exp(fitted_log_return[t])
onceki_fiyatlar_arima  <- as.numeric(train_p$Adjusted_Close)[1:(length(train_tarih) - 1)]
arima_fitted_p_onestep <- onceki_fiyatlar_arima * exp(arima_fitted_lr[2:length(arima_fitted_lr)])

arima_tarih_onestep  <- train_tarih[2:length(train_tarih)]
arima_gercek_onestep <- as.numeric(train_p$Adjusted_Close)[2:length(train_tarih)]

df_arima_train_v2 <- data.frame(
  tarih     = arima_tarih_onestep,
  GERCEK    = arima_gercek_onestep,
  ARIMA_FIT = arima_fitted_p_onestep
)

g_arima_train_v2 <- ggplot(df_arima_train_v2, aes(tarih)) +
  geom_line(aes(y = GERCEK,    colour = "GERCEK"), linewidth = 0.7) +
  geom_line(aes(y = ARIMA_FIT, colour = "ARIMA"),  linewidth = 0.6) +
  scale_colour_manual(
    name   = "Seri",
    values = c("GERCEK" = "black", "ARIMA" = "steelblue")
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(title = "ARIMA : GERCEK VS UYUM (EGITIM SETI - One-step ahead)",
       x = "Tarih", y = "USD/TRY") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(g_arima_train_v2)
ggsave("arima_train_uyum_onestep.png", g_arima_train_v2,
       dpi = 300, width = 11, height = 5)

# =============================================================================
# 13. ARIMA TEST METRIKLERI (LOG RETURN VE FIYAT OLCEGINDE)
# =============================================================================

# Test setindeki gercek degerler
gercek_lr <- as.numeric(test_lr)
gercek_p  <- as.numeric(test_p$Adjusted_Close)

# Log return olceginde metrikler
cat("rmse",  (rmse(gercek_lr, arima_pred_lr)))
cat("mae",   (mae(gercek_lr,  arima_pred_lr)))
cat("mase",  (mase(gercek_lr, arima_pred_lr)))
cat("mse",   (mse(gercek_lr,  arima_pred_lr)))
cat("mape",  (mape(gercek_lr, arima_pred_lr)))

# Fiyat olceginde metrikler
cat("rmse",  (rmse(gercek_p, arima_pred_p)))
cat("mae",   (mae(gercek_p,  arima_pred_p)))
cat("mase",  (mase(gercek_p, arima_pred_p)))
cat("mse",   (mse(gercek_p,  arima_pred_p)))
cat("mape",  (mape(gercek_p, arima_pred_p)))


# =============================================================================
# 14. ARIMA TEST GRAFIGI: GERCEK VS TAHMIN
# =============================================================================

library(ggplot2)

test_tarih_full <- index(test_p)

df_arima <- data.frame(
  tarih  = test_tarih_full,
  gercek = gercek_p,
  ARIMA  = arima_pred_p
)

g_arima <- ggplot(df_arima, aes(tarih)) +
  geom_line(aes(y = gercek, colour = "GERCEK")) +
  geom_line(aes(y = ARIMA,  colour = "ARIMA")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "ARIMA : GERCEK VS TAHMIN (TEST SETI)",
       x = "Tarih", y = "USD/TRY") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_arima)
ggsave("arima_tahmin.png", g_arima, dpi = 300)


# =============================================================================
# 15. LSTM ICIN VERI HAZIRLIGI
# =============================================================================

# Tekrarlanabilirlik icin random seed
set.seed(42)
tensorflow::set_random_seed(42)

# Min-max normalizasyon (sadece egitim setine fit edilir)
train_lr_vec <- as.numeric(train_lr)
min_val      <- min(train_lr_vec)
max_val      <- max(train_lr_vec)

normalize   <- function(x) (x - min_val) / (max_val - min_val)
denormalize <- function(x) (max_val - min_val) * x + min_val

# Tum log return serisini ayni min/max ile normalize et
log_return_norm <- normalize(as.numeric(log_return))

# LSTM penceresi: gecmis kac gune bakarak tahmin uretsin
look_back <- 20

# Pencere-hedef ciftleri olusturan yardimci fonksiyon
# X[i] = seri[i:(i+lookback-1)], y[i] = seri[i+lookback]
create_dataset <- function(seri, lookback) {
  X <- list()
  y <- c()
  for (i in seq_len(length(seri) - lookback)) {
    X[[i]] <- seri[i:(i + lookback - 1)]
    y[i]   <- seri[i + lookback]
  }
  list(
    X = array(unlist(X), dim = c(length(X), lookback, 1)),
    y = y
  )
}

# Egitim ve test setlerini olustur
# Test setine onceki look_back gun de eklenir ki ilk test pencere dolu olsun
train_norm <- log_return_norm[1:train_n]
test_norm  <- log_return_norm[(train_n - look_back + 1):n]

train_set <- create_dataset(train_norm, look_back)
test_set  <- create_dataset(test_norm,  look_back)


# =============================================================================
# 16. LSTM MIMARI VE EGITIM
# =============================================================================

# Iki katmanli LSTM + Dropout + Dense(1)
inputs  <- layer_input(shape = c(look_back, 1))
outputs <- inputs |>
  layer_lstm(units = 50, return_sequences = TRUE) |>
  layer_dropout(rate = 0.2) |>
  layer_lstm(units = 50) |>
  layer_dropout(rate = 0.2) |>
  layer_dense(units = 1)

lstm_model <- keras_model(inputs = inputs, outputs = outputs)

# Adam optimizer + MSE loss
compile(lstm_model,
        loss      = "mse",
        optimizer = optimizer_adam(learning_rate = 0.001))

summary(lstm_model)

# 50 epoch egitim, %10 validation
history <- fit(lstm_model,
               x                = train_set$X,
               y                = train_set$y,
               epochs           = 50,
               batch_size       = 16,
               validation_split = 0.1,
               verbose          = 1)


# =============================================================================
# 17. LSTM TEST TAHMINI (ITERATIF - MULTI-STEP RECURSIVE)
# -----------------------------------------------------------------------------
# Test seti boyunca LSTM gercek veriyi GORMEZ, kendi tahminlerini geri besler.
# Bu, ARIMA'nin multi-step forecast'i ile metodolojik olarak ESITTIR.
# Egitim sonunda son look_back gun ile baslar, sonra her tahmini geri besler.
# =============================================================================

son_pencere_test <- tail(log_return_norm[1:train_n], look_back)
n_test           <- nrow(test_lr)
lstm_pred_norm   <- numeric(n_test)

# Iteratif tahmin: her adimda kendi tahminini geri besle
for (i in seq_len(n_test)) {
  input_arr <- array(son_pencere_test, dim = c(1, look_back, 1))
  pred      <- as.numeric(predict(lstm_model, input_arr, verbose = 0))
  lstm_pred_norm[i] <- pred
  # Pencereyi kaydir: en eskiyi at, yeni tahmini ekle
  son_pencere_test <- c(son_pencere_test[-1], pred)
}

# Denormalize edip log return'e cevir
lstm_pred_lr <- denormalize(lstm_pred_norm)

# Fiyat olcegine cevir (ARIMA ile ayni mantik)
# Egitim sonu fiyatindan basla, kumulatif log return ile fiyat uret
lstm_pred_p <- son_egitim * exp(cumsum(lstm_pred_lr))
lstm_fitted_norm <- as.numeric(predict(lstm_model, train_set$X, verbose = 0))
lstm_fitted_lr   <- denormalize(lstm_fitted_norm)
# =============================================================================
# 18. LSTM - EGITIM SETI UYUM (FITTED VALUES, ONE-STEP-AHEAD)
# -----------------------------------------------------------------------------
# Egitim setinde LSTM her gun icin gercek onceki 20 gunu kullanir (one-step).
# Burada "training fit" gostermek icin bu uygundur - amac modelin egitim
# verisini ne kadar iyi yakaladigini gormek.
# =============================================================================

onceki_fiyatlar         <- as.numeric(train_p$Adjusted_Close)[look_back:(length(train_tarih) - 1)]
lstm_fitted_p_onestep   <- onceki_fiyatlar * exp(lstm_fitted_lr)


# Tarih ve gercek degerler
lstm_tarih_onestep  <- train_tarih[(look_back + 1):length(train_tarih)]
lstm_gercek_onestep <- as.numeric(train_p$Adjusted_Close)[(look_back + 1):length(train_tarih)]

df_lstm_train_v2 <- data.frame(
  tarih    = lstm_tarih_onestep,
  GERCEK   = lstm_gercek_onestep,
  LSTM_FIT = lstm_fitted_p_onestep
)

g_lstm_train_v2 <- ggplot(df_lstm_train_v2, aes(tarih)) +
  geom_line(aes(y = GERCEK,   colour = "GERCEK"), linewidth = 0.7) +
  geom_line(aes(y = LSTM_FIT, colour = "LSTM"),   linewidth = 0.6) +
  scale_colour_manual(
    name   = "Seri",
    values = c("GERCEK" = "black", "LSTM" = "firebrick")
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(title = "LSTM : GERCEK VS UYUM (EGITIM SETI - One-step ahead)",
       x = "Tarih", y = "USD/TRY") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(g_lstm_train_v2)
ggsave("lstm_train_uyum_onestep.png", g_lstm_train_v2,
       dpi = 300, width = 11, height = 5)


# =============================================================================
# 19. LSTM TEST METRIKLERI (LOG RETURN VE FIYAT OLCEGINDE)
# =============================================================================

# Log return olceginde metrikler
cat("rmse",  (rmse(gercek_lr, lstm_pred_lr)))
cat("mae",   (mae(gercek_lr,  lstm_pred_lr)))
cat("mase",  (mase(gercek_lr, lstm_pred_lr)))
cat("mse",   (mse(gercek_lr,  lstm_pred_lr)))
cat("mape",  (mape(gercek_lr, lstm_pred_lr)))

# Fiyat olceginde metrikler
cat("rmse",  (rmse(gercek_p, lstm_pred_p)))
cat("mae",   (mae(gercek_p,  lstm_pred_p)))
cat("mase",  (mase(gercek_p, lstm_pred_p)))
cat("mse",   (mse(gercek_p,  lstm_pred_p)))
cat("mape",  (mape(gercek_p, lstm_pred_p)))


# =============================================================================
# 20. LSTM TEST GRAFIGI: GERCEK VS TAHMIN
# =============================================================================

test_tarih_full <- index(test_p)

df_lstm <- data.frame(
  tarih  = test_tarih_full,
  gercek = gercek_p,
  LSTM   = lstm_pred_p
)

g_lstm <- ggplot(df_lstm, aes(tarih)) +
  geom_line(aes(y = gercek, colour = "GERCEK")) +
  geom_line(aes(y = LSTM,   colour = "LSTM")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "LSTM : GERCEK VS TAHMIN (TEST SETI)",
       x = "Tarih", y = "USD/TRY") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_lstm)
ggsave("lstm_tahmin.png", g_lstm, dpi = 300)



# =============================================================================
# 21. ARIMA vs LSTM - METRIK KARSILASTIRMA TABLOSU
# =============================================================================

karsilastirma <- data.frame(
  Model   = c("ARIMA", "LSTM"),
  RMSE_LR = c(rmse(gercek_lr, arima_pred_lr),
              rmse(gercek_lr, lstm_pred_lr)),
  MAE_LR  = c(mae(gercek_lr, arima_pred_lr),
              mae(gercek_lr, lstm_pred_lr)),
  MASE_LR = c(mase(gercek_lr, arima_pred_lr),
              mase(gercek_lr, lstm_pred_lr)),
  RMSE_P  = c(rmse(gercek_p, arima_pred_p),
              rmse(gercek_p, lstm_pred_p)),
  MAE_P   = c(mae(gercek_p, arima_pred_p),
              mae(gercek_p, lstm_pred_p)),
  MASE_P  = c(mase(gercek_p, arima_pred_p),
              mase(gercek_p, lstm_pred_p)),
  MAPE_P  = c(mape(gercek_p, arima_pred_p) * 100,
              mape(gercek_p, lstm_pred_p)  * 100)
)

karsilastirma[, -1] <- round(karsilastirma[, -1], 6)

cat("\n=========================================\n")
cat(" ARIMA vs LSTM\n")
cat("=========================================\n")
print(karsilastirma)

write.csv(karsilastirma, "model_karsilastirma.csv", row.names = FALSE)


# =============================================================================
# 22. ARIMA vs LSTM vs GERCEK KARSILASTIRMA GRAFIGI (TEST SETI)
# =============================================================================

df_karsilastirma <- data.frame(
  tarih  = test_tarih_full,
  GERCEK = gercek_p,
  ARIMA  = arima_pred_p,
  LSTM   = lstm_pred_p
)

g_karsilastirma <- ggplot(df_karsilastirma, aes(x = tarih)) +
  geom_line(aes(y = GERCEK, colour = "GERCEK"), linewidth = 0.8) +
  geom_line(aes(y = ARIMA,  colour = "ARIMA"),  linewidth = 0.7) +
  geom_line(aes(y = LSTM,   colour = "LSTM"),   linewidth = 0.7) +
  scale_colour_manual(
    name   = "Seri",
    values = c("GERCEK" = "black",
               "ARIMA"  = "steelblue",
               "LSTM"   = "firebrick")
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "ARIMA vs LSTM vs GERCEK (TEST SETI)",
    x = "Tarih",
    y = "USD/TRY"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(g_karsilastirma)
ggsave("arima_lstm_gercek.png", g_karsilastirma,
       dpi = 300, width = 10, height = 6)


# =============================================================================
# 23. GELECEK DONEM TAHMINI: HAZIRAN 2026 SONUNA KADAR
# =============================================================================

forecast_end_date <- as.Date("2026-06-30")
son_tarih         <- tail(index(price_business), 1)

# Gelecek is gunlerini hesapla
future_days     <- seq.Date(from = son_tarih + 1, to = forecast_end_date, by = "day")
future_business <- future_days[as.POSIXlt(future_days)$wday %in% 1:5]
h_future        <- length(future_business)

cat("Tahmin edilecek is gunu sayisi:", h_future, "\n")


# -----------------------------------------------------------------------------
# 23a. ARIMA: TUM VERI ILE YENIDEN EGIT, SONRA FORECAST
# -----------------------------------------------------------------------------

test_artisi <- nrow(test_lr)  # zaten tahmin edilen test gunu sayisi
toplam_h    <- test_artisi + h_future

arima_future_fc <- forecast(arima_model, h = toplam_h, level = c(80, 95))

# Son egitim fiyatindan baslayarak fiyat olcegine cevir
# (arima_model egitim sonunda durdugu icin son_egitim'den baslamali)
arima_future_p_full    <- son_egitim * exp(cumsum(as.numeric(arima_future_fc$mean)))
arima_future_lo80_full <- son_egitim * exp(cumsum(as.numeric(arima_future_fc$lower[, 1])))
arima_future_hi80_full <- son_egitim * exp(cumsum(as.numeric(arima_future_fc$upper[, 1])))
arima_future_lo95_full <- son_egitim * exp(cumsum(as.numeric(arima_future_fc$lower[, 2])))
arima_future_hi95_full <- son_egitim * exp(cumsum(as.numeric(arima_future_fc$upper[, 2])))

# Sadece gelecek kismini al (test sonrasi)
arima_future_p    <- arima_future_p_full[(test_artisi + 1):toplam_h]
arima_future_lo80 <- arima_future_lo80_full[(test_artisi + 1):toplam_h]
arima_future_hi80 <- arima_future_hi80_full[(test_artisi + 1):toplam_h]
arima_future_lo95 <- arima_future_lo95_full[(test_artisi + 1):toplam_h]
arima_future_hi95 <- arima_future_hi95_full[(test_artisi + 1):toplam_h]

# son_fiyat'i da tutarlilik icin tanimla (asagidaki bloklarda kullaniliyor)
son_fiyat <- as.numeric(tail(price_business$Adjusted_Close, 1))

# -----------------------------------------------------------------------------
# 23b. LSTM: ITERATIF (ROLLING) TAHMIN
# -----------------------------------------------------------------------------
# Not: lstm_model 16. bolumde train uzerinde egitilmisti.
# Burada onu yeniden egitmiyoruz, sadece iteratif olarak ileri tahmin yapiyoruz.
# ARIMA gibi: egitim sonundan basla, test + gelecek toplamini tahmin et,
# sonra sadece gelecek kismini ayir.

# Toplam tahmin ufku: test gunu sayisi + gelecek gun sayisi
# (test_artisi 23a'da tanimlandi)
toplam_h <- test_artisi + h_future

# Baslangic penceresi: egitim setinin son look_back gunu
# (lstm_model burada durdu, yani onun "su anki" hafizasi bu)
son_pencere      <- tail(log_return_norm[1:train_n], look_back)
lstm_future_norm <- numeric(toplam_h)

# Iteratif tahmin: her adimda kendi tahminini geri besle
for (i in seq_len(toplam_h)) {
  input_arr <- array(son_pencere, dim = c(1, look_back, 1))
  pred      <- as.numeric(predict(lstm_model, input_arr, verbose = 0))
  lstm_future_norm[i] <- pred
  # Pencereyi kaydir: en eskiyi at, yeni tahmini ekle
  son_pencere <- c(son_pencere[-1], pred)
}

# Denormalize edip log return'e cevir
lstm_future_lr_full <- denormalize(lstm_future_norm)

# Egitim sonu fiyatindan baslayarak fiyat olcegine cevir (cumsum)
lstm_future_p_full <- son_egitim * exp(cumsum(lstm_future_lr_full))

# Sadece gelecek kismini al (test sonrasi)
lstm_future_p <- lstm_future_p_full[(test_artisi + 1):toplam_h]

# =============================================================================
# 24. GECMIS + TEST TAHMINI + GELECEK BIRLESIK GRAFIK
# =============================================================================

# Test donemi tarihleri (modelin gercek veriyi gormedigi ama bizim bildigimiz)
test_tarihleri <- index(test_p)

# Test donemi ARIMA tahminleri (toplam tahminin ilk kismi)
arima_test_kisim <- arima_future_p_full[1:test_artisi]
lstm_test_kisim  <- lstm_future_p_full[1:test_artisi]

# Gecmis veri (sadece egitim donemi)
egitim_df <- data.frame(
  tarih = index(train_p),
  fiyat = as.numeric(train_p$Adjusted_Close),
  tip   = "Gercek (Egitim)"
)

# Test donemi gercek
test_gercek_df <- data.frame(
  tarih = test_tarihleri,
  fiyat = as.numeric(test_p$Adjusted_Close),
  tip   = "Gercek (Test)"
)

# Test donemi tahminleri
test_tahmin_df <- data.frame(
  tarih = rep(test_tarihleri, 2),
  fiyat = c(arima_test_kisim, lstm_test_kisim),
  tip   = rep(c("ARIMA Tahmin", "LSTM Tahmin"), each = test_artisi)
)

# Gelecek donemi tahminleri (Haz 2026'ya kadar)
future_df <- data.frame(
  tarih = rep(future_business, 2),
  fiyat = c(arima_future_p, lstm_future_p),
  tip   = rep(c("ARIMA Tahmin", "LSTM Tahmin"), each = h_future)
)

# ARIMA guven araliklari (sadece gelecek donemi - test icin de eklenebilir)
guven_df <- data.frame(
  tarih = future_business,
  lo80  = arima_future_lo80,
  hi80  = arima_future_hi80,
  lo95  = arima_future_lo95,
  hi95  = arima_future_hi95
)

# Egitim sonu (modelin durdugu nokta)
egitim_sonu <- tail(index(train_p), 1)

g_future <- ggplot() +
  # Gercek egitim verisi
  geom_line(data = egitim_df,
            aes(x = tarih, y = fiyat, colour = tip),
            linewidth = 0.6) +
  # Gercek test verisi
  geom_line(data = test_gercek_df,
            aes(x = tarih, y = fiyat, colour = tip),
            linewidth = 0.6) +
  # Test donemi tahminleri (ARIMA + LSTM)
  geom_line(data = test_tahmin_df,
            aes(x = tarih, y = fiyat, colour = tip),
            linewidth = 0.7, linetype = "dashed") +
  # Gelecek donemi tahminleri
  geom_line(data = future_df,
            aes(x = tarih, y = fiyat, colour = tip),
            linewidth = 0.8) +
  # Egitim/test ayirim cizgisi
  geom_vline(xintercept = as.numeric(egitim_sonu),
             linetype = "dotted", colour = "gray40") +
  # Bugunu isaretleyen cizgi
  geom_vline(xintercept = as.numeric(son_tarih),
             linetype = "dashed", colour = "gray40") +
  scale_colour_manual(
    name   = "Seri",
    values = c("Gercek (Egitim)" = "black",
               "Gercek (Test)"   = "darkgreen",
               "ARIMA Tahmin"    = "steelblue",
               "LSTM Tahmin"     = "firebrick")
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = "USD/TRY: Egitim, Test ve Haziran 2026 Sonuna Kadar Tahmin",
    subtitle = "Noktali cizgi: egitim sonu | Kesik cizgi: bugun",
    x = "Tarih", y = "USD/TRY"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(g_future)
ggsave("haziran_tahmin.png", g_future, dpi = 300, width = 12, height = 6)


# Sadece tahmin donemine zoom (son 30 gun + gelecek)
# Y-ekseni 42-50 ile sikistirilir ki yatay görünmesin
g_future_zoom <- g_future +
  coord_cartesian(
    xlim = c(son_tarih - 30, forecast_end_date),
    ylim = c(42, 50)
  )

print(g_future_zoom)
ggsave("haziran_tahmin_zoom.png", g_future_zoom,
       dpi = 300, width = 10, height = 6)

# =============================================================================
# 25. HEDEF GUN TAHMINI: 30 HAZIRAN 2026
# =============================================================================

hedef_tarih <- as.Date("2026-06-01")

# Hedef tarihin tahmin vektorundeki konumunu bul
hedef_index <- which(future_business == hedef_tarih)

# Tam o gun is gunu degilse en yakin is gununu al
if (length(hedef_index) == 0) {
  hedef_index <- which.min(abs(future_business - hedef_tarih))
  cat("Uyari: ", as.character(hedef_tarih),
      " is gunu degil. En yakin is gunu kullanildi: ",
      as.character(future_business[hedef_index]), "\n", sep = "")
}

# Hedef gun icin tahmin degerleri
arima_tahmin <- arima_future_p[hedef_index]
lstm_tahmin  <- lstm_future_p[hedef_index]
arima_lo80   <- arima_future_lo80[hedef_index]
arima_hi80   <- arima_future_hi80[hedef_index]
arima_lo95   <- arima_future_lo95[hedef_index]
arima_hi95   <- arima_future_hi95[hedef_index]

# Konsola yazdir
cat("\n=========================================\n")
cat(" ", as.character(future_business[hedef_index]), " TAHMINI\n", sep = "")
cat("=========================================\n")
cat(sprintf(" ARIMA nokta tahmin : %.4f TL\n", arima_tahmin))
cat(sprintf(" %%80 guven araligi : [%.4f , %.4f]\n", arima_lo80, arima_hi80))
cat(sprintf(" %%95 guven araligi : [%.4f , %.4f]\n", arima_lo95, arima_hi95))
cat(sprintf(" LSTM nokta tahmin  : %.4f TL\n", lstm_tahmin))
cat("=========================================\n")

# Hedef gunu grafik uzerinde isaretle
g_hedef <- g_future_zoom +
  # Hedef gun icin dikey cizgi
  geom_vline(xintercept = as.numeric(future_business[hedef_index]),
             linetype = "dotted", colour = "darkgreen", linewidth = 0.7) +
  # ARIMA noktasi
  geom_point(aes(x = future_business[hedef_index], y = arima_tahmin),
             colour = "steelblue", size = 3) +
  # LSTM noktasi
  geom_point(aes(x = future_business[hedef_index], y = lstm_tahmin),
             colour = "firebrick", size = 3) +
  # ARIMA etiketi
  annotate("label",
           x = future_business[hedef_index],
           y = arima_tahmin,
           label  = sprintf("ARIMA: %.2f", arima_tahmin),
           hjust  = 0.9, vjust = -0.3,
           colour = "steelblue", size = 3.5) +
  # LSTM etiketi
  annotate("label",
           x = future_business[hedef_index],
           y = lstm_tahmin,
           label  = sprintf("LSTM: %.2f", lstm_tahmin),
           hjust  = 1.3, vjust = 1.3,
           colour = "firebrick", size = 3.5)

print(g_hedef)
ggsave("hedef_gun_tahmin.png", g_hedef,
       dpi = 300, width = 10, height = 6)


# =============================================================================
# 26. ARIMA TANISI: ARTIK SERISI VE TEST SETI HATA GRAFIKLERI
# =============================================================================

# -----------------------------------------------------------------------------
# 26a. ARIMA artiklari (egitim seti, log return olceginde)
# -----------------------------------------------------------------------------

arima_resid <- as.numeric(residuals(arima_model))
# Gozlem numarasi yerine TARIH eksenini kullan
# Tarih ekseni ile artik serisi
arima_resid_df <- data.frame(
  tarih = index(train_p),
  artik = arima_resid
)

# Esik: artiklarin standart sapmasinin 2 kati
esik <- 2 * sd(arima_resid)

# Esigi asan gunleri bul
pik_df <- arima_resid_df[abs(arima_resid_df$artik) > esik, ]

g_resid_arima <- ggplot(arima_resid_df, aes(x = tarih, y = artik)) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  # Esik cizgileri (ust ve alt)
  geom_hline(yintercept = c(-esik, esik),
             linetype = "dotted", colour = "orange", linewidth = 0.6) +
  # Esigi asan gunler
  geom_point(data = pik_df, aes(x = tarih, y = artik),
             colour = "red", size = 2.5) +
  geom_text(data = pik_df,
            aes(x = tarih, y = artik,
                label = format(tarih, "%d %b")),
            vjust = ifelse(pik_df$artik > 0, -0.8, 1.5),
            size = 2.8, colour = "darkred") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(title = "ARIMA Artiklari ve Aykiri Gunler (Egitim Seti)",
       subtitle = sprintf("Turuncu kesik cizgi: +/- 2 standart sapma (%.4f). Asanlar isaretli.", esik),
       x = "Tarih", y = "Artik (Log Return)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_resid_arima)
ggsave("arima_artik_aykirilar.png", g_resid_arima,
       dpi = 300, width = 12, height = 6)

# Konsola yazdir
cat("\n=== ESIGI ASAN AYKIRI GUNLER ===\n")
cat("Esik (2 sd):", round(esik, 6), "\n")
cat("Toplam aykiri gun sayisi:", nrow(pik_df), "\n\n")
print(pik_df[order(abs(pik_df$artik), decreasing = TRUE), ])


# -----------------------------------------------------------------------------
# 26b. ARIMA artiklari (egitim seti, FIYAT olceginde)
# -----------------------------------------------------------------------------

arima_resid_p <- arima_gercek_onestep - arima_fitted_p_onestep

# Tarih ekseni ile artik serisi (fiyat olceginde)
arima_resid_p_df <- data.frame(
  tarih = arima_tarih_onestep,
  artik = arima_resid_p
)

# Esik: artiklarin standart sapmasinin 2 kati (TL cinsinden)
esik_p <- 2 * sd(arima_resid_p)

# Esigi asan gunleri bul
pik_p_df <- arima_resid_p_df[abs(arima_resid_p_df$artik) > esik_p, ]

g_resid_arima_p <- ggplot(arima_resid_p_df, aes(x = tarih, y = artik)) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  # Esik cizgileri (ust ve alt)
  geom_hline(yintercept = c(-esik_p, esik_p),
             linetype = "dotted", colour = "orange", linewidth = 0.6) +
  # Esigi asan gunler
  geom_point(data = pik_p_df, aes(x = tarih, y = artik),
             colour = "red", size = 2.5) +
  geom_text(data = pik_p_df,
            aes(x = tarih, y = artik,
                label = format(tarih, "%d %b")),
            vjust = ifelse(pik_p_df$artik > 0, -0.8, 1.5),
            size = 2.8, colour = "darkred") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(title = "ARIMA Artiklari ve Aykiri Gunler (Egitim Seti - Fiyat Olcegi)",
       subtitle = sprintf("Turuncu kesik cizgi: +/- 2 standart sapma (%.4f TL). Asanlar isaretli.", esik_p),
       x = "Tarih", y = "Artik (TL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_resid_arima_p)
ggsave("arima_artik_aykirilar_fiyat.png", g_resid_arima_p,
       dpi = 300, width = 12, height = 6)

# Konsola yazdir
cat("\n=== ESIGI ASAN AYKIRI GUNLER (FIYAT OLCEGI) ===\n")
cat("Esik (2 sd):", round(esik_p, 4), "TL\n")
cat("Toplam aykiri gun sayisi:", nrow(pik_p_df), "\n\n")
print(pik_p_df[order(abs(pik_p_df$artik), decreasing = TRUE), ])

# -----------------------------------------------------------------------------
# 26c. ARIMA test seti tahmin hatasi (fiyat olceginde)
# -----------------------------------------------------------------------------

fiyat_hatasi_arima <- gercek_p - arima_pred_p

g_hata_fiyat_arima <- ggplot(
  data.frame(tarih = test_tarih_full, h = fiyat_hatasi_arima),
  aes(tarih, h)
) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "ARIMA Fiyat Olceginde Tahmin Hatasi (Test Seti)",
       x = "Tarih", y = "Hata (TL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_hata_fiyat_arima)
ggsave("arima_fiyat_hatasi.png", g_hata_fiyat_arima,
       dpi = 300, width = 11, height = 5)

# =============================================================================
# 27. LSTM TANISI: ARTIK SERISI VE TEST SETI HATA GRAFIKLERI
# =============================================================================

# -----------------------------------------------------------------------------
# 27a. LSTM artiklari (egitim seti, log return olceginde)
# -----------------------------------------------------------------------------


# LSTM'in fit ettigi tarihler (ilk look_back gun yok)
lstm_resid_tarih  <- train_tarih[(look_back + 1):length(train_tarih)]
lstm_gercek_lr    <- as.numeric(train_lr)[(look_back + 1):length(train_lr)]

# Boyut tutarliligi kontrolu
n_lstm_fit <- length(lstm_fitted_lr)
lstm_resid_tarih <- tail(lstm_resid_tarih, n_lstm_fit)
lstm_gercek_lr   <- tail(lstm_gercek_lr,   n_lstm_fit)

# Artik = gercek - fit
lstm_resid <- lstm_gercek_lr - lstm_fitted_lr

# Tarih ekseni ile artik serisi
lstm_resid_df <- data.frame(
  tarih = lstm_resid_tarih,
  artik = lstm_resid
)

# Esik: artiklarin standart sapmasinin 2 kati
esik_lstm <- 2 * sd(lstm_resid)

# Esigi asan gunleri bul
pik_lstm_df <- lstm_resid_df[abs(lstm_resid_df$artik) > esik_lstm, ]

g_resid_lstm <- ggplot(lstm_resid_df, aes(x = tarih, y = artik)) +
  geom_line(color = "firebrick", linewidth = 0.4) +
  # Esik cizgileri
  geom_hline(yintercept = c(-esik_lstm, esik_lstm),
             linetype = "dotted", colour = "orange", linewidth = 0.6) +
  # Esigi asan gunler
  geom_point(data = pik_lstm_df, aes(x = tarih, y = artik),
             colour = "red", size = 2.5) +
  geom_text(data = pik_lstm_df,
            aes(x = tarih, y = artik,
                label = format(tarih, "%d %b")),
            vjust = ifelse(pik_lstm_df$artik > 0, -0.8, 1.5),
            size = 2.8, colour = "darkred") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(title = "LSTM Artiklari ve Aykiri Gunler (Egitim Seti - Log Return)",
       subtitle = sprintf("Turuncu kesik cizgi: +/- 2 standart sapma (%.4f). Asanlar isaretli.", esik_lstm),
       x = "Tarih", y = "Artik (Log Return)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_resid_lstm)
ggsave("lstm_artik_aykirilar.png", g_resid_lstm,
       dpi = 300, width = 12, height = 6)

cat("\n=== LSTM ESIGI ASAN AYKIRI GUNLER (LOG RETURN) ===\n")
cat("Esik (2 sd):", round(esik_lstm, 6), "\n")
cat("Toplam aykiri gun sayisi:", nrow(pik_lstm_df), "\n\n")
print(pik_lstm_df[order(abs(pik_lstm_df$artik), decreasing = TRUE), ])


# -----------------------------------------------------------------------------
# 27b. LSTM artiklari (egitim seti, FIYAT olceginde)
# -----------------------------------------------------------------------------

lstm_resid_p <- lstm_gercek_onestep - lstm_fitted_p_onestep

# Tarih ekseni ile artik serisi (fiyat olceginde)
lstm_resid_p_df <- data.frame(
  tarih = lstm_tarih_onestep,
  artik = lstm_resid_p
)

# Esik
esik_lstm_p <- 2 * sd(lstm_resid_p)

# Esigi asan gunleri bul
pik_lstm_p_df <- lstm_resid_p_df[abs(lstm_resid_p_df$artik) > esik_lstm_p, ]

g_resid_lstm_p <- ggplot(lstm_resid_p_df, aes(x = tarih, y = artik)) +
  geom_line(color = "firebrick", linewidth = 0.4) +
  geom_hline(yintercept = c(-esik_lstm_p, esik_lstm_p),
             linetype = "dotted", colour = "orange", linewidth = 0.6) +
  geom_point(data = pik_lstm_p_df, aes(x = tarih, y = artik),
             colour = "red", size = 2.5) +
  geom_text(data = pik_lstm_p_df,
            aes(x = tarih, y = artik,
                label = format(tarih, "%d %b")),
            vjust = ifelse(pik_lstm_p_df$artik > 0, -0.8, 1.5),
            size = 2.8, colour = "darkred") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(title = "LSTM Artiklari ve Aykiri Gunler (Egitim Seti - Fiyat Olcegi)",
       subtitle = sprintf("Turuncu kesik cizgi: +/- 2 standart sapma (%.4f TL). Asanlar isaretli.", esik_lstm_p),
       x = "Tarih", y = "Artik (TL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_resid_lstm_p)
ggsave("lstm_artik_aykirilar_fiyat.png", g_resid_lstm_p,
       dpi = 300, width = 12, height = 6)

cat("\n=== LSTM ESIGI ASAN AYKIRI GUNLER (FIYAT OLCEGI) ===\n")
cat("Esik (2 sd):", round(esik_lstm_p, 4), "TL\n")
cat("Toplam aykiri gun sayisi:", nrow(pik_lstm_p_df), "\n\n")
print(pik_lstm_p_df[order(abs(pik_lstm_p_df$artik), decreasing = TRUE), ])


# -----------------------------------------------------------------------------
# 27c. LSTM test seti tahmin hatasi (fiyat olceginde)
# -----------------------------------------------------------------------------

fiyat_hatasi_lstm <- gercek_p - lstm_pred_p

g_hata_fiyat_lstm <- ggplot(
  data.frame(tarih = test_tarih_full, h = fiyat_hatasi_lstm),
  aes(tarih, h)
) +
  geom_line(color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "LSTM Fiyat Olceginde Tahmin Hatasi (Test Seti)",
       x = "Tarih", y = "Hata (TL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_hata_fiyat_lstm)
ggsave("lstm_fiyat_hatasi.png", g_hata_fiyat_lstm,
       dpi = 300, width = 11, height = 5)


# =============================================================================
# ARTIK DURAGANLIK TESTLERI: ARIMA vs LSTM
# =============================================================================

# ARIMA artiklari (egitim seti, log return olceginde)
adf_arima <- adf.test(arima_resid)

# LSTM artiklari (egitim seti, log return olceginde)
adf_lstm <- adf.test(lstm_resid)

# Sonuclari yazdir
cat("\n=== ARTIK DURAGANLIK TESTLERI ===\n\n")

cat(">>> ARIMA Artiklari:\n")
cat("Test istatistigi:", round(as.numeric(adf_arima$statistic), 4), "\n")
cat("p-deger:", round(adf_arima$p.value, 4), "\n")
cat("Sonuc:", ifelse(adf_arima$p.value < 0.05,
                     "DURAGAN (iyi - artiklar beyaz gurultuye uyumlu)",
                     "DURAGAN DEGIL (model yapiyi yakalamamis)"), "\n\n")

cat(">>> LSTM Artiklari:\n")
cat("Test istatistigi:", round(as.numeric(adf_lstm$statistic), 4), "\n")
cat("p-deger:", round(adf_lstm$p.value, 4), "\n")
cat("Sonuc:", ifelse(adf_lstm$p.value < 0.05,
                     "DURAGAN (iyi - artiklar beyaz gurultuye uyumlu)",
                     "DURAGAN DEGIL (model yapiyi yakalamamis)"), "\n\n")


# Karsilastirmali grafik
adf_karsilastirma_df <- data.frame(
  etiket = c("ARIMA Artik\nTest Istatistigi",
             "LSTM Artik\nTest Istatistigi",
             "Kritik %1", "Kritik %5", "Kritik %10"),
  deger  = c(as.numeric(adf_arima$statistic),
             as.numeric(adf_lstm$statistic),
             -3.43, -2.86, -2.57),
  tip    = c("ARIMA", "LSTM", "Kritik", "Kritik", "Kritik")
)

g_adf_karsilastirma <- ggplot(adf_karsilastirma_df,
                              aes(x = reorder(etiket, deger),
                                  y = deger, fill = tip)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(deger, 3)), vjust = -0.5, size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("ARIMA"  = "steelblue",
                               "LSTM"   = "firebrick",
                               "Kritik" = "gray60")) +
  labs(title = "Artik Duraganlik Testi: ARIMA vs LSTM",
       subtitle = sprintf("ARIMA p: %.4f | LSTM p: %.4f | Esik: 0.05",
                          adf_arima$p.value, adf_lstm$p.value),
       x = "", y = "ADF Test Istatistigi") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 9))

print(g_adf_karsilastirma)
ggsave("adf_artik_karsilastirma.png", g_adf_karsilastirma,
       dpi = 300, width = 11, height = 6)


# Ljung-Box testleri
lb_arima <- Box.test(arima_resid, lag = 20, type = "Ljung-Box")
lb_lstm  <- Box.test(lstm_resid,  lag = 20, type = "Ljung-Box")



cat("\n=== LJUNG-BOX TESTLERI (ARTIK OTOKORELASYON) ===\n")
cat("ARIMA: Q =", round(as.numeric(lb_arima$statistic), 4),
    " p-deger =", round(lb_arima$p.value, 4),
    ifelse(lb_arima$p.value > 0.05, "(IYI - otokorelasyon yok)", "(KOTU - otokorelasyon var)"), "\n")
cat("LSTM:  Q =", round(as.numeric(lb_lstm$statistic), 4),
    " p-deger =", round(lb_lstm$p.value, 4),
    ifelse(lb_lstm$p.value > 0.05, "(IYI - otokorelasyon yok)", "(KOTU - otokorelasyon var)"), "\n")


# =============================================================================
# 28. METRIK KARSILASTIRMA GRAFIKLERI
# =============================================================================

# =============================================================================
# 28. METRIK KARSILASTIRMA: TRAINING + TEST (FACETED)
# =============================================================================

# -----------------------------------------------------------------------------
# 28a. ARIMA training metrikleri (one-step-ahead, fitted vs gercek)
# -----------------------------------------------------------------------------

# Training tahminleri ve gercek degerler (one-step-ahead, ilk gun haric)
arima_train_pred_lr <- arima_fitted_lr[2:length(arima_fitted_lr)]
arima_train_gercek_lr <- as.numeric(train_lr)[2:length(train_lr)]

# Fiyat olceginde (12. bolumde zaten hesapladik)
arima_train_pred_p   <- arima_fitted_p_onestep
arima_train_gercek_p <- arima_gercek_onestep


# -----------------------------------------------------------------------------
# 28b. LSTM training metrikleri (one-step-ahead, fitted vs gercek)
# -----------------------------------------------------------------------------

# LSTM training one-step-ahead tahminleri (look_back kadar gecikmeli)
# lstm_fitted_lr 17. bolumde tanimlandi
# lstm_gercek_lr ise 27. bolumde tanimlandi (ilk look_back gun yok)

# 27a'da yaptigimiz hizalamayi tekrar et
lstm_train_gercek_lr_local <- as.numeric(train_lr)[(look_back + 1):length(train_lr)]
n_lstm_fit <- length(lstm_fitted_lr)
lstm_train_gercek_lr_local <- tail(lstm_train_gercek_lr_local, n_lstm_fit)

# Fiyat olceginde (18. bolumde zaten hesapladik)
lstm_train_pred_p   <- lstm_fitted_p_onestep
lstm_train_gercek_p <- lstm_gercek_onestep

# =============================================================================
# 28c. Metrik veri cercevesi: TEST (FIYAT OLCEGINDE)
# =============================================================================

# ARIMA test metrikleri
arima_test_rmse_p  <- rmse(gercek_p, arima_pred_p)
arima_test_mae_p   <- mae(gercek_p,  arima_pred_p)
arima_test_mse_p   <- mse(gercek_p,  arima_pred_p)
arima_test_mase_p  <- mase(gercek_p, arima_pred_p)

# LSTM test metrikleri
lstm_test_rmse_p   <- rmse(gercek_p, lstm_pred_p)
lstm_test_mae_p    <- mae(gercek_p,  lstm_pred_p)
lstm_test_mse_p    <- mse(gercek_p,  lstm_pred_p)
lstm_test_mase_p   <- mase(gercek_p, lstm_pred_p)

# Konsola yazdir
cat("\n=== TEST METRIKLERI (FIYAT OLCEGI) ===\n")
cat("ARIMA: RMSE=", round(arima_test_rmse_p, 4),
    " MAE=", round(arima_test_mae_p, 4),
    " MSE=", round(arima_test_mse_p, 4),
    " MASE=", round(arima_test_mase_p, 4), "\n")
cat("LSTM:  RMSE=", round(lstm_test_rmse_p, 4),
    " MAE=", round(lstm_test_mae_p, 4),
    " MSE=", round(lstm_test_mse_p, 4),
    " MASE=", round(lstm_test_mase_p, 4), "\n")

# Veri cercevesi
metrik_p_long <- data.frame(
  Model  = c(rep("ARIMA", 4), rep("LSTM", 4)),
  Metrik = rep(c("RMSE", "MAE", "MSE", "MASE"), 2),
  Deger  = c(
    arima_test_rmse_p, arima_test_mae_p, arima_test_mse_p, arima_test_mase_p,
    lstm_test_rmse_p,  lstm_test_mae_p,  lstm_test_mse_p,  lstm_test_mase_p
  )
)

print(metrik_p_long)

# Metrik sirasini sabitle (MSE, RMSE, MAE, MASE)
metrik_p_long$Metrik <- factor(metrik_p_long$Metrik,
                               levels = c("MSE", "RMSE", "MAE", "MASE"))

# Grafik: 2x2 facet, her metrik ayri panelde
g_metrik_p <- ggplot(metrik_p_long,
                     aes(x = Model, y = Deger, fill = Model)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Deger, 4)),
            vjust = -0.5, size = 4) +
  facet_wrap(~ Metrik, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("ARIMA" = "steelblue", "LSTM" = "firebrick")) +
  labs(title = "Test Seti Performans Metrikleri (Fiyat Olcegi)",
       x = NULL, y = "Deger") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "gray90"),
        strip.text = element_text(face = "bold", size = 11),
        axis.text.x = element_text(size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))

print(g_metrik_p)
 ggsave("metrik_test_fiyat.png", g_metrik_p,
       dpi = 300, width = 10, height = 7)