# USD/TRY Forecasting: A Comparative Analysis of ARIMA and LSTM Models

## Overview
This repository contains the R code and analysis for predicting the daily logarithmic returns of the USD/TRY exchange rate. The project compares the forecasting performance of a traditional linear statistical model (ARIMA) against a non-linear deep learning architecture (Long Short-Term Memory - LSTM).

## Data & Methodology
* **Data Source:** Daily closing prices from Yahoo Finance (via `quantmod`), covering January 2024 to May 2026.
* **Preprocessing:** Business day calendar alignment, missing data interpolation, log-return transformation, and stationarity validation using the Augmented Dickey-Fuller (ADF) test.
* **Models:**
  * **ARIMA (0,0,2):** Selected via the Akaike Information Criterion (AIC) using a comprehensive search space.
  * **LSTM:** A 2-layer architecture with 50 units per layer, 0.2 dropout, and Adam optimizer, trained with a 20-day look-back window.
* **Evaluation:** Models were evaluated on a chronologically split test set (20%) using a multi-step recursive forecasting approach.

## Key Results
The LSTM architecture significantly outperformed the ARIMA model across all evaluated metrics (MSE, RMSE, MAE, MASE). 
* **MAE (Mean Absolute Error):** The LSTM model achieved a ~3.7x lower error rate in the price scale compared to ARIMA.
* ARIMA demonstrated a systematic over-estimation bias due to its linear extrapolation of the trend, whereas LSTM successfully captured the non-linear dynamics and trajectory of the high-volatility financial time series.

## Technologies & Packages
* **Language:** R
* **Deep Learning:** `keras3`, `tensorflow`, `reticulate`
* **Time Series & Finance:** `quantmod`, `forecast`, `tseries`, `xts`, `zoo`
* **Visualization & Metrics:** `ggplot2`, `Metrics`

## Author
**Sude Su** Hacettepe University, Actuarial Sciences
