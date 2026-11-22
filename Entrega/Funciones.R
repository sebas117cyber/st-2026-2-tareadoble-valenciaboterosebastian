# Funciones de simulacion de datos de ST

#paquetes

library(forecast)
library(tseries)
library(TSA)
library(lmtest)
library(ggplot2)


# ---- PASO 0: Partición de Datos ----
separar_datos <- function(serie, h, nombre_entrenamiento = "entrenamiento") {
  n_total <- length(serie)
  
  if (h >= n_total) {
    stop("El número de datos a separar (h) debe ser menor a la longitud total de la serie.")
  }
  
  # Extraer entrenamiento (inicio) y prueba (final)
  entrenamiento <- head(serie, n_total - h)
  prueba <- tail(serie, h)
  
  # Asignar el conjunto de entrenamiento al entorno global con el nombre indicado
  assign(nombre_entrenamiento, entrenamiento, envir = .GlobalEnv)
  
  cat("- Datos totales:", n_total, "\n")
  cat("- Datos en '", nombre_entrenamiento, "' (entrenamiento): ", length(entrenamiento), "\n", sep="")
  cat("- Datos separados para prueba (h):", h, "\n")
  
  # También retornamos ambos por si se quieren capturar en una lista
  return(invisible(list(entrenamiento = entrenamiento, prueba = prueba)))
}

# EJEMPLO DE USO:
# division <- separar_datos(serie = serie_ARIMA1, h = 12, nombre_entrenamiento = "entrenamiento_ARIMA1")
# datos_prueba <- division$prueba


#----Simulacion de series----
simular_serie <- function(tipo, orden, n, phi = NULL, theta = NULL) {
  p <- 0
  d <- 0
  q <- 0
  
  # Asignacion de orden segun el tipo
  if (tipo == "AR") {
    p <- orden[1]
  } else if (tipo == "MA") {
    q <- orden[1]
  } else if (tipo == "ARMA") {
    p <- orden[1]
    q <- orden[2]
  } else if (tipo == "ARI") {
    p <- orden[1]
    d <- orden[2]
  } else if (tipo == "IMA") {
    d <- orden[1]
    q <- orden[2]
  } else if (tipo == "ARIMA") {
    p <- orden[1]
    d <- orden[2]
    q <- orden[3]
  } else {
    stop("Tipo de modelo no reconocido. Utiliza AR, MA, ARMA, ARI, IMA o ARIMA.")
  }
  
  # Lista para arima.sim

  modelo <- list(order = c(p, d, q))
  
  # verificacion coeficientes AR
  if (p > 0) {
    if (is.null(phi) || length(phi) != p) stop("tienen que haber tantos coeficientes 'phi' iguales al orden p.")
    modelo$ar <- phi
  }
  
  #  verificacion coeficientes MA (se aplica -theta)
  if (q > 0) {
    if (is.null(theta) || length(theta) != q) stop("tienen que haber tantos coeficientes 'theta' iguales al orden q.")
    modelo$ma <- -theta
  }
  
  
  set.seed(2025) # Semilla para reproducibilidad
  serie_simulada <- arima.sim(model = modelo, n = n)
  
  return(serie_simulada)
}

# Ejemplo de uso

# # 1. Simular un ARMA(1,1) con phi = 0.75 y theta = -0.4
# serie_arma <- simular_serie(tipo = "ARMA", orden = c(1, 1), n = 200, phi = 0.75, theta = -0.4)
# 
# 
# serie_ARIMA1 <- simular_serie(tipo = "ARIMA", orden = c(2, 1, 2), n = 180, 
#                               phi = c(0.12, 0.56), theta = c(-0.2, 0.5))
# 
# # 2. Simular un IMA(1,1) con d=1 y q=1, theta = 0.4
# 
# serie_ima <- simular_serie(tipo = "IMA", orden = c(1, 1), n = 200, theta = 0.4)
# 
# # 3. Simular un ARI(1,1) con p=1 y d=1, phi = 0.5
# 
# serie_ari <- simular_serie(tipo = "ARI", orden = c(1, 1), n = 200, phi = 0.5)
# 
# 


#---- Ajuste de series ----

ajustar_serie <- function(serie, orden, metodo = "ML") {

  p <- orden[1]
  d <- orden[2]
  q <- orden[3]
  
  # Validamos y aplicamos el método seleccionado
  if (metodo %in% c("ML", "CSS", "CSS-ML")) {
    # "ML"     : Máxima Verosimilitud Exacta (Exact Maximum Likelihood)
    # "CSS"    : Suma de Cuadrados Condicional (Conditional Sum of Squares)
    # "CSS-ML" : Aproximación inicial por CSS seguida de ML exacta
    
    modelo_ajustado <- arima(serie, order = c(p, d, q), method = metodo)
    
  } else if (metodo %in% c("yw", "burg", "ols", "mle")) {
    # Estos métodos son específicos para modelos puramente AR (d=0, q=0)
    # "yw"   : Ecuaciones de Yule-Walker
    # "burg" : Algoritmo de Burg
    # "ols"  : Mínimos Cuadrados Ordinarios
    # "mle"  : Máxima Verosimilitud condicional para AR
    
    if (d != 0 || q != 0) {
      stop("Los métodos 'yw', 'burg', 'ols' y 'mle' de la función ar() solo son válidos para modelos AR (d=0 y q=0).")
    }
    
    
    modelo_ajustado <- ar(serie, aic = FALSE, order.max = p, method = metodo)
    
  } else {
    stop("Método no reconocido. Usa 'ML', 'CSS', 'CSS-ML' para arima(), o 'yw', 'burg', 'ols', 'mle' para ar().")
  }
  
  
  
  cat("Orden (p,d,q) :", p, d, q, "\n")
  cat("Método usado  :", metodo, "\n\n")
  print(modelo_ajustado)
  
  return(modelo_ajustado)
}

# --- EJEMPLOS DE USO ---

# # 1. Ajuste por Máxima Verosimilitud Exacta (ML)
# 
#  ajuste_ml <- ajustar_serie(serie = mi_serie, orden = c(1, 0, 1), metodo = "ML")
# 
# # 2. Ajuste por Suma de Cuadrados Condicional (CSS)
# 
#  ajuste_css <- ajustar_serie(serie = mi_serie, orden = c(1, 1, 1), metodo = "CSS")
# 
# # 3. Ajuste Combinado (CSS-ML)
# 
#  ajuste_css_ml <- ajustar_serie(serie = mi_serie, orden = c(2, 0, 1), metodo = "CSS-ML")
# 
# # 4. Ajuste por Ecuaciones de Yule-Walker (Solo AR)
# 
#  ajuste_yw <- ajustar_serie(serie = mi_serie, orden = c(2, 0, 0), metodo = "yw")




# ----  Transformación de Escala ----
evaluar_boxcox <- function(serie) {
  
  # Encontrar el lambda óptimo
  lambda_opt <- BoxCox.lambda(serie)
  
  # Aplicar la transformación
  serie_transformada <- BoxCox(serie, lambda_opt)
  
  cat("Evaluación de Box-Cox:\n")
  cat("Lambda óptimo estimado:", round(lambda_opt, 4), "\n")
  if (lambda_opt > 0.9 && lambda_opt < 1.1) {
    cat("Sugerencia: Lambda cercano a 1. No es estrictamente necesaria la transformación.\n")
  }
  
  return(list(lambda = lambda_opt, serie_transformada = serie_transformada))
}


# ---- Diferenciación ----
evaluar_diferenciacion <- function(serie) {
  
  adf_res <- adf.test(serie)
  cat("--- Prueba Dickey-Fuller Aumentada (ADF) ---\n")
  cat("Valor p:", adf_res$p.value, "\n")
  
  if (adf_res$p.value > 0.05) {
    cat(" La serie NO es estacionaria hay raíz unitaria.\n")
    cat("Se aplicará una 1 diferencia regular.\n")
    serie_diff <- diff(serie)
  } else {
    cat(" La serie ES estacionaria. No se requiere diferenciación.\n")
    serie_diff <- serie
  }
  
  return(serie_diff)
}



# ----  Identificación y Constante ----
identificar_y_constante <- function(serie_diferenciada) {
  
  cat("--- Función de Autocorrelación Extendida (EACF) ---\n")
  eacf(serie_diferenciada)
  
  cat("\n--- Prueba de Constante / Deriva ---\n")
  media_w <- mean(serie_diferenciada)
  error_estandar_w <- sd(serie_diferenciada) / sqrt(length(serie_diferenciada))
  
  # Estadístico t
  t_constante <- media_w / error_estandar_w
  
  cat("Media de Wt (serie_dif):", round(media_w, 4), "\n")
  cat("Estadístico t:", t_constante, "\n")
  
  if (abs(t_constante) > 2) {
    cat(" |t| > 2. La constante ES SIGNIFICATIVA \n")
  } else {
    cat(" |t| <= 2. La constante NO ES significativa.\n") # (include.mean = FALSE)
  }
}


# ---- Validación Diagnóstico ----

validar_modelo <- function(modelo) {
  
  residuos <- residuals(modelo)
  
  cat("--- 1. SIGNIFICANCIA DE PARÁMETROS ---\n")
  print(coeftest(modelo))
  
  cat("\n--- 2. PRUEBA DE LJUNG-BOX (Autocorrelación) ---\n")
 
  coeficientes_arma <- sum(!grepl("intercept", names(modelo$coef)))
  
  lb_test <- Box.test(residuos, lag = 24, type = "Ljung-Box", fitdf = coeficientes_arma)
  print(lb_test)
  if (lb_test$p.value > 0.05) {
    cat(" Los residuos son RUIDO BLANCO (p > 0.05).\n")
  } else {
    cat(" Los residuos No son Ruido Blanco (p <= 0.05).\n")
  }
  
  cat("\n--- 3. PRUEBA DE NORMALIDAD (Jarque-Bera) ---\n")
  jb_test <- jarque.bera.test(residuos)
  print(jb_test)
  
  
  
  print(autoplot(modelo) + 
          labs(title = "Raíces Inversas del Modelo") + 
          theme_minimal())
}


# ---- Pronóstico y Evaluación ----

pronosticar_y_evaluar <- function(modelo, datos_prueba, h) {
  
  
  # Generar el pronóstico
  pronostico <- forecast(modelo, h = h)
  
  # Graficar
  grafico <- autoplot(pronostico) +
    autolayer(datos_prueba, series = "Datos Reales (Prueba)", color = "red", linewidth = 0.8) +
    labs(title = "Pronóstico Fuera de Muestra vs Realidad", 
         x = "Tiempo", y = "Valor") +
    theme_minimal()
  
  print(grafico)
  
  cat("\n--- MÉTRICAS DE PRECISIÓN (Accuracy) ---\n")
  metricas <- accuracy(pronostico, datos_prueba)
  print(metricas)
  
  return(invisible(list(pronostico = pronostico, metricas = metricas)))
}
