# Funciones de simulacion de datos de ST


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

# 1. Simular un ARMA(1,1) con phi = 0.75 y theta = -0.4
# (Basado en el ejemplo de z_arma de las notas)
serie_arma <- simular_serie(tipo = "ARMA", orden = c(1, 1), n = 200, phi = 0.75, theta = -0.4)


serie_ARIMA1 <- simular_serie(tipo = "ARIMA", orden = c(2, 1, 2), n = 180, 
                              phi = c(0.12, 0.56), theta = c(-0.2, 0.5))

# 2. Simular un IMA(1,1) con d=1 y q=1, theta = 0.4
# (Basado en el ejemplo de z_ima de las notas)
serie_ima <- simular_serie(tipo = "IMA", orden = c(1, 1), n = 200, theta = 0.4)

# 3. Simular un ARI(1,1) con p=1 y d=1, phi = 0.5
# (Basado en el ejemplo de z_ari de las notas)
serie_ari <- simular_serie(tipo = "ARI", orden = c(1, 1), n = 200, phi = 0.5)


