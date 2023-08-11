# Borrado de datos anteriores =============================

rm(list = ls())

# Lectura de librerías útiles =============================

library(magrittr)
library(dplyr)
library(car)
library(agricolae)
library(MASS)
library(nortest)

# Lectura de los datos ====================================

datos <- read.csv('./Datos/tiempos_piloto.csv')
datos %>% str()
datos$papel %<>% as.factor()
datos$avion %<>% as.factor()
datos %>% str()
datos %>% head()
datos %>% tail()
datos %<>% arrange(orden)

# 1. Modelo uno. Datos crudos =============================

## 1.1. Gráfico de efectos ======================

plot.design(tiempo ~ avion * papel, data = datos,
            main = 'Gráfico de efectos principales', las = 1,
            xlab = 'Factores',
            ylab = 'Tiempo medio [s]')

## 1.2. Gráficos de cajas y bigotes =============

par(mfrow = c(1, 2))
boxplot(tiempo ~ avion, data = datos,
        xlab = 'Tipo de avión',
        ylab = 'Tiempo medio [s]',
        col = 'white')
grid(nx = NA, ny = NULL)
boxplot(tiempo ~ papel, data = datos,
        xlab = 'Tipo de papel',
        ylab = 'Tiempo medio [s]',
        col = 'white')
grid(nx = NA, ny = NULL)
mtext("Boxplots para el tiempo de vuelo según los niveles de cada factor",
      side = 3, line = -2.5, outer = TRUE, cex = 1.5, font = 4)

## 1.3. Gráficos de interacción =================

par(mfrow = c(1, 2))
with(datos,
     interaction.plot(papel, avion, tiempo,
                      type = 'b', pch = 20, fixed = TRUE, xlab = 'Papel',
                      ylab = 'Tiempo de vuelo [s]',
                      las = 1, col = c('red', 'blue', 'black')))
grid()

with(datos,
     interaction.plot(avion, papel, tiempo,
                      type = 'b', pch = 20, fixed = TRUE, xlab = 'Avión',
                      ylab = 'Tiempo de vuelo [s]',
                      las = 1, col = c('red', 'blue', 'black')))
grid()
mtext("Gráficos de interacción para cada factor",
      side = 3, line = -2.5, outer = TRUE, cex = 1.5, font = 4)

## 1.4. Tabla Anova =============================

modelo <- aov(tiempo ~ papel * avion, data = datos)
modelo %>% summary()

## 1.5. Validación de supuestos =================

### 1.5.1. Normalidad ============

#### Test paramétrico ====

# Test KS
datos$tiempo %>% shapiro.test()                  # Rechazo

# Test AD
datos$tiempo %>% ad.test()                       # Rechazo

# Test Lilliefors
datos$tiempo %>% lillie.test()                   # Rechazo

#### Gráfico cuantil cuantil ====

#Leyenda
leyenda <- c('Test de Shapiro-Wilk',
             expression(paste('W', ' = ', '0.904')),
             expression(paste('V'[p], ' = ', '3.4 e -6')))

# Gráfico cuantil-cuantil
par(mfrow = c(1, 1))
datos$tiempo %>% qqnorm(xlab = 'Cuantiles teóricos',
                        ylab = 'Cuantiles muestrales',
                        main = 'Gráfico cuantil-cuantil normal')
datos$tiempo %>% qqline()
grid()
legend('topleft', leyenda, cex = 1.25)

### 1.5.2. Homocedasticidad ======

bartlett.test(tiempo ~ avion, data = datos)      # Rechazo
bartlett.test(tiempo ~ papel, data = datos)      # Rechazo

### 1.5.3. Independencia y media nula ====

durbinWatsonTest(modelo)                         # Rechazo

plot(y = modelo$residuals, x = 1:nrow(datos),
     main = 'Orden de observación contra residuales',
     xlab = 'Orden de observación',
     ylab = 'Residual',
     pch = as.numeric(datos$avion %>% as.factor()),
     col = as.factor(datos$papel))
abline(a = 0, b = 0, col = 'black', lty = 4, lwd = 3)
grid()
legend('topright',
       legend = c('Bond', 'Capuccino', 'Sahara', 'Cuaderno'),
       pch = rep(20, 4),
       col = c('green', 'black', 'red', 'blue'),
       title = 'Papel', cex = 0.75)
legend('topleft',
       legend = c('Stable', 'Hunting', 'Heavy-Nosed'),
       pch = as.numeric(datos$avion %>% as.factor()),
       title = 'Avión', cex = 0.75)

plot(x = modelo$fitted.values, y = modelo$residuals,
     main = 'Valores ajustados contra residuales',
     xlab = 'Valor ajustado',
     ylab = 'Residual',
     pch = as.numeric(datos$avion %>% as.factor()),
     col = as.factor(datos$papel),
     bg = as.factor(datos$Marca))
abline(a = 0, b = 0, col = 'black', lty = 4, lwd = 3)
grid()
legend('bottomright',
       legend = c('Bond', 'Capuccino', 'Sahara', 'Cuaderno'),
       pch = rep(20, 4),
       col = c('green', 'black', 'red', 'blue'),
       title = 'Papel', cex = 0.75)
legend('bottomleft',
       legend = c('Stable', 'Hunting', 'Heavy-Nosed'),
       pch = as.numeric(datos$avion %>% as.factor()),
       title = 'Avión', cex = 0.75)

# /////////////////////////////////////////////////////////////////////////////

# 2. Modelo dos. Logaritmo ================================

datos$log_t <- datos$tiempo %>% log()

## 2.1. Gráfico de efectos ======================

plot.design(log_t ~ avion * papel, data = datos,
            main = 'Gráfico de efectos principales', las = 1,
            xlab = 'Factores',
            ylab = 'Tiempo medio [s]')

## 2.2. Gráficos de cajas y bigotes =============

par(mfrow = c(1, 2))
boxplot(log_t ~ avion, data = datos,
        xlab = 'Tipo de avión',
        ylab = 'Tiempo medio [s]',
        col = 'white')
grid(nx = NA, ny = NULL)
boxplot(log_t ~ papel, data = datos,
        xlab = 'Tipo de papel',
        ylab = 'Tiempo medio [s]',
        col = 'white')
grid(nx = NA, ny = NULL)
mtext("Boxplots para el log. del tiempo de vuelo según los niveles de c./factor",
      side = 3, line = -2.5, outer = TRUE, cex = 1.5, font = 4)

## 2.3. Gráficos de interacción =================

par(mfrow = c(1, 2))
with(datos,
     interaction.plot(papel, avion, log_t,
                      type = 'b', pch = 20, fixed = TRUE, xlab = 'Papel',
                      ylab = 'Tiempo de vuelo [s]',
                      las = 1, col = c('red', 'blue', 'black')))
grid()

with(datos,
     interaction.plot(avion, papel, log_t,
                      type = 'b', pch = 20, fixed = TRUE, xlab = 'Avión',
                      ylab = 'Tiempo de vuelo [s]',
                      las = 1, col = c('red', 'blue', 'black')))
grid()
mtext("Gráficos de interacción para cada factor",
      side = 3, line = -2.5, outer = TRUE, cex = 1.5, font = 4)

## 2.4. Tabla Anova =============================

modelo2 <- aov(log_t ~ papel * avion, data = datos)
modelo2 %>% summary()

## 2.5. Validación de supuestos =================

### 2.5.1. Normalidad ============

#### Test paramétrico ====

# Test KS
datos$log_t %>% shapiro.test()                  # Rechazo

# Test AD
datos$log_t %>% ad.test()                       # Rechazo

# Test Lilliefors
datos$log_t %>% lillie.test()                   # No rechazo

#### Gráfico cuantil cuantil ====

#Leyenda
leyenda2 <- c('Test de Shapiro-Wilk',
             expression(paste('W', ' = ', '0.964')),
             expression(paste('V'[p], ' = ', '0.010')))

# Gráfico cuantil-cuantil
par(mfrow = c(1, 1))
datos$log_t %>% qqnorm(xlab = 'Cuantiles teóricos',
                        ylab = 'Cuantiles muestrales',
                        main = 'Gráfico cuantil-cuantil normal')
datos$log_t %>% qqline()
grid()
legend('topleft', leyenda2, cex = 1.25)

### 2.5.2. Homocedasticidad ======

bartlett.test(log_t ~ avion, data = datos)      # Rechazo
bartlett.test(log_t ~ papel, data = datos)      # No rechazo

### 2.5.3. Independencia y media nula ====

durbinWatsonTest(modelo2)                       # No rechazo

plot(y = modelo2$residuals, x = 1:nrow(datos),
     main = 'Orden de observación contra residuales',
     xlab = 'Orden de observación',
     ylab = 'Residual',
     pch = as.numeric(datos$avion %>% as.factor()),
     col = as.factor(datos$papel))
abline(a = 0, b = 0, col = 'black', lty = 4, lwd = 3)
grid()
legend('topright',
       legend = c('Bond', 'Capuccino', 'Sahara', 'Cuaderno'),
       pch = rep(20, 4),
       col = c('green', 'black', 'red', 'blue'),
       title = 'Papel', cex = 0.75)
legend('topleft',
       legend = c('Stable', 'Hunting', 'Heavy-Nosed'),
       pch = as.numeric(datos$avion %>% as.factor()),
       title = 'Avión', cex = 0.75)

plot(x = modelo2$fitted.values, y = modelo2$residuals,
     main = 'Valores ajustados contra residuales',
     xlab = 'Valor ajustado',
     ylab = 'Residual',
     pch = as.numeric(datos$avion %>% as.factor()),
     col = as.factor(datos$papel),
     bg = as.factor(datos$Marca))
abline(a = 0, b = 0, col = 'black', lty = 4, lwd = 3)
grid()
legend('bottomright',
       legend = c('Bond', 'Capuccino', 'Sahara', 'Cuaderno'),
       pch = rep(20, 4),
       col = c('green', 'black', 'red', 'blue'),
       title = 'Papel', cex = 0.75)
legend('bottomleft',
       legend = c('Stable', 'Hunting', 'Heavy-Nosed'),
       pch = as.numeric(datos$avion %>% as.factor()),
       title = 'Avión', cex = 0.75)

# /////////////////////////////////////////////////////////////////////////////

# 3. Modelo tres. Box - Cox ===============================

boxcox <- lm(tiempo ~ 1, data = datos) %>% boxcox()

lambda <- boxcox$x[which.max(boxcox$y)]
lambda

datos$t_bc <- (datos$tiempo ^ lambda - 1) / lambda

## 3.1. Gráfico de efectos ======================

plot.design(t_bc ~ avion * papel, data = datos,
            main = 'Gráfico de efectos principales', las = 1,
            xlab = 'Factores',
            ylab = 'Tiempo medio [s]')

## 3.2. Gráficos de cajas y bigotes =============

par(mfrow = c(1, 2))
boxplot(t_bc ~ avion, data = datos,
        xlab = 'Tipo de avión',
        ylab = 'Tiempo medio [s]',
        col = 'white')
grid(nx = NA, ny = NULL)
boxplot(t_bc ~ papel, data = datos,
        xlab = 'Tipo de papel',
        ylab = 'Tiempo medio [s]',
        col = 'white')
grid(nx = NA, ny = NULL)
mtext("Boxplots para el tiempo transformado de vuelo según los
      niveles de c./factor",
      side = 3, line = -3, outer = TRUE, cex = 1.5, font = 4)

## 2.3. Gráficos de interacción =================

par(mfrow = c(1, 2))
with(datos,
     interaction.plot(papel, avion, t_bc,
                      type = 'b', pch = 20, fixed = TRUE, xlab = 'Papel',
                      ylab = 'Tiempo de vuelo [s]',
                      las = 1, col = c('red', 'blue', 'black')))
grid()

with(datos,
     interaction.plot(avion, papel, t_bc,
                      type = 'b', pch = 20, fixed = TRUE, xlab = 'Avión',
                      ylab = 'Tiempo de vuelo [s]',
                      las = 1, col = c('red', 'blue', 'black')))
grid()
mtext("Gráficos de interacción para cada factor",
      side = 3, line = -2.5, outer = TRUE, cex = 1.5, font = 4)

## 3.4. Tabla Anova =============================

modelo3 <- aov(t_bc ~ papel * avion, data = datos)
modelo3 %>% summary()

## 3.5. Validación de supuestos =================

### 3.5.1. Normalidad ============

#### Test paramétrico ====

# Test KS
datos$t_bc %>% shapiro.test()                  # No rechazo

# Test AD
datos$t_bc %>% ad.test()                       # No rechazo

# Test Lilliefors
datos$t_bc %>% lillie.test()                   # No rechazo

#### Gráfico cuantil cuantil ====

#Leyenda
leyenda3 <- c('Test de Shapiro-Wilk',
              expression(paste('W', ' = ', '0.976')),
              expression(paste('V'[p], ' = ', '0.071')))

# Gráfico cuantil-cuantil
par(mfrow = c(1, 1))
datos$t_bc %>% qqnorm(xlab = 'Cuantiles teóricos',
                       ylab = 'Cuantiles muestrales',
                       main = 'Gráfico cuantil-cuantil normal')
datos$t_bc %>% qqline()
grid()
legend('topleft', leyenda3, cex = 1.25)

### 3.5.2. Homocedasticidad ======

bartlett.test(t_bc ~ avion, data = datos)      # No rechazo
bartlett.test(t_bc ~ papel, data = datos)      # No rechazo

### 3.5.3. Independencia y media nula ====

durbinWatsonTest(modelo3)                       # No rechazo

plot(y = modelo3$residuals, x = 1:nrow(datos),
     main = 'Orden de observación contra residuales',
     xlab = 'Orden de observación',
     ylab = 'Residual',
     pch = as.numeric(datos$avion %>% as.factor()),
     col = as.factor(datos$papel))
abline(a = 0, b = 0, col = 'black', lty = 4, lwd = 3)
grid()
legend('topright',
       legend = c('Bond', 'Capuccino', 'Sahara', 'Cuaderno'),
       pch = rep(20, 4),
       col = c('green', 'black', 'red', 'blue'),
       title = 'Papel', cex = 0.75)
legend('topleft',
       legend = c('Stable', 'Hunting', 'Heavy-Nosed'),
       pch = as.numeric(datos$avion %>% as.factor()),
       title = 'Avión', cex = 0.75)

plot(x = modelo3$fitted.values, y = modelo3$residuals,
     main = 'Valores ajustados contra residuales',
     xlab = 'Valor ajustado',
     ylab = 'Residual',
     pch = as.numeric(datos$avion %>% as.factor()),
     col = as.factor(datos$papel),
     bg = as.factor(datos$Marca))
abline(a = 0, b = 0, col = 'black', lty = 4, lwd = 3)
grid()
legend('bottomright',
       legend = c('Bond', 'Capuccino', 'Sahara', 'Cuaderno'),
       pch = rep(20, 4),
       col = c('green', 'black', 'red', 'blue'),
       title = 'Papel', cex = 0.75)
legend('bottomleft',
       legend = c('Stable', 'Hunting', 'Heavy-Nosed'),
       pch = as.numeric(datos$avion %>% as.factor()),
       title = 'Avión', cex = 0.75)

# 3.6. Test de Tuckey ==========================

TukeyHSD(modelo3, which = 'avion')
TukeyHSD(modelo3, which = 'papel')

# 3.7. Test de Duncan ===========================

duncan_avion <- duncan.test(modelo, 'avion')
duncan_papel <- duncan.test(modelo, 'papel')

duncan_avion
duncan_papel

# 3.8. Test LSD =================================

lsd_avion <- LSD.test(modelo, 'avion')
lsd_papel <- LSD.test(modelo, 'papel')

lsd_avion
lsd_papel

