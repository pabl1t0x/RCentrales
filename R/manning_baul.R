# RCentrales: Herramientas en R para el Diseño de Canales y Túneles en Centrales Hidroeléctricas
# Copyright (C) 2026  Pablo Fuchs

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Cálculo hidráulico de un túnel de sección baúl cuadrada o no cuadrada
#'
#' Esta herramienta calcula la geometría y condiciones hidráulicas de un túnel de sección
#' baúl funcionando a sección parcial.
#'
#' @param Q numeric. Caudal en m3/s
#' @param K numeric. Relación tirante/radio
#' @param n numeric. Coeficiente de rugosidad de Manning
#' @param So numeric. Pendiente del túnel en m/m
#' @param x numeric. Relación altura/radio
#' @param seccion character. Tipo de sección (cuadrada o no_cuadrada)
#'
#' @section Notes:
#' Para `seccion="cuadrada"` no es necesario especificar `x`. Por el contrario,
#' este parámetro debe ser especificado si `seccion="no_cuadrada`.
#'
#' @returns La función genera una lista con  los siguientes parámetros:
#'
#'   * `radio`: radio de la sección semicircular del túnel tipo baúl.
#'   * `perim`: perímetro mojado.
#'   * `area`: área mojada.
#'   * `radio_h`: radio hidráulico.
#'   * `tirante`: tirante.
#'   * `velocidad`: velocidad del flujo.
#'   * `altura`: altura total del túnel.
#'   * `ancho`: ancho del túnel.
#'   * `BL`: altura de túnel no cubierta por agua.
#'   * `area_exc`: área de excavación del túnel.
#'
#' @examples
#' # Túnel baúl cuadrado
#' Q <- 3
#' So <- 0.0004
#' n <- 0.04
#' K <- 1.5
#' manning_baul(Q = Q, K = K, n = n, So = So, seccion = "cuadrada")
#' # Túnel baúl no cuadrado
#' Q <- 3
#' So <- 0.0004
#' n <- 0.04
#' K <- 3.2
#' x <- 2.7
#' manning_baul(Q = Q, K = K, n = n, So = So, x = x, seccion = "no_cuadrada")
#'
#' @export
manning_baul <- function(Q, K, n, So, x = NULL, seccion = c("cuadrada", "no_cuadrada")) {
    if (length(seccion) != 1) {
        stop("Especifique el tipo de sección (cuadrada o no_cuadrada).")
    }
    if (seccion == "cuadrada") {
        if(K < 1) {
            stop("K debe ser mayor o igual a 1.")
        }
        theta_rad <- asin(K - 1)
        theta_grad <- theta_rad * 180 / pi
        K1 <- 3.2146 + 0.0349 * theta_grad
        K2 <- 1.973 + 0.5 * sin(2 * theta_rad) + 0.0175 * theta_grad
        r <- ((Q * n * K1 ^ (2/3)) / (K2 ^ (5/3) * So ^ (1/2))) ^ (3/8) # radio de la bóveda
        P <- r * K1 # perímetro mojado
        A <- r ^ 2 * K2 # área mojada
        R <- A / P # radio hidráulico
        y <- r * (1 + sin(theta_rad)) # tirante
        V <- Q / A # velocidad
        H <- 2 * r # altura total del túnel
        b <- 2 * r # ancho del túnel
        alt <- 2 * r - y # altura de franco
        A_exc <- 3.544 * r ^ 2 # área de excavación
        res <- list(
            radio = r,
            perim = P,
            area = A,
            radio_h = R,
            tirante = y,
            velocidad = V,
            altura = H,
            ancho = b,
            BL = alt,
            area_exc = A_exc)
        return(res)
    } else if(seccion == "no_cuadrada") {
        if(K < 1) {
            stop("K debe ser mayor o igual a 1.")
        }
        if(missing(x)) {
            stop("Por favor introduzca un valor de x.")
        }
        if(x < 1) {
            stop("x debe ser mayor o igual a 1.")
        }
        theta_rad <- asin(K - x)
        theta_grad <- theta_rad * 180 / pi
        K1 <- 1.2146 + 0.0349 * theta_grad
        K2 <- 0.5 * sin(2 * theta_rad) -0.027 + 0.0175 * theta_grad
        r <- ((Q * n * (K1 + 2 * x) ^ (2/3)) / ((K2 + 2 * x) ^ (5/3) * So ^ (1/2))) ^ (3/8) # radio de la bóveda
        h <- x * r
        P <- 2 * h + r * K1 # perímetro mojado
        A <- 2 * r * h + r^ 2 * K2 # área mojada
        R <- A / P # radio hidráulico
        y <- r * (h + r * sin(theta_rad)) # tirante
        V <- Q / A # velocidad
        H <- h * r # altura total del túnel
        b <- 2 * r # ancho del túnel
        alt <- r + h - y # altura de franco
        A_exc <- 2 * r * h + 1.544 * r ^ 2 # área de excavación
        res <- list(
            radio = r,
            perim = P,
            area = A,
            radio_h = R,
            tirante = y,
            velocidad = V,
            altura = H,
            ancho = b,
            BL = alt,
            area_exc = A_exc)
        return(res)
    } else {
        stop("Sección incorrecta. Por favor especifique el tipo de sección.")
    }
}
