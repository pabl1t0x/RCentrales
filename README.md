# RCentrales

Este paquete de R está diseñado para la materia de Centrales Hidroléctricas (CIV-339) que llevan en décimo semestre los estudiantes
de la Mención Hidráulica de la Carrera de Ingeniería Civil de la Universidad Mayor de San Andrés (UMSA), La Paz, Bolivia.

En la versión actual, el paquete incluye una función para calcular la geometría y condiciones hidráulicas de un túnel de sección
baúl (cuadrada y no cuadrada) funcionando a sección parcial. Asimismo, se incluye un tutorial acerca del diseño hidráulico de
canales abiertos.

## Instalación

Para instalar el paquete `RCentrales` utilizar:

``` r
# install.packages("remotes")
remotes::install_github("pabl1t0x/RCentrales")
```

Una vez instalado el paquete, el tutorial se corre con la siguiente función:

``` r
# install.packages("learnr")
learnr::run_tutorial("canales", package = "RCentrales")
```

## Licencia

Este paquete se distribuye bajo la licencia MIT License. Vea la [Licencia](/LICENSE.md) para más detalles.
