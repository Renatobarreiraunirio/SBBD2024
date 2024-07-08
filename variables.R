# Definição das variáveis fuzzy
library(sets)


setVariables <- function() {
  set(
    laicidade = fuzzy_variable(
      baixo = fuzzy_trapezoid(corners = c(-2, -1, 0, 1)),
      medio = fuzzy_cone(center = 2, radius = 1, height = 1),
      alto = fuzzy_trapezoid(corners = c(3,4.1,90,100))
    ),
    grandemidia = fuzzy_variable(
      baixo = fuzzy_trapezoid(corners = c(-2, -1, 0, 1)),
      medio = fuzzy_cone(center = 2, radius = 1, height = 1),
      alto = fuzzy_trapezoid(corners = c(3,4,90,100))
    ),
    instituicoes = fuzzy_variable(
      baixo = fuzzy_trapezoid(corners = c(-2, -1, 0, 1)),
      medio = fuzzy_cone(center = 2, radius = 1, height = 1),
      alto = fuzzy_trapezoid(corners = c(3,3.9,90,100))
    ),
    populismo = fuzzy_variable(
      nulo = fuzzy_trapezoid(corners = c(-2, -1, 0, 1.5)),
      muitobaixo = fuzzy_cone(center = 2, radius = 0.5, height = 1),
      baixo = fuzzy_cone(center = 3, radius = 0.5, height = 1),
      medio = fuzzy_cone(center = 4, radius = 0.5, height = 1),
      alto = fuzzy_cone(center = 5, radius = 0.5, height = 1),
      muitoalto = fuzzy_cone(center = 6, radius = 0.5, height = 1),
      extremo = fuzzy_trapezoid(corners = c(6.5,8,90,100))
    )
  )
}


