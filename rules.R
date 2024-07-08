# Definição das regras fuzzy
library(sets)

setRules <- function() {
  set(
    fuzzy_rule(instituicoes %is% alto && laicidade %is% alto && grandemidia %is% alto, populismo %is% extremo),
    fuzzy_rule(instituicoes %is% medio && laicidade %is% alto && grandemidia %is% alto, populismo %is% muitoalto),
      fuzzy_rule(instituicoes %is% alto && laicidade %is% medio && grandemidia %is% alto, populismo %is% muitoalto),
      fuzzy_rule(instituicoes %is% alto && laicidade %is% alto && grandemidia %is% medio, populismo %is% muitoalto),
      fuzzy_rule(instituicoes %is% baixo && laicidade %is% alto && grandemidia %is% alto, populismo %is% muitoalto),
      fuzzy_rule(instituicoes %is% alto && laicidade %is% baixo && grandemidia %is% alto, populismo %is% muitoalto),
      fuzzy_rule(instituicoes %is% alto && laicidade %is% alto && grandemidia %is% baixo, populismo %is% muitoalto),
      fuzzy_rule(instituicoes %is% alto && laicidade %is% alto && grandemidia %is% baixo, populismo %is% muitoalto),
      fuzzy_rule(instituicoes %is% medio && laicidade %is% medio && grandemidia %is% alto, populismo %is% alto),
      fuzzy_rule(instituicoes %is% medio && laicidade %is% alto && grandemidia %is% medio, populismo %is% alto),
      fuzzy_rule(instituicoes %is% alto && laicidade %is% medio && grandemidia %is% medio, populismo %is% alto),
      fuzzy_rule(instituicoes %is% baixo && laicidade %is% medio && grandemidia %is% alto, populismo %is% alto),
      fuzzy_rule(instituicoes %is% baixo && laicidade %is% alto && grandemidia %is% medio, populismo %is% alto),
      fuzzy_rule(instituicoes %is% alto && laicidade %is% baixo && grandemidia %is% medio, populismo %is% alto),
      fuzzy_rule(instituicoes %is% alto && laicidade %is% medio && grandemidia %is% baixo, populismo %is% alto),
      fuzzy_rule(instituicoes %is% medio && laicidade %is% alto && grandemidia %is% baixo, populismo %is% alto),
      fuzzy_rule(instituicoes %is% medio && laicidade %is% baixo && grandemidia %is% alto, populismo %is% alto),
      fuzzy_rule(instituicoes %is% medio && laicidade %is% medio && grandemidia %is% medio, populismo %is% medio),
      fuzzy_rule(instituicoes %is% baixo && laicidade %is% medio && grandemidia %is% medio, populismo %is% baixo),
      fuzzy_rule(instituicoes %is% medio && laicidade %is% baixo && grandemidia %is% medio, populismo %is% baixo),
      fuzzy_rule(instituicoes %is% medio && laicidade %is% medio && grandemidia %is% baixo, populismo %is% baixo),
      fuzzy_rule(instituicoes %is% baixo && laicidade %is% baixo && grandemidia %is% alto, populismo %is% alto),
      fuzzy_rule(instituicoes %is% baixo && laicidade %is% alto && grandemidia %is% baixo, populismo %is% alto),
      fuzzy_rule(instituicoes %is% alto && laicidade %is% baixo && grandemidia %is% baixo, populismo %is% alto),
      fuzzy_rule(instituicoes %is% baixo && laicidade %is% baixo && grandemidia %is% medio, populismo %is% muitobaixo),
      fuzzy_rule(instituicoes %is% baixo && laicidade %is% medio && grandemidia %is% baixo, populismo %is% muitobaixo),
      fuzzy_rule(instituicoes %is% medio && laicidade %is% baixo && grandemidia %is% baixo, populismo %is% muitobaixo),
      fuzzy_rule(instituicoes %is% medio && laicidade %is% baixo && grandemidia %is% baixo, populismo %is% muitobaixo),
      fuzzy_rule(instituicoes %is% baixo && laicidade %is% baixo && grandemidia %is% baixo, populismo %is% nulo)
  )
}