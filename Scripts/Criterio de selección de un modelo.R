#Criterio de selección de un modelo

cmama2$diagnostico <- factor(cmama2$diagnostico,
                          levels = c("B", "M"),
                          labels = c(0, 1))

diagnostico_reg <-
  glm(
    diagnostico ~ radio_medio + perimetro_medio + simetria_media,
    family = binomial(link = "logit"),
    cmama2
  )

diagnostico1 <-
  glm(
    diagnostico ~ radio_medio,
    family = binomial(link = "logit"),
    cmama2
  )

diagnostico2 <-
  glm(
    diagnostico ~ perimetro_medio,
    family = binomial(link = "logit"),
    cmama2
  )

diagnostico3 <-
  glm(
    diagnostico ~ simetria_media,
    family = binomial(link = "logit"),
    cmama2
  )

diagnostico4 <-
  glm(
    diagnostico ~ radio_medio + perimetro_medio,
    family = binomial(link = "logit"),
    cmama2
  )

diagnostico5 <-
  glm(
    diagnostico ~ radio_medio + simetria_media,
    family = binomial(link = "logit"),
    cmama2
  )

diagnostico6 <-
  glm(
    diagnostico ~ perimetro_medio + simetria_media,
    family = binomial(link = "logit"),
    cmama2
  )

stepwise(diagnostico_reg, criterion = 'AIC')