# Cargar base de datos
library(readxl)
ejemplo_screening <- read_excel("~/GitHub/metodo_tamizaje/ejemplo_screening.xlsx")
View(ejemplo_screening)

# Crear offset
ejemplo_screening$o <- with(ejemplo_screening, log(pob_vac / (1 - pob_vac)))

#Definir como variable categórica
ejemplo_screening$rango_etario <- as.factor(ejemplo_screening$rango_etario)
ejemplo_screening$semana <- as.factor(ejemplo_screening$semana)

# Modelo UCI estratificado por semana
m_uci <- glm( cbind(uci_vac, uci_no_vac) ~ offset(o) + semana, family=binomial,data=ejemplo_screening)
summary(m_uci)
VE_uci <- (1-exp(as.numeric(m_uci[["coefficients"]][["(Intercept)"]])))
VE_uci_ic <- sort(1-exp(confint(m_uci, "(Intercept)")))

# Modelo UCI estratificado por edad y semana
m_uci_edad <- glm( cbind(uci_vac, uci_no_vac) ~ offset(o) + rango_etario + semana, family=binomial,data=ejemplo_screening)
summary(m_uci_edad)
VE_uci_21_30 <- (1-exp(m_uci_edad[["coefficients"]][["(Intercept)"]] + m_uci_edad[["coefficients"]][["rango_etario21_30"]]))
VE_uci_31_40 <- (1-exp(m_uci_edad[["coefficients"]][["(Intercept)"]] + m_uci_edad[["coefficients"]][["rango_etario31_40"]]))
VE_uci_41_50 <- (1-exp(m_uci_edad[["coefficients"]][["(Intercept)"]] + m_uci_edad[["coefficients"]][["rango_etario41_50"]]))
VE_uci_51_60 <- (1-exp(m_uci_edad[["coefficients"]][["(Intercept)"]] + m_uci_edad[["coefficients"]][["rango_etario51_60"]]))
VE_uci_61_70 <- (1-exp(m_uci_edad[["coefficients"]][["(Intercept)"]] + m_uci_edad[["coefficients"]][["rango_etario61_70"]]))
VE_uci_71_80 <- (1-exp(m_uci_edad[["coefficients"]][["(Intercept)"]] + m_uci_edad[["coefficients"]][["rango_etario71_80"]]))
VE_uci_80_mas <- (1-exp(m_uci_edad[["coefficients"]][["(Intercept)"]] + m_uci_edad[["coefficients"]][["rango_etario80_mas"]]))

# Modelo defunciones estratificado por semana
m_def <- glm( cbind(def_vac, def_no_vac) ~ offset(o) + semana, family=binomial,data=ejemplo_screening)
summary(m_def)
VE_def <- (1-exp(as.numeric(m_def[["coefficients"]][["(Intercept)"]])))
VE_def_ic <- sort(1-exp(confint(m_def, "(Intercept)")))

# Modelo defunciones estratificado por edad y semana
m_def_edad <- glm( cbind(def_vac, def_no_vac) ~ offset(o) + rango_etario + semana, family=binomial,data=ejemplo_screening)
summary(m_def_edad)
VE_def_21_30 <- (1-exp(m_def_edad[["coefficients"]][["(Intercept)"]] + m_def_edad[["coefficients"]][["rango_etario21_30"]]))
VE_def_31_40 <- (1-exp(m_def_edad[["coefficients"]][["(Intercept)"]] + m_def_edad[["coefficients"]][["rango_etario31_40"]]))
VE_def_41_50 <- (1-exp(m_def_edad[["coefficients"]][["(Intercept)"]] + m_def_edad[["coefficients"]][["rango_etario41_50"]]))
VE_def_51_60 <- (1-exp(m_def_edad[["coefficients"]][["(Intercept)"]] + m_def_edad[["coefficients"]][["rango_etario51_60"]]))
VE_def_61_70 <- (1-exp(m_def_edad[["coefficients"]][["(Intercept)"]] + m_def_edad[["coefficients"]][["rango_etario61_70"]]))
VE_def_71_80 <- (1-exp(m_def_edad[["coefficients"]][["(Intercept)"]] + m_def_edad[["coefficients"]][["rango_etario71_80"]]))
VE_def_80_mas <- (1-exp(m_def_edad[["coefficients"]][["(Intercept)"]] + m_def_edad[["coefficients"]][["rango_etario80_mas"]]))

# Tabla de VE obtenida según método de tamizaje

VE_edad_sem <- data.frame(Edad = rep(c("21 a 30 años", "31 a 40 años", "41 a 50 años", "51 a 60 años", "61 a 70 años", "71 a 80 años"), times = 2),
                          Desenlace = rep(c("UCI", "Fallecer"), each = 6),
                          VE = c(VE_uci_21_30, VE_uci_31_40, VE_uci_41_50, VE_uci_51_60, VE_uci_61_70, VE_uci_71_80, VE_def_21_30, VE_def_31_40, VE_def_41_50, VE_def_51_60, VE_def_61_70, VE_def_71_80))

library(ggplot2)
ggplot(VE_edad_sem, aes(x = Edad, y = VE)) + 
  geom_point() +
  ggtitle ("VE ajustada por semana para cada grupo de edad") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limit = c(0, 1), expand = c(0,0)) +
  facet_wrap(~ Desenlace)
  
# Gráfico - comparación VE método de tamizaje con otros valores reportados

efectividad_comparada <- data.frame(Fuente = c("Método de tamizaje (ajustado por edad y semana)", "Coronavac - Ministerio de Salud (Agosto 2021)", "Coronavac - Jara et al, 2021 (ajustado por sexo y edad)"),
                                    Desenlace = as.factor(c("UCI", "UCI", "UCI", "Fallecer", "Fallecer", "Fallecer")),
                                    VE = c(VE_uci, 0.8968, 0.875, VE_def, 0.8638, 0.844),
                                    lb = c(VE_uci_ic[1], 0.891, 0.857, VE_def_ic[1], 0.8557, 0.823),
                                    up = c(VE_uci_ic[2], 0.9023, 0.89, VE_def_ic[2], 0.8715, 0.862))

ggplot(efectividad_comparada, aes(x = Fuente, y=VE)) + 
  geom_errorbar(aes(ymin=lb, ymax=up), width=.1) +
  geom_point() +
  scale_y_continuous(limit = c(0, 1), expand = c(0, 0)) +
  coord_flip() +
  facet_wrap(~ Desenlace)

# Comparación de VE entre semanas

ejemplo_screening_sem <- split(ejemplo_screening, ejemplo_screening$semana)
m_uci_31 <- glm(cbind(uci_vac, uci_no_vac) ~ offset(o), family=binomial,data=ejemplo_screening_sem[["31"]])
m_uci_32 <- glm(cbind(uci_vac, uci_no_vac) ~ offset(o), family=binomial,data=ejemplo_screening_sem[["32"]])
m_uci_33 <- glm(cbind(uci_vac, uci_no_vac) ~ offset(o), family=binomial,data=ejemplo_screening_sem[["33"]])
m_uci_34 <- glm(cbind(uci_vac, uci_no_vac) ~ offset(o), family=binomial,data=ejemplo_screening_sem[["34"]])
m_def_31 <- glm(cbind(def_vac, def_no_vac) ~ offset(o), family=binomial,data=ejemplo_screening_sem[["31"]])
m_def_32 <- glm(cbind(def_vac, def_no_vac) ~ offset(o), family=binomial,data=ejemplo_screening_sem[["32"]])
m_def_33 <- glm(cbind(def_vac, def_no_vac) ~ offset(o), family=binomial,data=ejemplo_screening_sem[["33"]])
m_def_34 <- glm(cbind(def_vac, def_no_vac) ~ offset(o), family=binomial,data=ejemplo_screening_sem[["34"]])

VE_uci_31 <- (1-exp(as.numeric(m_uci_31[["coefficients"]][["(Intercept)"]])))
VE_uci_31_ic <- sort(1-exp(confint(m_uci_31, "(Intercept)")))
VE_def_31 <- (1-exp(as.numeric(m_def_31[["coefficients"]][["(Intercept)"]])))
VE_def_31_ic <- sort(1-exp(confint(m_def_31, "(Intercept)")))
VE_uci_32 <- (1-exp(as.numeric(m_uci_32[["coefficients"]][["(Intercept)"]])))
VE_uci_32_ic <- sort(1-exp(confint(m_uci_32, "(Intercept)")))
VE_def_32 <- (1-exp(as.numeric(m_def_32[["coefficients"]][["(Intercept)"]])))
VE_def_32_ic <- sort(1-exp(confint(m_def_32, "(Intercept)")))
VE_uci_33 <- (1-exp(as.numeric(m_uci_33[["coefficients"]][["(Intercept)"]])))
VE_uci_33_ic <- sort(1-exp(confint(m_uci_33, "(Intercept)")))
VE_def_33 <- (1-exp(as.numeric(m_def_33[["coefficients"]][["(Intercept)"]])))
VE_def_33_ic <- sort(1-exp(confint(m_def_33, "(Intercept)")))
VE_uci_34 <- (1-exp(as.numeric(m_uci_33[["coefficients"]][["(Intercept)"]])))
VE_uci_34_ic <- sort(1-exp(confint(m_uci_33, "(Intercept)")))
VE_def_34 <- (1-exp(as.numeric(m_def_33[["coefficients"]][["(Intercept)"]])))
VE_def_34_ic <- sort(1-exp(confint(m_def_33, "(Intercept)")))

efectividad_semana <- data.frame(semana = rep(c("31", "32", "33", "34"), each = 2),
                                 Desenlace = rep(c("UCI", "Fallecer"), times = 4),
                                 VE = c(VE_uci_31, VE_def_31, VE_uci_32, VE_def_32, VE_uci_33, VE_def_33, VE_uci_34, VE_def_34),
                                 lb = c(VE_uci_31_ic[1], VE_def_31_ic[1], VE_uci_32_ic[1], VE_def_32_ic[1], VE_uci_33_ic[1], VE_def_33_ic[1], VE_uci_34_ic[1], VE_def_34_ic[1]),
                                 ub = c(VE_uci_31_ic[2], VE_def_31_ic[2], VE_uci_32_ic[2], VE_def_32_ic[2], VE_uci_33_ic[2], VE_def_33_ic[2], VE_uci_34_ic[2], VE_def_34_ic[2]))
efectividad_semana$Desenlace <- factor(efectividad_semana$Desenlace, levels = c("UCI", "Fallecer")) 

ggplot(efectividad_semana, aes(x = semana, y=VE)) + 
  geom_point() +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.1) +
  ggtitle ("VE ajustada por edad estimada por método de tamizaje para cada semana") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  scale_y_continuous(limit = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.2)) +
  facet_wrap(~ Desenlace)