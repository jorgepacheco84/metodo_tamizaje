# Cargar base de datos
library(readxl)
ejemplo <- read_excel("~/GitHub/metodo_tamizaje/ejemplo_long.xlsx")
View(ejemplo)

#Definir variables categóricas
ejemplo$edad <- as.factor(ejemplo$edad)
ejemplo$semana <- as.factor(ejemplo$semana)
ejemplo$desenlace <- as.factor(ejemplo$desenlace)
ejemplo$dosis <- as.factor(ejemplo$dosis)

#Dividir base de datos
ejemplo_desenlace <- split(ejemplo, ejemplo$desenlace)

# Modelo de Poisson para caso
RTI_caso_ajustada <- glm(freq ~ offset(log(poblacion)) + dosis + edad + semana, family = "poisson", data = ejemplo_desenlace[["caso"]])
summary(RTI_caso_ajustada)
VE_caso_2 <- (1-exp(as.numeric(RTI_caso_ajustada[["coefficients"]][["dosis2"]])))
VE_caso_3 <- (1-exp(as.numeric(RTI_caso_ajustada[["coefficients"]][["dosis3"]])))
VE_caso_ic <- 1-exp(confint(RTI_caso_ajustada))
                    
# Modelo de Poisson para uci
RTI_uci_ajustada <- glm(freq ~ offset(log(poblacion)) + dosis + edad + semana, family = "poisson", data = ejemplo_desenlace[["uci"]])
summary(RTI_uci_ajustada)
VE_uci_2 <- (1-exp(as.numeric(RTI_uci_ajustada[["coefficients"]][["dosis2"]])))
VE_uci_3 <- (1-exp(as.numeric(RTI_uci_ajustada[["coefficients"]][["dosis3"]])))
VE_uci_ic <- 1-exp(confint(RTI_uci_ajustada))

# Modelo de Poisson para defunciones
RTI_def_ajustada <- glm(freq ~ offset(log(poblacion)) + dosis + edad + semana, family = "poisson", data = ejemplo_desenlace[["def"]])
summary(RTI_def_ajustada)
VE_def_2 <- (1-exp(as.numeric(RTI_def_ajustada[["coefficients"]][["dosis2"]])))
VE_def_3 <- (1-exp(as.numeric(RTI_def_ajustada[["coefficients"]][["dosis3"]])))
VE_def_ic <- 1-exp(confint(RTI_def_ajustada))

# Modelo de binomial negativo para caso
library(MASS)
RTI_caso_ajustada_nb <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + semana, data = ejemplo_desenlace[["caso"]])
summary(RTI_caso_ajustada_nb)
VE_caso_nb_2 <- (1-exp(as.numeric(RTI_caso_ajustada_nb[["coefficients"]][["dosis2"]])))
VE_caso_nb_3 <- (1-exp(as.numeric(RTI_caso_ajustada_nb[["coefficients"]][["dosis3"]])))
VE_caso_ic_nb <- 1-exp(confint(RTI_caso_ajustada_nb))

# Modelo binomial negativo para uci
RTI_uci_ajustada_nb <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + semana, data = ejemplo_desenlace[["uci"]])
summary(RTI_uci_ajustada_nb)
VE_uci_nb_2 <- (1-exp(as.numeric(RTI_uci_ajustada_nb[["coefficients"]][["dosis2"]])))
VE_uci_nb_3 <- (1-exp(as.numeric(RTI_uci_ajustada_nb[["coefficients"]][["dosis3"]])))
VE_uci_ic_nb <- 1-exp(confint(RTI_uci_ajustada_nb))

# Modelo binomial negativo para defunciones
RTI_def_ajustada_nb <- glm.nb(freq ~ offset(log(poblacion)) + dosis + edad + semana, data = ejemplo_desenlace[["def"]])
summary(RTI_def_ajustada_nb)
VE_def_nb_2 <- (1-exp(as.numeric(RTI_def_ajustada_nb[["coefficients"]][["dosis2"]])))
VE_def_nb_3 <- (1-exp(as.numeric(RTI_def_ajustada_nb[["coefficients"]][["dosis3"]])))
VE_def_ic_nb <- 1-exp(confint(RTI_def_ajustada_nb))

# Comparación de ajuste entre modelos para caso
logLik(RTI_caso_ajustada)
logLik(RTI_caso_ajustada_nb)
pchisq(2 * (logLik(RTI_caso_ajustada_nb) - logLik(RTI_caso_ajustada)), df = 1, lower.tail = FALSE)

# Comparación de ajuste entre modelos para uci
logLik(RTI_uci_ajustada)
logLik(RTI_uci_ajustada_nb)
pchisq(2 * (logLik(RTI_uci_ajustada_nb) - logLik(RTI_uci_ajustada)), df = 1, lower.tail = FALSE)

# Comparación de ajuste entre modelos para defunciones
logLik(RTI_def_ajustada)
logLik(RTI_def_ajustada_nb)
pchisq(2 * (logLik(RTI_def_ajustada_nb) - logLik(RTI_def_ajustada)), df = 1, lower.tail = FALSE)

# Tabla de efectividad
efectividad <- data.frame(Esquema = rep(c("Dos dosis", "Tres dosis"), times = 3),
                          Desenlace = as.factor(c("Caso", "Caso", "UCI", "UCI", "Fallecer", "Fallecer")),
                          Efectividad = c(VE_caso_nb_2, VE_caso_nb_3, VE_uci_nb_2, VE_uci_nb_3, VE_def_nb_2, VE_def_nb_3),
                          lb = c(VE_caso_ic_nb[2,2], VE_caso_ic_nb[3,2], VE_uci_ic_nb[2,2], VE_uci_ic_nb[3,2], VE_def_ic_nb[2,2], VE_def_ic_nb[3,2]),
                          ub = c(VE_caso_ic_nb[2,1], VE_caso_ic_nb[3,1], VE_uci_ic_nb[2,1], VE_uci_ic_nb[3,1], VE_def_ic_nb[2,1], VE_def_ic_nb[3,1]))
efectividad$Desenlace <- factor(efectividad$Desenlace, levels = c("Caso", "UCI", "Fallecer")) 

library(ggplot2)

g.efectividad <- ggplot(efectividad, aes(x = Esquema, y=Efectividad)) + 
  geom_errorbar(aes(ymin=lb, ymax=ub, color=Esquema), width=.1) +
  ggtitle ("Efectividad de la vacunación para esquema de dos y tres dosis ") +
  geom_point(aes(fill=Esquema),size=2, shape=23) +
  theme(text=element_text(size=20)) +
  theme(axis.text=element_text(size=20)) +
  theme(axis.title=element_text(size=20)) +
  theme(plot.title=element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limit = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.2), labels = scales::percent) +
  facet_wrap(~Desenlace)                                    

ggsave(plot = g.efectividad,
       filename = "/Users/Usuario/Desktop/efectividad ajustada.png",
       device = "png",
       dpi = "retina",
       width = 14, height = 12)

library(effects)
plot(predictorEffects(RTI_caso_ajustada_nb, ~ dosis), main = "Casos", axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(predictorEffects(RTI_uci_ajustada_nb, ~ dosis), main = "UCI", axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
plot(predictorEffects(RTI_def_ajustada_nb, ~ dosis), main = "Defunciones", axes=list(grid=FALSE,  y=list(transform=exp, lab="N° de casos (escala aritmética)")))
