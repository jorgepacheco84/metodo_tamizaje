# Crear gráfico

x <- c(seq(from = 0, to = 1, by = 0.01))
VE50 <- c(((1-0.5)*x)/(1-(0.5*x)))
VE60 <- c(((1-0.6)*x)/(1-(0.6*x)))
VE70 <- c(((1-0.7)*x)/(1-(0.7*x)))
VE80 <- c(((1-0.8)*x)/(1-(0.8*x)))
VE90 <- c(((1-0.9)*x)/(1-(0.9*x)))
tabla <- data.frame(x, VE50, VE60, VE70, VE80, VE90)
library(reshape2)
tabla.long <- melt(tabla, id = "x", measure = c("VE50", "VE60", "VE70", "VE80", "VE90"), value.name = "Porcentaje")
tabla.long['uci_vacunado'] <- NA
tabla.long$uci_vacunado[71] <- 0.567
tabla.long['fallecido_vacunado'] <- NA
tabla.long$fallecido_vacunado[71] <- 0.527

library(ggplot2)
ggplot(data = tabla.long, aes(x = x, y = Porcentaje, group = variable, colour = variable)) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Cobertura de vacunación P(V)", y = "Porcentaje de vacunados en la UCI P(V|UCI)", title = "Porcentaje de vacunados en los casos según cobertura de vacunación", colour = "Efectividad") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  geom_point(aes(x, uci_vacunado), colour = 'orangered')

ggplot(data = tabla.long, aes(x = x, y = Porcentaje, group = variable, colour = variable)) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Cobertura de vacunación P(V)", y = "Porcentaje de vacunados fallecidos P(V|fallecido)", title = "Porcentaje de vacunados en los casos según cobertura de vacunación", colour = "Efectividad") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  geom_point(aes(x, fallecido_vacunado), colour = 'orangered')