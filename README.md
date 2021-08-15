
# metodo_tamizaje

En este repositorio comparto datos y códigos para replicar la estimación de la efectividad de la vacunación usando el método de tamizaje.

El método de tamizaje permite estimar la efectividad de la vacunación (VE) en función de la cobertura de población vacunada [P(V)] y el porcentaje de población vacunada en la uci (o fallecido) utilizando la siguiente fórmula: VE = (P(V) - P(V|UCI))/(P(V)(1 - P(V|UCI))

Farrington (1993) propuso estimar la VE a través del método de tamizaje utilizando un modelo lineal generalizado binomial con función de enlace logit y utilizando un factor de compensación para el porcentaje de población vacunada [log(pob_vac / (1 - pob_vac)].

En este repositorio aplico a este método a los datos entregados recientemente por Ministerio de Salud (https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto89/incidencia_en_vacunados_edad.csv) obtenido la VE ajustada por edad. 

ATENCIÓN: Esto no debe considerarse la verdadera efectividad de la vacunación debido a que existen factores de confusión no medidos.

Artículo de Farrington (1993): https://academic.oup.com/ije/article-abstract/22/4/742/664122?redirectedFrom=fulltext
