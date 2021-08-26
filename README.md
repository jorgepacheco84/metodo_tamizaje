
# Método de tamizaje

En este repositorio comparto datos y códigos para replicar la estimación de la efectividad de la vacunación usando el método de tamizaje.

El método de tamizaje permite estimar la efectividad de la vacunación (VE) en función de la cobertura de población vacunada [P(V)] y el porcentaje de población vacunada en la uci (o fallecido) utilizando la siguiente fórmula: VE = (P(V) - P(V|UCI))/(P(V)(1 - P(V|UCI))

Farrington (1993) propuso estimar la VE a través del método de tamizaje utilizando un modelo lineal generalizado binomial con función de enlace logit y utilizando un factor de compensación para el porcentaje de población vacunada [log(pob_vac / (1 - pob_vac)]. A diferencia del OR tradicional que utiliza un grupo de control, en este caso se realiza una estandarización externa.

En este repositorio aplico a este método a los datos entregados recientemente por Ministerio de Salud (https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto89/incidencia_en_vacunados_edad.csv) obtenido la VE ajustada por edad y semana epidemiológica. Se podría obtener una VE ajustada según otras covariables a través de la estratificación (por ejemplo, sexo x edad x tipo de vacuna x semana). Debido a esto último no debe considerarse la verdadera efectividad debido a que existe factores de confusión no medidos.

Bibliografía sobre el método:

Artículo de Farrington (1993): https://academic.oup.com/ije/article-abstract/22/4/742/664122?redirectedFrom=fulltext

Documento de la OMS sobre evaluación de efectividad de vacunación en variantes (2021): https://www.who.int/publications/i/item/WHO-2019-nCoV-vaccine_effectiveness-variants-2021.1

Código original desarrollado por Michael Höhle: https://github.com/hoehleatsu/STA427_SM4IDE/blob/main/Lectures/Lecture05/lecture05-handout.pdf
