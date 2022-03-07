celsius <- function(far){
  celsius = 5*((far - 32) / 9)
  print(celsius)
}

celsius(53)

#############################
ROC <- roc(response = challenger$falha, 
           predictor = step_challenger$fitted.values)

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)


#############################################
#Exemplo 3:

modelo_preliminar <- glm(formula = fidelidade ~ idade,
                         data = dados_fidelidade,
                         family = "binomial") 
summary(modelo_preliminar)
ROC_preliminar <- roc(response = dados_fidelidade$fidelidade,
                      predictor = modelo_preliminar$fitted.values) 
ROC_preliminar

logLik(modelo_preliminar)
logLik(step_fidelidade_dummies)
lrtest(modelo_preliminar,step_fidelidade_dummies)

#Teste de DeLong (roc.test do pacote pROC - Elisabeth DeLong - Biometrics (1988)):
roc.test(ROC_preliminar,ROC)

plot(ROC_preliminar,col = "blue",lty = 2, main = "Comparação entre ROCs")
plot(ROC,col = "red", lty = 2, add = T)

#################
logLik(modelo_fidelidade_dummies)
logLik(step_fidelidade_dummies)

# Likelihood Ratio Test
lrtest(modelo_fidelidade_dummies, step_fidelidade_dummies)

chi2_lrtest <- 2*(logLik(modelo_fidelidade_dummies)-logLik(step_fidelidade_dummies)
)
chi2_lrtest

###############
table(dados_fidelidade$fidelidade)

#############################
#Exemplo 4:
modelo_errado <- lm(formula = atrasado ~ dist + sem,
                    data = AtrasadoMultinomial)
summary(modelo_errado)

AtrasadoMultinomial$atrasado <- as.numeric(AtrasadoMultinomial$atrasado)

modelo_errado <- lm(formula = atrasado ~ dist + sem,
                    data = AtrasadoMultinomial)
summary(modelo_errado)

modelo_errado2 <- glm(formula = atrasado ~ dist + sem,
                    data = AtrasadoMultinomial,
                    family = "gaussian")
summary(modelo_errado2)
logLik(modelo_errado2)


summ(modelo_atrasado)
