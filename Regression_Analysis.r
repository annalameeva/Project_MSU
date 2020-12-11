library('tidyverse')
library('memisc') # mtable function
library('stargazer')
library('lmtest') # Breusch-Pagan test
library('moments') # Jarque-Bera Normality Test, skewness, kurtosis
library('car') # vif
library('sandwich') # оценка Var для гетероскедастичности
library('jtools') # Beautiful summary for models
library('ggstance')
library('broom.mixed')
library('modelr')


data <- read.csv("~/Desktop/MSU HW Python/DATA_FINAL.csv")
View(data)
data$X <- NULL ## удаляем ненужный столбец

#data <- mutate(data, logprice = log(price))
#data <- mutate(data, logtotal_area = log(total_area))

# Сводные статистики по общему набору данных
summary(data)


# Гистограмма и плотность распределения для стоимости аренды квартиры
ggplot(data, aes(Цена)) + 
  geom_histogram(aes(y=..density..), fill = 'lightblue') + 
  geom_density(col = "darkred") +
  xlab('Стоимость квартиры') +
  ylab('Плотность распределения')

# Гистограмма и плотность распределения для логарифма стоимости аренды квартиры
ggplot(data, aes(log(Цена))) + 
  geom_histogram(aes(y=..density..), fill = 'lightblue') + 
  geom_density(col = "darkred") +
  xlab('Логарифм стоимости квартиры') +
  ylab('Плотность распределения')


# Диаграмма рассеяния
qplot(data = data, Цена, Общая, 
      xlab = 'Цена квартиры', ylab = 'Общая площадь')

# Диаграмма рассеяния и шестиугольники плотности
qplot(data = data, Цена, Общая) +
  geom_hex() +
  xlab('Стоимость квартиры') +
  ylab('Общая площадь')

# Диаграмма рассеяния и шестиугольники плотности
qplot(data = data, log(Цена), Общая) +
  geom_hex() +
  xlab('Логарифм стоимости квартиры') +
  ylab('Общая площадь')

# Ящичковые диаграммы + Проверим на наличие выбросов в данных
boxplot(data$Цена ~ data$Балкон.лоджия, col = 'lightblue', 
        xlab = 'Наличие балкона/лоджии в квартире', 
        ylab = 'Стоимость квартиры')
boxplot(data$Цена ~ data$Вторичка, col = 'lightblue', 
        xlab = 'Новостройка/ Вторичка',
        ylab = 'Стоимость квартиры')

# Pearson correlation
cor.test(data$Цена, data$Общая, method = 'pearson')

# Описательные статистики
stargazer(data[c("Цена","Общая","Жилая", "Кухня")], type = "text",
          title="Descriptive statistics/selected variables", digits=1, out="table2.txt")



##### МОДЕЛИРОВАНИЕ
# Посмотрим на модель без логарифмирования (все переменные, все 3 площади - это не совсем ок)
model0 <- lm(data = data, Цена ~.) 
summary(model0)
confint(model0)

# Далее логарифмируем
# Сначала логарифмируем только цену 
model1 <- lm(data = data, log(Цена) ~.)
summary(model1)
confint(model1)

# Теперь логарифмируем и цену, и площади 
model2 <- lm(data = data, log(Цена) ~ log(Общая) + log(Жилая) + Этаж + log(Высота.потолков) + Санузел + Балкон.лоджия +
               Комната2 + Комната3 + log(Кухня) + Ворошиловский + Дзержинский + Кировский + Красноармейский + Краснооктябрьский +
               Советский + Тракторозаводский + Вторичка)
summary(model2)
confint(model2)

## Сравнение моделей в таблице
mtable(model0, model1, model2)
stargazer(model0, model1, model2, type="html", out="models0_1_2.html") ## save

stargazer(model0, model1, model2, header=FALSE,
          title="Linear Models",
          keep.stat=c("rsq", "n"), omit.table.layout="n", type="html", out="modelsAIC.html",
          add.lines=list(c("AIC", round(AIC(model0),3), round(AIC(model1),3), round(AIC(model2),3))))


# Информационные критерии (Сравним также с нулевой моделью)
# Заметим, что модель без логагифмирования сильно проигрыввает на фоне других 
AIC(model0, model1, model2) # model 2 is better
BIC(model0, model1, model2) # model 2 is better - полностью логарифмы

# RESET Test - Тест Рамсея
resettest(model0) # p-value < 2.2e-16
resettest(model1) ## везде пропущено
resettest(model2) # p-value = 0.02373 -- на 1% уровне нет пропущенных


# Проверим нормальность остатков - Jarque-Bera Normality Test
jarque.test(model0$residuals) # Везде p-value < 2.2e-16
jarque.test(model1$residuals) 
jarque.test(model2$residuals)    

shapiro.test(model1$residuals) # p-value = 1.635e-13

# Проверим на мультиколлинеарность. Популярная граница для VIF — 10.
vif(model0)
vif(model1)
vif(model2)
## Тут ожидаемо площади показали громадные значения -- надо выкидывать

#vif_values <- vif(model0)
#vif_values <- vif(model1)
vif_values <- vif(model2)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 10
abline(v = 10, lwd = 3, lty = 2)


# Ridge regression -- один из способов для борьбы с мультиколлинеарностью
model_rr <- lm.ridge(data = data, log(Цена) ~ log(Общая) + log(Жилая) + Этаж + Высота.потолков + Санузел + Балкон.лоджия +
                       Комнат + log(Кухня) + Ворошиловский + Дзержинский + Кировский + Красноармейский + Краснооктябрьский +
                       Советский + Тракторозаводский + Вторичка)
model_rr$coef
# Другой способ - убрать лишние переменные (жилая площадь)



# Проверим на гетероскедостичность
# тест Бройша-Пагана Во вспомогательной регрессии квадраты остатков зависят от исходных регрессоров
## везде меньше 0,05 - значит есть гетеро
bptest(model0)

bptest(model1) ## studentized Breusch-Pagan test
bptest(model1,studentize=FALSE) ## Breusch-Pagan test
# gqtest(model1,fraction=0.2) ## Goldfeld-Quandt test нельзя, если нет нормальности остатков

bptest(model2)
bptest(model2,studentize=FALSE)

plot(model2$residuals, log(data$Цена), xlab = 'Остатки модели №2', ylab = 'Логарифм стомости квартиры', col = 'lightblue')


## оцениваем с гетероскедастичностью
coeftest(model0, vcov=vcovHC(model0, type = "HC3"))
coeftest(model1, vcov=vcovHC(model1, type = "HC3"))
coeftest(model2, vcov=vcovHC(model2, type = "HC3"))

stargazer(coeftest(model2, vcov=vcovHC(model2, type = "HC3")), type="html", out="model2HC.html")


### Удаляем жилую площадь
model3 <- lm(data = data, log(Цена) ~ log(Общая) + Этаж + log(Высота.потолков) + Санузел + Балкон.лоджия +
               Комната2 + Комната3 + log(Кухня) + Ворошиловский + Дзержинский + Кировский + Красноармейский + Краснооктябрьский +
               Советский + Тракторозаводский + Вторичка)
summary(model3)
confint(model3)

AIC(model3)
BIC(model3)

waldtest(model2, model3)  # 0.9243 - регрессоры действительно можно выбросить


vif(model3)

vif_values <- vif(model3)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue", 
        names.arg = c('log(Общая)', 'Этаж', 'log(Выс пот)', 'Санузел', 'Балкон', 
                      'Комн2', 'Комн3', 'log(Кухня)', 'Ворош р-н', 'Дзерж р-н', 
                      'Кир р-н', 'Красн р-н', 'Красноокт р-н', 'Сов р-н', 'Тракт р-н', 
                      'Вторичка'))
abline(v = 10, lwd = 3, lty = 2)


##### ИТОГО:
summ(model3, confint = TRUE, vifs = TRUE)
summ(model3, robust = "HC3", confint = TRUE) # Robust standard errors
summ(model3, robust = "HC3", confint = TRUE, vifs = TRUE)
AIC(model3)
BIC(model3)

plot_summs(model3) # the confidence interval is .95
#plot_summs(model3, inner_ci_level = .9) # the confidence interval is .9

# Different Robust standard errors
plot_summs(model3, model3, model3, robust = list(FALSE, "HC0", "HC3"),
           model.names = c("OLS", "HC0", "HC3"))


plot_summs(model3, plot.distributions = TRUE, rescale.distributions = TRUE) # with rescaled distributions

# to compare models
plot_summs(model2, model3)
plot_summs(model1,model2, model3)

plot_summs(model2, model3, plot.distributions = TRUE, rescale.distributions = TRUE) 


##
export_summs(model2, model3, digits = 5, robust = "HC3", vifs = TRUE,
             error_format = "[{conf.low}, {conf.high}]", to.file = "docx", file.name = "model2_3.docx")


# Metrics for model 3 -- AIC, BIC
data.frame(
  R2 = rsquare(model3, data = data),
  RMSE = rmse(model3, data = data),
  MAE = mae(model3, data = data),
  AIC = AIC(model3),
  BIC = BIC(model3)
)

#library('broom')
#glance(model3)

stargazer(model0, model1, model2, model3, type="text", header=FALSE,
          title="Linear Models",
          keep.stat=c("rsq", "n"), omit.table.layout="n",
          add.lines=list(c("AIC", round(AIC(model0),3), round(AIC(model1),3), round(AIC(model2),3), round(AIC(model3),3))))

# без модели0
cov3         <- vcovHC(model3, type = "HC3")
robust_se    <- sqrt(diag(cov3))

stargazer(model1, model2, model3, type="text", header=FALSE,
          title="Linear Models",
          keep.stat=c("rsq", "n"), omit.table.layout="n",
          se = list(NULL, NULL, NULL, robust_se),
          add.lines=list(c("AIC", round(AIC(model1),3), round(AIC(model2),3), round(AIC(model3),3))))

# With robust errors
stargazer(model3, model3, type="text", header=FALSE,
          title="Linear Models",
          keep.stat=c("rsq", "n", 'aic'), omit.table.layout="n",
          se = list(NULL, robust_se))




