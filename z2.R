#Белова Софья 121 группа
#Зфдфние 2.

#проверка рабочей директории
getwd()

#создать модель множественной линейной регресии дневные потоки паров воды за период 2013 года по данным измерений методом турбулентной пульсации

library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")

#считываем файл
eddypro = read.csv("eddypro.csv", skip = 1, na = c ("","NA","-9999","-9999.0"), comment = c("["))

#готовим данные
# Удаляем ненужную пустую первую строку
eddypro = eddypro [-1,]

# Удаляем ненужный пустой столбец "roll"
eddypro = select(eddypro,-(roll))

# Преобразуем в факторы (factor) столбы типа char(символ)
eddypro = eddypro %>% mutate_if(is.character,factor)

#Заменим специальные символы в названии стобцов на допустимые для переменных имена
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")

#Возвратим столбцы таблицы в виде векторов для проверки,посмотрим
glimpse(eddypro)

#Удалим строки в которых содержится NA
eddypro = drop_na(eddypro)

# Отфильтруем по заданию данные только за летний период. 
eddypro = filter(eddypro,DOY >= 151 & DOY < 242)

# Отфильтруем данные по заданию только за дневное время
eddypro = filter(eddypro, daytime ==TRUE)

# Получим таблицу, состоящую только из чисел для работы с ней
eddypro_numeric = eddypro[,sapply(eddypro, is.numeric)]

# Получим таблицу, содержащую остальные колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro, is.numeric)]

# Создадим обучающую и тестирующую непересекающиеся выборки с помощью базового функционала
row_numbers = 1:length(eddypro_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
test = row_numbers[-teach]

#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]

#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]

#Модель 1 по обучающей выборке
mod1 = lm(h2o_flux ~ (.), data = testing_tbl)

#Информация 
summary(mod1)

#Коэффициенты 
coef(mod1)

#Остатки
resid(mod1)

#Доверительный интервал
confint(mod1)

#Дисперсионный анализ модели
anova(mod1)

#Графическое представление модели
plot(mod1)

# МОДЕЛЬ 2
mod2= lm ( h2o_flux~ DOY + file_records + Tau+qc_Tau + rand_err_Tau + H +qc_H 
            + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux + qc_h2o_flux
            + rand_err_co2_flux + rand_err_h2o_flux + H_strg 
            + co2_molar_density + co2_mole_fraction + co2_mixing_ratio 
            + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag 
            + sonic_temperature + air_temperature + air_pressure + air_density 
            + air_heat_capacity + air_molar_volume + water_vapor_density + e + es 
            + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot 
            + v_rot + w_rot + max_speed + yaw + pitch + TKE + L + bowen_ratio 
            + x_peak + x_offset  + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux 
            + w_spikes + ts_spikes + mean_value + v_var + ts_var + h2o_var 
            + co2_signal_strength_7200 + h2o_signal_strength_7200, data = teaching_tbl)
mod2

coef(mod2)
resid(mod2)
confint(mod2)
summary(mod2)
anova(mod2)
anova(mod2,mod1)
plot(mod2) 


# МОДЕЛЬ 3
mod3 = lm ( h2o_flux~ DOY + file_records + Tau+qc_Tau + rand_err_Tau + H +qc_H 
            + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux + qc_h2o_flux
            + rand_err_co2_flux + rand_err_h2o_flux + H_strg 
            + co2_molar_density + co2_mole_fraction + co2_mixing_ratio 
            + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag 
            + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot 
            + v_rot + w_rot + max_speed + yaw + pitch + TKE + L + bowen_ratio 
            + x_peak + x_offset  + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux 
            + w_spikes + ts_spikes + mean_value + v_var + ts_var + h2o_var 
            + co2_signal_strength_7200 + h2o_signal_strength_7200, data = teaching_tbl)

coef(mod3)
resid(mod3)
confint(mod3)
summary(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3)


# МОДЕЛЬ 4
mod4 = lm ( h2o_flux~ DOY + file_records + Tau+qc_Tau + rand_err_Tau + H +qc_H 
            + rand_err_H + LE + qc_LE + rand_err_LE + co2_flux + qc_h2o_flux
            + rand_err_co2_flux + rand_err_h2o_flux + H_strg 
            + co2_molar_density + co2_mole_fraction + co2_mixing_ratio 
            + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag 
            + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot 
            + x_peak + x_offset  + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux 
            + w_spikes + ts_spikes + mean_value + v_var + ts_var + h2o_var 
            + co2_signal_strength_7200 + h2o_signal_strength_7200, data = teaching_tbl)

coef(mod4)
resid(mod4)
confint(mod4)
summary(mod4)
anova(mod4)
anova(mod4, mod3)
plot(mod4)

#Корреляционный анализ переменных участвующих в линейной модели
cor_teaching_tbl = select(teaching_tbl,DOY,file_records,Tau, qc_Tau,rand_err_Tau,H, qc_H, 
                          rand_err_H, LE, qc_LE, rand_err_LE,co2_flux,qc_h2o_flux,
                          rand_err_co2_flux,rand_err_h2o_flux,H_strg,
                          co2_molar_density, co2_mole_fraction, co2_mixing_ratio, 
                          h2o_molar_density, h2o_mole_fraction, h2o_mixing_ratio, h2o_time_lag, 
                          specific_humidity, RH, VPD, Tdew, u_unrot, v_unrot, w_unrot, u_rot, 
                          x_peak, x_offset, un_Tau, Tau_scf,un_H, H_scf, un_LE, LE_scf,un_co2_flux,un_h2o_flux, 
                          w_spikes, ts_spikes, mean_value, v_var,ts_var,h2o_var,
                          co2_signal_strength_7200,h2o_signal_strength_7200)

#Получение таблицы коэффициентов корреляций
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

#Построение графиков по полученной моделе
#Построение точек по значениями обучающей выборки и наложение предсказанных значений по 4 модели
qplot(h2o_flux , h2o_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))

#Построение точек по значением тестирующей выборки и наложение предсказанных значений по 4 модели
qplot(h2o_flux , h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod4, testing_tbl)))

#Примеры
qplot(DOY, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod4, testing_tbl)))
qplot(Tau, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod4, testing_tbl)))
qplot(co2_flux, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod4, testing_tbl)))

