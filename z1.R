#Белова Софья - для региона 58 Пензенская область
#Задание:рассчитайте урожайность пшеницы в 2002 году, 
#взяв для рассчета средние суммы активных температур за предыдущие 5 лет, 
#с метеостанций в радиусе не более 140 км


#проверка рабочей директории
getwd()
#Устанавливаем пакеты

library(tidyverse)
library(rnoaa)

#Скачивание  СПИСКА МЕТЕОСТАНЦИЙ 

#station_data = ghcnd_stations() 

write.csv(station_data,"station_data.csv")

station_data = read.csv("station_data.csv")

station_data

# ФОРМИРОВАНИЕ СПИСКА МЕТЕОСТАНЦИЙ
#После получения списка всех станций, выберем из него список станций ближайших к Пензе,
#создав таблицу с именем региона и координатами его столицы
#координаты должны быть в десятых градусов
penza = data.frame(id = "PENZA", latitude = 53.2006600,  longitude = 45.0046400)

#прочитаем справку команды meteo_nearby_stations
#? meteo_nearby_stations
#можно выбирать метеостанции в некотором фиксированном радиусе от Пензы 
#или конечное число станций, которые имеют необходимые данные
#в заданный временной период, и выбрать переменные, которые обязательно должны быть в наличии
penza_around = meteo_nearby_stations(lat_lon_df = penza, 
                                     station_data = station_data,
                                     limit = 6,
                                     radius = 140, var =  c("TAVG"),
                                     year_min = 1997, year_max = 2002)

#penza_around это список единственным элементом которого является таблица, 
#содержащая идентификаторы метеостанций, отсортиров-ых по их удаленности от Пензы 
#вспомним, как работать со списками
#1)очевидно что первым элементом таблицы будет
#идентификатор метеостанции Пензы, его то мы и попытаемся получить
penza_id = penza_around[["PENZA"]][["id"]][1]
summary (penza_id)

#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используйте #след. команду
all_penza_data=meteo_tidy_ghcnd(stationid=penza_id , var = "TAVG" )

#2)чтобы получить таблицу всех метеостанций вокруг Пензы нужно выбрать целиком первый объект из списка
penza_table = penza_around[[1]]
summary(penza_table)

#в таблице penza_table оказалось 14 объектов, ранжированных по расстоянию от Пензы
#нужно убедится, что этот список включает нужные по условию задачи метеостанции

penza_stations = penza_table 
str(penza_stations)

#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит
penza_stations$id

#Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()

#Создадим объект, куда скачаем все данные всех метеостанций
all_penza_meteodata = data_frame()
all_i = data_frame()

#Цикл для всех метеостанций
for(i in 1:6) 
{ 
  print(penza_table$id[i])
  print(i)
  #выберем нужные свойства 
  all_i = meteo_tidy_ghcnd(stationid = penza_table$id[i])
  all_i=all_i[,c("id","date","tavg")]
  
  #с помощью команды rbind соединяем данные, 
  #полученные на предыдущих и данном этапах цикла
  all_penza_meteodata = rbind(all_penza_meteodata, all_i)
}

#Записываем полученные результаты
write.csv ( all_penza_meteodata , " all_penza_meteodata.csv " )

#Cчитываем данные из файла all_penza_meteodata.csv
all_penza_meteodata = read.csv("all_penza_meteodata.csv")


#посмотрим на данные
str(all_penza_meteodata)

# Добавим год, месяц, день
all_penza_meteodata = mutate(all_penza_meteodata, year = year(date), 
                             month = month(date), day = day(date))
#проверим результат
str(all_penza_meteodata)

#отфильтруем данные за 1997 по 2002 год
years_penza_meteodata =filter(all_penza_meteodata, year %in% c(1997:2002))

#проверим результат
str(years_penza_meteodata)
summary(years_penza_meteodata)

#Превратим в нули все NA и где  5<tavg>30 

years_penza_meteodata [is.na(years_penza_meteodata$tavg), "tavg"] = 0
years_penza_meteodata [years_penza_meteodata$tavg<5, "tavg"] = 0
years_penza_meteodata [years_penza_meteodata$tavg>30, "tavg"] = 0

#проверяем, что температура получилась или 0, или больше 5 градусов
summary(years_penza_meteodata)

#Расчитаем суммарную температуру за месяц для всех станций 
#группируем по метеостанциям, годам и месяцам
#??group_by
alldays = group_by(years_penza_meteodata,id,year,month)

#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum
sumT_alldays_penza = summarize(alldays, tsum = sum(tavg))

#максимальная суммарная температура за месяц 656,4, следовательно,если поделить на 30 будет 21,88,что является приемлемым
summary(sumT_alldays_penza) 

#Сгруппируем данные по месяцам  
groups_penza_months = group_by(sumT_alldays_penza,month)
groups_penza_months

#найдем для всех метеостанций среднее по месяцам
sumT_months = summarize(groups_penza_months, St = mean(tsum))
sumT_months

###### Подготовка к расчету по формуле Урожая
### Ввод констант
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)

#константы выше взяты из первой таблицы

y1 = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;
Kf = 300 # - коэффициент использования ФАР посевом;
Qj = 1600 # - калорийность урожая культуры; 
Lj = 2.2 #  - сумма частей основной и побочной продукции; 
Ej = 25 # - стандартная влажность культуры; 

#Рассчитаем Fi по месяцам
#Fi= afi+bfi∗y∗(St>5℃)
sumT_months = mutate(sumT_months, Fi = afi+bfi*y1*St)

#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))

#Расчитываем урожай как сумму по месяцам и думаем разумный ли он
Yield = sum(sumT_months$Yi)  
Yield

#Ответ: 6.445806 ц/га
#По данным аналитики на  2013 года средняя урожайность в ЕАО составляла 6.445806 ц/га

