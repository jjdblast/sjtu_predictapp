source('preprocess-data.R')

id = '0001'

datePattern = '%Y%m%d'
data1 = data[data$id == id 
   & data$time >= strptime('20141103', datePattern) 
   & data$time < strptime('20141108', datePattern) ,]

frequency = 60 * 24
ind_size = length(data1$id)
five_day_ind = 1:ind_size
plot(data1$ratio[five_day_ind], type = 'l')
data1.ts = ts(data1$ratio, start=1, frequency=frequency)
sum(is.na(data1.ts))
fit = stl(data1.ts, 'periodic')
colnames(fit$time.series)
head(fit$time.series)
length(fit$time.series)

plot(data1$ratio[five_day_ind], type = 'l', ylim= c(-0.4, 1.25), xlim= c(0, ind_size))
lines(fit$time.series[five_day_ind,1], col = 2)
lines(fit$time.series[five_day_ind,2], col = 3)
leg.txt = c('origin', 'seasonal', 'trends')
legend(ind_size - frequency, 1.25, leg.txt, cex = 0.6, lty = 1, col = 1:3)

plot(data1$ratio[five_day_ind], type = 'l', ylim= c(-0.1, 1.2), xlim= c(0, ind_size))
lines(fit$time.series[five_day_ind,1] + fit$time.series[five_day_ind,2], col = 4)
leg.txt = c('origin', 'approx')
legend(ind_size - frequency, 1.2, leg.txt, cex = 0.8, lty = 1, col = c(1, 4))
