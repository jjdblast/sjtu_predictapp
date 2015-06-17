load("predictionByMensaId.rda")

stationId = 1
timeIdx = 150

ratios = pmax(predictionByMensaId[[stationId]]$freq, 1)
ratios = ratios/max(ratios)

dataLength = 17 * 12
chances = ratios * 100;
data <- data.frame(x=1:length(chances),y=chances)
color = sapply(ratios, getColor)
interval = 20
minIdx = max(timeIdx - interval, 1) - max(timeIdx + interval - dataLength, 0)
maxIdx = min(timeIdx + interval, dataLength) + max(interval + 1 - timeIdx, 0)
maxChance = max(chances[minIdx:maxIdx])
ggplot(data=data, aes(x=x)) + 
  geom_linerange(color=color, ymax=chances, ymin=-0.1, size=1) + 
  geom_text(x=min(max(timeIdx, 10), 12 * 17 - 10), y=max(maxChance/2, 5), size=8, label=paste0(round(chances[timeIdx]), '%')) +
  xlab("Time") +
  ylab("Capacity(%)") +
  scale_y_continuous(limits = c(0, max(maxChance, 10)), breaks=0:10 * 10) +
  scale_x_continuous(
    limits = c(minIdx, maxIdx), 
    breaks = (5:22) * 12,
    labels = idxToTime
  )


getColor = function(ratio){
  if(ratio <= 0.1){
    ratio = 0
  }else{
    ratio = ratio * 2
  }
  ratio = min(max(ratio, 0), 1)
  color1 = 'ff4444'
  color2 = '99cc00'
  hex = function(x) {
    hex = as.character(as.hexmode(x))
    if(nchar(hex) == 1){
      hex = paste0('0', hex)
    }
    hex
  }
  r = ceiling(as.integer(paste0('0x', substring(color1, 1, 2))) * ratio + as.integer(paste0('0x', substring(color2, 1,2))) * (1-ratio))
  g = ceiling(as.integer(paste0('0x', substring(color1, 3, 4))) * ratio + as.integer(paste0('0x', substring(color2, 3,4))) * (1-ratio))
  b = ceiling(as.integer(paste0('0x', substring(color1, 5, 6))) * ratio + as.integer(paste0('0x', substring(color2, 5,6))) * (1-ratio))
  paste0('#', hex(r), hex(g), hex(b))
}

idxToTime = function(idx){
  idx = idx
  # calculate the n-st of 5-min, and plus 5 because mensa start at 5:00am
  hours = formatC(5 + idx %/%  12, width = 2, format = "d", flag = "0") 
  # calculate the 
  minutes = formatC((idx %% 12)*5, width = 2, format = "d", flag = "0")
  paste0(hours, ':', minutes)
}