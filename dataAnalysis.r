# To Generate Visuals to be used on ASA national poster
# Darwin Clark 12.13
# UTC minus (Back in time) 8 hours is PST
library(ggplot2)
library(scales) # to access breaks/formatting functions (Used for date example from stack overflow)
library(plyr)
library(reshape2) # melt()
library(viridis)

# Importing Data (Note that read.csv() returns a dataframe object)
as532 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/as532.csv")

at200 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/at200.csv")
au467 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/au467.csv")
c5456 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/c5456.csv")
c8405 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/c8405.csv")
d2607 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/d2607.csv")
d5864 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/d5864.csv")
d8947 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/d8947.csv")
e0123 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/e0123.csv")
e3896 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/e3896.csv")
e8148 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/e8148.csv")
e9360 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/e9360.csv")
f0240 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/f0240.csv")
f0676 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/f0676.csv")
f1628 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/f1628.csv")
f2133 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/f2133.csv")
f4468 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/f4468.csv")
f4781 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/f4781.csv")
f5962 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/f5962.csv")
f7640 = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/f7640.csv")

kpwt = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/kpwt.csv")
ktiw = read.csv("~/Programming/HAppliedStats/ASAPoster/data/weatherstation/ktiw.csv")

# --- Data Formatting / Culling ---
# Weather station object list
weatherstations = list(at200, au467, c5456, c8405, d2607, d5864, d8947,
                       e0123, e3896, e8148, e9360, f0240, f0676, f1628, f2133,
                       f4468, f4781, f5962, f7640, kpwt, ktiw, as532) 

weatherstationsStr = list("at200", "au467", "c5456", "c8405", "d2607", "d5864", "d8947",
                       "e0123", "e3896", "e8148", "e9360", "f0240", "f0676", "f1628", "f2133",
                       "f4468", "f4781", "f5962", "f7640", "kpwt", "ktiw") 

# -- Fixing Date / Time Issues by treating all the dates and times as POSIX times --
at200$Time..UTC. = as.POSIXct(at200$Time..UTC.)
au467$Time..UTC. = as.POSIXct(au467$Time..UTC.) 
c5456$Time..UTC. = as.POSIXct(c5456$Time..UTC.)
c8405$Time..UTC. = as.POSIXct(c8405$Time..UTC.)
d2607$Time..UTC. = as.POSIXct(d2607$Time..UTC.)
d5864$Time..UTC. = as.POSIXct(d5864$Time..UTC.)
d8947$Time..UTC. = as.POSIXct(d8947$Time..UTC.)
e0123$Time..UTC. = as.POSIXct(e0123$Time..UTC.)
e3896$Time..UTC. = as.POSIXct(e3896$Time..UTC.)
e8148$Time..UTC. = as.POSIXct(e8148$Time..UTC.)
e9360$Time..UTC. = as.POSIXct(e9360$Time..UTC.)
f0240$Time..UTC. = as.POSIXct(f0240$Time..UTC.)
f0676$Time..UTC. = as.POSIXct(f0676$Time..UTC.)
f1628$Time..UTC. = as.POSIXct(f1628$Time..UTC.)
f2133$Time..UTC. = as.POSIXct(f2133$Time..UTC.)
f4468$Time..UTC. = as.POSIXct(f4468$Time..UTC.)
f4781$Time..UTC. = as.POSIXct(f4781$Time..UTC.)
f5962$Time..UTC. = as.POSIXct(f5962$Time..UTC.)
f7640$Time..UTC. = as.POSIXct(f7640$Time..UTC.)
kpwt$Time..UTC. = as.POSIXct(kpwt$Time..UTC.)
ktiw$Time..UTC. = as.POSIXct(ktiw$Time..UTC.)
as532$Time..UTC.  = as.POSIXct(as532$Time..UTC.)

# CAUTION: When you create the x=time and y=temp aggregate database like this, 
#  the TIMES AND TEMS WON'T ADD UP PROPERLY (Because the frequency of recordings isn't the same for each *.csv)
timeTempAggregate = data.frame(at200$Time..UTC.)
n = 1
for (weatherstation in weatherstations) {
  timeTempAggregate[as.character(n)] = weatherstation$Temperature..degrees.F. #timeTempAggregate[weatherstationsStr[[n]]] = 
  n = n + 1
}

myData = tail(c5456,n=5) # can also replace 'tail' with head()

cullNum = 25000 # 1000
tailDataAS532 = tail(au467, n=cullNum) # can also replace tail() with head()
# === Exploration Graphs === 
# --- Basic line plot, help you get a snapshot of a tail or head ---
ggplot(tailDataAS532, aes( x = tailDataAS532$Time..UTC., y = tailDataAS532$Temperature..degrees.F., group=1)) +
  geom_line(color = "darkcyan") +
  #geom_point() +
  xlab("Time in UTC (5 minute minimum resolution)") + ylab("Temperature (F)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + # Vertical x labels 
  scale_x_discrete(limits = tailDataAS532$Time..UTC., breaks = tailDataAS532$Time..UTC.[seq(1, length(tailDataAS532$Time..UTC.), by=50)])


# --- Basic Line Plot w/ Dates
ggplot(tailDataAS532, aes( x = as.POSIXct(tailDataAS532$Time..UTC., tz= "UCT"), y = value, color=variable)) +
  geom_line(color = "darkcyan") +
  #geom_point() +
  xlab("Time in UTC (5 minute minimum resolution)") + ylab("Temperature (F)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + # Vertical x labels 
  scale_x_datetime() 


# --- Stacked line plot v0.1 --- (Done by hand with 2 data frames with 2 weatherstations) 
testStacked = data.frame(at200$Time..UTC., as532$Temperature..degrees.F.)
names(au467)[names(au467)=="Temperature..degrees.F."] <- "Temperature..degrees.F.au467"

a = rbind.fill(at200[c("Time..UTC.", "Temperature..degrees.F.")],au467[c("Time..UTC.","Temperature..degrees.F.au467")] )
#at200$Temperature..degrees.F.,
a = tail(a, n= cullNum)
ggplot(a, aes(x = a$Time..UTC., y=a$Temperature..degrees.F., group=1)) +
  geom_line(color = "blue") +
  #geom_point() +
  xlab("Time in UTC (5 minute minimum resolution)") + ylab("Temperature (F)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))  + # Vertical x labels 
  scale_x_discrete(limits = a$Time..UTC., breaks = a$Time..UTC.[seq(1, length(a$Time..UTC.), by=50)])

# --- Stacked line plot v0.2 --- (Done by hand with 2 data frames with 2 weatherstations) 
# To be used as an example / proof of concept, don't delete this part
testDataFrame = data.frame(dateTime = as.POSIXct(tailDataAS532$Time..UTC., tz= "UCT"), 
                           #tempAS532 = tailDataAS532$Temperature..degrees.F.,
                           #tempAT200 = tail(at200, n=cullNum)$Temperature..degrees.F.,
                           tempAU467 = tail(au467, n=cullNum)$Temperature..degrees.F.,
                           tempC5456 = tail(c5456, n=cullNum)$Temperature..degrees.F.,
                           tempD8947 = tail(c8405, n=cullNum)$Temperature..degrees.F.
                           )

testDataFrame = melt(testDataFrame, id.vars = "dateTime")
ggplot(testDataFrame, aes(x= testDataFrame$dateTime, y=value, color = variable)) + geom_line() #scale_x_datetime(limits = "")
  

# === More in Depth Data Analysis ===
# --- Segregating By Area ---

# with(df, df[(date >= "2008-01-02" & date <= "2008-01-05") | (date >= "2008-01-09" & date <= "2008-01-11"), ])
gigHarborDataFrame = data.frame()  # e8148, f0674, d1373, f4468, c7999, kitw  
silverdaleDataFrame = data.frame() # kpwt, at200, a5532, f7640, f0240, d2607, f5962, f1628
northPennsula = data.frame()       # e0123, d5864, TODO FINISH


# --- Monthly Temperature Averages ---
# as532
# as532[as532$Time..UTC. >= "2020-05-02",]

dates = seq(as.Date("2020-03-01"), by = "month", length.out = 9) # Date Objects
datesString = c("2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01")
# A string of dates, in the same format

monthlyTempAverages = data.frame(month = dates, 
                                 at200 = 0,
                                 au467 = 0 ,
                                 c5456 = 0 ,
                                 c8405 = 0 ,
                                 d2607 = 0 ,
                                 d5864 = 0 ,
                                 d8947 = 0,
                                 e0123 = 0,
                                 e3896 = 0 ,
                                 e8148 = 0 ,
                                 e9360 = 0 ,
                                 f0240 = 0 ,
                                 f0676 = 0 ,
                                 f1628 = 0 ,
                                 f2133 = 0,
                                 f4468 = 0 ,
                                 f4781 = 0 ,
                                 f5962 = 0 ,
                                 f7640 = 0 ,
                                 kpwt = 0 ,
                                 ktiw = 0,
                                 as532 = 0)

# For() loop to create monthly temperatures (Have to do weatherstation at a time, but it's a start)
# Having to do this by hand because r was designed by apes
n = 1

for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$as532[[n]] = mean(as532[as532$Time..UTC. >= datesString[[n]] & as532$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}

n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$at200[[n]] = mean(at200[at200$Time..UTC. >= datesString[[n]] & at200$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$au467[[n]] = mean(au467[au467$Time..UTC. >= datesString[[n]] & au467$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}

n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$c5456[[n]] = mean(c5456[c5456$Time..UTC. >= datesString[[n]] & c5456$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$c8405[[n]] = mean(c8405[c8405$Time..UTC. >= datesString[[n]] & c8405$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}

n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$d2607[[n]] = mean(d2607[d2607$Time..UTC. >= datesString[[n]] & d2607$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$d5864[[n]] = mean(d5864[d5864$Time..UTC. >= datesString[[n]] & d5864$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}

n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$d8947[[n]] = mean(d8947[d8947$Time..UTC. >= datesString[[n]] & d8947$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$e0123[[n]] = mean(e0123[e0123$Time..UTC. >= datesString[[n]] & e0123$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}

n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$e3896[[n]] = mean(e3896[e3896$Time..UTC. >= datesString[[n]] & e3896$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$e8148[[n]] = mean(e8148[e8148$Time..UTC. >= datesString[[n]] & e8148$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$e9360[[n]] = mean(e9360[e9360$Time..UTC. >= datesString[[n]] & e9360$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$f0240[[n]] = mean(f0240[f0240$Time..UTC. >= datesString[[n]] & f0240$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$f0676[[n]] = mean(f0676[f0676$Time..UTC. >= datesString[[n]] & f0676$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$f1628[[n]] = mean(f1628[f1628$Time..UTC. >= datesString[[n]] & f1628$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$f2133[[n]] = mean(f2133[f2133$Time..UTC. >= datesString[[n]] & f2133$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$f4468[[n]] = mean(f4468[f4468$Time..UTC. >= datesString[[n]] & f4468$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$f4781[[n]] = mean(f4781[f4781$Time..UTC. >= datesString[[n]] & f4781$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$f5962[[n]] = mean(f5962[f5962$Time..UTC. >= datesString[[n]] & f5962$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$f7640[[n]] = mean(f7640[f7640$Time..UTC. >= datesString[[n]] & f7640$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$kpwt[[n]] = mean(kpwt[kpwt$Time..UTC. >= datesString[[n]] & kpwt$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}
n = 1
for (date in dates) {
  #print(str(datesString[[n]]))
  monthlyTempAverages$ktiw[[n]] = mean(ktiw[ktiw$Time..UTC. >= datesString[[n]] & ktiw$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)
  n = n + 1
}


# Stacking the monthly temperature

# --- Graph showing all known weather stations monthly average plotted
monthlyTempAverages = melt(monthlyTempAverages, id.vars = "month") # Note, After doing this the data becomes unreadable, creates a two column dataset
names(monthlyTempAverages)[names(monthlyTempAverages)=="variable"] <- "Weatherstation_Callsign" # This is a temporary workaround for being unable to rename the legend

ggplot(monthlyTempAverages, aes(x= monthlyTempAverages$month, y=value, color = Weatherstation_Callsign)) + geom_line() + #scale_x_datetime(limits = "")
  labs(title = "Line Plot Comparing Average Monthly Temperature to Month", x = "Month (UTC)", y = "Mean Temperature (F)", caption = "Figure X") +
  theme(text =element_text(family = "Times New Roman", size = 12)) + ylim(40, 65)

ggplot(as532MonthlyTempAverage, aes( x = as532MonthlyTempAverage$month, y = as532MonthlyTempAverage$temp, group=1)) +
  geom_line(color = "darkcyan") +
  #geom_point() +
  xlab("Month in UTC ") + ylab("Mean Temperature (F)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))  # Vertical x labels 

# --- Computing a linear model predicting average monthly temperate deviation by altitude 
# NOTE: A bunch of these numbers are copy-pasted from google sheets, it's much easier to do certain calculations in google sheets because it is a much
#       more visual program than r. While not good for long term learning, it is very effective at short-term goals
elevationList = c(#135.97, # In meters WITHOUT AT200
                  73.18,
                  18.88,
                  29.45,
                  119.03, 
                  66.35 ,
                  97.68,
                  47.12,
                  24.54 ,
                  58.32 ,
                  105.67 ,
                  77.34,
                  #43.57 ,
                  12.89 ,
                  100.55 ,
                  56.97 ,
                  2.42 ,
                  7.82 ,
                  7.62 ,
                  135.06,
                  90.02 ,
                  172.75 
                  )
mylist = c()
# The 9 comes from months: March, April, May, June, July, August, Sepetmber, October, November
for (num in elevationList) {
  mylist = c(mylist, rep(num, 9))
}

# TODO: Add a column in the `linearmodelinput` dataframe for date, color /size dots by date as well 
weatherStationResiduals = c(#3.562841, 3.562841,	5.001744737,	14.312727,	16.79817667,	12.76741381,	6.92385,	7.422964737,	7.396479524, # One #VALUE subbed # WITHOUT AT200
                            0.6270463158,	-0.681879,	-0.2527552632,	-1.175123,	-1.081813333,	-1.11179619,	-0.91407,	0.3406247368,	0.3683695238,
                            0.6203463158,	-1.205599,	-2.405965263,	-3.689703,	-5.666623333,	-4.80841619,	-3.83483,	-0.8774052632,	0.7143095238,
                            1.254406316,	0.087151,	-0.4061252632,	-1.112873,	-2.310893333,	-1.68503619,	-1.6119,	-0.02549526316,	0.6231595238,
                            0.3961263158,	0.790031,	0.8409347368,	0.252387,	0.4860066667,	0.8465738095,	1.46268,	0.1227947368,	-0.6027304762,
                            0.9279563158,	0.939731,	1.080594737,	0.070587,	0.1558766667,	0.1942138095,	0.43662,	0.6389247368,	1.096969524,
                            1.164886316,	0.914221,	0.5823047368,	-0.592693,	-0.2309033333,	-0.1226061905,	0.36636,	0.6102847368,	0.7488795238,
                            0.1845963158,	0.057881,	0.09040473684,	-0.170283,	-0.3206833333,	-0.3725461905,	-0.37939,	-0.5653352632,	-0.6054704762,
                            1.481396316,	0.630011,	0.2717247368,	-0.892053,	-1.363183333,	-0.6733761905,	-0.15091,	-0.2596152632,	1.312099524,
                            -0.6301136842,	-0.450459,	-0.006355263158,	-0.398883,	0.02634666667,	0.1402938095,	0.41218,	-1.059885263,	-0.6546304762,
                            -0.6536836842,	-0.686609,	-0.7298552632,	-1.841463,	-1.696393333,	-1.50315619,	-1.50315619, -1.50315619,	-1.128950476,
                            -0.02638368421,	0.445631,	1.377494737,	1.825227,	2.461716667,	2.89380381,	1.61153,	0.09954473684,	-0.5150504762,
                            #VALUE!	#VALUE!	#VALUE!	#VALUE!	#VALUE!	#VALUE!	#VALUE!	#VALUE!	#VALUE!
                            0.06341631579,	0.216031,	0.3473847368,	0.188467,	0.01253666667,	0.2038338095,	0.33735,	0.1805247368,	-0.3314404762,
                            1.146106316,	0.464111,	-0.2255652632,	-1.339373, -1.989343333,	-1.68201619,	-1.68201619,	-0.006195263158,	0.6096395238, # One subbed #VALUE
                            -0.6076236842,	0.020901,		0.020901,		0.020901,	0.8519766667,	0.7669138095,	0.09386,		0.020901,	-1.658450476, # 3 Subbed #VALUE
                            0.2806363158,	-0.646629,	-0.3522952632,	-0.938903,	-1.342053333,	-1.38593619,	-1.15147,	-0.5611252632,	0.4802595238,
                            -0.2535736842,	-0.522519,	-0.8841552632,	-1.305403,	-1.921683333,	-1.86532619,	-1.77603,	-1.613585263,	-0.9243004762,
                            1.431037,	1.431037,	1.431037,	1.431037,	0.3967066667,	0.4483038095,	0.39492,	-0.05472526316,	-0.3309804762, # 3 #VALUE substituteded
                            -3.067463684,	-1.735169,	-2.066955263,	-1.718813,	-1.219673333,	-1.57040619,	-0.95142,	-1.694485263,	-2.941370476,
                            -1.407533684,	-1.188679,	-0.9605152632,	-1.415253,	-1.123243333,	-0.7894161905,	-1.12985,	-0.7562352632,	-1.203150476,
                            -1.500543684,	-1.010999,	-1.302045263,	-1.489613,	-0.9228533333,	-0.6913161905,	-0.13948,	-1.941575263,	-2.453640476)

linearModelInput = data.frame(elevation = mylist, residual = weatherStationResiduals)


ggplot(linearModelInput, aes(x = linearModelInput$elevation, y = linearModelInput$residual, group=1)) +
  geom_point(color = "darkcyan") +
  labs(title = "Scatter Plot Comparing Elevation And Residual From Monthly Average", x = "Elevation (m)", y = "Residual From Aggregate Monthly Average (F)" ) +
  theme(text =element_text(family = "Times New Roman", face = "bold", size = 12)) 


residualFromElevation = lm(linearModelInput$residual~linearModelInput$elevation)
summary(residualFromElevation)

plot(residualFromElevation)
# --- Computing a linear model based on shore distance --- 

shoreDistance = c(#997.12, WITHOUT AT200
                  2984.99,
                  291.84,
                  825.31,
                  11088,
                  3262,
                  4135.61,
                  5544,
                  293.26,
                  2737.3,
                  3028.74,
                  2735.49,
                  #5913.6,
                  129,
                  6441.6,
                  1590.02,
                  70,
                  227.79,
                  710,
                  21014.4,
                  2785,
                  9292.8
                  )

mylist = c()
# The 9 comes from months: March, April, May, June, July, August, Sepetmber, October, November
for (num in shoreDistance) {
  mylist = c(mylist, rep(num, 9))
}

linearModelInput = data.frame(shoreDist = mylist, residual = weatherStationResiduals)


ggplot(linearModelInput, aes( x = linearModelInput$shoreDist, y = linearModelInput$residual, group=1)) +
  geom_point(color = "darkcyan") +
  #geom_point() +
  labs(title = "Scatter Plot Comparing Distance To Shore And Residual From Monthly Average", x = "Shortest Distance to Shore (ft)", y = "Residual From Aggregate Monthly Average (F)" ) +
  theme(text =element_text(family = "Times New Roman", face = "bold", size = 12)) 


residualFromShoreDistance = lm(linearModelInput$residual~linearModelInput$shoreDist)
summary(residualFromShoreDistance)


# --- Plotting Monthly Temperature Means vs. Distance ---
# (The poor man's way)

weatherStationMonthlyAverages = c(#monthlyTempAverages$at200, #WITHOUT AT200
                                  monthlyTempAverages$au467,
                                  monthlyTempAverages$c5456,
                                  monthlyTempAverages$c8405,
                                  monthlyTempAverages$d2607,
                                  monthlyTempAverages$d5864,
                                  monthlyTempAverages$d8947,
                                  monthlyTempAverages$e0123,
                                  monthlyTempAverages$e3896,
                                  monthlyTempAverages$e8148,
                                  monthlyTempAverages$e9360,
                                  monthlyTempAverages$f0240,
                                  monthlyTempAverages$f1628,
                                  monthlyTempAverages$f2133,
                                  monthlyTempAverages$f4468,
                                  monthlyTempAverages$f4781,
                                  monthlyTempAverages$f5962,
                                  monthlyTempAverages$f7640,
                                  monthlyTempAverages$kpwt,
                                  monthlyTempAverages$ktiw,
                                  monthlyTempAverages$as532)


mylist = c()
# The 9 comes from months: March, April, May, June, July, August, Sepetmber, October, November
for (num in elevationList) {
  mylist = c(mylist, rep(num, 9))
}

linearModelInputMonthly = data.frame(elevationList = mylist, average = weatherStationMonthlyAverages)

ggplot(linearModelInputMonthly, aes( x = linearModelInputMonthly$elevationList, y = linearModelInputMonthly$average, group=1)) +
  geom_point(color = "darkcyan") +
  #geom_point() +
  labs(title = "Scatter Plot Comparing Elevation And Monthly Average Temperature", x = "Elevation (m)", y = "Monthly Average (F)" ) +
  theme(text =element_text(family = "Times New Roman", face = "bold", size = 12)) 

# --- Linear Model Predicting one from BOTH ---
elevation = mylist # assuming the mylist object was elevation

residualFromShoreDistanceANDElevation = lm(linearModelInput$residual~linearModelInput$shoreDist+elevation)
summary(residualFromShoreDistanceANDElevation)

# --- Combining both geographic variables into a graph ---

bivariateDataFrame = data.frame(residual = linearModelInput$residual, elevation = elevation, Shortest_Distance_To_Shore = linearModelInput$shoreDist)


ggplot(bivariateDataFrame, aes( x = bivariateDataFrame$elevation, y = bivariateDataFrame$residual, group=1)) +
  geom_point(aes(size = Shortest_Distance_To_Shore), color = "darkcyan") +
  #geom_point() +
  labs(title = "Scatter Plot Comparing Elevation And Residual From Monthly Average, Sized by Shore Distance", x = "Elevation (m)", y = "Residual From Monthly Average (F)" ) +
  theme(text =element_text(family = "Times New Roman", face = "bold", size = 12)) +
  scale_color_viridis()

# --- Combinging 3 variabels: Time, ShoreDistance, Elevation ---
trivariateDataFrame = data.frame(residual = linearModelInput$residual, elevation = elevation, shoreDist = linearModelInput$shoreDist, month = rep(seq(3,11),20))

residualFromShoreDistanceANDElevationANDDate = lm(trivariateDataFrame$residual~trivariateDataFrame$shoreDist+trivariateDataFrame$elevation+trivariateDataFrame$month)
summary(residualFromShoreDistanceANDElevation)


ggplot(trivariateDataFrame, aes( x = trivariateDataFrame$month, y = trivariateDataFrame$residual, group=1)) +
  geom_point(aes(size = trivariateDataFrame$shoreDist), color = "darkcyan") +
  #geom_point() +
  labs(title = "Scatter Plot Comparing Elevation And Monthly Average Temperature, Colored by Shore Distance", x = "Elevation (m)", y = "Residual From Monthly Average" ) +
  theme(text =element_text(family = "Times New Roman", face = "bold", size = 12)) #+
  #scale_color_viridis()


# --- Averaging the residual, only 20 data points so not that useful ---

averageMonthlyResidualByWeatherstation = data.frame(averageResidual = c(-0.4312662456,
                                                    -2.350431801,
                                                    -0.5764006901,
                                                    0.5105337544,
                                                    0.6157193099,
                                                    0.3823037544,
                                                    -0.2312029123,
                                                    0.03956597661,
                                                    -0.2912784678,
                                                    -1.24960255,
                                                    1.130390421,
                                                    0.1353448655,
                                                    -0.5227391345,
                                                    -0.04408452047,
                                                    -0.6241684678,
                                                    -1.229619579,
                                                    0.7309303041,
                                                    -1.885084023,
                                                    -1.108208468,
                                                    -1.272451801), shoreDistance = c(#997.12, WITHOUT AT200, WITHOUT F0607
                                                      2984.99,
                                                      291.84,
                                                      825.31,
                                                      11088,
                                                      3262,
                                                      4135.61,
                                                      5544,
                                                      293.26,
                                                      2737.3,
                                                      3028.74,
                                                      2735.49,
                                                      #5913.6,
                                                      129,
                                                      6441.6,
                                                      1590.02,
                                                      70,
                                                      227.79,
                                                      710,
                                                      21014.4,
                                                      2785,
                                                      9292.8
                                                    ), elevation = c(#135.97, # In meters WITHOUT AT200 WITHOUT F0607
                                                      73.18,
                                                      18.88,
                                                      29.45,
                                                      119.03, 
                                                      66.35 ,
                                                      97.68,
                                                      47.12,
                                                      24.54 ,
                                                      58.32 ,
                                                      105.67 ,
                                                      77.34,
                                                      #43.57 ,
                                                      12.89 ,
                                                      100.55 ,
                                                      56.97 ,
                                                      2.42 ,
                                                      7.82 ,
                                                      7.62 ,
                                                      135.06,
                                                      90.02 ,
                                                      172.75 
                                                    ) )

                                                    
ggplot(averageMonthlyResidualByWeatherstation, aes( x = averageMonthlyResidualByWeatherstation$shoreDistance, y = averageMonthlyResidualByWeatherstation$averageResidual, group=1)) +
  geom_point(color = "darkcyan") +
  #geom_point() +
  labs(title = "Scatter Plot Comparing Average Residual From Monthly Temperature to Shore Distance", x = "shoreDistance (ft)", y = "Average 9-Month Residual From Monthly Average" ) +
  theme(text =element_text(family = "Times New Roman", face = "bold", size = 12)) #+
#scale_color_viridis()

ggplot(averageMonthlyResidualByWeatherstation, aes( x = averageMonthlyResidualByWeatherstation$elevation, y = averageMonthlyResidualByWeatherstation$averageResidual, group=1)) +
  geom_point(color = "darkcyan") +
  #geom_point() +
  labs(title = "Scatter Plot Comparing Average Residual From Monthly Temperature to Elevation", x = "shoreDistance (ft)", y = "Average 9-Month Residual From Monthly Average" ) +
  theme(text =element_text(family = "Times New Roman", face = "bold", size = 12)) #+







###########################################################3
mean(weatherstation[weatherstation$Time..UTC. >= datesString[[n]] & weatherstation$Time..UTC. <= datesString[[n+1]],]$Temperature..degrees.F.)

# Deprecated
as532MonthlyTempAverage$temp[[1]] = mean(as532[as532$Time..UTC. >= "2020-02-01" & as532$Time..UTC. <= "2020-02-29",]$Temperature..degrees.F.)
as532MonthlyTempAverage$temp[[2]] = mean(as532[as532$Time..UTC. >= "2020-03-01" & as532$Time..UTC. <= "2020-03-31",]$Temperature..degrees.F.)
as532MonthlyTempAverage$temp[[3]] = mean(as532[as532$Time..UTC. >= "2020-04-01" & as532$Time..UTC. <= "2020-04-30",]$Temperature..degrees.F.)
as532MonthlyTempAverage$temp[[4]] = mean(as532[as532$Time..UTC. >= "2020-05-01" & as532$Time..UTC. <= "2020-05-31",]$Temperature..degrees.F.)
as532MonthlyTempAverage$temp[[5]] = mean(as532[as532$Time..UTC. >= "2020-06-01" & as532$Time..UTC. <= "2020-06-30",]$Temperature..degrees.F.)
as532MonthlyTempAverage$temp[[6]] = mean(as532[as532$Time..UTC. >= "2020-07-01" & as532$Time..UTC. <= "2020-07-31",]$Temperature..degrees.F.)
as532MonthlyTempAverage$temp[[7]] = mean(as532[as532$Time..UTC. >= "2020-08-01" & as532$Time..UTC. <= "2020-08-31",]$Temperature..degrees.F.)
as532MonthlyTempAverage$temp[[8]] = mean(as532[as532$Time..UTC. >= "2020-09-01" & as532$Time..UTC. <= "2020-09-30",]$Temperature..degrees.F.)
as532MonthlyTempAverage$temp[[9]] = mean(as532[as532$Time..UTC. >= "2020-10-01" & as532$Time..UTC. <= "2020-10-31",]$Temperature..degrees.F.)
as532MonthlyTempAverage$temp[[10]]= mean(as532[as532$Time..UTC. >= "2020-11-01" & as532$Time..UTC. <= "2020-11-30",]$Temperature..degrees.F.)
as532MonthlyTempAverage$temp[[11]]= mean(as532[as532$Time..UTC. >= "2020-12-01" & as532$Time..UTC. <= "2020-12-10",]$Temperature..degrees.F.)

