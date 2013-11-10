from collections import deque
import random

random.seed()
def getArrivalTime():
    p = random.random()
    if p < .1:
            return 0
    elif p < .25:
            return 1
    elif p < .35:
            return 2
    elif p < .7:
            return 3
    elif p < .95:
            return 4
    else:
            return 5

s = .9
def getServiceTime():
    p = random.random()
    if p < .25:
            return 1*s
    elif p < .45:
            return 2*s
    elif p < .85:
            return 3*s
    else:
            return 4*s

numDays = 100000
numPeople = 150
lineLengths = []
waitTimes = []

for i in range(numDays):
    t = 0
    waitTime = 0
    sumLengths = 0

    tEndService = getServiceTime() #time until end of current service
    tNextArrival = 0 #time until next arrival

    i = 0 #number that have arrived
    qLength = 0
    #loop this until people stop arriving
    while True:
        #print(qLength)
        #next person arrives
        if qLength > 0:
            #if end of service is sooner then next arrival, move to that time
            if tEndService < tNextArrival:
                t += tEndService
                waitTime += tEndService*qLength
                tNextArrival -= tEndService
                #service ends
                qLength -= 1
                tEndService = getServiceTime()
                
            #otherwise, move to next arrival
            else:
                t += tNextArrival
                waitTime += tNextArrival*qLength
                tEndService -= tNextArrival
                #next arrival
                qLength += 1
                i += 1
                if i < numPeople:
                    tNextArrival = getArrivalTime() #ignore arrivalTimes[0]; only need 149 inter-arrival times
                else:
                    break
                
        #length of queue is 0       
        else:
            #wait for next arrival
            t += tNextArrival
            tEndService -= tNextArrival
            qLength += 1
            i += 1
            if i < numPeople:
                tNextArrival = getArrivalTime() #ignore arrivalTimes[0]; only need 149 inter-arrival times
            else:
                break
            #this person starts getting served right away if last person is not still being served
            if tEndService <= 0:
                qLength -= 1
                tEndService = getServiceTime()

    #empty out the rest of the line
    while qLength > 0:
        t += tEndService
        waitTime += tEndService*qLength
        qLength -= 1
        tEndService = getServiceTime()

    lineLengths.append(waitTime/t)
    waitTimes.append(waitTime/numPeople)

avgLineLength = 0
avgWaitTime = 0
for i in range(numDays):
    avgLineLength += lineLengths[i]
    avgWaitTime += waitTimes[i]
avgLineLength /= numDays
avgWaitTime /= numDays
print("Average line length =", avgLineLength)
print("Average wait time =", avgWaitTime)










        
        
