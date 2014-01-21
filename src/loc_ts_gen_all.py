import sys
from datetime import date, timedelta

countries = ["CHN", "FRA", "RUS", "GBR", "USA", "JPN", "KOR", "DEU", "BRA", "PRK",
             "AFG", "IRN", "IRQ", "SAU", "SYR"]

cset = set(countries)

def parseDate(daystr):
    if daystr == "NA":
        return None
    year = int(daystr[:4])
    mon = int(daystr[4:6])
    day = int(daystr[6:8])
    return date(year, mon, day)

def date2str(dobj):
    return str(dobj.year)+"-"+str(dobj.month)+"-"+str(dobj.day)

def main():

    dateset = dict()
    dateset2 = dict()

    mindate = date(2025, 1,1)
    maxdate = date(1970, 1, 1)

    predir = "tsdata\\"
    
    f = open("..\\data\\"+sys.argv[1]+"\\"+sys.argv[1]+".csv", "r")
    line = f.readline()
    
    filedict = dict()

    tmpind = 0
    while line != None and len(line) > 1:
        sp = line.split('\t')
        sp[1] = parseDate(sp[1])
        if sp[1] != None:
            if sp[1] < mindate:
                    mindate = sp[1]
            if sp[1] > maxdate:
                    maxdate = sp[1]
        else:
            line = f.readline()
            continue
        if sp[7] in cset and sp[17] in cset:
	    key1 = sp[7].lower()+sp[17].lower()+sys.argv[1]   
	    if not key1 in filedict.keys():
		filedict[key1] = dict()      
            if not sp[1] in filedict[key1].keys():
                filedict[key1][sp[1]] = 1
            else:
                filedict[key1][sp[1]] += 1
	tmpind += 1
	if tmpind % 50000 == 0:
	    print "line="+str(tmpind)
        line = f.readline()

    timespan = maxdate-mindate

    for filekey in filedict.keys():
        fw = open("tsdata\\"+filekey+".csv", "w")
    	for i in range(timespan.days+1):
            tmpdate = mindate+timedelta(i)
            if tmpdate in filedict[filekey].keys():
            	fw.write(date2str(tmpdate)+","+str(filedict[filekey][tmpdate])+"\n")
            else:
                fw.write(date2str(tmpdate)+","+"0"+"\n")

        fw.close()
    f.close()
            
if __name__ == '__main__':
    main()
