#! /usr/bin/env python2.7

'''
This script creates an API call for publicly avaialble BLS statistics,
and outputs requested statistics into a .csv file.
BLS API details are available at (including example code:
http://www.bls.gov/developers/

Bryce Bartlett
'''

#@@@
#Globals
#@@@

import requests
import json
import csv
headers = {'Content-type': 'application/json'}
apicall = 'https://api.bls.gov/publicAPI/v2/timeseries/data/'
outdir = "H:/projects/happy_gene/output/"
keyfile = 'H:/projects/proposal/r_study/code/blskey~.txt'

#@@@
#Load apikey
#@@@

apikey=open(keyfile).readlines()[0].rstrip('\n')
#print(apikey)

#@@@
#Select data  to import
#@@@

#data series formats can be found at http://www.bls.gov/help/hlpforma.htm
#Relevant survey is Local Area Unemployment Statistics (women's rate in industry can be retrieved from employment [ces])
#Also, mass layoff series might be relevant

'''
IDENTIFIER:
index seasonal adjustment          area    datatype
#LA           U           RD8100000000000     03
CENSUS regions
N	RD8100000000000	New England division
N	RD8200000000000	Middle Atlantic division
N	RD8300000000000	East North Central division
N	RD8400000000000	West North Central division
N	RD8500000000000	South Atlantic division
N	RD8600000000000	East South Central division
N	RD8700000000000	West South Central division
N	RD8800000000000	Mountain division
N	RD8900000000000	Pacific division
'''

#populate a list of series to request (summary above)
s = []
for i in range(1,10):
    s.append('LAURD8%s0000000000003' %i)

#prepare dictionary for JSON submission to BLS
#limited to 10 years at a time; daily query of 25; series 25 limit
#registered api's limited to 20 years; 500 daily queries
sd = {}
sd["registrationkey"] = apikey
sd["seriesid"] = s
sd["startyear"] = "1976"
sd["endyear"] = "1995"

#reformat dictionary to JSON format
data = json.dumps(sd)

for i in (json.loads(data)):
    print(i)

#@@@
#Make API call
#@@@

p = requests.post(apicall, data=data, headers=headers)
json_data = json.loads(p.text)

#@@@
#Save Data
#@@@

#save raw json to file:
#with open(outdir + 'bls_json.txt', 'w') as outfile:
#    json.dump(json_data, outfile)


#write output to  csv
#note earliest LAU is 1976 per https://www.bls.gov/lau/laufaq.htm
tocsv=list()

for d in json_data['Results']['series']:
    for i in d['data']:
        del i['footnotes']
        newline = i
        #drop "M" off of month line
        newline['period'] = newline['period'][1:]
        newline['seriesID'] = d['seriesID']
        #recover region number 1-9 (matches GSS from census)
        newline['region'] = d['seriesID'][6:7]
        newline=newline

        tocsv.append(newline)


keys = tocsv[0].keys()
with open(outdir + 'blsdat1976.csv', 'w') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(tocsv)


