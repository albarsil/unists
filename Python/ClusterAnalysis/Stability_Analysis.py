from ClusterAnalysis.Functions import stab_clust
import json
import numpy as np

testSet = []
trainSet = []
unionSet = []


with open('/home/alozhkin/PycharmProjects/resultConsolidationModel/Predictions/feb/svm-synonym_hyperonym-setEP1_setEP2.json') as json_data:
    ds2 = json.load(json_data)
    for i in ds2:
        testSet.append(i)
    json_data.close()

with open('/home/alozhkin/PycharmProjects/resultConsolidationModel/Predictions/feb/svm-synonym_hyperonym-setEP1_setEP3.json') as json_data:
    d2 = json.load(json_data)
    for i in d2:
        trainSet.append(i)
    json_data.close()

for j in range(len(testSet)):
    setOfValue = []
    setOfValue.extend([testSet[j]['pt_similarity'], testSet[j]['en_similarity']])
    unionSet.append(setOfValue)

for j in range(len(trainSet)):
    setOfValue = []
    setOfValue.extend([trainSet[j]['pt_similarity'], trainSet[j]['en_similarity']])
    unionSet.append(setOfValue)

for i in range(5):
    print('Noise volume: ' + str(0.05 + 0.03*i) + 'Stability level: ' + str(stab_clust(np.array(unionSet), 0.05 + 0.03*i, 8, 30)))
