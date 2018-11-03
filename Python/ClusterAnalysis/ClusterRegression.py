from sklearn.cluster import MiniBatchKMeans
import json
import numpy as np
from sklearn.metrics import mean_squared_error
from sklearn import svm, linear_model

testSet = []
trainSet = []

with open('/home/alozhkin/PycharmProjects/resultConsolidationModel/Predictions/feb/svm-clear-setEP1_setEP2.json') as json_data:
    ds2 = json.load(json_data)
    for i in ds2:
        testSet.append(i)
    json_data.close()

with open('/home/alozhkin/PycharmProjects/resultConsolidationModel/Predictions/feb/svm-clear-setEP1_setEP3.json') as json_data:
    d2 = json.load(json_data)
    for i in d2:
        trainSet.append(i)
    json_data.close()

train_x = []
test_x = []
train_y = []
test_y = []
test_p = []

for j in range(len(trainSet)):
    setOfValue = []
    setOfValue.extend([trainSet[j]['pt_similarity'], trainSet[j]['en_similarity']])
    train_x.append(setOfValue)
    train_y.append(trainSet[j]['target'])

train_x = np.array(train_x)
train_y = np.array(train_y)

for j in range(len(testSet)):
    setOfValue = []
    setOfValue.extend([testSet[j]['pt_similarity'], testSet[j]['en_similarity']])
    test_x.append(setOfValue)
    test_y.append(testSet[j]['target'])
    test_p.append(testSet[j]['pt_similarity'])

test_x = np.array(test_x)
test_y = np.array(test_y)
test_p = np.array(test_p)


cluster = 2
c = MiniBatchKMeans(n_clusters=cluster).fit(train_x)
test = c.predict(test_x)
trainC = {}
trainCy = {}
testC = {}
testCy = {}
modelsSet = {}
mseSet = {}
predict = []
predictTest = []
for i in range(0, cluster):
    res1 = []
    res2 = []
    test1 = []
    test2 = []
    for j in range(train_x.shape[0]):
        if c.labels_[j] == i:
            res1.append(train_x[j])
            res2.append(train_y[j])
    for j in range(test_x.shape[0]):
        if test[j] == i:
            test1.append(test_x[j])
            test2.append(test_y[j])
    trainC[i] = res1
    trainCy[i] = res2
    testC[i] = test1
    testCy[i] = test2
    modelsSet[i] = linear_model.LinearRegression().fit(res1, res2)
    predict.extend(test2)
    predictTest.extend(modelsSet[i].predict(test1))
    mseSet[i] = mean_squared_error(test2, modelsSet[i].predict(test1))

print(mseSet)
print('Cluster regression MSE : ' + str(mean_squared_error(predict, predictTest)))
print('Portuguese similarities vs target MSE: ' + str(mean_squared_error(test_p, test_y)))
