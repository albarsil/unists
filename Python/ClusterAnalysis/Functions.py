import numpy as np
from sklearn import cluster


def sim_matrix(setClustC):
    setClustC = setClustC.labels_
    matrixClust = []
    lenClust = len(setClustC)
    for it1 in range(0, lenClust):
        row = []
        for it2 in range(0, lenClust):
            if it1 != it2:
                if setClustC[it1] == setClustC[it2]:
                    row.append(1)
                else:
                    row.append(0)
            else:
                row.append(0)
        matrixClust.append(row)

    return matrixClust


def comp_metric(set1, set2):
    matrixClust1 = sim_matrix(set1)
    matrixClust2 = sim_matrix(set2)
    res = np.sum(np.multiply(np.array(matrixClust1), np.transpose(np.array(matrixClust2))))/(len(set1.labels_)*len(set1.labels_))
    return res


def stab_clust(dataSet, noiseLevel, maxClusters, iterNum):
    changesSet = {}
    size = dataSet.shape
    for clustNum in range(2, maxClusters+1):
        print('cluster number = ' + str(clustNum))
        changesSetIter = []
        initClust = cluster.KMeans(clustNum).fit(dataSet)
        for iter in range(1, iterNum + 1):
            noisesSet = np.random.randn(size[0], size[1])*noiseLevel
            noisedData = dataSet + noisesSet
            noisedClust = cluster.KMeans(clustNum).fit(noisedData)
            changesSetIter.append(comp_metric(initClust, noisedClust))
        changesSet[clustNum] = np.sum(changesSetIter)
    return changesSet
