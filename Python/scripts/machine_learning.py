from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.linear_model import LinearRegression
from gensim.models import KeyedVectors
from sklearn.model_selection import cross_val_score
from sklearn.decomposition import PCA
from scipy.stats import pearsonr
from functools import reduce
from sklearn.svm import SVR
from xml.dom import minidom
from numpy import array

"""

Author: Nathan Hartmman

"""

def print_result(gold_values, sys_values, name):
    """Docstring."""
    pearson = pearsonr(gold_values, sys_values)[0]
    absolute_diff = gold_values - sys_values
    mse = (absolute_diff ** 2).mean()

    print()
    print('Similarity evaluation\t' + name)
    print('Pearson\t\tMean Squared Error')
    print('-------\t\t------------------')
    print('{:7.2f}\t\t{:18.2f}'.format(pearson, mse))

def evaluate(x, y, ml_model, model_name):
    """Docstring."""

    ml_model = ml_model.fit(x, y)
    y_rbf = ml_model.predict(x)

    print_result(y, y_rbf, model_name)
    scores = cross_val_score(
        ml_model,
        x,
        y,
        cv=10,
        scoring='neg_mean_squared_error'
    )
    
    print("MSQ (cross_validation): %0.2f (+/- %0.2f)"
          % (scores.mean(), scores.std() * 2))

    return ml_model

def write_xml(filename, pred):
    """Docstring."""
    with open(filename) as fp:
        xml = minidom.parse(fp)
    pairs = xml.getElementsByTagName('pair')
    for pair in pairs:
        pair.setAttribute('similarity', str(pred[pairs.index(pair)]))
    with open(filename, 'w') as fp:
        fp.write(xml.toxml())