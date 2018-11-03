import nltk
import codecs
from num2words import num2words
from nltk.stem.snowball import PortugueseStemmer
import string
import pickle
import numpy
import re
import json
from gensim.models import KeyedVectors
from nltk.stem import WordNetLemmatizer
import pandas as pd
import math
import wordnet

_stemmer = PortugueseStemmer()
stopwords = nltk.corpus.stopwords.words('portuguese')
word_tokenize = nltk.word_tokenize
punct = string.punctuation

"""

Author: Allan Barcelos

"""

def filter_stopwords(tokens):
    """Docstring."""
    return [i.lower() for i in tokens if
            i.lower() not in stopwords and i not in punct]

def get_palavras_words(tree):
    """Docstring."""
    tree = tree.replace('\\n', '\n')
    return re.findall('\[(.*)\]', tree)

def factor(col, codeDict):
    ''' call example: factor(data["Loan_Status"], {'N':0,'Y':1})'''
    colCoded = pd.Series(col, copy=True)
    for key, value in codeDict.items():
        colCoded.replace(key, value, inplace=True)
    
    return colCoded

def replace_missing(x, to = 'desconhecido'):
    if all(isinstance(y, int) for y in x):
        return [to if math.isnan(y) else y for y in x]
    elif all(isinstance(y, int) for y in x):
        return [to if math.isnan(y) else y for y in x]
    else:
        return [to if y is None else y for y in x]

def normalize(x):
    return [((y - min(x))/(max(x) - min(x))) for y in x]

def binning(col, cut_points, labels=None):

    #Define min and max values:
    minval = col.min()
    maxval = col.max()

    #create list by adding min and max to cut_points
    break_points = [minval] + cut_points + [maxval]

    #if no labels provided, use default labels 0 ... (n-1)
    if not labels:
        labels = range(len(cut_points)+1)

    #Binning using cut function of pandas
    colBin = pd.cut(col,bins=break_points,labels=labels,include_lowest=True)

    return colBin

def count_missing(x):
    return sum(x.isnull())

def stemming(x):
    return _stemmer.stem(x)

def replace_word_synonyms(sentence1, sentence2, language, synonym_deep = 2):
    sentence1 = sentence1.split()
    sentence2 = sentence2.split()
    
    sentence2_grams = ['_'.join(x) for x in ngrams(sentence2, 3)]
    
    lt = []
    
    for t in sentence1:
        if t in sentence2:
            lt.append(t)
        else:
            ths = wordnet.find_synonyms(t, language, synonym_deep)
            ts = t
            for th in ths:
                if th in sentence2 or th in sentence2_grams:
                    ts = th
                    break
            lt.append(ts)
                    
    return ' '.join(lt)

def replace_word_hyponym(sentence1, sentence2, language, hyponym_deep = 2):
    sentence1 = sentence1.split()
    sentence2 = sentence2.split()
    
    sentence2_grams = ['_'.join(x) for x in ngrams(sentence2, 3)]
    
    lt = []
    
    for t in sentence1:
        if t in sentence2:
            lt.append(t)
        else:
            ths = wordnet.find_hyponyms(t, language, hyponym_deep)
            ts = t
            for th in ths:
                if th in sentence2 or th in sentence2_grams:
                    ts = th
                    break
            lt.append(ts)
                    
    return ' '.join(lt)

def replace_word_hypernyms(sentence1, sentence2, language, hyperonym_deep = 2):
    sentence1 = sentence1.split()
    sentence2 = sentence2.split()
    
    sentence2_grams = ['_'.join(x) for x in ngrams(sentence2, 3)]
    
    lt = []
    
    for t in sentence1:
        if t in sentence2:
            lt.append(t)
        else:
            ths = wordnet.find_hypernyms(t, language, hyperonym_deep)
            ts = t
            for th in ths:
                if th in sentence2 or th in sentence2_grams:
                    ts = th
                    break
            lt.append(ts)
                    
    return ' '.join(lt)

def replace_word_hypernyms_of_synonyms(sentence1, sentence2, language, synonym_deep = 2, hyperonym_deep = 2):   
    sentence1 = sentence1.split()
    sentence2 = sentence2.split()
    
    sentence2_grams = ['_'.join(x) for x in ngrams(sentence2, 3)]
    
    lt = []
    
    for t in sentence1:
        if t in sentence2:
            lt.append(t)
        else:
            reserved_words = wordnet.find_synonyms(t, language, synonym_deep)
            reserved_words.append([wordnet.find_hypernyms(x, language, hyperonym_deep) for x in reserved_words])
            
            changed_word = t
            for x in reserved_words:
                if x in sentence2 or x in sentence2_grams:
                    changed_word = x.replace('_', ' ')
                    
            lt.append(changed_word)
                    
    return ' '.join(lt)

def is_number_tryexcept(s):
    """ Returns True is string is a number. """
    try:
        float(s)
        return True
    except ValueError:
        return False

def number_to_word(word, language):

    if language == 'por':
        language = 'pt_BR'
    elif language == 'eng':
        language = 'en'

    try:
        return num2words(float(word), to = 'cardinal', lang = language)
    except NotImplementedError:
        return word

# Retorna os ngramas variando de 1 até N
def ngrams(s, max_n):
    if isinstance(s, list) == False:
        s = s.split()

    grams = list()
    n = 1
    while n <= max_n:
        k = nltk.ngrams(s, n)
        n = n + 1
        grams.extend([' '.join(i) for i in k])

    return grams