import nltk
from nltk.corpus import wordnet as wn

"""

Author: Allan Barcelos

"""

# Aceita qualquer uma das linguagens suportadas pela wordnet:
# ['als', 'arb', 'cat', 'cmn', 'dan', 'eng', 'eus', 'fas', 'fin', 'fra', 'fre', 'glg', 'heb', 'ind', 'ita', 'jpn', 'nno', 'nob', 'pol', 'por', 'spa', 'tha', 'zsm']


def find_synonyms(word, language, deep):
    synonyms = _find_synonyms(word, language, deep, 0)
    synonyms = list(set(synonyms))
    return synonyms

def _find_synonyms(word, language, deep, actual_deep):   
    synonyms = []
    if(deep == actual_deep):
        return [word]
    else:
        for ss in wn.synsets(word, lang=language):
            for lemma in ss.lemma_names(lang=language):
                synonyms.extend(_find_synonyms(lemma, language, deep, actual_deep + 1))
        return synonyms

def find_hypernyms(word, language, deep):
    hypernyms = _find_hypernyms(word, language, deep, 0)
    hypernyms = list(set(hypernyms))
    return hypernyms

def _find_hypernyms(word, language, deep, actual_deep):   
    hypernyms = []
    if(deep == actual_deep):
        return [word]
    else:
        for ss in wn.synsets(word, lang=language):
            for h in ss.hypernyms():
                for lemma in h.lemma_names(lang=language):
                    hypernyms.extend(_find_hypernyms(lemma, language, deep, actual_deep + 1))
        return hypernyms

def find_hyponyms(word, language, deep):
    hyponyms = _find_hyponyms(word, language, deep, 0)
    hyponyms = list(set(hyponyms))
    return hyponyms    

def _find_hyponyms(word, language, deep, actual_deep):   
    hyponyms = []
    if(deep == actual_deep):
        return [word]
    else:
        for ss in wn.synsets(word, lang=language):
            for h in ss.hyponyms():
                for lemma in h.lemma_names(lang=language):
                    hyponyms.extend(_find_hyponyms(lemma, language, deep, actual_deep + 1))
        return hyponyms

def find_antonyms(word, language, deep):
    antonyms = _find_antonyms(word, language, deep, 0)
    antonyms = list(set(antonyms))
    return antonyms    

def _find_antonyms(word, language, deep, actual_deep):   
    antonyms = []
    if(deep == actual_deep):
        return [word]
    else:
        for ss in wn.synsets(word, lang=language):
            for ss_lemma in ss.lemmas(lang=language):
                if ss_lemma.antonyms():
                    #for lemma in a.lemma_names(lang=language):
                    lemma = ss_lemma.antonyms()[0].name()
                    antonyms.extend(_find_antonyms(lemma, language, deep, actual_deep + 1))
        return antonyms