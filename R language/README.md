# unists

[en]

  This project aims to propose and evaluate the use of a hybrid approach, in which both resources of distributed representation and also lexical and linguistic aspects are integrated for the evaluation of semantic textual similarity between short sentences in Brazilian Portuguese. 

[pt-BR]

  Este projeto visa propor e avaliar o uso de uma abordagem híbrida, na qual são integrados tanto recursos de representação distribuída como também aspectos léxicos e linguístico para a avaliação da semelhança semântica textual entre frases curtas em português do Brasil.

---

## Organização do diretório:

* Data - Contém os recursos utilizados pelo algoritmo para extração de informações linguísticas das sentenças;
    
    * Propor - Conjunto de dados do evento PROPOR 2016;
    
* Scripts - Os códigos fontes utilizados na abordagem
  
  * Commons - Operações base que são utilizadas nos scripts do Propor;
  
  * Propor - Códigos utilizados para gerar os resultados obtidos pela abordagem.

---

## Trabalhos publicados:

1. [O uso de recursos linguísticos para mensurar a semelhança semântica entre frases curtas através de uma abordagem híbrida](http://www.repositorio.jesuita.org.br/handle/UNISINOS/6974)

    SILVA, A. O uso de recursos linguísticos para mensurar a semelhança semântica entre frases curtas através de uma abordagem híbrida. Dissertação (Mestrado em Computação Aplicada) – Programa de Pós-Graduação em Computação Aplicada, Universidade do Vale do Rio dos Sinos. São Leopoldo, p. 87. 2017.

2. [Avaliando a similaridade semântica entre frases curtas através de uma abordagem híbrida](http://www.aclweb.org/anthology/W17-6612)

    Silva, A., Rigo, S., Alves, I. M., & Barbosa, J. (2017). Avaliando a similaridade semântica entre frases curtas através de uma abordagem híbrida (A hybrid approach to measure Semantic Textual Similarity between short sentences in Brazilian Portuguese)[In Portuguese]. In Proceedings of the 11th Brazilian Symposium in Information and Human Language Technology (pp. 93-102).
    
3. [Enhancing Brazilian Portuguese Textual Entailment Recognition with a Hybrid Approach](http://thescipub.com/abstract/10.3844/ofsp.12054)

    BARCELOS, Allan; RIGO, Sandro José. Enhancing Brazilian Portuguese Textual Entailment Recognition with a Hybrid Approach. Journal of Computer Science, [s. l.], 2018.
        
---

## Conjunto de dados utilizado para treinamento do algoritmo

O conjunto foi disponibilizado na tarefa ASSIN do evento PROPOR de 2016 ([Disponível aqui](
http://nilc.icmc.usp.br/assin/)) e conta com 10.000 pares de sentenças para português do Brasil e de Portugal, as quais são divididas em 60% para treinamento e 40% para teste.

Uma vez que o foco da abordagem é somente português do Brasil, utilizamos apenas as sentenças anotadas para a língua escolhida. 
Deste modo, houve apenas 3000 dados para treinamento e 2000 para teste. 

As métricas utilizadas para mensurar o desempenho foram a Correlação de Pearson e o Erro Quadrático Médio.

## Atributos extraídos das sentenças

| Índice |                                   Atributo                                  |
|:------:|:---------------------------------------------------------------------------:|
|    1   | Substituição de sinônimos                                                   |
|    2   | Substituição dos hipônimos e hiperônimos nas sentenças                      |
|    3   | Contagem de antônimos nas sentenças                                         |
|    4   | Proporção de palavras diferentes entre as sentenças                         |
|    5   | Proporção de ngramas em comum das sentenças                                 |
|    6   | Proporção de palavras em comum entre as sentenças                           |
|    7   | Coeficiente de penalização pelo tamanho das sentenças                       |
|    8   | Similaridade do cosseno entre a soma dos word embeddings                    |
|    9   | Distância euclidiana entre o primeiro componente principal de cada sentença |
|   10   | Similaridade do cosseno entre os vetores TF-IDF de cada sentença            |

## Algoritmos utilizados nos experimentos

* Generalized Linear Models (GLM);
* Redes Neurais Artificiais (RNA);
* Support Vector Machines (SVM).

## Técnicas utilizadas

* Term Frequency - Inverse Document Frequency (TF-IDF);
* Ngramas;
* Principal Component Analysis (PCA);
* Global Representation of Word Vectors (GloVe) proposto por Pennington, Socher e Manning (2014).

## Recursos utilizados:
* Thesaurus para o Português do Brasil (MAZIERO et al.,  2008);

* Portuguese Unified Lexical Ontology (SIMÕES; GUINOVART, 2014);

* Word embeddings obtidos com o GloVe - [Disponível aqui](https://drive.google.com/open?id=1jl5Hrx7_2qLQ_-akx9Gh8dpJjS2H9bHT)

## Fluxo de execução dos scripts:

A abordagem contida neste repositório contempla somente os itens contidos dentro da área destacada.

![alt text](https://github.com/albarsil/unists/blob/master/model-train.png "teste")

## Fluxo para utilização na web:

A aplicação web esta disponível no repositório [unists-website](https://github.com/albarsil/unists-website);

O web service desenvolvido encontra-se no repositório [unists-webservice](https://github.com/albarsil/unists-webservice-textmining).

O fluxo idealizado para utilização conjunta das informações é descrita na figura abaixo:

![alt text](https://github.com/albarsil/unists/blob/master/model-test.png "teste")


## Referências

MAZIERO, E. G.; PARDO, T. a. S.; Di Felippo, A.; SILVA, B. C. Dias-da. A base de dados lexical e a interface web do TeP 2.0. In: BRAZILIAN SYMPOSIUM ON  MULTIMEDIA AND THE WEB, 2008, New York, USA. Anais... ACM Press, 2008. p. 390.

PENNINGTON, J.; SOCHER, R.; MANNING, C. Glove: global vectors for word representation. In: CONFERENCE ON EMPIRICAL METHODS IN NATURAL LAN-  GUAGE PROCESSING, 2014, Stroudsburg, USA. Anais...  Association for Computational Linguistics, 2014.  p.  1532–1543.

SIMÕES, A.; GUINOVART, X. Bootstrapping a Portuguese WordNet from Galician, Spanish and English Wordnets. In: INTERNATIONAL CONFERENCE OF ADVANCES IN SPEECH AND LANGUAGE TECHNOLOGIES FOR IBERIAN LANGUAGES, 2014, Cham, Germany.  Anais...  Springer International Publishing, 2014.  p.  239–248.
