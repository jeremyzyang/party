### read and analyze text 
# load party corpus

from nltk.corpus import PlaintextCorpusReader

corpus_root = 'C:\Users\Jeremy\Desktop\Workspace\Python\party\party_democrat' # read in democrat 
d_corpus = PlaintextCorpusReader(corpus_root,'.*')

corpus_root = 'C:\Users\Jeremy\Desktop\Workspace\Python\party\speech_democrat' # read in democrat speech
d_speech_corpus = PlaintextCorpusReader(corpus_root,'.*')

corpus_root = 'C:\Users\Jeremy\Desktop\Workspace\Python\party\party_republican' # read in republican     
r_corpus = PlaintextCorpusReader(corpus_root,'.*')

corpus_root = 'C:\Users\Jeremy\Desktop\Workspace\Python\party\speech_republican' # read in republican speech
r_speech_corpus = PlaintextCorpusReader(corpus_root,'.*')


# define a function to count the frequency of key words in speech
def wordcount (corpus,keyword):  # arguments:corpus and keyword
    index = corpus.fileids()    # creat an index for indivicual txt file in the corpus
    counts = range(len(index))
    for i in range(len(index)):
        list_lower = [w.lower() for w in corpus.words(index[i])]  # change all to lowercase
        counts[i] = len([w for w in list_lower if w.startswith(keyword)]) 
    return counts

def length (corpus):
    index = corpus.fileids()
    length = range(len(index))
    for i in range(len(index)):
        length[i] = len(set(corpus.words(index[i])))
    return length

from operator import truediv

def density (corpus,keyword):
    density = map(truediv,wordcount(corpus,keyword), length(corpus))
    return density

wordcount(d_speech_corpus,'abort') # compute frequency
density(d_speech_corpus,'abort') # compute density

