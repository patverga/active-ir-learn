__author__ = 'pv'

from gensim.models.word2vec import LineSentence
import gensim.models as gm
import logging

sentences = LineSentence('out.docs')
bigrams = gm.Phrases(sentences)
trigrams = gm.Phrases(bigrams[sentences])

# for sentence in trigrams[sentences]:
#     print (sentence)


logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

model = gm.Word2Vec(trigrams[sentences], workers=8, size=500, window=10, negative=10, min_count=1)
# model.train(bigrams[sentences])
# model.train(sentences)
model.save("model")