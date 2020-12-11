import nltk
import sklearn
import csv

corpus = []
with open('hasil sentimen/sentimen_covid/parangtritis/label_parangtritis_covid.csv', 'r', errors='ignore') as csv_file:
    csv_reader = csv.reader(csv_file)
    next(csv_reader)
    for line in csv_reader:
        corpus.append(line[3])

# initialize
clean_text = []

for row in corpus:
    clean_text.append(row)

all_text = clean_text

# sklearn countvectorizer
from sklearn.feature_extraction.text import CountVectorizer
# Convert a collection of text documents to a matrix of token counts
cv = CountVectorizer(ngram_range=(1,1))
# matrix of token counts
X = cv.fit_transform(all_text)
Xc = (X.T * X) # matrix manipulation
Xc.setdiag(0) # set the diagonals to be zeroes as it's pointless to be 1

import pandas as pd
names = cv.get_feature_names() # This are the entity names (i.e. keywords)
df = pd.DataFrame(data = Xc.toarray(), columns = names, index = names)
df.to_csv('coocurence_label_parangtritis_covid.csv', sep = ',')