'''
Created on Oct 14, 2014

@author: Nirmalkumar
'''

import numpy as np
from sklearn.cross_validation import train_test_split,KFold
from sklearn.metrics import accuracy_score,precision_score,recall_score,roc_curve, auc,f1_score
import pylab as pl
from collections import defaultdict
from math import log
import operator

"""
readInputFile- reads the given file in list
"""
def readInputFile(fileName):
    with open(fileName,'r') as f:
        lines = f.readlines()
    print 'read ',len(lines),' documents'
    return lines

"""
nameDoc - the first word of document tells whether its ham or spam
uses this to generate X,Y
"""
def nameDoc(lines):
    y=[]
    x=[]
    for line in lines:
        y += ['spam' if line.split()[0] == 'spam' else 'ham']
        x += [" ".join([word.lower() for word in line.split()[1:]])]
    return (x,y)
"""
splitTrainTest - splits in to train and test set
"""
def splitTrainTest(a,b,testSize=0.30):
    a_train, a_test, b_train, b_test = train_test_split(a, b, test_size=testSize, random_state=42)
    return(a_train,a_test,b_train,b_test)
"""
calculateAlpha - used to calculate spam ham values for given word
"""
def calculateAlpha(docDict,y,total_spam_words,total_ham_words,smoothing=1.):
    #split doc into spam and ham
    spam_sum = sum([docDict[doc_id] for doc_id in docDict if y[doc_id] == 'spam'])
    ham_sum = sum([docDict[doc_id] for doc_id in docDict if y[doc_id] == 'ham'])
    return ((spam_sum+smoothing)/(total_spam_words+2.*smoothing),(ham_sum+smoothing)/(total_ham_words+2.*smoothing))
    
"""
 naiveBayesClassifier estimate parameters with with X and Y values
"""
def naiveBayesClassifier(x,y):
    word_dict = defaultdict(lambda: defaultdict(lambda: 0))
    parameters = {}
    total_spam_words = 0
    total_ham_words = 0
    for doc_id in range(0,len(x)):
        for word in x[doc_id].split():
            if y[doc_id] == 'spam': total_spam_words += 1 #to count spam and ham words
            else:  total_ham_words += 1
            word_dict[word][doc_id] = word_dict[word][doc_id] + 1
    
    for word in word_dict:
        parameters[word] = calculateAlpha(word_dict[word],y,total_spam_words,total_ham_words)
    
    #calculate priors
    prior_spam = len([val for val in y if val=='spam'])/float(len(y))
    prior_ham = len([val for val in y if val=='ham'])/float(len(y))
    return (parameters,prior_spam,prior_ham)
"""
predict - predicts spam or ham with given value of X
"""
def predict(x,parameters,prior_spam,prior_ham):
    y=[]
    for doc in x:
        ham_meter =0.
        spam_meter =0.
        for word in doc.split():
            if word in parameters: #ignore unseen words
                spam_meter += log(parameters[word][0])
                ham_meter += log(parameters[word][1])
        y +=['ham' if spam_meter*prior_spam <= ham_meter*prior_spam else 'spam']
    return y 

"""
metrics - used to calculate various metrics like
precision recall f-measure accuracy
"""
def metrics(y_test,y_predict,showplot=False):
    yhat=[]
    y=[]
    for i in range(0,len(y_test)):
        y+=[1 if y_test[i] == 'spam' else 0]
        yhat+=[1 if y_predict[i] == 'spam' else 0]
        
    fpr, tpr, thresholds = roc_curve(y,yhat)
    roc_auc = auc(fpr, tpr)
    accuracy = accuracy_score(y,yhat)
    precision = precision_score(y,yhat)
    recall = recall_score(y,yhat)
    f1= f1_score(y,yhat)
    
    
    if(showplot):
        pl.clf()
        pl.plot(fpr, tpr, label='ROC curve (area = %0.2f)' % roc_auc)
        pl.plot([0, 1], [0, 1], 'k--')
        pl.xlim([0.0, 1.0])
        pl.ylim([0.0, 1.0])
        pl.xlabel('False Positive Rate')
        pl.ylabel('True Positive Rate')
        pl.title('Receiver operating characteristic example')
        pl.legend(loc="lower right")
        pl.show()
    return (accuracy,precision,recall,f1,roc_auc)    

def prettyPrint(predict):
    print '**********************************************************************'    
    print 'Accuracy\t',predict[0]
    print 'Precision\t',predict[1]
    print 'Recall\t', predict[2]
    print 'F measure\t',predict[3]
    print "Area under the ROC curve\t" ,predict[4]
    print '**********************************************************************'  
   
    
def crossValidation(x,y,folds=5):
    print folds,' fold cross validation'
    x = np.array(x)
    y = np.array(y)
    kf = KFold(len(y), n_folds=folds)
    t = (0,0,0,0,0)
    for train, test in kf:
        parameters,prior_spam,prior_ham = naiveBayesClassifier(x[train],y[train])
        y_predict = predict(x[test],parameters,prior_spam,prior_ham)
        m = metrics(y[test],y_predict)
        t =tuple(map(operator.add,t ,m ))
        
    return tuple([e/float(folds) for e in t])

"""
main - this is where program starts execution
"""
def main():
    fileName ='SMSSpamCollection'
    lines = readInputFile(fileName)
    x,y=nameDoc(lines)
    x_train,x_test,y_train,y_test= splitTrainTest(x,y)
    parameters,prior_spam,prior_ham = naiveBayesClassifier(x_train,y_train)
    y_predict = predict(x_test,parameters,prior_spam,prior_ham)
    mert = metrics(y_test,y_predict,True)
    prettyPrint(mert)
    avgmert = crossValidation(x,y)
    prettyPrint(avgmert)
"""
call to main
"""
if __name__ == '__main__':
    main()
