#
#This routine using standard machine leaning software keras/tensorflow to develop a cloud mask for the SLSTR instrument. The code is written in python however ti should be relatively simple to follow if not ask for help.
#This code is based on the results decsribed in
#https://eartharxiv.org/fmw32/
# The collocated data set is courtesy of Kenza Taxi and Thomas Zhu, formerly imperial college London.


###

import pickle
import numpy as np
import matplotlib.pyplot as plt
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.layers.normalization import BatchNormalization

#
#load in the collocated data
#
with open('/g/data/k10/cp2786/fouthyear/1805P4.pkl', 'rb') as f: 
    data=pickle.load(f)
### and have a look at the shape.    
print('#---#---#---#---#---#---#---#---#---#---#---#---#---#')
print(data.shape)
print(data.keys())

 #you can see what is in the file by uncommenting this
 #dump out the variables in the file
#pickle.dump(data,f)

#
#create a variable that gives a random number that enables us to later split the data set randomly into a training and test data set

data['split'] = np.random.uniform(size=len(data))

# create the truth flag this is a specific bit out of the collocated file
data["cloud"] = (data["Feature_Classification_Flags"] & 7) - 1

#the data set is full of Nan values the next step removes them and create a clean data set
#have a read about the instrument this web link is a good start
#https://sentinel.esa.int/web/sentinel/missions/sentinel-3
#https://sentinel.esa.int/web/sentinel/technical-guides/sentinel-3-slstr/instrument
# which spectral range does each of the following variables'SX' correspond to?

#https://sentinel.esa.int/documents/247904/1872792/Sentinel-3-SLSTR-Product-Data-Format-Specification-Level-1

cleandata = data[data["S1_an"].notna() &
                 data["S2_an"].notna() &
                 data["S3_an"].notna() &
#                 data["S4_an"].notna() &
#                 data["S5_an"].notna() &
#                 data["S6_an"].notna() &
#                 data["S7_in"].notna() &
#                 data["S8_in"].notna() &
#                 data["S9_in"].notna() &
                 data["cloud"].notna()]

# divide up the data set for training and testing
train = cleandata[cleandata['split']<0.8]
validate = cleandata[cleandata['split']>=0.8]

# These are the parameter that can be use to train your data, try removing/adding some of the channels to understand the impact on the test data set, which are the most important values?
#A tensor is a generalization of vectors and matrices and is easily understood as a multidimensional array. 

train_variables = train[[
    "S1_an",
    "S2_an",
    "S3_an",]]
#    "S4_an",
#    "S5_an",
#    "S6_an",
#    "S7_in",
#    "S8_in",
#    "S9_in",]]

train_truth = train[["cloud"]]

validate_variables = validate[[
    "S1_an",
    "S2_an",
    "S3_an",]]
#    "S4_an",
#    "S5_an",
#    "S6_an",
#    "S7_in",
#    "S8_in",
#    "S9_in",]]

validate_truth = validate[["cloud"]]


# Create the model we choosae a sequntial model A Sequential model is appropriate for a plain stack of layers where each layer has exactly one input tensor and one output tensor.
# Instantiate model
model = Sequential()

# Now define the input later The input layer
model.add(Dense(16, input_dim=train_variables.shape[1]))

#Batch normalization is a technique for improving the speed, performance, and stability of artificial neural networks. Batch normalization was introduced in a 2015 paper. It is used to normalize the input layer by re-centering and re-scaling.
model.add(BatchNormalization())

#ReLU stands for rectified linear unit, and is a type of (default) activation function. we could also use a sigmoid as we are after a binary output.

model.add(Activation('relu'))

# set up the layers of the model, what happens if you change the number in the first (input)or second (hidden) layer? does this make a difference?

# A hidden layer
model.add(Dense(8, activation = 'relu'))

# The output layer
model.add(Dense(1, activation = 'sigmoid'))

# Build as a binary classification
model.compile(optimizer='rmsprop',
              loss='binary_crossentropy',
              metrics=['accuracy'])

# Train the model, iterating on the data in batches of 32 samples
# try changing the number of epochs and batch sizez how doe this effect your result.
# each epoch will come up with a new set of weightings (i.e the fingers between) layers each epoch these are minimised and this is what the loss function is providing information on.
# batchsize and epoch may appear to do the same thing but there is a subtle difference
#The batch size is a hyperparameter of gradient descent that controls the number of training samples to work through before the model’s internal parameters are updated.
#The number of epochs is a hyperparameter of gradient descent that controls the number of complete passes through the training dataset.

model.fit(train_variables, train_truth, epochs=4, batch_size=32, verbose=1)


#print out what the model looked like
model.summary()
# Ok now you have produced a NN save the model to  a file prehaps give it a name that reflectas how it has been set up. This model will be used later on on your data.

model.save('/g/data/k10/cp2786/fouthyear//my_model')


# Now Evaluate model on some independant  testing data You need to to this to confirm that you have not over trained your model. The result of this should be slightly less accurate then the training data set. If it is not then you have over trained your data.
print('shape validate_variables',validate_variables.shape)
validate_output = model.predict(validate_variables)


#https://towardsdatascience.com/understanding-auc-roc-curve-68b2303cc9c5
# now plot some diagnostic metrics.
# Create a ROC
from sklearn.metrics import roc_curve, auc

fpr_keras, tpr_keras, thresholds_keras = roc_curve(validate_truth,
                                                   validate_output)
auc_keras = auc(fpr_keras, tpr_keras)

fig, ax = plt.subplots(1)

ax.plot([0, 1], [0, 1], 'k--')
ax.plot(fpr_keras, tpr_keras, label='Trained NN')
ax.set_xlabel('False positive rate')
ax.set_ylabel('True positive rate')
ax.set_title('ROC curve')
ax.legend(loc='best')
plt.show()


#now you have a good feel for the data can you use the above machine learning technique to either identify seaice or use the output to help isolate the seaice in the image.

# comment in your code how you went about this
#advanced can you design a mask to identify sea ice from cloud? modify your code to demonstrate this and illustrate with a plot identifying the seaice.


# what about snow over land can you identify snow over land with high accuracy?
# what are the key issues limiting the accuracy of the algorithm.

#
# asssuming a fixed emissivity( define and justify the emissivity used) and a trasparent atmosphere ( a reasonable approximation in the Artic in cloud free conditions calculate the Ice/snow surface temperature.
