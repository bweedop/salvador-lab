import numpy as np
import pandas as pd
from keras.models import Sequential
from keras import layers
import matplotlib.pyplot as plt
from sklearn.preprocessing import LabelEncoder

def data_preprocessing ():
    # Load the data
    data = pd.read_csv("data/table_matrix_p_line_atr_tded.csv")
    # Change all behaviors by the copper ring to one classifier
    copper_behaviors = ['93', '83', '94', '92', '91', '82', '84', '81']
    for i in copper_behaviors:
        data.beh1_name = data.beh1_name.replace(i, "copper_beh")
    data.beh1_name = data.beh1_name.fillna("copper_beh")
    # initialize label encoder 
    lb_make = LabelEncoder()
    data["beh1_code"] = lb_make.fit_transform(data["beh1_name"])



def worm_plot (worm_ID = 1):
    fig = plt.figure(figsize=(16,4))
    plt.plot(data.frame[data.worm_ID == worm_ID], data.beh1_code[data.worm_ID == worm_ID])
    plt.savefig(''.join(["figures/pyworm-", str(worm_ID), "-behavior.png"]), dpi=fig.dpi)


    
