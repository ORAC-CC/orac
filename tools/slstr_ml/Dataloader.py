
##############################################
# (c) Copyright 2018-2019 Kenza Tazi and Thomas Zhu
# This software is distributed under the terms of the GNU General Public
# Licence version 3 (GPLv3)
##############################################

import os
import platform
import random as rdm
from getpass import getuser
from glob import glob
from time import time

import matplotlib.pyplot as plt
import numpy as np
from pyhdf.SD import SD, SDC
from satpy import Scene


def get_random_SLSTR():
    if os.path.exists('/vols/lhcb/egede/cloud'):
        q = os.listdir('/vols/lhcb/egede/cloud/SLSTR/2018')
        random_month = rdm.choice(q)
        w = os.listdir('/vols/lhcb/egede/cloud/SLSTR/2018/' + random_month)
        random_file = rdm.choice(w)
    return('/vols/lhcb/egede/cloud/SLSTR/2018/' + random_month + '/' + random_file)




def upscale_repeat(x, h=2, w=2):
    """
    Upscales an array, credit to https://stackoverflow.com/questions/46215414/upscaling-a-numpy-arr\
ay-and-evenly-distributing-values
    """
    return(x.repeat(h, axis=0).repeat(w, axis=1))


def fixdir(list_in):
    for i in range(len(list_in)):
        list_in[i] = list_in[i].replace('\\', '/')
    return(list_in)


def path_to_public():
    user = getuser()
    path = ("/home/hep/" + str(user) + "/public_html")
    return(path)

def scene_loader(path):
    # Returns a satpy scene object from the provided file
    if path[-1] == '/':
        path = path + "*"
    elif path[-1] == '*':
        pass
    else:
        path = path + "/*"

    olddir = os.getcwd()

    if platform.platform().startswith("Windows-10"):
        string1 = "S3A_SL_1"
        index = path.find(string1)
        if index == 0:
                   pass
        else:
            newdir = path[:index]
            os.chdir(newdir)
            path = path[index:]
    filenames = glob(path)
    filenames = fixdir(filenames)
    scn = Scene(filenames=filenames, reader='slstr_l1b')
    os.chdir(olddir)
    return(scn)
    

def summary(scene, filenames=None, saveimage=False, outputpath='public'):
    # Loads positional S1_n channel data. Prints lat/lon of corner pixel
    # If saveimage is True, saves png to current directory with metadata
    scene.load(['S1_an', 'latitude', 'longitude'])
    lat = scene['latitude'].values[0][0]  # Latitude of corner pixel
    lon = scene['longitude'].values[0][0]  # Longitude of corner pixel
    if saveimage is not False:
        if outputpath == 'public':
            # cd to public folder
            os.chdir(path_to_public())
        if filenames is not None:
            imagename = ('S1n_'
                         + str(filenames[0][:31])
                         + '_'
                         + str(filenames[0][82:94])
                         + '-('
                         + str(lat)
                         + ','
                         + str(lon)
                              + ')')
        else:
            imagename = 'test'
        scene.save_dataset('S1_an', str(imagename) + '.png')
    print(str(lat) + ', ' + str(lon))


def makepltimage(scene, channel='S1_an'):
    # Use matplotlib to produce image of specified channel
    scene.load([channel])
    data = scene[channel].values
    data = np.nan_to_num(data)
    plt.figure()
    plt.imshow(data, cmap='gray')
    def makepngimage(scene, channel='S1_an', outputpath='public'):
    if outputpath == 'public':
        # cd to public folder
        os.chdir(path_to_public())
    scene.save_dataset(channel, str(time()) + '.png')


def norm(band):
    """ Normalises the bands for the false color image"""
    band_min, band_max = band.min(), band.max()
    return ((band - band_min) / (band_max - band_min))


def extract_mask(Sreference, MaskFile, MaskBit):
    if type(Sreference) == str:
        scn = scene_loader(Sreference)
    else:
        scn = Sreference

    scn.load([MaskFile])

    mask = np.nan_to_num(scn[MaskFile].values)
    if MaskFile.endswith('in'):
        mask = upscale_repeat(mask)

    mask = mask.astype(int)
    mask = mask & MaskBit
    mask = mask / MaskBit
    mask = np.ones(mask.shape) - mask
    return(mask)

def load_hdf(filename):
    """Loads the hdf4 object into memory"""
    file = SD(filename, SDC.READ)
    return(file)


def get_header_names(file):
    """Print the names of the dataset names"""
    datasets_dic = file.datasets()
    for idx, sds in enumerate(datasets_dic.keys()):
        print(idx, sds)


def load_data(file, variable):
    """From the file, load the chosen variable. Valid options in get_header_names()"""
    sds_obj = file.select(variable)
    data = sds_obj.get()
    return(data)
     
def get_SLSTR_path(Sfilename):
    """For a given SLSTR filename, return path to a local copy of the file"""
    if os.path.exists('/vols/lhcb/egede/cloud'):
        Sfile_fragments = Sfilename.split('_')
        TimeString = Sfile_fragments[7]
        Year = TimeString[0:4]
        Month = TimeString[4:6]
        return('/vols/lhcb/egede/cloud/SLSTR/' + Year + '/' + Month + '/' + Sfilename)
    elif os.path.exists('D:/'):
        return('D:/SatelliteData/SLSTR/' + Sfilename)
        
        
class SDopener():
    # Class to call when using context manager
    def __init__(self, path, mode=SDC.READ):
        self.path = path
        self.mode = mode
        self.SD = SD(self.path, self.mode)

    def __enter__(self):
        return(self.SD)

    def __exit__(self, exc_type, exc_value, exc_traceback):
        self.SD.__del__()


def vfm_feature_flags(val):
    """ Python version of the IDL code to read the bitwise flags"""
    feature_type = val & 7
    return(feature_type)



               
