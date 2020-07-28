#This routine plots out some useful images to help undertand and diagnose your results

import matplotlib.pyplot as plt
import numpy as np
import satpy
import Visualisation as vis
from satpy import Scene
import glob
from matplotlib import colors as mcolors

def norm(band):
    """ Normalises the bands for the false color image"""
    band_min, band_max = band.min(), band.max()
    return ((band - band_min) / (band_max - band_min))

filenames = glob.glob('/g/data/k10/cp2786/fouthyear/S3A_SL_1_RBT____20170418T101506_20170418T101806_20181003T020407_0180_016_336______LR1_R_NT_003.SEN3/*')

def plot_slstr(filenames,mask):

    scn = Scene(filenames=filenames, reader='slstr_l1b')
    scn.load(['S1_an', 'S2_an', 'S3_an', 'S4_an', 'S5_an', 'S6_an', 'S7_in',
              'S8_in', 'S9_in', 'bayes_an', 'bayes_in', 'cloud_an',
              'latitude_an', 'longitude_an',
              'satellite_zenith_angle_n',
              'solar_zenith_angle_n','confidence_an',
    ])
    print('scn info',scn)

    #plot out the channels
    plt.imshow(scn['S1_an'], cmap='gray',vmin=0,vmax=100)
    plt.colorbar()
    plt.title('S1')
    plt.show()
    
    plt.imshow(scn['S2_an'], cmap='jet',vmin=0,vmax=100)
    plt.colorbar()
    plt.title('S2')
    plt.show()
    
    
    S1 = np.nan_to_num(scn['S1_an'].values)
    S2 = np.nan_to_num(scn['S2_an'].values)
    S3 = np.nan_to_num(scn['S3_an'].values)
    S4 = np.nan_to_num(scn['S4_an'].values)
    S5 = np.nan_to_num(scn['S5_an'].values)
    S6 = np.nan_to_num(scn['S6_an'].values)
    CAN = np.nan_to_num(scn['confidence_an'].values)

#create some false colour plots
    
# this does more natural colours    
#    green = norm(S1)
#    red = norm(S2)
#    IR = norm(S3 + S4 + S5 + S6)
#    blue = norm(0.8 * green - 0.1 * red - 0.1 * IR)

    
    red = norm(S5) #1.6
    green = norm(S3) #.87
    blue = norm(S2) # .67

    rgb = np.dstack((red, green, blue))
    brightness=0.2
    hsv = mcolors.rgb_to_hsv(rgb)
    hsv[:, :, 2] += brightness
    rgb = mcolors.hsv_to_rgb(hsv)
    rgb[rgb > 1] = 1
    print('here a')
    LatPos = str(round(np.array(scn['latitude_an'].values)[0, 0], 6))
    LonPos = str(round(np.array(scn['longitude_an'].values)[0, 0], 6))
    FileStr='xx'
    TitleStr = '(' + LatPos + ', ' + LonPos + ')\n' + FileStr
    #   if plot is True:
    plt.figure()
    print('here b')
    plt.imshow(rgb)
    
    plt.title('False colour image\n' + TitleStr)
    plt.show()

#now mask it    
    #plt.subplot(1,2,2)
    if mask is not None:
        mask = mask.astype('bool')
        red[mask] = 254 / 255
        green[mask] = 253 / 255
        blue[mask] = 185 / 255
    
    rgb = np.dstack((red, green, blue))
    brightness=0.2
    hsv = mcolors.rgb_to_hsv(rgb)
    hsv[:, :, 2] += brightness
    rgb = mcolors.hsv_to_rgb(hsv)
    rgb[rgb > 1] = 1
    print('here a')
    LatPos = str(round(np.array(scn['latitude_an'].values)[0, 0], 6))
    LonPos = str(round(np.array(scn['longitude_an'].values)[0, 0], 6))
    FileStr='xx'
    TitleStr = '(' + LatPos + ', ' + LonPos + ')\n' + FileStr
    #   if plot is True:
    plt.imshow(rgb)
    plt.title('False colour image masked\n' + TitleStr)
    plt.show()

    # now plot snow mask
    #0.4 thresholf
    ndsi=(S2-S5)/(S2+S5)

    plt.figure()
    print('here b')
    plt.imshow(ndsi)
    plt.colorbar()
    plt.title('NDSI image\n' + TitleStr)
    plt.show()

    
    bitmeanings = {
            'Coastline': 1,
            'Ocean': 2,
            'Tidal': 4,
            'Dry land': 24,
            'Inland water': 16,
            'Cosmetic': 256,
            'Duplicate': 512,
            'Day': 1024,
            'Twilight': 2048,
            'NDSI snow': 8192}

#plot the masks

    mask_ocean= CAN & 2
    plt.imshow(mask_ocean)
    plt.title('mask ocean\n')
    plt.show()


    mask_dryland= CAN & 24
    plt.imshow(mask_dryland)
    plt.title('mask dryland\n')
    plt.show()


    mask_coastline= CAN & 1
    plt.imshow(mask_coastline)
    plt.title('mask coastline\n')
    plt.show()


    mask_ndsi= CAN & 8192
    plt.imshow(mask_ndsi)
    plt.title('mask NSDI\n')
    plt.show() 
    
