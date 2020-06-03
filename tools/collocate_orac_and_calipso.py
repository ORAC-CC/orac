#
#
'''this routine collocates 2 files  SLSTR ORAC file and calipso and outputs an array of matches and write matches to a netcdf file will work on 1km or 5km calipso data'''
#
import sys # command line argument
import numpy as np 
from netCDF4 import Dataset as NetCDFFile 
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
import xarray as xr
#import os.path
import matplotlib.pyplot as plt
import math
import os
import traceback
import xml.etree.ElementTree as ET
from datetime import datetime, timedelta
import h5py
import requests
from tqdm import tqdm

from pyhdf.SD import SD, SDC

import convj
from netCDF4 import Dataset
# set calfive to 1 if using 5km calipso data
calfive=0
#set the version of slstr as V2 and V3 have different variables
version='V3'
#from cal_l2tau2cth import cal_l2tau2cth
#Read in files from command line
#NB file called is first argument
narg=len(sys.argv)
print('narg',narg)
if narg > 1:
    SLSTR_filename=sys.argv[1]
else:
    SLSTR_filename='/g/data//k10/cp2786/cci_data/l2/2008/06/ESACCI-L2-CLOUD-CLD-AATSR_CC4CL_Envisat_200806201027_fv20.0.primary.nc'
if narg > 2:
    Cfilename=sys.argv[2]
else:
    Cfilename='/g/data//k10/cp2786/calipso/2008/06/CAL_LID_L2_01kmCLay-Standard-V4-20.2008-06-20T10-27-43ZD.hdf'

#if calfive > 0:
#    Cfilename='/short/k10/cp2786/calipso/2008/06/CAL_LID_L2_01kmCLay-Standard-V4-20.2008-06-20T10-27-43ZD.hdf'

#write a collocated netcdf file into this directory
dirout='/g/data3/k10/cp2786/cci_data/colloc_files/'
#extract rooname for using later

base=os.path.basename(SLSTR_filename)
base=os.path.splitext(base)[0]
base=os.path.splitext(base)[0]
fileroot=os.path.splitext(base)[0]
print('fileroot',fileroot)


# Load SLSTR data
#read in level2 cloud file
#if you are reading inanother type of file you may need to read in different variables
nc = NetCDFFile(SLSTR_filename)

newslat = nc.variables['lat'][:,:]
newslon = nc.variables['lon'][:,:]
print('type newslon0',len(newslon[1]))
print('type newslon1',len(newslon))

nlon=len(newslon[1])
nlat=len(newslon)

satcth = nc.variables['cth'][:,:] # cloud top height
satcthc = nc.variables['cth_corrected'][:,:] # cloud top height corrected
print('type cth0',len(satcth[1]))
print('type cth1',len(satcth))


satctt = nc.variables['ctt'][:,:] # cloud top temperature
satctp = nc.variables['ctp'][:,:] # cloud top pressue
if version == 'V2':
    satcph = nc.variables['phase'][:,:] # cloud phase
else:
    satcph = nc.variables['ann_phase'][0,:,:] # cloud phase
satcot = nc.variables['cot'][:,:] # cloud optical thickness
satcer = nc.variables['cer'][:,:] # cloud effective radius
satcfc = nc.variables['cc_total'][:,:] # cloud fraction
satlsf = nc.variables['lsflag'][:,:] # land sea flag
satsol = nc.variables['solar_zenith_view_no1'][:,:]
satsatz = nc.variables['satellite_zenith_view_no1'][:,:]
sattime = nc.variables['time'][:,:]
cost_ja = nc.variables['costja'][:,:]
cost_jm = nc.variables['costjm'][:,:]
satcost=cost_jm*0.
satcost=cost_ja+cost_jm

nc.close()
print('close file start looping',nlon,nlat)
sathours=sattime*0.
#[[0 for mm in range(nlat)] for mm in range(nlon)] 
#extract the time from the file
for mm in range(0,nlon):
    for nn in range(0,nlat):

        sathours[nn,mm]=convj.jd_to_date(sattime[nn,mm])[2]-math.floor(convj.jd_to_date(sattime[nn,mm])[2])

#Now read in the active calipso data

print('end loop')

if Cfilename.endswith('f'):
    
    file=SD(Cfilename)
    print(file.info())
    
    datasets_dic = file.datasets()
    #check what is in the file first
    for idx,sds in enumerate(datasets_dic.keys()):
        print(idx,sds)

# Load Calipso coords
sds_obj = file.select('Latitude') # select sds
clat = sds_obj.get() # get sds data

sds_obj = file.select('Longitude') # select sds
clon = sds_obj.get() # get sds data
# Profile_Time
#Time, expressed in International Atomic Time (TAI). Units are in seconds, starting from January 1, 1993. For the 5 km profile products, three values are reportsed: the time for the first pulse included in the 15 shot average; the time at the temporal midpoint (i.e., at the 8th of 15 consecutive laser shots); and the time for the final pulse.
sds_obj = file.select('Solar_Zenith_Angle')
calsol= sds_obj.get() 
sds_obj = file.select('IGBP_Surface_Type')
igbp= sds_obj.get() 
sds_obj = file.select('Feature_Classification_Flags')
fsf= sds_obj.get() 
sds_obj = file.select('DEM_Surface_Elevation')
dem= sds_obj.get() 
sds_obj = file.select('Snow_Ice_Surface_Type')
sis= sds_obj.get() 
sds_obj = file.select('Number_Layers_Found')
nl= sds_obj.get() 
sds_obj = file.select('Layer_Top_Altitude')
cth= sds_obj.get() 

sds_obj = file.select('Layer_Top_Pressure')
ctp= sds_obj.get() 
sds_obj = file.select('Midlayer_Pressure')
mctp= sds_obj.get() 
sds_obj = file.select('Opacity_Flag')
opf= sds_obj.get() 
sds_obj = file.select('Profile_UTC_Time')
time= sds_obj.get()


#if we are using the 5km Calipso file then there are a few more variables that can be read in

if calfive == 1:
    sds_obj = file.select('Ice_Water_Path')
    iwp=sds_obj.get()
    sds_obj = file.select('Column_Optical_Depth_Cloud_532')
    cot=sds_obj.get()
    print('cot',cot)
    print('shape cot',np.shape(cot))

    sds_obj = file.select('Layer_Base_Altitude') # select sds
    base_alt = sds_obj.get() # get sds data
    sds_obj = file.select('Layer_Top_Altitude') # select sds
    top_alt = sds_obj.get() # get sds data ssLayer_Base_Altitude

    print('top_alt',top_alt)
    print('shapetop_alt',np.shape(top_alt))
    sds_obj = file.select('Column_Optical_Depth_Cloud_532')
    opd = sds_obj.get() # opd profile
    print('opd',opd)
    print('shape opd',np.shape(opd))
file.end()

# initialise variables
cnlat=len(time)
cnlon=len(time[1])


calhours=time*0.0
#[[0 for mm in range(nlat)] for mm in range(nlon)] 
for mm in range(0,cnlon):
    for nn in range(0,cnlat):
        #print(mm,nn)
        calhours[nn,mm]=time[nn,mm]-math.floor(time[nn,mm]) 



snlat=len(newslon)    # 3 rows in your example
snlon = len(newslon[1]) # 
print('sat nlat nlon',snlat,snlon)

cnlat=len(clat)    # 3 rows in your example
cnlon = len(clon[1]) # 
print('calipso nlat nlon',cnlat,cnlon)



#mask top of Satellte file because no Calips matches outside this range this makes it run faster!
minlat=65 # 70.
maxlat=85 #72.   
#absolute to account for winter
#mask1=(abs(newslat) > minlat)  &  (abs(newslat) < maxlat)

    # Want the latitude and longitude to be within 250m of each others
    # 250m = 0.00224577793 degrees lon at equator
    # 250m = 0.00224577793 / cos(lat) degrees lon at lat

if calfive == 1:
    lattolerance = 0.00224577793
    lattolerance = 0.00224577793*12.#3km
else:
    lattolerance = 0.00224577793
    lattolerance = 0.00224577793*4.31km
#initialise variables
timediffx=[]
calsolx=[] 
igbpx=[] 
fsfx=[] 
demx=[] 
sisx=[] 
cotx=[] 
iwpx=[] 
topaltx=[]
basealtx=[]

nlx=[] 
cthx=[] 
ctpx=[] 
mctpx=[] 
opfx=[] 
timex=[]
callatx=[]
callonx=[]

satcthx=[]
satcthcx=[]
satcttx=[]
satctpx=[]
satcotx=[]
satcerx=[]
satcfcx=[]
satlsfx=[]
satcphx=[]
satsolx=[]
satsatzx=[]
satcostx=[]
satlatx=[]
satlonx=[]

for i in range(cnlat):
    if (abs(clat[i,0]) > minlat) & (abs(clat[i,0]) < maxlat): 
        
#look for coll               
                #print(abs(slatnew-clat[i,0]))
        mask3=abs(newslat-clat[i,0]) < lattolerance       
        mask4=abs(newslon-clon[i,0]) < lattolerance       
        matchmask= mask3 & mask4

        ngood=np.sum(matchmask)
        ncth=np.sum(satcth)
        nslat=np.sum(newslat)

        #only write out if you get a match   
        if ngood > 0:

            print('matched',sum(newslat[matchmask])/ngood,sum(newslon[matchmask])/ngood,clat[i,0],clon[i,0])

            timediff=sum(sathours[matchmask])/ngood -  calhours[i,0]
            timediffx.append(timediff)
            
            satcthx.append(sum(satcth[matchmask])/ngood)
            satcthcx.append(sum(satcthc[matchmask])/ngood)
            satcttx.append(sum(satctt[matchmask])/ngood)
            satctpx.append(sum(satctp[matchmask])/ngood)
            satcotx.append(sum(satcot[matchmask])/ngood)
            satcerx.append(sum(satcer[matchmask])/ngood)
            satcfcx.append(sum(satcfc[matchmask])/ngood)
            satlsfx.append(sum(satlsf[matchmask])/ngood)
            satcphx.append(sum(satcph[matchmask])/ngood)
            satsolx.append(sum(satsol[matchmask])/ngood)
            satsatzx.append(sum(satsatz[matchmask])/ngood)
            satcostx.append(sum(satcost[matchmask])/ngood)
            satlatx.append(sum(newslat[matchmask])/ngood)
            satlonx.append(sum(newslon[matchmask])/ngood)

            calsolx.append(calsol[i,0]) 
            igbpx.append(igbp[i,0]) 
            fsfx.append(fsf[i,0]) 
            demx.append(dem[i,0]) 
            sisx.append(sis[i,0]) 
            nlx.append(nl[i,0]) 
            cthx.append(cth[i,0]) 
            
            ctpx.append(ctp[i,0]) 
            mctpx.append(mctp[i,0]) 
            opfx.append(opf[i,0]) 
            callatx.append(clat[i,0])            
            callonx.append(clon[i,0])

            if calfive == 1:
                cotx.append(cot[i,0]) 
                iwpx.append(iwp[i,0]) 
                topaltx.append(top_alt[i,0]) 
                basealtx.append(base_alt[i,0]) 
            
# now write out the data in a collocation netcdf or text filefile
#write netcdf file
# the output array to write will be nx x ny



fileout=dirout+'colloc_'+fileroot+'.nc'
if calfive == 1:
    fileout=dirout+'fivekm_colloc_'+fileroot+'.nc'
print('fileout',fileout)
nx=len(satsolx)
print('nx',nx)
# open a new netCDF file for writing.
dataset = Dataset(fileout,'w',format='NETCDF4_CLASSIC') 
# create the output data.

#data_out.shape = (nx,ny) # reshape to 2d array
# create the x and y dimensions.
dataset.createDimension('nx',nx)
#dataset.createDimension('y',ny)
# create the variable (4 byte integer in this case)
# first argument is name of variable, second is datatype, third is
# a tuple with the names of dimensions.
nx = dataset.createVariable('point',np.int32,('nx',))

sat_cth = dataset.createVariable('satcth',np.float32,('nx',))
sat_cthc = dataset.createVariable('satcthc',np.float32,('nx',))
sat_ctt = dataset.createVariable('satctt',np.float32,('nx',))
sat_ctp = dataset.createVariable('satctp',np.float32,('nx',))
sat_cer = dataset.createVariable('satcer',np.float32,('nx',))
sat_cot = dataset.createVariable('satcot',np.float32,('nx',))
sat_cfc = dataset.createVariable('satcfc',np.float32,('nx',))
sat_cph = dataset.createVariable('satcph',np.float32,('nx',))
sat_sol = dataset.createVariable('satsol',np.float32,('nx',))
sat_satz = dataset.createVariable('satsatz',np.float32,('nx',))
sat_cost = dataset.createVariable('satcost',np.float32,('nx',))
sat_lon = dataset.createVariable('satlon',np.float32,('nx',))
sat_lat = dataset.createVariable('satlat',np.float32,('nx',))
sat_lsf = dataset.createVariable('satlsf',np.float32,('nx',))

calsol=dataset.createVariable('calsol',np.float32,('nx',)) 
igbp=dataset.createVariable('igbp',np.float32,('nx',)) 
fsf=dataset.createVariable('fsf',np.int8,('nx',)) 
dem=dataset.createVariable('dem',np.float32,('nx',)) 
sis=dataset.createVariable('sis',np.float32,('nx',)) 
nl=dataset.createVariable('nl',np.float32,('nx',)) 
cth=dataset.createVariable('cth',np.float32,('nx',)) 

ctp=dataset.createVariable('ctp',np.float32,('nx',)) 
mctp=dataset.createVariable('mctp',np.float32,('nx',)) 
opf=dataset.createVariable('opf',np.float32,('nx',)) 
time=dataset.createVariable('time',np.float32,('nx',))            
callat=dataset.createVariable('callat',np.float32,('nx',)) 
callon=dataset.createVariable('callon',np.float32,('nx',)) 
timediff=dataset.createVariable('timediff',np.float32,('nx',)) 

if calfive==1:
    cot=dataset.createVariable('cot',np.float32,('nx',)) 
    iwp=dataset.createVariable('iwp',np.float32,('nx',)) 
    basealt=dataset.createVariable('basealt',np.float32,('nx',)) 
    topalt=dataset.createVariable('topalt',np.float32,('nx',)) 

# write data to variable.
sat_cth[:] = satcthx
sat_cthc[:] = satcthcx
sat_ctt[:] = satcttx
sat_ctp[:] = satctpx
sat_cer[:] = satcerx
sat_cot[:] = satcotx
sat_cfc[:] = satcfcx
sat_cph[:] = satcphx
sat_sol[:] = satsolx
sat_satz[:] = satsatzx
sat_cost[:] = satcostx
sat_lat[:] = satlatx
sat_lon[:] = satlonx
sat_lsf[:] = satlsfx

calsol[:] = calsolx
igbp[:] =igbpx
fsf[:] =fsfx
dem[:] =demx
sis[:] =sisx
nl[:] =nlx
cth[:] =cthx
ctp[:] =ctpx
mctp[:] =mctpx
opf[:] =opfx
#time[:] =timex
callat[:] =callatx
callon[:] =callonx
timediff[:] =timediffx

if calfive==1:
    cot[:] =cotx
    iwp[:] =iwpx
    topalt[:] =topaltx
    basealt[:] =basealtx

# close the file.
dataset.close()
print ('*** SUCCESS writing collocated AATSR/SLSTR and calipso.nc!')


#label=['xx']
    # this is for plotting purpose
#    index = np.arange(len(label))
#    plt.bar(index, no_movies)
#    plt.xlabel('Genre', fontsize=5)
#    plt.ylabel('No of Movies', fontsize=5)
#    plt.xticks(index, label, fontsize=5, rotation=30)
#    plt.title('Market Share for Each Genre 1995-2017')
#    plt.show() 
