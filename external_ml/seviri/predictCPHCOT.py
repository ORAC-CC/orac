"""
    This module contains functions to do neural network COT (CMA) and CPH 
    predictions based on SEVIRI measurements.
    
    - func _all_same(): Helper function. Checks whether all elements in a 
                      list are equal.
    - func prepare_input_array(): Bring arrays in the right format for the 
                                  Neural Network.
    - func getdata_dummy(): For DWD Servers. Load collocated dummy test data. 
    - func predict_ANN(): To be called to execute the prediction.
    
    First author: Daniel Philipp (DWD)
    ---------------------------------------------------------------------------
    2020/07/20, DP: Initial version
    2020/07/23, DP: Added a neural network driver file containing the
                    backend, paths and thresholds
    2020/08/13, DP: Added masking of invalid (space) pixels before prediction.
                    Only valid pixels are predicted to increase efficiency.
                    Implemented correct fill value usage.
    2020/08/18, DP: Implemented fully working uncertainty estimation.
"""

import neuralnet
import xarray as xr
import numpy as np
import readdriver
import os
import time
import logging
import helperfuncs as hf
from definitions import (SREAL_FILL_VALUE, BYTE_FILL_VALUE, SREAL,
                         IS_CLEAR, IS_CLOUD, IS_WATER, IS_ICE)
#import tensorflow as tf
import theano

fmt = '%(levelname)s : %(filename)s : %(message)s'
logging.basicConfig(level=logging.DEBUG,
                    format=fmt)

theano.gof.compilelock.set_lock_status(False)

def _prepare_input_arrays(vis006, vis008, nir016, ir039, ir062, ir073, ir087,
				      ir108, ir120, ir134, lsm, skt, networks):
    """ 
        Prepare input array for the neural network. Takes required feature 
        arrays, flattens them using row-major ordering and combines all flat 
        arrays into a single array with shape (nsamples, nfeatures) to be 
        used for prediction.
        
        Input:
        - vis006 (2d numpy array): SEVIRI VIS 0.6 um (Ch 1)
        - vis008 (2d numpy array): SEVIRI VIS 0.8 um (Ch 2)
        - nir016 (2d numpy array): SEVIRI NIR 1.6 um (Ch 3)
        - ir039 (2d numpy array):  SEVIRI IR 3.9 um  (Ch 4)
        - ir062 (2d numpy array):  SEVIRI WV 6.2 um  (Ch 5)
        - ir073 (2d numpy array):  SEVIRI WV 7.3 um  (Ch 6)
        - ir087 (2d numpy array):  SEVIRI IR 8.7 um  (Ch 7)
        - ir108 (2d numpy array):  SEVIRI IR 10.8 um (Ch 9)
        - ir120 (2d numpy array):  SEVIRI IR 12.0 um (Ch 10)
        - ir134 (2d numpy array):  SEVIRI IR 13.4 um  (Ch 11)
        - lsm (2d numpy array):    Land-sea mask
        - skt (2d numpy array):    (ERA5) Skin Temperature
        
        Return:
        - idata (2d numpy array): Scaled input array for ANN 
    """
    
    # Change VIS/IR range from [0,1] to [0,100] to match trained network
    vis006 = np.where(vis006 > SREAL_FILL_VALUE, vis006 * 100, vis006)
    vis008 = np.where(vis008 > SREAL_FILL_VALUE, vis008 * 100, vis008)
    nir016 = np.where(nir016 > SREAL_FILL_VALUE, nir016 * 100, nir016)

    # calculate channel differences
    ir087_108 = ir087 - ir108
    ir108_120 = ir108 - ir120
    
    # list of arrays must be kept in this order!
    data_lst = [
                ir039,     # 1    
                ir087,     # 2   
                ir087_108, # 3   
                ir108,     # 4    
                ir108_120, # 5   
                ir120,     # 6    
                ir134,     # 7    
                lsm,       # 8    
                nir016,    # 9    
                skt,       # 10   
                vis006,    # 11  
                vis008,    # 12  
                ir062,     # 13   
                ir073      # 14 
                ]
 
    # check if array dimensions are equal throughout all arrays
    # if all dimensions are equal: set dimension constants for reshaping
    xdims = []
    ydims = []
    for tmp in data_lst:
        xdims.append(tmp.shape[0])
        ydims.append(tmp.shape[1])
    
    if hf.all_same(xdims) and hf.all_same(ydims):
        xdim = data_lst[0].shape[0]
        ydim = data_lst[0].shape[1]
    else:
        msg = 'xdim or ydim differ between input arrays for neural network.'
        #logging.error(msg)
        raise Exception(RuntimeError, msg)
    
    #fill neural network input array with flattened data fields
    idata = np.empty((xdim*ydim, len(data_lst)))
    has_invalid_item = np.empty((xdim*ydim))
    
    for cnt, d in enumerate(data_lst):
        idata[:, cnt] = d.ravel()
          
    # check for each pixel if any channels is invalid (1), else 0
    has_invalid_item = np.any(np.where(idata < 0, 1, 0), axis=1)
    
    all_chs = np.array([ir039, ir087, ir108, ir120, 
               ir134, nir016, vis006, vis008, 
               ir062, ir073])
    
    # pixels with all channels invalid = 1, else 0
    all_channels_invalid = np.all(np.where(all_chs < 0, 1, 0), axis=0)
    # indices of pixels with all channels valid
    all_channels_valid_indxs = np.nonzero(~all_channels_invalid.ravel())
    # dictionary of invalid pixel masks
    masks = {'hii': has_invalid_item.reshape((xdim, ydim)), 
             'aci': all_channels_invalid,
             'acvi': all_channels_valid_indxs[0]}

    scaled_data = {'COT': networks['COT'].scale_input(idata),
                   'CPH': networks['CPH'].scale_input(idata)}

    # apply scaling to input array
    return scaled_data, (xdim, ydim), masks
    

def _select_networks():
    """ Read driver file and select correct network. """
    # read driver file for SEVIRI neural network
    # assume driver file is in same directory as this file
    basepath = os.path.dirname(os.path.realpath(__file__))
    drifile = 'nn_driver.txt'
    ptf = os.path.join(basepath, drifile)
    logging.info("SEVIRI ANN DRIVER: {}".format(ptf))

    opts = readdriver.parse_nn_driver(ptf)
    
    networks = {'COT': neuralnet.NetworkCOT(opts),
                'CPH': neuralnet.NetworkCPH(opts)}
    return networks


def _thresholding(prediction, variable, opts):
    """ Determine binary array by applying thresholding. """
    # read threshold from driver file content
    threshold = opts['{}_THRESHOLD'.format(variable)]
    
    # appply threshold   
    if variable == 'COT':
        bin = np.where(prediction > threshold, IS_CLOUD, IS_CLEAR)
    elif variable == 'CPH':
        bin = np.where(prediction > threshold, IS_ICE, IS_WATER)
        
    bin = bin.astype(SREAL)

    # mask pixels where regression array has fill value
    bin[prediction == SREAL_FILL_VALUE] = BYTE_FILL_VALUE
    
    return bin


def _unc_approx_cot_1(pred, th, unc_params):
    """ Calculate uncertainty for cloudy/ice pixels. """
    norm_diff = (pred-th) / (1-th)

    m = unc_params['m1']
    b = unc_params['b1']

    return m * norm_diff + b


def _unc_approx_cot_0(pred, th, unc_params):
    """ Calculate uncertainty for clear/water pixels """
    norm_diff = (pred-th) / th
    
    m = unc_params['m0']
    b = unc_params['b0']

    return m * norm_diff + b


def _uncertainty(prediction, binary, variable, opts):
    """ Calculate CMA/CPH uncertainy. """
    try:
        threshold = float(opts['{}_THRESHOLD'.format(variable)])
    except KeyError:
        msg = '{}_THRESHOLD not found in SEVIRI neural network driver file.'
        raise Exception(msg.format(variable))

    if variable == 'COT':
        unc_params = {'m1': opts['UNC_CLD_SLOPE'],
                      'b1': opts['UNC_CLD_INTERCEPT'],
                      'm0': opts['UNC_CLR_SLOPE'],
                      'b0': opts['UNC_CLR_INTERCEPT']
                          }
    elif variable == 'CPH':
        unc_params = {'m1': opts['UNC_WAT_SLOPE'],
                      'b1': opts['UNC_WAT_INTERCEPT'],
                      'm0': opts['UNC_ICE_SLOPE'],
                      'b0': opts['UNC_ICE_INTERCEPT']
                     }
    
    unc = np.where(binary > 0, 
                   _unc_approx_cot_1(prediction, threshold, unc_params),  # where cloudy
                   _unc_approx_cot_0(prediction, threshold, unc_params)   # where clear
                   )

    return unc.astype(SREAL)


def _check_prediction(prediction, opts, masks):
    """ Check neural net regression for invalid values. """
    # mask prediction values outside valid regression limits
    condition = np.logical_or(prediction > float(opts['VALID_REGRESSION_MAX']), 
                              prediction < float(opts['VALID_REGRESSION_MIN']))
    prediction = np.where(condition, SREAL_FILL_VALUE, prediction)

    # mask pixels where all channels are invalid (i.e. space pixels)
    prediction = np.where(masks['aci']==1, SREAL_FILL_VALUE, prediction)
    return prediction


def _postproc_prediction(prediction, variable, opts, masks): 
    """ Check for invalid predictions, apply thresholding and get uncertainty. """
    # regression 
    reg = _check_prediction(prediction, opts, masks)
    # binary cloud flag
    bin = _thresholding(prediction, variable, opts)
    # uncertainty
    unc = _uncertainty(prediction, bin, variable, opts)
    
    # penalize cases where at least 1 input variable is invalid with higher unc
    unc = np.where(masks['hii']==1, unc * 1.1, unc)
    # mask cases where all channels are invalid
    bin = np.where(masks['aci']==1, BYTE_FILL_VALUE, bin)
    unc = np.where(masks['aci']==1, SREAL_FILL_VALUE, unc)
           
    return {'reg': reg, 'bin': bin, 'unc': unc}


def _run_prediction(variable, networks, scaled_data, masks, dims):
    """ Run prediction with neural network. """
    # load correct model
    model = networks[variable].get_model()
  
    # select scaled data for correct variable 
    idata = scaled_data[variable]
    # predict only pixels indices where all channels are valid  
    idata = idata[masks['acvi'], :]
    # run prediction on valid pixels
    prediction = np.squeeze(model.predict(idata)).astype(SREAL)
    # empty results array 
    pred = np.ones((dims[0]*dims[1]), dtype=SREAL) * SREAL_FILL_VALUE
    # fill indices of predicted pixels with predicted value
    pred[masks['acvi']] = prediction
        
    return pred


def predict_CPH_COT(vis006, vis008, nir016, ir039, ir062, ir073, ir087,
				ir108, ir120, ir134, lsm, skt):
    """ 
        Main function that calls the neural network for COT and 
        CPH prediction.
    
        Input:
        - vis006 (2d numpy array): SEVIRI VIS 0.6 um (Ch 1)
        - vis008 (2d numpy array): SEVIRI VIS 0.8 um (Ch 2)
        - nir016 (2d numpy array): SEVIRI NIR 1.6 um (Ch 3)
        - ir039 (2d numpy array):  SEVIRI IR 3.9 um  (Ch 4)
        - ir062 (2d numpy array):  SEVIRI WV 6.2 um  (Ch 5)
        - ir073 (2d numpy array):  SEVIRI WV 7.3 um  (Ch 6)
        - ir087 (2d numpy array):  SEVIRI IR 8.7 um  (Ch 7)
        - ir108 (2d numpy array):  SEVIRI IR 10.8 um (Ch 9)
        - ir120 (2d numpy array):  SEVIRI IR 12.0 um (Ch 10)
        - ir134 (2d numpy array):  SEVIRI IR 13.4 um  (Ch 11)
        - lsm (2d numpy array):    Land-sea mask
        - skt (2d numpy array):    (ERA5) Skin Temperature
        - variable (str):          String that seths the variable to be 
                                   predicted (COT, CPH)
            
        Return:
        - prediction (1d numpy array): NN output array in 1d
    """
    start = time.time()
    # select network to be used (CPH or COT)
    networks = _select_networks()
    logging.info("Time for selecting network: {:.3f}".format(time.time() - start))

    # change fill value of skt from >> 1000 to SREAL_FILL_VALUE
    skt = np.where(skt > 1000, SREAL_FILL_VALUE, skt)

    start = time.time()
    # scale and put input arrays into the right format for the model
    prepped = _prepare_input_arrays(vis006, vis008, nir016, 
                                   ir039, ir062, ir073, 
                                   ir087, ir108, ir120, 
                                   ir134, lsm, skt, 
                                   networks)
    (scaled_data, dims, masks) = prepped
    logging.info("Time for preparing input data: {:.3f}".format(time.time() - start))
    
    # predict COT
    v = 'COT'
    #if networks[v].backend.lower() == "tensorflow":
    #    import tensorflow as tf
    #    config = tf.ConfigProto(intra_op_parallelism_threads = 1,
    #                            inter_op_parallelism_threads = 1)
    #    sess = tf.Session(config=config)
    #    tf.keras.backend.set_session(sess)

    start = time.time()
    pred = _run_prediction(v, networks, scaled_data, masks, dims)    
    results_COT = _postproc_prediction(pred.reshape((dims[0], dims[1])),
                                       v,
                                       networks[v].opts,
                                       masks)
    logging.info("Time for prediction COT: {:.3f}".format(time.time() - start))
    
    # predict CPH
    v = 'CPH'
    #if networks[v].backend.lower() == "tensorflow":
    #    import tensorflow as tf
    #    tf.keras.backend.clear_session()
    #    config = tf.ConfigProto(intra_op_parallelism_threads = 1,
    #                            inter_op_parallelism_threads = 1)
    #    sess = tf.Session(config=config)
    #    tf.keras.backend.set_session(sess)
    
    start = time.time()
    pred = _run_prediction(v, networks, scaled_data, masks, dims)
    results_CPH = _postproc_prediction(pred.reshape((dims[0], dims[1])),
                                       v,
                                       networks[v].opts,
                                       masks)
    logging.info("Time for prediction CPH: {:.3f}".format(time.time() - start))
    
    # mask CPH pixels where binary CMA is clear (0)
    clear_mask = (results_COT['bin'] == IS_CLEAR)  
    #fill_mask = (results_COT[1] == BYTE_FILL_VALUE)
    #clear_fill_mask = np.logical_or(clear_mask, fill_mask)

    results_CPH['reg'][clear_mask] = SREAL_FILL_VALUE
    results_CPH['bin'][clear_mask] = IS_CLEAR
    results_CPH['unc'][clear_mask] = SREAL_FILL_VALUE

    #results_CPH['reg'] = np.where(clear_mask, SREAL_FILL_VALUE, results_CPH['reg'])
    #results_CPH['bin'] = np.where(clear_mask, BYTE_FILL_VALUE, results_CPH['bin'])
    #results_CPH['unc'] = np.where(clear_mask, SREAL_FILL_VALUE, results_CPH['unc'])

    results = [results_COT['reg'], results_COT['bin'], results_COT['unc'],
               results_CPH['reg'], results_CPH['bin'], results_CPH['unc']]
    
    #hf.save_output_netcdf('/scratch/ms/de/sf7/dphilipp/orac_output/nn_output.nc',
    #                    {'cot_reg': results[0], 
    #                     'cot_bin': results[1],
    #                     'cot_unc': results[2],
    #                     'cph_reg': results[3],
    #                     'cph_bin': results[4],
    #                     'cph_unc': results[5]})
 
    return results
