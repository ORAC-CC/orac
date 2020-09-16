""" Helper functions for seviri neural networks"""

import xarray as xr
import warnings
import logging

fmt = '%(levelname)s : %(filename)s : %(message)s'
logging.basicConfig(level=logging.DEBUG,
                    format=fmt)

def save_output_netcdf(filepath, kwargs):
    """ 
        Save arrays to netcdf. kwargs is dict in the form
        of {'variable_name': array, ...}. Array has to be
        a 2d array with dimensions (xdim, ydim). Filepath
        is path and filename to be saved.
    """
    ds = xr.Dataset()
    for key, value in kwargs.items():
        ds[key] = (('x', 'y'), value)
     
    ds.to_netcdf(filepath)
     

def all_same(items):
    """
        Check whether all elements in a list are equal. 
        Returns True or False.
    """
    return all(x == items[0] for x in items)


def check_theano_version(modelpath):
    """ 
        Check if installed Theano version matches the 
        Theano version used for training.
    """
    import theano

    cot_version = modelpath.split('__')[1]
    curr_version = theano.__version__

    if curr_version != cot_version:
        msg = 'WARNING: Mismatch between Theano version {} for training '+\
              'and your currently used version {}. Version mismatch may ' +\
              'lead to errors or unexpected behaviour.'
        msg = msg.format(cot_version, curr_version)
        logging.warning(msg)
