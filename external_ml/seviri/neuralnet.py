"""
    This module contains classes for configuration, preparation	and prediction
    of COT (CMA) and CPH using artificial neural networks. This module is
    used in predictCPHCOT.py.

    - class NetworkBase: Basic parent class for NetworkCPH and NetworkCOT
    - class NetworkCPH(NetworkBase): Class for CPH prediction
    - class NetworkCOT(NetworkBase): Class for COT prediction

    First author: Daniel Philipp (DWD)
    ---------------------------------------------------------------------------
    24/04/2020, DP: Initial version
    23/07/2020, DP: Import of load_model depending on used backend. Includes
                    filename checking.
"""

import os
import joblib
import warnings
import helperfuncs as hf
import logging

fmt = '%(levelname)s : %(filename)s : %(message)s'
logging.basicConfig(level=logging.DEBUG,
                    format=fmt)


def _throw_backend_not_found_error(s):
    msg = 'Backend {} not recognized.'
    raise Exception(RuntimeError, msg.format(s))


class NetworkBase:
    def __init__(self, modelpath, scalerpath, backend):
        """
        Initialize NetworkBase class.

        Args:
        - modelpath (str):  Path to trained ANN model file
        - scalerpath (str): Path to file containing scaling values
        - backend (str):    Used Keras neural network backend
        """

        self.modelpath = modelpath
        self.scalerpath = scalerpath
        self.backend = backend

        logging.info('Modelpath SEVIRI ANN: {}'.format(modelpath))
        logging.info('Scalerpath SEVIRI ANN: {}'.format(scalerpath))

    def get_model(self):
        """ Load Tensorflow or Theano trained model (.h5 file) from disk. """

        if self.backend.lower() == 'tensorflow':
            raise NotImplementedError('Tensorflow backendnot implemented yet.')

        elif self.backend.lower() == 'theano':
            logging.info('Setting KERAS_BACKEND env. variable  to theano')
            os.environ['KERAS_BACKEND'] = 'theano'

            with warnings.catch_warnings():
                warnings.filterwarnings('ignore', category=FutureWarning)
                from keras.models import load_model

        else:
            _throw_backend_not_found_error(self.backend)

        return load_model(self.modelpath, compile=False)

    def _get_scaler(self):
        """ Load sklearn.preprocessing scaler from disk (.pkl file). """
        return joblib.load(self.scalerpath)

    def scale_input(self, arr):
        """ Scale input with the correct sklearn.preprocessing scaler. """
        scaler = self._get_scaler()
        return scaler.transform(arr)


class NetworkCPH(NetworkBase):
    def __init__(self, opts):

        self.opts = opts

        modelpath = opts['CPH_MODEL_FILEPATH']
        scalerpath = opts['CPH_SCALING_FILEPATH']
        backend = opts['BACKEND']

        hf.check_theano_version(modelpath)

        super().__init__(modelpath, scalerpath, backend)


class NetworkCOT(NetworkBase):
    def __init__(self, opts):

        self.opts = opts

        modelpath = opts['COT_MODEL_FILEPATH']
        scalerpath = opts['COT_SCALING_FILEPATH']
        backend = opts['BACKEND']

        hf.check_theano_version(modelpath)

        super().__init__(modelpath, scalerpath, backend)
