""" Static definitions for use in various modules """

import numpy as np

# data types
SREAL = np.float32

# fill values
SREAL_FILL_VALUE = -999.0
DREAL_FILL_VALUE = -999.0
BYTE_FILL_VALUE  = -127
LINT_FILL_VALUE  = -32767
SINT_FILL_VALUE  = -32767

# flags
IS_CLEAR = 0
IS_CLOUD = 1
IS_WATER = 1
IS_ICE = 2

# miscellaneous constants
DATA_NAMES = ['ir039', 'ir087', 'ir087_108', 'ir108', 'ir108_120',
              'ir120', 'ir134', 'lsm', 'nir016', 'skt', 'vis006',
              'vis008', 'ir062', 'ir073']

MANDATORY_OPTS = {'COT_SCALING_FILEPATH': 'PATH',
                  'CPH_SCALING_FILEPATH': 'PATH',
                  'COT_MODEL_FILEPATH': 'PATH',
                  'CPH_MODEL_FILEPATH': 'PATH',
                  'BACKEND': 'STR',
                  'COT_THRESHOLD': 'FLOAT',
                  'CPH_THRESHOLD': 'FLOAT',
                  'VALID_REGRESSION_MIN': 'FLOAT',
                  'VALID_REGRESSION_MAX': 'FLOAT', 
                  'UNC_CLD_MIN': 'FLOAT', 
                  'UNC_CLD_MAX': 'FLOAT',
                  'UNC_CLR_MIN': 'FLOAT', 
                  'UNC_CLR_MAX': 'FLOAT',
                  'UNC_WAT_MIN': 'FLOAT',
                  'UNC_WAT_MAX': 'FLOAT',
                  'UNC_ICE_MIN': 'FLOAT',
                  'UNC_ICE_MAX': 'FLOAT'}


