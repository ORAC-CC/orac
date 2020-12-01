""" Helper functions for seviri neural networks"""

import xarray as xr
import logging
import shutil
import os
import readdriver

fmt = '%(levelname)s : %(filename)s : %(message)s'
logging.basicConfig(level=logging.DEBUG,
                    format=fmt
                    )


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


def _get_driver_opts():
    """ Set path to driver file and read driver file. """
    # read driver file for SEVIRI neural network
    # assume driver file is in same directory as this file

    basepath = os.path.dirname(os.path.realpath(__file__))
    drifile = 'nn_driver.txt'
    ptf = os.path.join(basepath, drifile)
    logging.info("SEVIRI ANN DRIVER: {}".format(ptf))
    return readdriver.parse_nn_driver(ptf)


def check_theano_version(modelpath):
    """
    Check if installed Theano version matches the
    Theano version used for training.
    """
    import theano
    cot_version = modelpath.split('__')[1]
    curr_version = theano.__version__
    if curr_version != cot_version:
        msg = 'WARNING: Mismatch between Theano version {} for training ' + \
              'and your currently used version {}. Version mismatch may' + \
              'lead to errors or unexpected behaviour.'
        msg = msg.format(cot_version, curr_version)
        logging.warning(msg)


class ConfigTheano:
    def __init__(self, opts):
        self.use_pid_compiledir = opts['USE_PID_COMPILEDIR']
        self.use_compiledir_lock = opts['USE_THEANO_COMPILEDIR_LOCK']
        self.cdir_pid = None
        self.configure_theano_compile_locking()

    def configure_theano_compile_locking(self):
        """ Configure how to deal with compile dir locking. """
        # enable usage of PID dependent compile directory
        # creates new compile_directory
        print("USE_PID_COMPILEDIR: ", self.use_pid_compiledir)
        if self.use_pid_compiledir:
            print("Setting PID compiledir 1")
            self._set_pid_compiledir()
    
        # enable or disable compile directory locking
        if not self.use_compiledir_lock:
            import theano
            theano.gof.compilelock.set_lock_status(False)

    def _set_pid_compiledir(self):
        """
        Set Theano compile diretory so that the directory.
        is process id depending. In case you are running
        ORAC with MPI support you are not suffering
        from compile lock, as otherwise Theano uses the
        same compile directory for each process.
        """
        print("Setting PID compiledir 2")
        pid = os.getpid()
        tflags = os.getenv('THEANO_FLAGS').split(',')
        for f in tflags:
            if f.startswith('base_compiledir'):
                cdir = f.split('=')[1]

        cdir_pid = os.path.join(cdir, 'pid' + str(pid))
        self.cdir_pid = cdir_pid
        print("PID COMPILEDIR: ", self.cdir_pid)
        os.environ['THEANO_FLAGS'] = 'base_compiledir={}'.format(cdir_pid)

    def remove_pid_compiledir(self):
        """ Remove PID dependent compile directory. """
        if self.use_pid_compiledir:
            if os.path.isdir(self.cdir_pid):
                shutil.rmtree(self.cdir_pid)
            else:
                msg = 'Cannot delete {} because not existing'
                msg = msg.format(self.cdir_pid)
                logging.warning(msg)
