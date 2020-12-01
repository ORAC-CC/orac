""" Read driver file for SEVIRI neural network library """

import os
from definitions import MANDATORY_OPTS
from definitions import TRUE_OPTS, FALSE_OPTS


def _check_parsed_opts(opts):
    """ Check if parsed options are as expected. """
    for opt, filetype in MANDATORY_OPTS.items():
        try:
            opts[opt]
        except KeyError:
            msg = 'Mandatory option {} missing in driver file.'
            raise Exception(RuntimeError, msg.format(opt))

        if filetype == 'PATH':
            if not os.path.isfile(opts[opt]):
                msg = 'SEVIRI neural network file for {}' + \
                      ' with filepath {} not found'
                raise Exception(RuntimeError, msg.format(opt, opts[opt]))

        if filetype == 'FLOAT':
            opts[opt] = float(opts[opt])

        if filetype == 'BOOL':
            if opts[opt] in TRUE_OPTS:
                opts[opt] = 1
            elif opts[opt] in FALSE_OPTS:
                opts[opt] = 0
            else:
                msg = '{} invalid argument for {}'
                msg = msg.format(opts[opt], opt)
                raise Exception(msg)

        if opt == 'BACKEND':
            if opts[opt] != 'THEANO':
                msg = 'BACKEND {} not valid. Use THEANO.'
                raise Exception(msg.format(opts[opt]))
    return opts


def parse_nn_driver(driver_path):
    """ Parse through driver file and put options to a dictionary. """
    opts = {}

    if os.path.isfile(driver_path):
        with open(driver_path, 'r') as dri:
            for line in dri:
                args = []
                tmp = line.split('=')
                if len(tmp) == 1:
                    # skip empty lines or comments
                    if tmp[0] == '' or tmp[0] == '\n' or \
                            tmp[0].startswith('#'):
                        continue
                for arg in tmp:
                    arg = arg.strip()
                    if arg[-2:] == '\n':
                        arg = arg[:-2]

                    args.append(arg)

                if len(args) != 2:
                    msg = 'Error in parsing NN driver: Extracted list ' + \
                          'contains {} arguments (2 allowed)'
                    raise Exception(RuntimeError, msg.format(len(args)))

                opts[args[0]] = args[1]

    return _check_parsed_opts(opts)
