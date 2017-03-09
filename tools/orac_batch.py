# Class for interfacing with batch processing systems.
#
# DESCRIPTION:
# Various batch processing systems exist, depending on your local server setup.
# The BatchSystem class defines the syntax of the various command arguments it
# accepts using dictionaries. For example,
#    qsub = BatchSystem('qsub',
#                       'Your job (\d+) \("(\w+)"\) has been submitted',
#                       {'depend'   : '-hold-jid {}'.format,
#                        'ram'      : '-l h_vmem={}M'.format})
# - The first argument is simply the name of the command to call to queue a task.
# - The second argument defines a regular expression to understand the
#   output of the scheduler  and each group should have a name in that
#   expression. Here, a QSUB call outputs an ID number and the name of the job.
# - The fourth argument is a dictionary specifying the command syntax. Each key
#   should describe the purpose of the statement and the contents are a string
#   format function with a single bracket. Here, if we want this job to wait for
#   job 1234 to complete, QSUB should be given the argument '-hold-jid 1234'.
#
# A command is generated with the PrintBatch function. It must be given a
# dictionary. The dictionary's keys should match elements defined when the class
# was created and the contents are the value that argument should take. To
# produce the wait example above:
#    >>> qsub.PrintBatch({'depend' : 1234, 'ram' : 2})
#    'qsub -l h_vmem=2M -hold-jid 1234'
# The optional argument 'exe' adds the command you want to queue, which can be a
# single string or a subprocess.call()-style tuple/list.
#
# ParseOut is used to extract information from the output of the command, usually
# to determine the job ID. Given the output of the call, it returns a dictionary
# of contents. The optional 'key' argument specifies a single term from that
# dictionary to return (usually 'ID').
#
# HISTORY:
# 27 Jul 2016, AP: Initial version
# 09 Mar 2017, GT: Bug fix to formating of job-IDs as dependencies for a new job
#                  submission using bsub (LSF) 

import re

#-----------------------------------------------------------------------------
#----- CLASS DEFINITION AND ITS METHODS --------------------------------------
#-----------------------------------------------------------------------------

class BatchSystem:
    """Container for syntax to call a batch queuing system.
    Member variables:
    command      - The name of the batch queuing system.
    args         - A dictionary of string format functions, each taking one
                   argument, to produce an argument of the queuing function.
    regex        -  # Regex to parse command output.
    depend_arg   - Command for the job dependencies. This would be an element of
                   args, but has a more complicated structure.
    depend_delim - String require to space consequetive dependencies."""

    def __init__(self, command, regex, depend_arg, depend_delim, args):
        self.command = command
        self.args = args
        self.regex = re.compile(regex)
        self.depend_arg = depend_arg
        self.depend_delim = depend_delim
        self.args.update({'depend' : self.ParseDepend})

    def ParseDepend(self, item):
        """Deal with slightly more complex syntax for declaring dependencies"""
        if isinstance(item, str):
            return self.depend_arg.format(item)
        else:
            return self.depend_arg.format(self.depend_delim.join(item))

    def PrintBatch(self, values, exe=None, depend_arg=None):
        """Returns the queuing shell command. 'exe' is the thing to run."""
        arguments = [self.command]
        arguments.extend([ self.args[key](values[key])
                           for key in values.keys() if values[key] ])
        if type(depend_arg) in [list, tuple]:
            arguments.extend(depend_arg)
        elif depend_arg:
            arguments.append(depend_arg)
        if type(exe) in [list, tuple]:
            arguments.extend(exe)
        elif exe:
            arguments.append(exe)
        return ' '.join(arguments)

    def ParseOut(self, text, key=None):
        """Parse output of queuing function. Returns all regex groups unless
        'key' is specified, where it just returns that."""
        m = self.regex.match(text)
        if m == None:
            raise SyntaxError('Unexpected output from queue system ' + text)
        if key:
            return m.group(key)
        else:
            return m.groupdict()

#-----------------------------------------------------------------------------
#----- BATCH PROCESSING SYSTEM DEFINITIONS -----------------------------------
#-----------------------------------------------------------------------------

# QSUB, used by the Oxford Yau cluster
qsub = BatchSystem('qsub',
                   'Your job (?P<ID>\d+) \("(?P<job_name>[\w\.-]+)"\) '
                   'has been submitted',
                   '-hold_jid {}', ',',
                   {'duration' : '-l h_rt={}:00'.format,
                    'email'    : '-M {}'.format,
                    'err_file' : '-e {}'.format,
                    'job_name' : '-N {}'.format,
                    'log_file' : '-o {}'.format,
                    'priority' : '-p {}'.format,
                    'procs'    : '-n num_proc={}'.format,
                    'queue'    : '-q {}'.format,
                    'ram'      : '-l h_vmem={}M'.format,
                    'shell'    : '-S {}'.format})

# BSUB, used by the JASMIN cluster at RAL
bsub = BatchSystem('bsub',
                   'Job <(?P<ID>\d+)> is submitted to (?P<desc>\w*)( ?)queue <(?P<queue>.+)>.',
                   '-w done({})', ')&&done(',
                   {'duration' : '-W {}'.format,
                    'email'    : '-u {}'.format,
                    'err_file' : '-e {}'.format,
                    'job_name' : '-J {}'.format,
                    'log_file' : '-o {}'.format,
                    'order'    : '-R "order[{}]"'.format,
                    'procs'    : '-n {}'.format,
                    'priority' : '-p {}'.format,
                    'queue'    : '-q {}'.format,
                    'ram'      : '-R "rusage[mem={}]"'.format})
