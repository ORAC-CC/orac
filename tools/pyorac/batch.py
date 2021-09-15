r"""Class for interfacing with batch processing systems.

DESCRIPTION:
Various batch processing systems exist, depending on your local server setup.
The BatchSystem class defines the syntax of the various command arguments it
accepts using dictionaries. For example,
   qsub = BatchSystem('qsub',
                      'Your job (\d+) \("(\w+)"\) has been submitted',
                      {'depend'   : '-hold-jid {}'.format,
                       'ram'      : '-l h_vmem={}M'.format})
- The first argument is simply the name of the command to call to queue a task.
- The second argument defines a regular expression to understand the
  output of the scheduler and each group should have a name in that
  expression. Here, a QSUB call outputs an ID number and the name of the job.
- The third argument is the syntax used to convey dependencies.
- The fourth argument is the delimeter between multiple dependencies.
- The fifth argument is bash script that must be run to use OpenMP.
- The sixth argument is a dictionary specifying the command syntax. Each key
  should describe the purpose of the statement and the contents are a string
  format function with a single bracket. Here, if we want this job to wait for
  job 1234 to complete, QSUB should be given the argument '-hold-jid 1234'.

A command is generated with the print_batch() function. It must be given a
dictionary. The dictionary's keys should match elements defined when the class
was created and the contents are the value that argument should take. To
produce the wait example above:
   >>> qsub.print_batch({'depend' : 1234, 'ram' : 2})
   'qsub -l h_vmem=2M -hold-jid 1234'
The optional argument 'exe' adds the command you want to queue, which can be a
single string or a subprocess.call()-style tuple/list.

parse_out() is used to extract information from the output of the command,
usually to determine the job ID. Given the output of the call, it returns a
dictionary of contents. The optional 'key' argument specifies a single term
from that dictionary to return (usually 'ID').
"""

import re


# -----------------------------------------------------------------------------
# ----- CLASS DEFINITION AND ITS METHODS --------------------------------------
# -----------------------------------------------------------------------------


class BatchSystem:
    """Container for syntax to call a batch queuing system.
    Member variables:
    command      - The name of the batch queuing system.
    args         - A dictionary of string format functions, each taking one
                   argument, to produce an argument of the queuing function.
    regex        -  # Regex to parse command output.
    depend_arg   - Command for the job dependencies. This would be an element
                   of args, but has a more complicated structure.
    depend_delim - String require to space consequetive dependencies.
    openmp       - Bash added to scripts in order to use OpenMP."""

    def __init__(self, command, regex, depend_arg, depend_delim, openmp, args):
        self.command = command
        self.args = args
        self.regex = re.compile(regex)
        self.depend_arg = depend_arg
        self.depend_delim = depend_delim
        self.openmp = openmp
        self.args.update({'depend': self.parse_depend})

    def parse_depend(self, item):
        """Deal with slightly more complex syntax for declaring dependencies"""
        if isinstance(item, (str, int)):
            return self.depend_arg.format(item)
        if item:
            return self.depend_arg.format(self.depend_delim.join(item))

        return None

    def print_batch(self, values, exe=None):
        """Returns the queuing shell command. 'exe' is the thing to run."""
        return ' '.join(self.list_batch(values, exe=exe))

    def list_batch(self, values, exe=None):
        """Lists the queuing shell command. 'exe' is the thing to run."""
        arguments = [self.command]
        for key, val in values.items():
            try:
                test = len(val) > 0
            except TypeError:
                test = val is not None
            if test:
                arguments.extend(self.args[key](val).split("%%"))

        if isinstance(exe, (list, tuple)):
            arguments.extend(exe)
        elif exe is not None:
            arguments.append(exe)

        return arguments

    def parse_out(self, text, key=None):
        """Parse output of queuing function. Returns all regex groups unless
        'key' is specified, where it just returns that."""
        mat = self.regex.match(text)
        if mat is None:
            raise SyntaxError('Unexpected output from queue system: ' + text)
        if key:
            return mat.group(key)

        return mat.groupdict()

    def add_openmp_to_script(self, file_obj):
        """Include OpenMP control within this script."""
        file_obj.write(self.openmp)


# -----------------------------------------------------------------------------
# ----- BATCH PROCESSING SYSTEM DEFINITIONS -----------------------------------
# -----------------------------------------------------------------------------


# QSUB, used by the Oxford Yau cluster
QSUB = BatchSystem(
    'qsub',
    r'Your job (?P<ID>\d+) \("(?P<job_name>[\w\.-]+)"\) has been submitted',
    '-hold_jid%%{}', ',', '',
    {'duration': '-l%%h_rt={}:00'.format,
     'email': '-M%%{}'.format,
     'err_file': '-e%%{}'.format,
     'job_name': '-N%%{}'.format,
     'log_file': '-o%%{}'.format,
     'priority': '-p%%{}'.format,
     'procs': '-n%%num_proc={}'.format,
     'queue': '-q%%{}'.format,
     'ram': '-l%%h_vmem={}M'.format,
     }
)

# BSUB, used by the JASMIN cluster at RAL
BSUB = BatchSystem(
    'bsub',
    r'Job <(?P<ID>\d+)> is submitted to (?P<desc>[\w\s]*)queue '
    r'<(?P<queue>[\w\.-]+)>.', '-w%%ended({})', ')&&ended(', '',
    {'duration': '-W%%{}'.format,
     'email': '-u%%{}'.format,
     'err_file': '-e%%{}'.format,
     'err_clob': '-eo%%{}'.format,
     'job_name': '-J%%{}'.format,
     'log_file': '-o%%{}'.format,
     'log_clob': '-oo%%{}'.format,
     'order': '-R%%order[{}]'.format,
     'procs': '-n%%{}'.format,
     'priority': '-p%%{}'.format,
     'queue': '-q%%{}'.format,
     'ram': '-R%%rusage[mem={0}]%%-M%%{0}000'.format,
     }
)

# SLURM, the new Oxford queuing system
SLURM = BatchSystem(
    'sbatch',
    r'Submitted batch job (?P<ID>\d+)', '--dependency=afterok:{}', ':', """
if [ -n "$SLURM_CPUS_PER_TASK" ]; then
    omp_threads=$SLURM_CPUS_PER_TASK
else
    omp_threads=1
fi
export OMP_NUM_THREADS=$omp_threads
""",
    {'duration': '--time={}:00'.format,
     'estimated': '--time-min={};00'.format,
     'email': '--mail-user={}'.format,
     'err_file': '--error={}'.format,
     'job_name': '--job-name={}'.format,
     'log_file': '--output={}'.format,
     'priority': '--nice={}'.format,
     'procs': '--cpus-per-task={}'.format,
     'queue': '--partition={}'.format,
     'ram': '--mem={}M'.format,
     'exclude': '--exclude={}'.format,
     'project': '--comment={}'.format,
     'account': '--account={}'.format,
     'environ': '--export={}'.format,
    }
)
