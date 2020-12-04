import os
import os.path

from setuptools import setup, find_packages, Command
from pkg_resources import require, DistributionNotFound, VersionConflict
from pyorac import __version__, __website__


root_path = os.path.dirname(__file__)

dependencies = ["cartopy",
                "cf-units",
                "h5py",
                "matplotlib>=3.0",
                "netcdf4>=1.0",
                "numpy>=1.16",
                "scipy>=0.15.0",
                "opencv"]

optional_dependencies = {}

test_dependencies = []


class check_dep(Command):
    """
    Command to check that the required dependencies are installed on the system
    """
    description = "Checks that the required dependencies are installed on the system"
    user_options = []

    def initialize_options(self):
        pass

    def finalize_options(self):
        pass

    def run(self):

        for dep in dependencies:
            try:
                require(dep)
                print(dep + " ...[ok]")
            except (DistributionNotFound, VersionConflict):
                print(dep + "... MISSING!")

# Extract long-description from README
#README = open(os.path.join(root_path, '..', 'README'), 'rb').read().decode('utf-8')

setup(
    name='pyorac',
    version=__version__,
    description='Wrapper for calling ORAC executables',
    #long_description=README,
    maintainer='A.C. Povey',
    maintainer_email='adam.povey@physics.ox.ac.uk',
    url=__website__,
    packages=find_packages(),
    scripts=['orac.py', 'regression.py', 'single_process.py'],
    cmdclass={"checkdep": check_dep},
    install_requires=dependencies,
    extras_require=optional_dependencies,
    tests_require=test_dependencies
)
