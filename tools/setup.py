from distutils.core import setup

setup(
    name='pyorac',
    version='1.0.0',
    author='A.C. Povey',
    author_emai='adam.povey@physics.ox.ac.uk',
    packages=['pyorac'],
    scripts=['orac.py', 'regression.py', 'single_process.py'],
    url='https://github.com/ORAC-CC/orac',
    license='COPYING',
    description='Wrapper for calling ORAC executables',
    long_description=open('../README').read(),
    install_requires=[
        'netcdf4', 'h5py', 'cf_units', 'scipy', 'opencv',
        'cartopy', 'matplotlib',
    ],
)
