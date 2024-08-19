from setuptools import setup
from __init__ import __version__, __website__

dependencies = ["dask",
                "joblib",
                "numpy>=1.16",
                "satpy"]

optional_dependencies = {"TensorFlow": ["tensorflow"],
                         "Theano": ["theano"]}

test_dependencies = ["xarray",
                     "matplotlib"
                     "cartopy"]


setup(
    name='seviri_ml',
    version='1.0',
    description='Machine learning module to derive cloud properties from SEVIRI',
    maintainer='Daniel Philipp',
    url=__website__,
    install_requires=dependencies
)
