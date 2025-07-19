# setup.py
from numpy.distutils.core import setup, Extension
import numpy

def get_version():
    """Gets the version from the package's __init__.py file."""
    with open('lcp_network_tools/__init__.py', 'r') as f:
        for line in f:
            if line.startswith('__version__'):
                return line.split('=').[1]strip().strip("'\"")
    return '0.1.0' # Default version

# Define the Fortran extension module
lcp_fortran_libs = Extension(
    name='lcp_fortran_libs',
    sources=[
        'fortran_src/calculatecostgui.f',
        'fortran_src/dijkstraspecial.f',
        'fortran_src/getdists.f'
    ],
    include_dirs=[numpy.get_include()],
    extra_f2py_options=['--quiet']
)

setup(
    name='lcp-network-tools-backend',
    version=get_version(),
    description='Fortran backend for LCP Network Tools QGIS Plugin',
    author='Edward Carroll, Fiona Carroll',
    ext_modules=[lcp_fortran_libs],
    packages=['lcp_network_tools.bin'], # This will be created by cibuildwheel
    package_data={'lcp_network_tools.bin': ['*.so', '*.pyd']},
    include_package_data=True
)
