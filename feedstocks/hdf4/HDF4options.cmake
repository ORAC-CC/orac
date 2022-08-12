#############################################################################################
####  Change default configuration of options in config/cmake/cacheinit.cmake file        ###
####  format: set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DXXX:YY=ZZZZ")                 ###
#############################################################################################

### uncomment/comment and change the following lines for other configuration options

#############################################################################################
####      alternate toolsets       ####
#set(CMAKE_GENERATOR_TOOLSET "Intel C++ Compiler 17.0")

#############################################################################################
####      ext libraries       ####

### ext libs on system
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_PREFIX_PATH:FILEPATH=${PREFIX}")
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DSZIP_LIBRARY:FILEPATH=${PREFIX}/lib/libsz.so -DSZIP_INCLUDE_DIR:PATH=${PREFIX}/include")

##############################################################################################
### disable test program builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_TESTING:BOOL=OFF")

#############################################################################################
### change install prefix (default use INSTALLDIR value)
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_INSTALL_PREFIX:PATH=${PREFIX}")

#############################################################################################
### disable netcdf
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF4_ENABLE_NETCDF:BOOL=OFF")

#############################################################################################
### enable everything else
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF4_ENABLE_JPEG_LIB_SUPPORT:BOOL=ON")
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF4_ENABLE_SZIP_SUPPORT:BOOL=ON")
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF4_ENABLE_SZIP_ENCODING:BOOL=ON")
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF4_ENABLE_Z_LIB_SUPPORT:BOOL=ON")

#############################################################################################
### disable packaging
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF4_NO_PACKAGES:BOOL=ON")

### Create install package with external libraries (szip, zlib, jpeg)
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF4_PACKAGE_EXTLIBS:BOOL=ON")

#############################################################################################
### import conda environmment
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} ${CMAKE_ARGS}")
