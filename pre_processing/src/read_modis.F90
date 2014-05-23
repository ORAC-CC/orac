module read_modis

implicit none

contains

include 'get_modis_time.F90'
include 'map_time_to_pixel.F90'
include 'modis_bright.F90'
include 'read_L1b_radiances_reflectances.F90'
include 'read_modis_angles.F90'
include 'read_modis_dimensions.F90'
include 'read_modis_geo.F90'
include 'read_modis_l1b.F90'
include 'read_modis_lat_lon.F90'
include 'read_modis_lsflag.F90'
include 'read_modis_time.F90'

end module read_modis
