module input_structures
  implicit none

  type modis_input_d
     integer :: l1b_id, geo_id
     integer :: dummy_var_id
     integer :: lat_id
     integer :: n_across_track, n_along_track, n_along_track_10,along_track_ratio
     character(len=64),dimension(6) ::  modis_channel_names
  end type modis_input_d

end module input_structures
