/*
  A set of C functions for reading (A)ATSR L1B data in Envisat format
  (i.e. .N1, .E2 or .E1 files). Included functions are:
  - read_aatsr_orbit         Main C function for reading data.
  - fetch_aatsr_float_values Function that reads the data via a "raster", which
                             is then stored in the necessary float array.
  - fetch_aatsr_short_values As above, but for short int values.
  - calculate_rel_azi        Brief function to convert solar and satellite
                             zenith angles into a relative azimuth.
  - get_aatsr_dimension      Function that inspects the orbit to determine the
                             length and width of the data, subsetted to
                             consider only day or night (as required).
  - extrap_aatsr_angle       Reads angular data at tie points and then
                             extrapolates to the full grid.
  - extrapolate2d            Performs bilinear interpolation.
  - interpol_fraction        Prepares for that bilinear interpolation.

   Note this code requires the ESA "EPR_API" (available from Brockmann Consult
   http://www.brockmann-consult.de/cms/web/beam/software). It has been tested
   with version 2.2 of the API.

   See the descriptions of each function below for details.

   History:
   2013/10/08, AP: Original version.
   2014/04/25, GM: Add the "is_lut_drift_corrected" flag to the output from
      read_aatsr_orbit().
   2017/07/19, AP: The band data for relative azimuth is poorly interpolated.
      Add routines to read the tie points and interpolate it ourselves.
   2017/07/25, AP: Add force_zero_azimuth to fix incorrect tie point values in
      the nadir satellite azimuth.
*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>

#ifdef INCLUDE_ATSR_SUPPORT
#include "epr_api.h"
#else
typedef void EPR_SProductId;
#endif

#ifndef aatsr_orbit_h
#define aatsr_orbit_h

/*Name: read_aatsr_orbit
!
! Purpose:
! Read data from an (A)ATSR Envisat file and copy it into arrays initialised
! in Fortran. Pointers to the arrays are passed and as an array is effectively
! a pointer in C, the arguments of this function are mostly pointers to
! pointers.
!
! Description and Algorithm details:
! 1) Collect the array pointers into new arrays. These simplify the syntax
!    later and enable the use of for loops over the channels.
! 2) Initialise the BEAM-API.
! 3) Read the latitude, longitude, and calibration correction information.
! 4) For each view that has been requested, read the measurement time, angular
!    information, and cloud/land-sea flags.
! 5) For each channel that has been requested, read the data and produce the
!    uncertainty values.
! 6) Close the files.
!
! Arguments:
! Name          Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! l1b_file      char   In  Path to the L1B file to be read.
! verbose       bool   In  True: Print progress to the screen. False: Don't.
! nch           short  In  The number of channels to be read.
! ch            short  In  Array specifying the channel numbers to be read.
! view          short  In  Array specifying the view to be read for each channel
!                          1=nadir, 2=forward.
! nx|y          long   In  Number of pixels to be read across and along track.
! startx|y      long   In  Initial pixel to be read across and along track.
! stat          short  Out Status of the routine. 0 indictates success.
! lat           float  Out Latitude values.
! lon           float  Out Longitude values.
! nsza|fsza     float  Out Nadir|forward solar zenith angle.
! niza|fiza     float  Out Nadir|forward instrument zenith angle.
! nsaz|fsaz     float  Out Nadir|forward solar azimuth angle.
! niaz|fiaz     float  Out Nadir|forward instrument azimuth angle.
! nraz|fraz     float  Out Nadir|forward relative azimuth angle.
! nflg|fflg     short  Out Nadir|forward cloud flag.
! nqul|fqul     short  Out Nadir|forward confid flags.
! nday|fday     double Out Nadir|forward Julian date of measurement.
! nch1-7|fch1-7 float  Out Nadir|forward measurements for channel 1-7.
! start_date    char   Out String giving the beginning of the observation.
! gc1_file      char   Out Name of the general calibration file applied.
! vc1_file      char   Out Name of the visible calibration file applied.
!
! History:
! 2013/10/08, AP: Original version
! 2014/04/25, GM: Add the "is_lut_drift_corrected" flag to the output from
!    read_aatsr_orbit().
!
! Bugs:
! None known.
*/
void read_aatsr_orbit(const char *l1b_file, const bool *verbose,
                      const short *nch, const short *ch,
                      const short *view, const long *nx, const long *ny,
                      const long *startx, const long *starty, short *stat,
                      float **lat,  float **lon,
                      float **nsza, float **niza, float **nsaz, float **niaz,
                      float **nraz, short  *nflg, short  *nqul, double *nday,
                      float **nch1, float **nch2, float **nch3, float **nch4,
                      float **nch5, float **nch6, float **nch7,
                      float **fsza, float **fiza, float **fsaz, float **fiaz,
                      float **fraz, short  *fflg, short  *fqul, double *fday,
                      float **fch1, float **fch2, float **fch3, float **fch4,
                      float **fch5, float **fch6, float **fch7,
                      char start_date[30], char gc1_file[62], char vc1_file[62],
                      bool *is_lut_drift_corrected);

#ifdef INCLUDE_ATSR_SUPPORT
/*Name: fetch_aatsr_float_values AND fetch_aatsr_short_values
!
! Purpose:
! Transfers data from its native raster format into the desired Fortran array.
!
! Description and Algorithm details:
! 1) Fetch the ID for the desired band and create and appropriate raster.
! 2) Read the data into the raster, taking entire rows as EPR_CREATE_COMPATIBLE_
!    RASTER indexs rows in the opposite sense to ORAC.
! 3) Copy data, pixel by pixel, from the raster into the desired array.
! 4) Close the raster.
!
! Arguments:
! Name          Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! pid     EPR_SProductId In ID from EPR_OPEN_PRODUCT.
! name    char   In  Name of the field to be read.
! nx|y    long   In  Number of pixels to read across and along track.
! x0|y0   long   In  Index of first pixel to read across and along track.
! out     *      Out Pointer to array into which to store data. Its type
!                    depends on the function called (float or short).
! verbose bool   In  True: Print progress information to screen. False: Don't.
!
! History:
! 2013/10/08, AP: Original version
!
! Bugs:
! None known.
*/
void fetch_aatsr_float_values(EPR_SProductId *pid, const char *name,
                              const long nx, const long ny, const long x0,
                              const long y0, float *out, const bool verbose);
void fetch_aatsr_short_values(EPR_SProductId *pid, const char *name,
                              const long nx, const long ny, const long x0,
                              const long y0, short *out, const bool verbose);

/*Name: calculate_zenith
!
! Purpose:
! Convert elevation angles into zenith angles.
!
! Description and Algorithm details:
! 1) Subtract the angles.
!
! Arguments:
! Name  Type  In/Out/Both Description
! ------------------------------------------------------------------------------
! saz   float In  Solar zenith angle.
! iaz   float In  Satellite zenith angle.
! raz   float Out Relative azimuth angle.
! nx|y  int   In  Number of pixels across and along track.
!
! History:
! 2020/03/03, AP: Original version
!
! Bugs:
! None known.
*/
void calculate_zenith(float *za, const int nx, const int ny);

/*Name: calculate_rel_azi
!
! Purpose:
! Convert solar and satellite azimuth angles into a relative azimuth.
!
! Description and Algorithm details:
! 1) Subtract the angles.
! 2) Cast the angle on the range 0-180 degrees.
!
! Arguments:
! Name  Type  In/Out/Both Description
! ------------------------------------------------------------------------------
! saz   float In  Solar zenith angle.
! iaz   float In  Satellite zenith angle.
! raz   float Out Relative azimuth angle.
! nx|y  int   In  Number of pixels across and along track.
!
! History:
! 2013/10/08, AP: Original version
!
! Bugs:
! None known.
*/
void calculate_rel_azi(float *saz, float *iaz, float *raz, const int nx,
                       const int ny);
#endif


/*Name: get_aatsr_dimension
!
! Purpose:
! Convert solar and satellite azimuth angles into a relative azimuth.
!
! Description and Algorithm details:
! 1) Subtract the angles.
! 2) Cast the angle on the range 0-180 degrees.
!
! Arguments:
! Name       Type  In/Out/Both Description
! ------------------------------------------------------------------------------
! infile     char  In  Path to L1B file.
! daynight   short In  1: daytime data; 2: night data.
! limit      float In  (/ minimum latitude, minimum longitude,
!                         maximum latitude, maximum longitude /)
! half_orbit short In  Only considered for night data. 1: Take section from end
!                      of orbit. 0: Take section from beginning of orbit.
! nxp|nyp    long  In  Pointer to number of pixels across and along track.
! minyp      long  In  Pointer to index of first pixel along track to consider.
! stat       short Out Status of routine. 0 indicates success.
! verbose    bool  In  True: Print progress information to screen. False: Don't.
!
! History:
! 2012/06/13, GT: First version completed.
! 2012/08/28, GT: Changes made for interfacing with Fortran using
!    "iso_c_binding". Various bug fixes.
! 2012/09/12, GT/CP: Fixed bug in indexing of arrays changed <= to <.
! 2012/09/13, GT: Removed some array allocation test code from deallocate_l1b.
! 2012/09/12, GT/CP: Deallocated lat/lon arrays and changed day allocation.
! 2012/09/13, CP: Bug fix: longitide being assigned latitude.
! 2012/09/13, GT: Added start_date and gc1 and vc1 file names to output of
!    read_aatsr_beam_ctof90 (needed by shortwave calibration correction code).
! 2012/09/14, GT: Bug fix: solar and instrument elevation angles are now
!    converted into zenith angles. Also added epr_free_raster statements to
!    get_aatsr_dimension. Bug fix read_aatsr_beam: Indexing of lat/lon rasters
!    was incorrectly including the y-offset value.
! 2012/11/22, CP: Calculation of relative azimuth. Bug fix: phi=180.0 - phi
!    and absolute value of angle.
! 2012/12/12, GT: Bug fix in applying lat-lon limits in get_aatsr_dimension,
!    miny is now set correctly, if statement checking for the presence of lat-lon
!    limits now works as it should. Also, reduced the amount of debugging info
!    coming back from the EPR API.
! 2012/12/13, GT: Added a check to make sure the L1B structure has been
!    initialised to the read_aatsr_beam function.
! 2013/08/14, GT: Added okselev array for applying day-night masking in
!    get_aatsr_dimension().
! 2013/10/08, AP: Adapted routine to the changes to the other C routines.
!
! Bugs:
! None known.
*/
void get_aatsr_dimension(const char* infile, const short* daynight,
                         const float* limit, const short* half_orbit,
                         long* nxp, long* nyp, long* minyp, short* stat,
                         const bool *verbose);

#ifdef INCLUDE_ATSR_SUPPORT
/*Name: extrap_aatsr_angle
!
! Purpose:
! Reads angular data at the tie points (rather than the full-swath band data)
! and extrapoloates that to the full grid. To deal with the -180/180
! discontinuity, the sin and cos of the angles are both interpolated and then
! translated back to a single angle with arctan2.
!
! Description and Algorithm details:
! 1) Open dataset.
! 2) Read tie point locations.
! 3) Read the angular data at the tie points, converting to radians.
! 4) Extrapolate the sin and cos of that data onto the instrument grid.
! 5) Take arctan2 of those fields and output to the Fortran array.
!
! Arguments:
! Name         Type   In/Out/Both Description
! ------------------------------------------------------------------------------
! pid          EPR_SProductId In ID from EPR_OPEN_PRODUCT.
! dataset_name char   In  Name of the datset to open.
! tie_name     char   In  Name of the tie point array to use.
! field_name   char   In  Name of the field to be read.
! nx|y         long   In  Number of pixels to read across and along track.
! x0|y0        long   In  Index of first pixel to read across and along track.
! out          float  Out Pointer to array into which to store data.
! verbose      bool   In  If true, print progress information to screen.
! ignore_centre_tie_pnt bool In When true, separately extrapolates the pixels
!                         left and right of the sub-satellite point.
!
! History:
! 2017/07/19, AP: Original version
! 2017/07/25, AP: Add force_zero_azimuth to fix incorrect tie point values in
!    the nadir satellite azimuth.
!
! Bugs:
! None known.
*/
void extrap_aatsr_angle(EPR_SProductId *pid, const char *dataset_name,
                        const char *tie_name, const char *field_name,
                        const long nx, const long ny,
                        double *x_out, double *y_out,
                        float *out, const bool verbose,
                        const bool ignore_centre_tie_pnt);

/*Name: extrapolate2d
!
! Purpose:
! Perform bilinear interpolation on an array, acting on that array with some
! function first.
!
! Description and Algorithm details:
! 1) Call interpol_fraction() to determine the interpolation increments.
! 2) Act on every value of z with func.
! 3) Perform a bilinear interpolation.
!
! Arguments:
! Name         Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! x0_in|y0_in   long    In  Index of first element of x|y input array to use.
! nx_in|ny_in   long    In  Length of the x|y axis of the input data.
! x_in|y_in     double  In  Axes of the inputt data.
! x0_out|y0_out long    In  Index of first element of x|y output array to use.
! nx_out|ny_out long    In  Length of the x|y axes of the output data.
! x_out|y_out   long    In  Axes of the output data.
! z             double  In  Data to interpolate.
! func          pointer In  Function to apply to input data.
! out           pointer Out Pointer to output data.
!
! History:
! 2017/07/19, AP: Original version
! 2017/07/25, AP: Add force_zero_azimuth to fix incorrect tie point values in
!    the nadir satellite azimuth.
!
! Bugs:
! None known.
*/
void extrapolate2d(const long x0_in, const long nx_in, double *x_in,
                   const long y0_in, const long ny_in, double *y_in,
                   const long x0_out, const long nx_out, double *x_out,
                   const long y0_out, const long ny_out, double *y_out,
                   double **z, double (*func)(double), double **out);

/*Name: interpol_fraction
!
! Purpose:
! For every point of OUT, this finds the pair of values in IN that bound it
! (or the edges of IN if the point is outside of that grid) and calculates
! delta = (out - in_below) / (in_above - in_below) for use in linear
! interpolation. IN and OUT are assumed to increase monotonically. As the grids
! are read from the ATSR file, I'm not going to assume that they're regular.
!
! Description and Algorithm details:
! 1) Extrapolate to points below the grid.
! 2) Extrapolate to points above the grid.
! 3) Interpolate for points within the grid, using the bound found for the
!    previous pixel to save time.
!
! Arguments:
! Name         Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! in0|out0     long    In  Index of the first element of the x|y array to use.
! n_in|n_out   long    In  Length of the x|y axis of the tie point data.
! in|out       double  In  Axes of the tie point data.
! bounds       long    Out Array storing the index of the input grid point
!                          beneath each output point.
! delta        double  Out Array storing the relative position of each output
!                          point within the input grid.
!
! History:
! 2017/07/19, AP: Original version
! 2017/07/25, AP: Add force_zero_azimuth to fix incorrect tie point values in
!    the nadir satellite azimuth.
!
! Bugs:
! None known.
*/
void interpol_fraction(const long in0, const long n_in,  double *in,
                       const long out0, const long n_out, double *out,
                       long *bounds, double *delta);

#endif
#endif
