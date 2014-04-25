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

   Note this code requires the ESA "EPR_API" (available from Brockmann Consult
   http://www.brockmann-consult.de/cms/web/beam/software). It has been tested
   with version 2.2 of the API.

   See the descriptions of each function below for details.

   HISTORY:
   2013/10/08: AP Original version.
   2014/04/25: GM Add the "is_lut_drift_corrected" flag to the output from
      read_aatsr_orbit().

   $Id$
*/
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "epr_api.h"

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
! nraz|fraz     float  Out Nadir|forward relative azimuth angle.
! nflg|fflg     short  Out Nadir|forward cloud flag.
! nqul|fqul     short  Out Nadir|forward confid flags.
! nday|fday     double Out Nadir|forward Julian date of measurement.
! nch1-7|fch1-7 float  Out Nadir|forward measurements for channel 1-7.
! ner1-7|fer1-7 float  Out Nadir|forward uncertainties for channel 1-7.
! start_date    char   Out String giving the beginning of the observation.
! gc1_file      char   Out Name of the general calibration file applied.
! vc1_file      char   Out Name of the visible calibration file applied.
!
! Local variables:
! Name Type Description
!
!
! History:
! 2013/10/08: AP Original version
! 2014/04/25: GM Add the "is_lut_drift_corrected" flag to the output from
!    read_aatsr_orbit().
!
! $Id$
!
! Bugs:
! none known
*/
void read_aatsr_orbit(const char *l1b_file, const bool *verbose,
                      const short *nch, const short *ch, 
                      const short *view, const long *nx, const long *ny, 
                      const long *startx, const long *starty, short *stat,
                      float **lat,  float **lon, 
                      float **nsza, float **niza, float **nsaz, float **nraz,  
                      short  *nflg, short  *nqul, double *nday,
                      float **nch1, float **nch2, float **nch3, float **nch4, 
                      float **nch5, float **nch6, float **nch7, float **ner1, 
                      float **ner2, float **ner3, float **ner4, float **ner5, 
                      float **ner6, float **ner7, 
                      float **fsza, float **fiza, float **fsaz, float **fraz, 
                      short  *fflg, short  *fqul, double *fday, 
                      float **fch1, float **fch2, float **fch3, float **fch4, 
                      float **fch5, float **fch6, float **fch7, float **fer1, 
                      float **fer2, float **fer3, float **fer4, float **fer5, 
                      float **fer6, float **fer7,
                      char start_date[30], char gc1_file[62], char vc1_file[62],
                      bool *is_lut_drift_corrected);

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
! Local variables:
! Name Type Description
!
!
! History:
! 2013/10/08: AP Original version
!
! $Id$
!
! Bugs:
! none known
*/
void fetch_aatsr_float_values(EPR_SProductId *pid, const char *name, 
                              const long nx, const long ny, const long x0, 
                              const long y0, float *out, const bool verbose);
void fetch_aatsr_short_values(EPR_SProductId *pid, const char *name, 
                              const long nx, const long ny, const long x0, 
                              const long y0, short *out, const bool verbose);

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
! Local variables:
! Name Type Description
!
!
! History:
! 2013/10/08: AP Original version
!
! $Id$
!
! Bugs:
! none known
*/
void calculate_rel_azi(float *saz, float *iaz, float *raz, const int nx, 
                       const int ny);


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
! Local variables:
! Name Type Description
!
!
! History:
  13/06/2012 Gareth Thomas: First version completed
  28/08/2012 Gareth Thomas: Changes made for interfacing with Fortran using
                            "iso_c_binding"
		     Various bug fixes
  12/09/2012 Gareth and Caroline: fixed bug in indexing of arrays changed <= 
                            to <
  13/09/2012 Gareth Thomas: Removed some array allocation test code from
                            deallocate_l1b
  12/09/2012 Gareth and Caroline: deallocated lat/lon arrays and changed day allocation
  13/09/2012 Caroline Poulsen: bug fix longitide being assigned latitude 
  13/09/2012 Gareth Thomas: Added start_date and gc1 and vc1 file names to
                            output of read_aatsr_beam_ctof90 (needed by 
		     shortwave calibration correction code)
  14/09/2012 Gareth Thomas: Bug fix: solar and instrument elevation angles are now
                            converted into zenith angles.
		     Also added epr_free_raster statements to get_aatsr_dimension
		     Bug fix read_aatsr_beam: Indexing of lat/lon rasters was
		     incorrectly including the y-offset value
  22/11/2012 Caroline Poulsen: calculation of relative azimuth Bug fix:phi=180.0 - phi 
                            and absolutte value of angle
  12/12/2012 Gareth Thomas: Bug fix in applying lat-lon limits in get_aatsr_dimension
                            miny is now set correctly if statement checking for the 
		     presence of lat-lon limits now works as it should.
		     Also, reduced the amount of debugging info coming back from
		     the EPR API.
  13/12/2012 Gareth Thomas: Added a check to make sure the L1B structure has been
                            initialised to the read_aatsr_beam function.
  14/08/2013 Gareth Thomas: Added okselev array for applying day-night masking in
                            get_aatsr_dimension()
!  08/10/2013 Adam Povey:    Adapted routine to the changes to the other C
!                            routines.
!
! $Id$
!
! Bugs:
! none known
*/
void get_aatsr_dimension(const char* infile, const short* daynight, 
			 const float* limit, const short* half_orbit, 
			 long* nxp, long* nyp, long* minyp, short* stat,
                         const bool *verbose);
#endif
