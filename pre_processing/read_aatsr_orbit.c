/**
   See the header file "read_aatsr_orbit.h" for documentation.

   HISTORY:
   08/10/2013, Adam Povey: Original version.
   11/10/2013, Greg McGarragh: Fixed band names for the forward view which are
      not *_forward_* but are actually *_fward_*.
   04/25/2014, Greg McGarragh: Add the "is_lut_drift_corrected" flag to the
      output from read_aatsr_orbit().
   01/06/2015, Greg McGarragh: Fixed a bug when requesting a startx and nx other
      than 0 and 512.

   $Id$
 **/

#include "read_aatsr_orbit.h"

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void read_aatsr_orbit(const char *l1b_file, const bool *verbose,
                      const short *nch, const short *ch,
                      const short *view, const long *nx, const long *ny,
                      const long *startx, const long *starty, short *stat,
                      float **lat,  float **lon,
                      float **nsza, float **niza, float **nsaz, float **nraz,
                      short  *nflg, short  *nqul, double *nday,
                      float **nch1, float **nch2, float **nch3, float **nch4,
                      float **nch5, float **nch6, float **nch7,
                      float **fsza, float **fiza, float **fsaz, float **fraz,
                      short  *fflg, short  *fqul, double *fday,
                      float **fch1, float **fch2, float **fch3, float **fch4,
                      float **fch5, float **fch6, float **fch7,
                      char start_date[30], char gc1_file[62], char vc1_file[62],
                      bool *is_lut_drift_corrected)
{
  float *iaz;
  const char *ax_label[2][4], *ch_label[2][7], *fg_label[2][2], *dy_label[2];
  int view_present[2] = { 0, 0 };
  uint i, j, k;
  iaz = calloc(*nx * *ny, sizeof(float));
  if (iaz == NULL) {
    printf("ERROR: read_aatsr_orbit(): Insufficient memory available.");
    exit(1);
  }

  // Gather output array pointers into arrays for ease of use
  float *ax_array[2][5] = { {*nsza,*niza,*nsaz,iaz,*nraz},
                            {*fsza,*fiza,*fsaz,iaz,*fraz} };
  float *ch_array[2][7] = { {*nch1,*nch2,*nch3,*nch4,*nch5,*nch6,*nch7},
                            {*fch1,*fch2,*fch3,*fch4,*fch5,*fch6,*fch7} };
  short *fg_array[2][2] = { {nflg, nqul},
                            {fflg, fqul} };
  double *dy_array[2]   =   {nday, fday};

  // Variables for the EPR file read routines
  EPR_SProductId *pid;
  EPR_SDatasetId *did;
  const EPR_SField *fid;
  EPR_SRecord *mphid, *rec;
  EPR_SDSD *vc1, *gc1, *vdt;
  const EPR_STime *time;

  // Set the names for the fields that will be read
  ax_label[0][0] = "sun_elev_nadir";
  ax_label[0][1] = "view_elev_nadir";
  ax_label[0][2] = "sun_azimuth_nadir";
  ax_label[0][3] = "view_azimuth_nadir";
  ax_label[1][0] = "sun_elev_fward";
  ax_label[1][1] = "view_elev_fward";
  ax_label[1][2] = "sun_azimuth_fward";
  ax_label[1][3] = "view_azimuth_fward";

  ch_label[0][0] = "reflec_nadir_0550";
  ch_label[0][1] = "reflec_nadir_0670";
  ch_label[0][2] = "reflec_nadir_0870";
  ch_label[0][3] = "reflec_nadir_1600";
  ch_label[0][4] = "btemp_nadir_0370";
  ch_label[0][5] = "btemp_nadir_1100";
  ch_label[0][6] = "btemp_nadir_1200";
  ch_label[1][0] = "reflec_fward_0550";
  ch_label[1][1] = "reflec_fward_0670";
  ch_label[1][2] = "reflec_fward_0870";
  ch_label[1][3] = "reflec_fward_1600";
  ch_label[1][4] = "btemp_fward_0370";
  ch_label[1][5] = "btemp_fward_1100";
  ch_label[1][6] = "btemp_fward_1200";

  fg_label[0][0] = "cloud_flags_nadir";
  fg_label[0][1] = "confid_flags_nadir";
  fg_label[1][0] = "cloud_flags_fward";
  fg_label[1][1] = "confid_flags_fward";

  dy_label[0]    = "11500_12500_NM_NADIR_TOA_MDS";
  dy_label[1]    = "11500_12500_NM_FWARD_TOA_MDS";

  // Determine which views are present
  for (i=0; i<*nch; i++) {
    view_present[0] = view_present[0] || (view[i] == 1);
    view_present[1] = view_present[1] || (view[i] == 2);
  }

  // Initialise the BEAM-API and open the data file
  epr_init_api(e_log_warning, epr_log_message, NULL);
  pid = epr_open_product(l1b_file);

  // Check requested limits are sensible
  if (*startx+*nx > epr_get_scene_width(pid)) {
    printf("ERROR: read_aatsr_orbit: Reading beyond range of scene along x.");
    exit(1);
  }
  if (*starty+*ny > epr_get_scene_height(pid)) {
    printf("ERROR: read_aatsr_orbit: Reading beyond range of scene along y.");
    exit(1);
  }

  // Read geolocation (view independent)
  fetch_aatsr_float_values(pid, "latitude", *nx, *ny, *startx, *starty,
                           *lat, *verbose);
  fetch_aatsr_float_values(pid, "longitude", *nx, *ny, *startx, *starty,
                           *lon, *verbose);

  /* Read in the information needed for calibration corrections to be applied
     from the Main Product Header */
  mphid = epr_get_mph(pid);
  // 1) Sensing start time (date format is "dd-mmm-yyyy hh:nn:ss.ssssss")
  fid   = epr_get_field(mphid, "SENSING_START");
  strcpy(start_date, epr_get_field_elem_as_str(fid));
  // 2) VC1 file (Visible Calibration file)
  vc1 = epr_get_dsd_at(pid, 29);
  // Check that we have what we're expecting
  if (*verbose && strcasecmp(vc1->ds_name, "VISIBLE_CALIBRATION_FILE") != 0)
    printf("read_aatsr_orbit: Didn't find VC file in %s. %s found instead.\n",
           l1b_file, vc1->ds_name);
  strcpy(vc1_file, vc1->filename);
  // 3) GC1 file (General Calibration file)
  gc1 = epr_get_dsd_at(pid, 30);
  if (*verbose && strcasecmp(gc1->ds_name, "GENERAL_CALIBRATION_FILE") != 0)
    printf("read_aatsr_orbit: Didn't find GC file in %s. %s found instead.\n",
           l1b_file, gc1->ds_name);
  strcpy(gc1_file, gc1->filename);
  // 4) vdt (VISCAL_DRIFT_TABLE)
  vdt = epr_get_dsd_at(pid, 36);
  if (! vdt)
    *is_lut_drift_corrected = false;
  else
    *is_lut_drift_corrected = true;

  // Read auxilliary and flag data from nadir/forward views
  for (j=0; j<2; j++) {
    if (view_present[j]) {
      // Extract measurement time (not stored as a raster)
      did = epr_get_dataset_id(pid, dy_label[j]);
      rec = epr_create_record(did);
      for (i=0; i<*ny; i++) {
        rec = epr_read_record(did, i, rec);
        fid = epr_get_field(rec, "dsr_time");
        time = epr_get_field_elem_as_mjd(fid);
        dy_array[j][i] = ((double)time->seconds +
                          (double)time->microseconds*1E-6)/86.4E3
          + (double)time->days;
      }
      epr_free_record(rec);

      // Read angular information
      for (i=0; i<4; i++) {
        fetch_aatsr_float_values(pid, ax_label[j][i], *nx, *ny, *startx,
                                 *starty, ax_array[j][i], *verbose);
      }
      calculate_rel_azi(ax_array[j][2], ax_array[j][3], ax_array[j][4],
                        *nx, *ny);

      // Read flags
      for (i=0; i<2; i++) {
        fetch_aatsr_short_values(pid, fg_label[j][i], *nx, *ny, *startx,
                                 *starty, fg_array[j][i], *verbose);
      }
    }
  }

  // Loop through channels specified by VIEW and CH
  for (j=0; j<*nch; j++) {
    // Read channel information
    fetch_aatsr_float_values(pid, ch_label[view[j]-1][ch[j]-1], *nx, *ny,
                             *startx, *starty, ch_array[view[j]-1][ch[j]-1],
                             *verbose);
  }

  // Close files
  *stat = epr_close_product(pid);
  epr_close_api();
  free(iaz);
  return;
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void fetch_aatsr_float_values(EPR_SProductId *pid, const char *name,
                              const long nx, const long ny, const long x0,
                              const long y0, float *out, const bool verbose)
{
  EPR_SBandId *bid;
  EPR_SRaster *raster = NULL;
  uint stepx=1, stepy=1, i, ii, j;
  int stat;

  // Get a band ID for the requested band
  bid = epr_get_band_id(pid, name);
  if (bid == NULL) {
    printf("fetch_aatsr_raster_beam: Band '%s' not found.\n", name);
    exit(1);
  }

  /* Read data as raster. Must read entire 512 pixel rows as EPR defines the
     0th elements at the opposite end of the array to us */
  raster = epr_create_compatible_raster(bid, 512, ny, stepx, stepy);
  stat = epr_read_band_raster(bid, 0, y0, raster);
  if (stat != 0) {
    printf("fetch_aatsr_raster_beam: Failure to read band '%s'.\n", name);
    exit(1);
  }

  /* Copy data into write array. NOTE: this function will quite happily
     accept values of i or j outside the valid range if you are reading less
     than the entire array. */
  for (j=0; j<ny; j++) {
    for (i=x0, ii=0; i<x0+nx; i++, ii++) {
      out[ii + j*nx] = epr_get_pixel_as_float(raster, i, j);
    }
  }

  epr_free_raster(raster);
  if (verbose) { printf("Fetched %s\n", name); }
  return;
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void fetch_aatsr_short_values(EPR_SProductId *pid, const char *name,
                              const long nx, const long ny, const long x0,
                              const long y0, short *out, const bool verbose)
{
  EPR_SBandId *bid;
  EPR_SRaster *raster = NULL;
  uint stepx=1, stepy=1, i, ii, j;
  int stat;

  // Get a band ID for the requested band
  bid = epr_get_band_id(pid, name);
  if (bid == NULL) {
    printf("fetch_aatsr_raster_beam: Band '%s' not found.\n", name);
    return;
  }

  /* Read data as raster. Must read entire 512 pixel rows as EPR defines the
     0th elements at the opposite end of the array to us */
  raster = epr_create_compatible_raster(bid, 512, ny, stepx, stepy);
  stat = epr_read_band_raster(bid, 0, y0, raster);
  if (stat != 0) {
    printf("fetch_aatsr_raster_beam: Failure to read band '%s'.\n", name);
    return;
  }

  /* Copy data into write array. NOTE: this function will quite happily
     accept values of i or j outside the valid range if you are reading less
     than the entire array. */
  for (j=0; j<ny; j++) {
    for (i=x0, ii=0; i<x0+nx; i++, ii++) {
      out[ii + j*nx] = (short) epr_get_pixel_as_uint(raster, i, j);
    }
  }

  epr_free_raster(raster);
  if (verbose) { printf("Fetched %s\n", name); }
  return;
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void calculate_rel_azi(float *saz, float *iaz, float *raz, const int nx,
                       const int ny)
{
  uint i, j, k;
  float phi=0.0;

  for (j=0; j<ny; j++) {
    for (i=0; i<nx; i++) {
      k = i + j*nx;
      phi = saz[k] - iaz[k];
      if (phi < 0.0) phi = -phi;

      if (phi > 180.0)
        raz[k] = phi - 180.0;
      else
        raz[k] = 180.0 - phi;
    }
  }
  return;
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void get_aatsr_dimension(const char* infile, const short* daynight,
                         const float* limit, const short* half_orbit,
                         long* nxp, long* nyp, long* minyp, short* statp,
                         const bool *verbose)
{
  long i, j;
  long nx, ny, miny=0;
  long new_ny, maxy, miny2, maxy2;
  float selev1, lat1, lon1;
  short row_in;
  long* okselev;
  int stat;

  EPR_SBandId* bid;
  EPR_SProductId* pid;

  EPR_SRaster* selev = NULL;
  EPR_SRaster* lat   = NULL;
  EPR_SRaster* lon   = NULL;

  if (*verbose) { printf("Starting get_aatsr_dimension (C)\n"); }
  /* Initialize the API. */
  /* Note that you can set different levels of feedback from the EPR library */
  /* The different levels are: e_log_debug
                               e_log_info
                               e_log_warning
                               e_log_error
     The default log output "epr_log_message" is stdout */
  epr_init_api(e_log_warning, epr_log_message, NULL);

  pid = epr_open_product(infile);

  /* Determine the product size */
  nx = epr_get_scene_width(pid);
  ny = epr_get_scene_height(pid);

  /* If the orbit_half parameter is not zero, select the required
     half of the orbit */
  if (*half_orbit == 1)
    {
      ny = ny/2;
    }
  else if (*half_orbit == 2)
    {
      ny = ny/2;
      miny = ny;
    }

  okselev = calloc(ny, sizeof(long));
  if (okselev == NULL) {
    printf("read_aatsr_dimensions: Insufficient memory available.");
    return;
  }

  /* The daynight parameter determines if we are to read daylight data
     night time data, or both
     We judge day/night based on the nadir solar zenith angle */
  if ((*daynight == 1) || (*daynight == 2)){
    bid = epr_get_band_id(pid, "sun_elev_nadir");
    if (bid == NULL) {
      printf("get_aatsr_dimension ERROR: band sun_elev_nadir not found\n");
      return;
    }
    selev = epr_create_compatible_raster(bid, nx, ny, 1, 1);
    stat = epr_read_band_raster(bid, 0, miny, selev);
  }

  /* Set initial limits for range checking:
     maximum value of miny, minimum value of maxy */
  maxy = 0;
  miny = ny-1;
  new_ny = 0;

  if (*daynight == 1)
    {
      /* Only want daylight data. This forms the middle section of
         the orbit */
      for (j=0; j<ny; j++)
        {
          selev1 = epr_get_pixel_as_float(selev, 255, j);
          if (selev1 > 0) {
            okselev[j] = 1;
            new_ny = new_ny + 1;
            if (miny > j) miny = j;
            if (maxy < j) maxy = j; }
          else okselev[j] = 0;
        }
      epr_free_raster(selev);
    }
  else if (*daynight == 2)
    {
      /* Only want night data. This is found at each end of the orbit,
         so dealing with it is significantly more complicated. */
      for (j=0; j<ny; j++)
        {
          selev1 = epr_get_pixel_as_float(selev, 255, j);
          if (selev1 < 0) {
            okselev[j] = 1;
            new_ny = new_ny + 1;
            if (miny > j) miny = j;
            if (maxy < j) maxy = j; }
          else okselev[j] = 0;
        }
      epr_free_raster(selev);
    }
  else
    {
      miny = maxy; maxy = ny-1; new_ny = ny;
      for (j=0; j<ny; j++) okselev[j] = 1;
    }
  if (*half_orbit == 2) {
    miny = miny+ny;
    maxy = maxy+ny; }
  ny = new_ny;

  if (ny == 0) {
    fprintf(stderr,"\n read_aatsr_beam: No matching data found in file %s\n",
            infile);
    return; }

  /* If we have lat-lon limits, then apply them.
     A couple of things to note here:
     (i)  The code reads whole image rows. If a row has a single pixel within
          the limit, that row is read.
     (ii) This test takes no account of whether you are reading both day and
          night sides of the orbit. If both sides are read, the orbit will be
          read from the first index that lies in the region, to the last. I.e.
          you may well get the entire day-side of the orbit. The lat-lon
          limiting therefore only really makes sense when used in conjunction
          with the daynight switch. */
  if ((limit[0] > -90.0) || (limit[1] > -180.0) ||
      (limit[2] <  90.0) || (limit[3] <  180.0))
    {
      /* Read in the latitude and longitude grid */
      bid = epr_get_band_id(pid, "latitude");
      if (bid == NULL) {
        printf("get_aatsr_dimension ERROR: band latitude not found\n");
        return;
      }
      lat = epr_create_compatible_raster(bid, nx, ny, 1, 1);
      stat = epr_read_band_raster(bid, 0, miny, lat);
      bid = epr_get_band_id(pid, "longitude");
      if (bid == NULL) {
        printf("get_aatsr_dimension ERROR: band longitude not found\n");
        return;
      }
      lon = epr_create_compatible_raster(bid, nx, ny, 1, 1);
      stat = epr_read_band_raster(bid, 0, miny, lon);

      /* Again, initialise miny to maximum value and maxy to minimum */
      miny2 = ny-1;
      maxy2 = 0;
      new_ny = 0;
      for (j=0; j<ny; j++)
        {
          row_in = 0;
          for (i=0; i<nx; i++)
            {
              lat1 = epr_get_pixel_as_float(lat, i, j);
              lon1 = epr_get_pixel_as_float(lon, i, j);
              if ((lat1 >= limit[0]) & (lat1 <= limit[2]) &
                  (lon1 >= limit[1]) & (lon1 <= limit[3]) &
                  okselev[j+miny]) row_in=row_in+1;
            }

          if (row_in >= nx/2)
            {
              new_ny = new_ny+1;
              if (miny2 > j) miny2 = j;
              if (maxy2 < j) maxy2 = j;
            }
        }
      if (new_ny == 0) {
        fprintf(stderr,"\n read_aatsr_beam: No matching data found in file %s\n",
                infile);
        return; }
      ny = new_ny;
      miny = miny + miny2;
      epr_free_raster(lat);
      epr_free_raster(lon);
    }
  free(okselev);
  *nxp = (long) nx;
  *nyp = (long) ny;
  *minyp = (long) miny;
  stat = epr_close_product(pid);
  epr_close_api();
  *statp = stat;
  if (*verbose) { printf("Have completed get_aatsr_dimension\n"); }
  return;
}
