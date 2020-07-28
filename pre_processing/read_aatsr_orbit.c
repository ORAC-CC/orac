/**
   See the header file "read_aatsr_orbit.h" for documentation.

   History:
   2013/10/08, AP: Original version.
   2013/11/10, GM: Fixed band names for the forward view which are not
      *_forward_* but are actually *_fward_*.
   2014/04/25, GM: Add the "is_lut_drift_corrected" flag to the output from
      read_aatsr_orbit().
   2015/01/06, GM: Fixed a bug when requesting a startx and nx other than 0
      and 512.
   2016/10/26, GT: Fixed a bug whereby measurement time was always being read
      from first row of orbit regardless of starty value.
   2020/05/11, GT: Fixed bug in selection of day/night data in get_aatsr_dimension.
**/

#include "read_aatsr_orbit.h"

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
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
                      bool *is_lut_drift_corrected)
{
#ifdef INCLUDE_ATSR_SUPPORT
     const char *geo_dataset, *geo_pts_name, *geo_label[2];
     const char *ax_dataset[2], *ax_pts_name, *ax_label[4];
     const char *ch_label[2][7], *fg_label[2][2], *dy_label[2];
     int view_present[2] = { 0, 0 };
     long i, j;
     double x_out[*nx], y_out[*ny];

     // Gather output array pointers into arrays for ease of use
     float *geo_array[2]   =   {*lat, *lon};
     float *ax_array[2][5] = { {*nsza,*niza,*nsaz,*niaz,*nraz},
                               {*fsza,*fiza,*fsaz,*fiaz,*fraz} };
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
     geo_dataset  = "GEOLOCATION_ADS";
     geo_pts_name = "LAT_LONG_TIE_POINTS";
     geo_label[0] = "tie_pt_lat";
     geo_label[1] = "tie_pt_long";

     ax_dataset[0] = "NADIR_VIEW_SOLAR_ANGLES_ADS";
     ax_dataset[1] = "FWARD_VIEW_SOLAR_ANGLES_ADS";
     ax_pts_name   = "VIEW_ANGLE_TIE_POINTS";
     ax_label[0] = "tie_pt_sol_elev";
     ax_label[1] = "tie_pt_sat_elev_nad";
     ax_label[2] = "tie_pt_sol_az";
     ax_label[3] = "tie_pt_sat_azi";

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
          printf("ERROR: read_aatsr_orbit:");
          printf("Reading beyond range of scene along x.\n");
          exit(1);
     }
     if (*starty+*ny > epr_get_scene_height(pid)) {
          printf("ERROR: read_aatsr_orbit:");
          printf("Reading beyond range of scene along y.\n");
          exit(1);
     }

     /* Form output grids in units of km from the img_scan_y variable. The
        offset of 256 along x is taken from description of m_actrk_pix_num at
        http://envisat.esa.int/handbooks/aatsr/CNTR6-6-5.html
        The x/y coordinates define the start of a pixel, so to get the
        coordinates of cell centres, add 0.5. */
     for (i=0; i<*nx; i++) x_out[i] = (double)(*startx + i) - 255.5;
     // Use position from the time field (it's independent of view)
     did = epr_get_dataset_id(pid, dy_label[0]);
     rec = epr_create_record(did);
     for (i=0; i<*ny; i++) {
          rec = epr_read_record(did, i + *starty, rec);
          fid = epr_get_field(rec, "img_scan_y");
          y_out[i] = epr_get_field_elem_as_double(fid, 0) * 1e-3;
     }
     // Shift the y coordinate to cell centres
     for (i=0; i<*ny-1; i++)
          y_out[i] += 0.5 * (y_out[i+1] - y_out[i]);
     // The last bin requires the next img_scan_y value
     if (*starty+*ny < epr_get_scene_height(pid)) {
          rec = epr_read_record(did, *ny + *starty, rec);
          fid = epr_get_field(rec, "img_scan_y");
          y_out[*ny-1] += 0.5 * (epr_get_field_elem_as_double(fid, 0) * 1e-3
                                 - y_out[*ny-1]);
     } else
          y_out[*ny-1] += 0.5 * (y_out[*ny-1] - y_out[*ny-2]);
     epr_free_record(rec);

     // Read geolocation (view independent)
     for (i=0; i<2; i++) {
          extrap_aatsr_angle(pid, geo_dataset, geo_pts_name, geo_label[i],
                             *nx, *ny, x_out, y_out, geo_array[i], *verbose,
                             false);
     }

     /* Read in the information needed for calibration corrections to be applied
        from the Main Product Header */
     mphid = epr_get_mph(pid);
     // 1) Sensing start time (date format is "dd-mmm-yyyy hh:nn:ss.ssssss")
     fid   = epr_get_field(mphid, "SENSING_START");
     strcpy(start_date, epr_get_field_elem_as_str(fid));
     // 2) VC1 file (Visible Calibration file)
     vc1 = epr_get_dsd_at(pid, 29);
     // Check that we have what we're expecting
     if (*verbose && strcasecmp(vc1->ds_name, "VISIBLE_CALIBRATION_FILE") != 0) {
          printf("read_aatsr_orbit: Didn't find VC file in %s. ", l1b_file);
          printf("%s found instead.\n", vc1->ds_name);
     }
     strcpy(vc1_file, vc1->filename);
     // 3) GC1 file (General Calibration file)
     gc1 = epr_get_dsd_at(pid, 30);
     if (*verbose && strcasecmp(gc1->ds_name, "GENERAL_CALIBRATION_FILE") != 0) {
          printf("read_aatsr_orbit: Didn't find GC file in %s. ", l1b_file);
          printf("%s found instead.\n", gc1->ds_name);
     }
     strcpy(gc1_file, gc1->filename);
     // 4) vdt (VISCAL_DRIFT_TABLE)
     if (epr_get_num_dsds(pid) >= 37)
          vdt = epr_get_dsd_at(pid, 36);
     else
          vdt = NULL;
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
                    rec = epr_read_record(did, i+*starty, rec);
                    fid = epr_get_field(rec, "dsr_time");
                    time = epr_get_field_elem_as_mjd(fid);
                    dy_array[j][i] = ((double)time->seconds +
                                      (double)time->microseconds*1E-6)/86.4E3
                         + (double)time->days;
               }
               epr_free_record(rec);

               /* Section 2.6.1.1.5.1.2.2 (Algorithm Definition) of the AATSR
                  Handbook (http://envisat.esa.int/handbooks/aatsr/toc.html)
                  states that the line of sight of the instrument is determined
                  from the pixel number (there being 2000 pixels in each scan).
                  The elevation and azimuth of the view relative to the
                  satellite's motion are calculated from that using a series
                  of matrix transformations.
                  Those are input into the library function pp_target() to
                  determine the geometry at the surface. That routine is defined
                  in Section 7.1 of document PO-IS-DMS-GS-0559 (PPF_POINTING
                  SOFTWARE USER MANUAL). It's Table 15 indicates that the angles
                  are given in the Topocentric Coordinate System.
                  Document PO-IS-ESA-GS-0561 (ENVISAT-1 MISSION CFI SOFTWARE
                  MISSION CONVENTIONS DOCUMENT) defines the topocentric
                  coordinate system in Figure 9 as azimuth measured from the
                  y-axis towards x and elevation above the x-y plane. Section
                  5.1.7 states that the x- and y-axes point east and north,
                  respectively. */

               // Read angular information
               for (i=0; i<4; i++) {
                    extrap_aatsr_angle(pid, ax_dataset[j],
                                       ax_pts_name, ax_label[i],
                                       *nx, *ny, x_out, y_out,
                                       ax_array[j][i], *verbose, i == 3);
               }
               calculate_zenith(ax_array[j][0], *nx, *ny);
               calculate_zenith(ax_array[j][1], *nx, *ny);
               calculate_rel_azi(ax_array[j][2], ax_array[j][3], ax_array[j][4],
                                 *nx, *ny);

               // Read flags
               for (i=0; i<2; i++) {
                    fetch_aatsr_short_values(pid, fg_label[j][i],
                                             *nx, *ny, *startx, *starty,
                                             fg_array[j][i], *verbose);
               }
          }
     }

     // Loop through channels specified by VIEW and CH
     for (j=0; j<*nch; j++) {
          // Read channel information
          fetch_aatsr_float_values(pid, ch_label[view[j]-1][ch[j]-1], *nx, *ny,
                                   *startx, *starty,
                                   ch_array[view[j]-1][ch[j]-1], *verbose);
     }

     // Close files
     *stat = epr_close_product(pid);
     epr_close_api();
     return;
#else
     printf("ERROR: the ORAC pre-processor has not been compiled with ");
     printf("ATSR support. Recompile with -DINCLUDE_ATSR_SUPPORT.\n");
     return;
#endif
}

#ifdef INCLUDE_ATSR_SUPPORT
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void fetch_aatsr_float_values(EPR_SProductId *pid, const char *name,
                              const long nx, const long ny, const long x0,
                              const long y0, float *out, const bool verbose)
{
     EPR_SBandId *bid;
     EPR_SRaster *raster = NULL;
     uint stepx=1, stepy=1;
     long i, ii, j;
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
     if (verbose) printf("Fetched %s\n", name);
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
     uint stepx=1, stepy=1;
     long i, ii, j;
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
     if (verbose) printf("Fetched %s\n", name);
     return;
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void calculate_zenith(float *za, const int nx, const int ny)
{
     long i, j, k;
     float phi=0.0;

     for (j=0; j<ny; j++) {
          for (i=0; i<nx; i++) {
               k = i + j*nx;
               phi = 90.0 - za[k];
               za[k] = phi;
          }
     }
     return;
}

void calculate_rel_azi(float *saz, float *iaz, float *raz, const int nx,
                       const int ny)
{
     long i, j, k;
     float phi=0.0;

     for (j=0; j<ny; j++) {
          for (i=0; i<nx; i++) {
               k = i + j*nx;
               phi = saz[k] - iaz[k];
               if (phi < 0.0) phi = -phi;

               if (phi > 180.0)
                    raz[k] = 360.0 - phi;
               else
                    raz[k] = phi;
          }
     }
     return;
}
#endif

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void get_aatsr_dimension(const char* infile, const short* daynight,
                         const float* limit, const short* half_orbit,
                         long* nxp, long* nyp, long* minyp, short* statp,
                         const bool *verbose)
{
#ifdef INCLUDE_ATSR_SUPPORT
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

     if (*verbose) printf("Starting get_aatsr_dimension (C)\n");
     /* Initialize the API.
        Note that you can set different levels of feedback from the EPR library
        The different levels are: e_log_debug
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
     if (*half_orbit == 1) {
          ny = ny/2;
     } else if (*half_orbit == 2) {
          ny = ny/2;
          miny = ny;
     }

     okselev = calloc(ny, sizeof(long));
     if (okselev == NULL) {
          printf("read_aatsr_dimensions: Insufficient memory available.\n");
          return;
     }

     /* The daynight parameter determines if we are to read daylight data
        night time data, or both
        We judge day/night based on the nadir solar zenith angle */
     if ((*daynight == 1) || (*daynight == 2)){
          bid = epr_get_band_id(pid, "sun_elev_nadir");
          if (bid == NULL) {
               printf("ERROR: get_aatsr_dimension: ");
               printf("band sun_elev_nadir not found\n");
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

     if (*daynight == 1) {
          /* Only want daylight data. This forms the middle section of
             the orbit */
          for (j=0; j<ny; j++) {
               selev1 = epr_get_pixel_as_float(selev, 255, j);
               if (selev1 > 0) {
                    okselev[j] = 1;
                    new_ny = new_ny + 1;
                    if (miny > j) miny = j;
                    if (maxy < j) maxy = j;
               } else
                    okselev[j] = 0;
          }
          epr_free_raster(selev);
     } else if (*daynight == 2) {
          /* Only want night data. This is found at each end of the orbit,
             so dealing with it is significantly more complicated. */
          for (j=0; j<ny; j++) {
               selev1 = epr_get_pixel_as_float(selev, 255, j);
               if (selev1 < 0) {
                    okselev[j] = 1;
                    new_ny = new_ny + 1;
                    if (miny > j) miny = j;
                    if (maxy < j) maxy = j;
               } else
                    okselev[j] = 0;
          }
          epr_free_raster(selev);
     } else {
          miny = maxy; maxy = ny-1; new_ny = ny;
          for (j=0; j<ny; j++) okselev[j] = 1;
     }
     if (*half_orbit == 2) {
          miny = miny+ny;
          maxy = maxy+ny;
     }
     ny = new_ny;

     if (ny == 0) {
          fprintf(stderr,"read_aatsr_beam: No matching data found in file %s\n",
                  infile);
          return;
     }

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
         (limit[2] <  90.0) || (limit[3] <  180.0)) {
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
          for (j=0; j<ny; j++) {
               row_in = 0;
               for (i=0; i<nx; i++) {
                    lat1 = epr_get_pixel_as_float(lat, i, j);
                    lon1 = epr_get_pixel_as_float(lon, i, j);
                    if ((lat1 >= limit[0]) & (lat1 <= limit[2]) &
                        (lon1 >= limit[1]) & (lon1 <= limit[3]) &
                        okselev[j+miny]) row_in=row_in+1;
               }

               if (row_in >= nx/2) {
                    new_ny = new_ny+1;
                    if (miny2 > j) miny2 = j;
                    if (maxy2 < j) maxy2 = j;
               }
          }
          if (new_ny == 0) {
               fprintf(stderr,"read_aatsr_beam: ");
               fprintf(stderr,"No matching data found in file %s\n",
                       infile);
               return;
          }
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
     if (*verbose) printf("Have completed get_aatsr_dimension\n");
     return;
#else
     printf("ERROR: the ORAC pre-processor has not been compiled with ");
     printf("ATSR support. Recompile with -DINCLUDE_ATSR_SUPPORT.\n");
     return;
#endif
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
#ifdef INCLUDE_ATSR_SUPPORT
void extrap_aatsr_angle(EPR_SProductId *pid, const char *dataset_name,
                        const char *tie_name, const char *field_name,
                        const long nx, const long ny,
                        double *x_out, double *y_out,
                        float *out, const bool verbose,
                        const bool ignore_centre_tie_pnt)
{
     EPR_SDatasetId *did;
     EPR_SRecord *sph, *record;
     const EPR_SField *field;

     long n_records, n_tie_pts, half_tie, x0;
     double *tie_pts, *scan_y;
     double **data, *sin_data[ny], *cos_data[ny];

     long i, j;

     const char *unit;
     double scale;
     const double rad2deg  = 180. / M_PI;


     // Get a band ID for the requested band
     did = epr_get_dataset_id(pid, dataset_name);
     if (did == NULL) {
          printf("ERROR: extrap_aatsr_angle: Dataset '%s' not found.\n",
                 dataset_name);
          exit(1);
     }

     // Allocate array to read tie point locations
     sph   = epr_get_sph(pid);
     field = epr_get_field(sph, tie_name);
     n_tie_pts = epr_get_field_num_elems(field);
     tie_pts   = calloc(n_tie_pts, sizeof(double));
     if (tie_pts == NULL) {
          printf("ERROR: extrap_aatsr_angle: Cannot allocate tie points.\n");
          exit(1);
     }
     // Read tie points from the Secondary Product Header
     epr_copy_field_elems_as_doubles(field, tie_pts, n_tie_pts);

     // Allocate and read angular data
     n_records = epr_get_num_records(did);
     record    = epr_create_record(did);
     data      = calloc(n_records, sizeof(double *));
     scan_y    = calloc(n_records, sizeof(double));
     if ((data == NULL) || (scan_y == NULL)) {
          printf("ERROR: extrap_aatsr_angle: Cannot allocate y arrays.\n");
          exit(1);
     }
     for (i=0; i<n_records; i++) {
          data[i] = calloc(n_tie_pts, sizeof(double));
          if (data[i] == NULL) {
               printf("ERROR: extrap_aatsr_angle: Cannot allocate x array.\n");
               exit(1);
          }
     }

     // Determine scaling factor from units field
     epr_read_record(did, 0, record);
     field = epr_get_field(record, field_name);
     unit = epr_get_field_unit(field);
     if (strcmp(unit, "mdeg") == 0) {
          scale = M_PI / 180e3;
     } else if (strcmp(unit, "(1e-6) degrees") == 0) {
          scale = M_PI / 180e6;
     } else {
          scale = 1.;
     }

     // Iterate the records
     for (j=0; j<n_records; j++) {
          epr_read_record(did, j, record);

          // Get row number in units of km
          field = epr_get_field(record, "img_scan_y");
          scan_y[j] = epr_get_field_elem_as_double(field, 0) * 1e-3;

          // Get requested data
          field = epr_get_field(record, field_name);
          epr_copy_field_elems_as_doubles(field, data[j], n_tie_pts);

          // Convert angle from the input units to radians
          for (i=0; i<n_tie_pts; i++) {
               if (data[j][i] == -999999) {
                    data[j][i] = NAN;
               } else {
                    data[j][i] *= scale;
               }
          }
     }
     epr_free_record(record);

     // Allocate temporary arrays
     for (i=0; i<ny; i++) {
          sin_data[i] = calloc(nx, sizeof(double));
          cos_data[i] = calloc(nx, sizeof(double));
          if (sin_data[i] == NULL || cos_data[i] == NULL) {
               printf("ERROR: extrap_aatsr_angle: ");
               printf("Cannot allocate sin/cos arrays.\n");
               exit(1);
          }
     }

     /* Extrapolate data onto the inst grid. Take the sin and cos to retain
        the sign of the angle, by taking arctan when we copy. */
     if (ignore_centre_tie_pnt) {
          /* In some cases, the central tie point is invalid and we need to
             extrapolate the two sides of the orbit separately. */
          if (n_tie_pts % 2 == 0) {
               printf("ERROR: extrapolate2d: ignore_centre_tie_pnt requested ");
               printf("for an even length array.\n");
               exit(1);
          }

          // Find halfway points
          half_tie = n_tie_pts / 2;
          x0 = 0;
          while (x_out[x0] < 0.) x0++;

          if (x0 > 0) {
               extrapolate2d(0, half_tie, tie_pts, 0, n_records, scan_y,
                             0, x0, x_out, 0, ny, y_out,
                             data, sin, sin_data);
               extrapolate2d(0, half_tie, tie_pts, 0, n_records, scan_y,
                             0, x0, x_out, 0, ny, y_out,
                             data, cos, cos_data);
          }

          if (x0 != nx) {
               extrapolate2d(half_tie+1, half_tie, tie_pts, 0, n_records, scan_y,
                             x0, nx-x0, x_out, 0, ny, y_out,
                             data, sin, sin_data);
               extrapolate2d(half_tie+1, half_tie, tie_pts, 0, n_records, scan_y,
                             x0, nx-x0, x_out, 0, ny, y_out,
                             data, cos, cos_data);
          }
     } else {
          // Interpolate the entire field
          extrapolate2d(0, n_tie_pts, tie_pts, 0, n_records, scan_y,
                        0, nx, x_out,  0, ny, y_out,
                        data, sin, sin_data);
          extrapolate2d(0, n_tie_pts, tie_pts, 0, n_records, scan_y,
                        0, nx, x_out,  0, ny, y_out,
                        data, cos, cos_data);
     }

     // Copy into write array.
     for (j=0; j<ny; j++) {
          for (i=0; i<nx; i++)
               out[i + j*nx] = (float)(atan2(sin_data[j][i], cos_data[j][i]) *
                                       rad2deg);
     }

     // Tidying
     free(tie_pts);
     free(scan_y);
     for (i=0; i<n_records; i++) free(data[i]);
     for (i=0; i<ny; i++) {
          free(sin_data[i]);
          free(cos_data[i]);
     }
     free(data);
     if (verbose) printf("Extrapolated %s\n", field_name);
     return;
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void extrapolate2d(const long x0_in, const long nx_in, double *x_in,
                   const long y0_in, const long ny_in, double *y_in,
                   const long x0_out, const long nx_out, double *x_out,
                   const long y0_out, const long ny_out, double *y_out,
                   double **z, double (*func)(double), double **out)
{
     long i, j;
     long u[nx_out], v[ny_out];
     double dx[nx_out], dy[ny_out], tx, ty;
     double *f[ny_in];
     for (i=0; i<ny_in; i++) {
          f[i] = calloc(nx_in, sizeof(double));
          if (f[i] == NULL) {
               printf("ERROR: extrapolate2d: Cannot allocate f array.\n");
               exit(1);
          }
     }

     // Prepare for interpolation
     interpol_fraction(x0_in, nx_in, x_in, x0_out, nx_out, x_out, u, dx);
     interpol_fraction(y0_in, ny_in, y_in, y0_out, ny_out, y_out, v, dy);

     // Transform input data with given function
     for (j=0; j<ny_in; j++) {
          for (i=0; i<nx_in; i++)
               f[j][i] = func(z[y0_in + j][x0_in + i]);
     }

     // Perform bilinear interpolation
     for (j=0; j<ny_out; j++) {
          ty = 1. - dy[j];
          for (i=0; i<nx_out; i++) {
               tx = 1. - dx[i];
               out[y0_out + j][x0_out + i] =
                    f[v[j]][u[i]]     * tx    * ty    +
                    f[v[j]][u[i]+1]   * dx[i] * ty    +
                    f[v[j]+1][u[i]]   * tx    * dy[j] +
                    f[v[j]+1][u[i]+1] * dx[i] * dy[j];
          }
     }

     for (i=0; i<ny_in; i++) free(f[i]);
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
void interpol_fraction(const long in0, const long n_in,  double *in,
                       const long out0, const long n_out, double *out,
                       long *bounds, double *delta)
{
     long i, j;
     long previous_bound;
     const long in1 = in0 + n_in - 1;

     i = 0;
     // Points below the bottom of the grid
     while ((out[out0 + i] < in[in0]) && (i < n_out)) {
          bounds[i] = 0;
          delta[i]  = (out[out0 + i] - in[in0]) / (in[in0 + 1] - in[in0]);
          i++;
     }

     // Points off the top of the grid
     j = n_out - 1;
     while ((out[out0 + j] > in[in1]) && (j >= i)) {
          bounds[j] = n_in - 2;
          delta[j]  = (out[out0 + j] - in[in1 - 1]) / (in[in1] - in[in1 - 1]);
          j--;
     }

     // Points within the grid
     previous_bound = 0;
     for (; i<=j; i++) {
          bounds[i] = previous_bound;
          while (out[out0 + i] > in[in0 + bounds[i] + 1]) bounds[i]++;
          delta[i] = (out[out0 + i] - in[in0 + bounds[i]]) /
                     (in[in0 + bounds[i] + 1] - in[in0 + bounds[i]]);
          previous_bound = bounds[i];
     }
}
#endif
