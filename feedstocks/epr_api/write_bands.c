#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include "epr_api.h"

void get_meris_sun_spec_flux(EPR_SProductId* product_id, float sun_spec_flux[15])
{
    EPR_SDatasetId* dataset_id;
    EPR_SRecord* record;
    const EPR_SField* field;
    dataset_id = epr_get_dataset_id(product_id, "Scaling_Factor_GADS");
    record = epr_read_record(dataset_id, 0, NULL);
    field = epr_get_field(record, "sun_spec_flux");
    epr_copy_field_elems_as_floats(field, sun_spec_flux, 15);
    epr_free_record(record);
}
int write_raw_image(const char* output_dir, EPR_SProductId* product_id, const char* band_name);

#if defined(WIN32) && defined(_DEBUG)
#include <crtdbg.h>
#endif /* if defined(WIN32) && defined(_DEBUG) */

/**
 * A program for converting producing ENVI raster information from dataset.
 *
 * It generates as many raster as there are dataset entrance parameters.
 *
 * Call: write_bands <ENVISAT-Product file path>
 *                  <Output directory for the raster file>
 *                  <Dataset name 1>
 *                  [<Dataset name 2> ... <Dataset name N>]
 *
 * Example:
 *
 *    write_bands
 *              "d:/ENVISAT/data/MERIS/L1b/MER_RR__1PNPDK20020415_103725_000002702005_00094_00649_1059.N1"
 *              "./"
 *              "latitude"
 *              ["longitude" "dem_alt" "dem_rough" "lat_corr" "lon_corr" "sun_zenith" "sun_azimuth"
 *               "view_zenith" "view_azimuth" "zonal_wind" "merid_wind" "atm_press" "ozone" "rel_hum"
 *               "radiance_1" "radiance_2" "radiance_3" "radiance_4" "radiance_5" "radiance_6"
 *               "radiance_7" "radiance_8" "radiance_9" "radiance_10" "radiance_11" "radiance_12"
 *               "radiance_13" "radiance_14" "radiance_15"]
 *          OR
 *              "data/MERIS/L2/MER_RR__2PNPDK20020329_092030_000002701003_00036_00405_0054.N1"
 *
 *          OR
 *              "data/AATSR/L1b/ATS_TOA_1PTRAL19950510_071649_00000000X000_00000_00000_0000.N1"
 *                              ATS_TOA_1PNETB20020411_074000_000049912005_00035_00590_0289.N1
 *              "./"
 *              "latitude"
 *              ["longitude" "lat_corr_nadir" "lon_corr_nadir" "lat_corr_fward" "lon_corr_fward" "altitude"
 *               "sun_elev_nadir" "view_elev_nadir" "sun_azimuth_nadir" "view_azimuth_nadir" "sun_elev_fward"
 *               "view_elev_fward" "sun_azimuth_fward" "view_azimuth_fward"
 *               "btemp_nadir_1200" "btemp_nadir_1100" "btemp_nadir_0370" "reflec_nadir_1600" "reflec_nadir_0870" "reflec_nadir_0670" "reflec_nadir_0550"
 *               "btemp_fward_1200" "btemp_fward_1100" "btemp_fward_0370" "reflec_fward_1600" "reflec_fward_0870" "reflec_fward_0670" "reflec_fward_0550"
 *               "confid_flags_nadir" "confid_flags_fward" "cloud_flags_nadir" "cloud_flags_fward"]
 *
 */
int main(int argc, char** argv)
{
    EPR_SProductId* product_id;
    int i;
    const char* product_file_path;
    const char* output_dir_path;

    if (argc <= 3) {
        printf("Usage: write_bands <envisat-product> <output-dir> <dataset-name-1>");
        printf("                   [<dataset-name-2> ... <dataset-name-N>]\n");
        printf("  where envisat-product is the input filename\n");
        printf("  and output-dir is the output directory\n");
        printf("  and dataset-name-1 is the name of the first band to be extracted (mandatory)\n");
        printf("  and dataset-name-2 ... dataset-name-N are the names of further bands to be extracted (optional)\n");
        printf("Example:\n");
        printf("  write_bands \"./MER_RR__2P_TEST.N1\" \".\\\" \"latitude\"\n\n");
        exit(1);
    }

    product_file_path = argv[1];
    output_dir_path = argv[2];

    /* Initialize the API. Set log-level to DEBUG and use default log-output (stdout) */
    epr_init_api(e_log_debug, epr_log_message, NULL);

    /* Open the product; an argument is a path to product data file */
    product_id = epr_open_product(product_file_path);

    for (i = 3; i < argc; i ++) {
        write_raw_image(output_dir_path, product_id, argv[i]);
    }

    /* Close product_id and release rest of the allocated memory */
    epr_close_product(product_id);
    /* Closes product reader API, release all allocated resources */
    epr_close_api();

    return 0;
}


/**
 * Generate the ENVI binary pattern image file for an actual DS.
 *
 * The first parameter is the output directory path.
 *
 * The function returns 1, if the file is generated, 0 otherwise.
 */
int write_raw_image(const char* output_dir, EPR_SProductId* product_id, const char* band_name)
{
    FILE *out_stream;
    uint y, numwritten;
    char image_file_path[1024];
    EPR_SBandId* band_id = NULL;
    int err_code;
    EPR_SRaster* raster = NULL;
    uint source_w, source_h;
    uint source_step_x, source_step_y;

    /* Build ENVI file path, DS name specifically */
#ifdef WIN32
    sprintf(image_file_path, "%s\\%s.raw", output_dir, band_name);
#else
    sprintf(image_file_path, "%s/%s.raw", output_dir, band_name);
#endif

    band_id = epr_get_band_id(product_id, band_name);
    if (band_id == NULL) {
        printf("Error: band '%s' not found\n", band_name);
        return 1;
    }
    source_w = epr_get_scene_width(product_id);
    source_h = epr_get_scene_height(product_id);
    source_step_x = 1;
    source_step_y = 1;

    raster = epr_create_compatible_raster(band_id, source_w, source_h, source_step_x, source_step_y);

    printf("Reading band '%s'...\n", band_name);
    err_code = epr_read_band_raster(band_id, 0, 0, raster);
    if (err_code != 0) {
        printf("Error: can't read raster data from '%s'\n", band_name);
        return 2;
    }

    out_stream = fopen(image_file_path, "wb");
    if (out_stream == NULL) {
        printf("Error: can't open '%s'\n", image_file_path);
        return 3;
    }

    for (y = 0; y < (uint)raster->raster_height; y ++) {
        numwritten = fwrite(epr_get_raster_line_addr(raster, y),
                            raster->elem_size,
                            raster->raster_width,
                            out_stream);

        if (numwritten != raster->raster_width) {
            printf("Error: can't write to %s\n", image_file_path);
            return 4;
        }
    }
    fclose(out_stream);

    printf("Raw image data successfully written to '%s'.\n", image_file_path);
    printf("C data type is '%s', element size %u byte(s), raster size is %u x %u pixels.\n",
        epr_data_type_id_to_str(raster->data_type),
        raster->elem_size,
        raster->raster_width,
        raster->raster_height);

    epr_free_raster(raster);

    return 0;
}
