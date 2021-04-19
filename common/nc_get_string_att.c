/**
    Routine to read a string attribute from a NetCDF variable.

    This isn't supported natively by netcdf-fortran and so this is an
    interface to the C routine that can. Code is largely cribbed from
    https://www.unidata.ucar.edu/software/netcdf/docs/group__attributes.html#ga0d66350856a4a6dd3f459fd092937c27

    History:
    2021/03/16, AP: Original version.
**/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <netcdf.h>

void nc_check(const int stat)
{
     if (stat != NC_NOERR) {
          printf("ERROR: ncdf_get_string_att(): %s\n", nc_strerror(stat));
          exit(1);
     }
}

void nc_get_string_att (const char *fname, const char *vname,
                        const char *aname, char str[128])
{
     int stat = 0;

     int ncid = 0;
     stat = nc_open(fname, NC_NOWRITE, &ncid); nc_check(stat);

     int varid = 0;
     stat = nc_inq_varid(ncid, vname, &varid); nc_check(stat);

     size_t attlen = 0;
     stat = nc_inq_attlen(ncid, varid, aname, &attlen); nc_check(stat);

     char **string_attr = (char**)malloc(attlen * sizeof(char*));
     memset(string_attr, 0, attlen * sizeof(char*));

     stat = nc_get_att_string(ncid, varid, aname, string_attr); nc_check(stat);
     // Copy the first, and only the first, element of the array
     strcpy(str, string_attr[0]);

     stat = nc_free_string(attlen, string_attr); nc_check(stat);
     free(string_attr);

     stat = nc_close(ncid); nc_check(stat);

     return;
}
