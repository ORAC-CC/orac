/*******************************************************************************
 * Name: system_utils_c.c
 *
 * Purpose:
 *
 * History:
 * 2015/10/05, GM: Original version.
 *
 ******************************************************************************/

#include <dirent.h>
#include <regex.h>
#include <stdio.h>
#include <string.h>


/*******************************************************************************
 * Name: system_utils_match_file
 *
 * Purpose:
 *
 * Description and Algorithm details:
 *
 * Arguments:
 * Name : Description
 *
 * History:
 * 2015/10/05, GM: Original version.
 *
 * Bugs:
 * None known.
 ******************************************************************************/
int system_utils_match_file(const char *dir_name, const char *file_regex,
                            char *file_name, int n)
{
    int flag;

    DIR           *dir;
    struct dirent *entry;

    regex_t regex;

    if ((dir = opendir(dir_name)) == NULL) {
        fprintf(stderr, "ERROR: Error opening directory: %s\n", dir_name);
        return -1;
    }

    if (regcomp(&regex, file_regex, 0)) {
        fprintf(stderr, "ERROR: Error compiling regular expression: %s\n", file_regex);
        return -1;
    }

    flag = 0;

    while ((entry = readdir(dir)) != NULL) {
        if (regexec(&regex, entry->d_name, 0, NULL, 0) == 0) {
            if (file_name)
                strncpy(file_name, entry->d_name, n);
            flag = 1;
            break;
        }
    }

    regfree(&regex);

    closedir(dir);

    return flag;
}
