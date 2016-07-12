static int XCAT(snprint_scaler_, TYPE_NAME)(char *buf, size_t size, const TYPE_C x) {

    return snprintf(buf, size, TYPE_FORMAT, x);
}


static int XCAT(TYPE_NAME, _array_snprint)(char *buf, size_t size, const TYPE_C *array,
                unsigned int n, const unsigned int *dims, const unsigned int i, const
                unsigned int stride) {

    unsigned int j;
    unsigned int count = 0;

    for (j = 0; j < dims[i]; ++j) {
        if (i == 0) {
            count += XCAT(snprint_scaler_, TYPE_NAME)(buf + count, XXX(size, count),
                                                      array[j]);
            if (j < dims[i] - 1)
                count += snprintf(buf + count, XXX(size, count), ", ");
        }
        else {
            count += XCAT(TYPE_NAME, _array_snprint)(buf + count, XXX(size, count),
                                                     &array[j * stride], n, dims,
                                                     i - 1, stride / dims[i]);
            if (j < dims[i] - 1)
                count += snprintf(buf + count, XXX(size, count), "; ");
        }
    }

    return count;
}


static int XCAT(snprint_array_, TYPE_NAME)(char *buf, size_t size, const TYPE_C *array,
                unsigned int n, const unsigned int *dims) {

    unsigned int i;
    unsigned int stride;

    stride = 1;
    for (i = 0; i < n - 1; ++i)
        stride *= dims[i];

    return XCAT(TYPE_NAME, _array_snprint)(buf, size, array, n, dims, n - 1, stride);
}
