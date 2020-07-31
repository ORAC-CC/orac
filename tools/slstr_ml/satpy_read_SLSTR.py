"""Plot some useful images to help undertand and diagnose your results."""
import click


def norm(band):
    """ Normalises the bands for the false color image."""
    band_min, band_max = band.min(), band.max()
    return ((band - band_min) / (band_max - band_min))


def plot_slstr(filenames,
               mask=None,
               verbose=False,
               save=True,
               show=False,
               odir=None):
    """Plot the SLSTR granule."""
    from matplotlib import colors as mcolors
    import matplotlib.pyplot as plt
    # import Visualisation as vis
    from satpy import Scene
    import numpy as np

    if save and odir is None:
        print("ERROR: You must specify an output directory for saving.")
        quit()

    scn = Scene(filenames=filenames, reader='slstr_l1b')
    scn.load(['S1_an', 'S2_an', 'S3_an', 'S4_an', 'S5_an', 'S6_an', 'S7_in',
              'S8_in', 'S9_in', 'bayes_an', 'bayes_in', 'cloud_an',
              'latitude_an', 'longitude_an',
              'satellite_zenith_angle_n',
              'solar_zenith_angle_n', 'confidence_an'])
    if verbose:
        print('scn info', scn)

    # Plot the channels
    plt.imshow(scn['S1_an'], cmap='gray', vmin=0, vmax=100)
    plt.colorbar()
    plt.title('S1')
    if show:
        plt.show()
    if save:
        plt.savefig(odir + 'S1.png')

    plt.imshow(scn['S2_an'], cmap='jet', vmin=0, vmax=100)
    plt.colorbar()
    plt.title('S2')
    if show:
        plt.show()
    if save:
        plt.savefig(odir + 'S2.png')

    S2 = np.nan_to_num(scn['S2_an'].values)
    S3 = np.nan_to_num(scn['S3_an'].values)
    S5 = np.nan_to_num(scn['S5_an'].values)
    CAN = np.nan_to_num(scn['confidence_an'].values)

    # create some false colour plots

    # this does more natural colours
    #    green = norm(S1)
    #    red = norm(S2)
    #    IR = norm(S3 + S4 + S5 + S6)
    #    blue = norm(0.8 * green - 0.1 * red - 0.1 * IR)

    red = norm(S5)  # 1.6
    green = norm(S3)  # .87
    blue = norm(S2)  # .67

    rgb = np.dstack((red, green, blue))
    brightness = 0.2
    hsv = mcolors.rgb_to_hsv(rgb)
    hsv[:, :, 2] += brightness
    rgb = mcolors.hsv_to_rgb(hsv)
    rgb[rgb > 1] = 1

    LatPos = str(round(np.array(scn['latitude_an'].values)[0, 0], 6))
    LonPos = str(round(np.array(scn['longitude_an'].values)[0, 0], 6))
    FileStr = 'xx'
    TitleStr = '(' + LatPos + ', ' + LonPos + ')\n' + FileStr

    plt.figure()
    plt.imshow(rgb)
    plt.title('False colour image\n' + TitleStr)
    if show:
        plt.show()
    if save:
        plt.savefig(odir + 'False_Color.png')

    # now mask it
    if mask is not None:
        mask = mask.astype('bool')
        red[mask] = 254 / 255
        green[mask] = 253 / 255
        blue[mask] = 185 / 255

    rgb = np.dstack((red, green, blue))
    brightness = 0.2
    hsv = mcolors.rgb_to_hsv(rgb)
    hsv[:, :, 2] += brightness
    rgb = mcolors.hsv_to_rgb(hsv)
    rgb[rgb > 1] = 1

    LatPos = str(round(np.array(scn['latitude_an'].values)[0, 0], 6))
    LonPos = str(round(np.array(scn['longitude_an'].values)[0, 0], 6))
    FileStr = 'xx'
    TitleStr = '(' + LatPos + ', ' + LonPos + ')\n' + FileStr

    plt.imshow(rgb)
    plt.title('False colour image masked\n' + TitleStr)
    if show:
        plt.show()
    if save:
        plt.savefig(odir + 'False_Color_Masked.png')

    # now plot Normalised Difference Snow Index
    ndsi = (S2-S5)/(S2+S5)

    plt.figure()
    plt.imshow(ndsi)
    plt.colorbar()
    plt.title('NDSI image\n' + TitleStr)
    if show:
        plt.show()
    if save:
        plt.savefig(odir + 'NDSI.png')

    bitmeanings = {
            'Coastline': 1,
            'Ocean': 2,
            'Tidal': 4,
            'Dry land': 24,
            'Inland water': 16,
            'Cosmetic': 256,
            'Duplicate': 512,
            'Day': 1024,
            'Twilight': 2048,
            'NDSI snow': 8192}

    # Plot the masks
    mask_ocean = CAN & bitmeanings['Ocean']
    plt.imshow(mask_ocean)
    plt.title('mask ocean\n')
    if show:
        plt.show()
    if save:
        plt.savefig(odir + 'Mask_Ocean.png')

    mask_dryland = CAN & bitmeanings['Dry land']
    plt.imshow(mask_dryland)
    plt.title('mask dryland\n')
    if show:
        plt.show()
    if save:
        plt.savefig(odir + 'False_DryLand.png')

    mask_coastline = CAN & bitmeanings['Coastline']
    plt.imshow(mask_coastline)
    plt.title('mask coastline\n')
    if show:
        plt.show()
    if save:
        plt.savefig(odir + 'False_Coast.png')

    mask_ndsi = CAN & bitmeanings['NDSI snow']
    plt.imshow(mask_ndsi)
    plt.title('mask NSDI\n')
    if show:
        plt.show()
    if save:
        plt.savefig(odir + 'False_NDSI.png')


@click.command()
@click.option('--idir', default='./')
@click.option('--odir', default=None)
@click.option('--save', default=False)
@click.option('--show', default=True)
@click.option('--verbose', default=False)
def main(idir, odir, save, show, verbose):
    """To run from command line."""
    from glob import glob

    filenames = glob(idir + '/*.nc')
    if len(filenames) < 5:
        print("ERROR: You must specify a valid Sentinel-3 directory.")
        quit()

    plot_slstr(filenames,
               odir=odir,
               save=save,
               show=show,
               verbose=verbose)


if __name__ == "__main__":
    main()
