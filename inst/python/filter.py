from scipy import signal


def filter_signal(x, n, wn, type):
    """Apply a forward-backward butterworth digital filter to a signal using.

    Parameters
    ----------
    x : The array of data to be filtered.
    n : The  order of the filter.
    wn : The critical frequency or frequencies.
    type : The type of the filter.

    Returns
    -------
    ndarray
        The filtered output with the same shape as x.
    """
    sos = signal.butter(n, wn, type, output='sos')
    x_filt = signal.sosfiltfilt(sos, x)
    return(x_filt)
