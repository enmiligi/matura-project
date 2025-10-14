def iterate(currentRe, currentIm, cRe, cIm, n):
    for i in range(n):
        nextRe = currentRe * currentRe - currentIm * currentIm + cRe
        nextIm = 2.0 * currentRe * currentIm + cIm
        if nextRe * nextRe + nextIm * nextIm > 4.0:
            return False

    return True


def printPoint(re, im):
    if iterate(0.0, 0.0, re, im, 100):
        print("#", end="")
    else:
        print(" ", end="")


def printRow(im, reStart, reEnd, stride):
    re = reStart
    while re < reEnd:
        printPoint(re, im)
        re += stride


def printMandelbrot(reStart, reEnd, imStart, imEnd, resolution):
    im = imStart
    while im <= imEnd:
        printRow(im, reStart, reEnd, resolution)
        print("")
        im += resolution


printMandelbrot((-2.0), 2.0, -2.0, 2.0, 0.1)
