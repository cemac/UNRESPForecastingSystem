from __future__ import print_function
import argparse
import os
import matplotlib as mpl
from dateutil.parser import parse
import maptoolkit as mtk
# University System python may be broken
# If some one insists on using it...
backend = mpl.get_backend()
if backend == 'Qt4Agg' and sys.version_info[0] == 2:
    # Fix the backend
    print('swapping to Agg Backend')
    print('Please consider using anaconda')
    mpl.use('Agg')
# DO NOT MOVE ABOVE BACKEND FIX
import matplotlib.pyplot as plt  # KEEP ME HERE!!!
################################

mpt = mtk.Masaya_Maps('20190412')
im = mtk.gen_im(mpt.lonMin, mpt.latMin, mpt.lonMax,
                mpt.latMax, imtype='World_Shaded_Relief')
Figletter = ['a) ', 'b) ', 'c) ', 'd) ', 'e) ', 'f) ', 'g) ', 'h) ', 'j) ',
             'k) ', 'l) ']
filelist = ['2018201806avP.txt', '2018201807avP.txt', '2018201808avP.txt',
            '2018201809avP.txt', '2018201810avP.txt', '2018201811avP.txt',
            '2018201812avP.txt', '2019201901avP.txt', '2019201902avP.txt',
            '2019201903avP.txt']
mnth = ['Jun 2018', 'Jul 2018', 'Aug 2018', 'Sep 2018', 'Oct 2018', 'Nov 2018',
        'Dec 2018', 'Jan 2019', 'Feb 2019', 'Mar 2019']
SOX = 'SO2'
fig = plt.figure(figsize=(30, 16))
for i, fle in enumerate(filelist):
    let = Figletter[i]
    ax = fig.add_subplot(3, 4, i+1)
    flepath = 'analysis_tools/' + fle
    concA = mpt.plot_av_stamp(ax, mnth[i], let, im, flepath, SOX)
plt.tight_layout()
PNGfile = 'P_stamp.png'
print("Writing out file " + PNGfile)
PNGpath = os.path.join('analysis_tools/images/', PNGfile)
plt.savefig(PNGpath, dpi=250)
plt.close()
