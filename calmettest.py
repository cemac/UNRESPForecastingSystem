#!/usr/bin/env python
# -*- coding:utf-8 -*-
# https://github.com/yeyuguo/metamet/tree/master/metlib/calmet
# calmet6.py

import os
import sys
#import re
from datetime import datetime, timedelta
import numpy as np
#import scipy as sp
#from scipy.io.numpyio import fread as sp_fread
#import matplotlib.pyplot as plt
#from mpl_toolkits.basemap import Basemap
#from matplotlib import mlab

__all__ = ['Calmet6Dataset']


class CalmetStopIteration(Exception):
    pass


_Calmet2DLabVar = [
    ('SC', 'IPGT'), ('US', 'USTAR'), ('ZI', 'ZI'),
    ('L', 'EL'), ('WS', 'WSTAR'),
    ('RMM', 'RMM'),
    ('TK', 'TEMPK'), ('D', 'RHO'), ('Q', 'QSW'),
    ('RH', 'IRH'),
    ('PC', 'IPCODE')
]
_Calmet2DLabVarCALGRD = [
    ('SC', 'IPGT'), ('US', 'USTAR'), ('ZI', 'ZI'),
    ('L', 'EL'), ('WS', 'WSTAR'),
    #('RMM', 'RMM'),
    ('TK', 'TEMPK'), ('D', 'RHO'), ('Q', 'QSW'),
    ('RH', 'IRH'),
    #('PC', 'IPCODE')
]
_CalmetFileDecl = [
    ('DATASET', 'S16'), ('DATAVER', 'S16'), ('DATAMOD', 'S64')
]
_CalmetControlParas = [
    ('IBYR', '%(INTTYPE)s'), ('IBMO', '%(INTTYPE)s'), ('IBDY', '%(INTTYPE)s'),
    ('IBHR', '%(INTTYPE)s'), ('IBSEC', '%(INTTYPE)s'),
    ('IEYR', '%(INTTYPE)s'), ('IEMO', '%(INTTYPE)s'), ('IEDY', '%(INTTYPE)s'),
    ('IEHR', '%(INTTYPE)s'), ('IESEC', '%(INTTYPE)s'), ('AXTZ', 'S8'),
    ('IRLG', '%(INTTYPE)s'), ('IRTYPE', '%(INTTYPE)s'),
    ('NX', '%(INTTYPE)s'), ('NY', '%(INTTYPE)s'), ('NZ', '%(INTTYPE)s'),
    ('DGRID', '%(REALTYPE)s'), ('XORIGR',
                                '%(REALTYPE)s'), ('YORIGR', '%(REALTYPE)s'),
    ('IWFCOD', '%(INTTYPE)s'), ('NSSTA', '%(INTTYPE)s'),
    ('NUSTA', '%(INTTYPE)s'), ('NPSTA', '%(INTTYPE)s'), ('NOWSTA',
                                                         '%(INTTYPE)s'), ('NLU', '%(INTTYPE)s'),
    ('IWAT1', '%(INTTYPE)s'), ('IWAT2',
                               '%(INTTYPE)s'), ('LCALGRD', '%(INTTYPE)s'),
    ('PMAP', 'S8'), ('DATUM', 'S8'), ('DATEN', 'S12'),
    ('FEAST', '%(REALTYPE)s'), ('FNORTH',
                                '%(REALTYPE)s'), ('UTMHEM', 'S4'), ('IUTMZN', '%(INTTYPE)s'),
    ('RNLAT0', '%(REALTYPE)s'), ('RELON0', '%(REALTYPE)s'), ('XLAT1',
                                                             '%(REALTYPE)s'), ('XLAT2', '%(REALTYPE)s')
]
_CalmetCellFaceHeights = [
    ('CLAB1', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                        '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['ZFACEM', '(%(NCELLFACE)d,)%(REALTYPE)s']
]
_CalmetXYSurfStations1 = [
    ('CLAB2', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                        '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['XSSTA', '(%(NSSTA)d,)%(REALTYPE)s']
]
_CalmetXYSurfStations2 = [
    ('CLAB3', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                        '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['YSSTA', '(%(NSSTA)d,)%(REALTYPE)s']
]
_CalmetXYUpperStations1 = [
    ('CLAB4', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                        '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['XUSTA', '(%(NUSTA)d,)%(REALTYPE)s'],
]
_CalmetXYUpperStations2 = [
    ('CLAB5', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                        '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['YUSTA', '(%(NUSTA)d,)%(REALTYPE)s']
]
_CalmetXYPrecStations1 = [
    ('CLAB6', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                        '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['XPSTA', '(%(NPSTA)d,)%(REALTYPE)s']
]
_CalmetXYPrecStations2 = [
    ('CLAB7', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                        '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['YPSTA', '(%(NPSTA)d,)%(REALTYPE)s']
]
_CalmetSurfRough = [
    ('CLAB8', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                        '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['Z0', '(%(NY)d, %(NX)d)%(REALTYPE)s']
]
_CalmetLanduse = [
    ('CLAB9', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                        '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['ILANDU', '(%(NY)d, %(NX)d)%(INTTYPE)s']
]
_CalmetElev = [
    ('CLAB10', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                         '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['ELEV', '(%(NY)d, %(NX)d)%(REALTYPE)s']
]
_CalmetLeafAreaIndex = [
    ('CLAB11', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                         '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['XLAI', '(%(NY)d, %(NX)d)%(REALTYPE)s']
]
_CalmetNearestSurfStationNo = [
    ('CLAB12', 'S8'), ('IDUM', '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ('IDUM',
                                                                         '%(INTTYPE)s'), ('IDUM', '%(INTTYPE)s'), ['NEARS', '(%(NY)d, %(NX)d)%(INTTYPE)s']
]
_CalmetData = [
    ('LAB', 'S8'), ('NDATHRB', '%(INTTYPE)s'), ('IBSEC', '%(INTTYPE)s'), ('NDATHRE',
                                                                          '%(INTTYPE)s'), ('IESEC', '%(INTTYPE)s'), ('DATA', '(%(NY)d, %(NX)d)%(REALTYPE)s')
]
_CalmetDataI = [
    ('LAB', 'S8'), ('NDATHRB', '%(INTTYPE)s'), ('IBSEC', '%(INTTYPE)s'), ('NDATHRE',
                                                                          '%(INTTYPE)s'), ('IESEC', '%(INTTYPE)s'), ('DATA', '(%(NY)d, %(NX)d)%(INTTYPE)s')
]
# TODO: sub NY, NX!!!


class Calmet6Dataset(object):
    """Calmet6Dataset represents Calmet version 6 datafile.
    Usage:
        data = Calmet6Dataset(filename)

    each variable in the usermanual can be accessed via data.VARNAME or data.__dict__[varname] .
    Some helper attrs:
    data.parameters: list of parameters like 'NX', 'NY', etc,
    invar: list of other header invariables
    var2d: list of 2d vars like 'USTAR', 'RHO', 'ZI', etc
    var3d: list of 3d vars like 'U', 'V', 'W', 'ZTEMP'
    vartime: list of time vars: 'BEGTIME', 'ENDTIME'
    TOTAL_BEGTIME: begin time as a python datetime object
    TOTAL_ENDTIME: end time as a python datetime object
    BEGTIME: array of each timestep's begin time
    ENDTIME: array of each timestep's end time
    """

    def __init__(self, fname, bit=32):
        self.INT4TYPE = 'i4'
        self.INT8TYPE = 'i8'
        self.REAL4TYPE = 'f4'
        self.REAL8TYPE = 'f8'

        self.parameters = []
        self.invar = ['ZFACEM', 'Z0', 'ILANDU', 'ELEV', 'XLAI']
        self.var3d = []
        self.var2d = []

        if bit == 64:
            self._recheadlen = 8
            self.INTTYPE = 'i4'
            self.REALTYPE = 'f8'
        else:
            self._recheadlen = 4
            self.INTTYPE = 'i4'
            self.REALTYPE = 'f4'

        self.fname = fname
        self._f = open(fname, 'rb')
        # Get file size
        self._f.seek(0, 2)
        self._filesize = self._f.tell()

        now_pos = 0

        # # Read Header
        tmp, now_pos = self._read_record(
            now_pos, 'S', convert_type=_CalmetFileDecl, add_to_dataset=True)
        self.NCOM, now_pos = self._read_record(now_pos, 'i4')
        self.parameters.append('NCOM')
        self.COMMENT = []
        for i in range(self.NCOM):
            comment, now_pos = self._read_record(now_pos, 'S')
            self.COMMENT.append(comment)
        tmp, now_pos = self._read_record(
            now_pos, 'S', convert_type=_CalmetControlParas, add_to_dataset=True)
        self.NCELLFACE = self.NZ + 1
        self.TOTAL_BEGTIME = datetime(
            self.IBYR, self.IBMO, self.IBDY, self.IBHR) + timedelta(seconds=int(self.IBSEC))
        self.TOTAL_ENDTIME = datetime(
            self.IEYR, self.IEMO, self.IEDY, self.IEHR) + timedelta(seconds=int(self.IESEC))
        self.parameters.extend([n for n, t in _CalmetControlParas])
        self.parameters.extend(['NCELLFACE', 'TOTAL_BEGTIME', 'TOTAL_ENDTIME'])
        tmp, now_pos = self._read_record(
            now_pos, 'S', convert_type=_CalmetCellFaceHeights, add_to_dataset=True)

        if self.NSSTA >= 1:
            tmp, now_pos = self._read_record(
                now_pos, 'S', convert_type=_CalmetXYSurfStations1, add_to_dataset=True)
            tmp, now_pos = self._read_record(
                now_pos, 'S', convert_type=_CalmetXYSurfStations2, add_to_dataset=True)
            self.invar.extend(['XSSTA', 'YSSTA'])
        if self.NUSTA >= 1:
            tmp, now_pos = self._read_record(
                now_pos, 'S', convert_type=_CalmetXYUpperStations1, add_to_dataset=True)
            tmp, now_pos = self._read_record(
                now_pos, 'S', convert_type=_CalmetXYUpperStations2, add_to_dataset=True)
            self.invar.extend(['XUSTA', 'YUSTA'])
        if self.NPSTA >= 1:
            tmp, now_pos = self._read_record(
                now_pos, 'S', convert_type=_CalmetXYPrecStations1, add_to_dataset=True)
            tmp, now_pos = self._read_record(
                now_pos, 'S', convert_type=_CalmetXYPrecStations2, add_to_dataset=True)
            self.invar.extend(['XPSTA', 'YPSTA'])

        for rec in [_CalmetSurfRough, _CalmetLanduse, _CalmetElev, _CalmetLeafAreaIndex]:
            tmp, now_pos = self._read_record(
                now_pos, 'S', convert_type=rec, add_to_dataset=True)

        if self.NSSTA >= 1:
            tmp, now_pos = self._read_record(
                now_pos, 'S', convert_type=_CalmetNearestSurfStationNo, add_to_dataset=True)
            self.invar.append('NEARS')

        # # Read Data
        # # Option LCALGRD
        MET2D_LIST = _Calmet2DLabVarCALGRD if self.LCALGRD else _Calmet2DLabVar
        # # Create arrays
        UVW_jumpsize = 8 + 4 * \
            np.dtype(self.INTTYPE).itemsize + self.NX * \
            self.NY * np.dtype(self.REALTYPE).itemsize
        # # Calc total record length (IRLG is only the hour number)
        pos_mem = now_pos
        tmpU, now_pos = self._read_record(
            now_pos, 'S', convert_type=_CalmetData, jumpsize=UVW_jumpsize)
        if tmpU is not None:
            NDATHRB, IBSEC, NDATHRE, IESEC = list(tmpU)[1:5]
            first_begt = datetime.strptime(
                str(NDATHRB), "%Y%j%H") + timedelta(seconds=int(IBSEC))
            first_endt = datetime.strptime(
                str(NDATHRE), "%Y%j%H") + timedelta(seconds=int(IESEC))
            tdelta_sec = (first_endt - first_begt).total_seconds()
            ttotal_sec = (self.TOTAL_ENDTIME -
                          self.TOTAL_BEGTIME).total_seconds()
            self.rec_num = int(np.ceil(ttotal_sec / tdelta_sec))
        else:
            self.rec_num = self.IRLG
        now_pos = pos_mem

        self.U = np.zeros((self.rec_num, self.NZ, self.NY,
                           self.NX), dtype=self.REALTYPE)
        self.V = np.zeros((self.rec_num, self.NZ, self.NY,
                           self.NX), dtype=self.REALTYPE)
        self.var3d.extend(['U', 'V'])
        if self.LCALGRD:
            self.W = np.zeros((self.rec_num, self.NZ, self.NY,
                               self.NX), dtype=self.REALTYPE)
            self.var3d.append('W')
        if self.LCALGRD and self.IRTYPE == 1:
            self.ZTEMP = np.zeros(
                (self.rec_num, self.NZ, self.NY, self.NX), dtype=self.REALTYPE)
            self.var3d.append('ZTEMP')
        if self.IRTYPE == 1:
            for labelname, varname in MET2D_LIST:
                dt = 'i4' if varname.startswith('I') else 'f4'
                self.__dict__[varname] = np.zeros(
                    (self.rec_num, self.NY, self.NX), dtype=dt)
                self.var2d.append(varname)

        # # BEGTIME and ENDTIME are datetime arrays, with shape (rec_num, )
        self.BEGTIME = np.zeros((self.rec_num,), dtype='O')
        self.ENDTIME = np.zeros((self.rec_num,), dtype='O')
        self.vartime = ['BEGTIME', 'ENDTIME']

        # # UVW
        for t in range(self.rec_num):
            try:
                for k in range(self.NZ):
                    tmpU, now_pos = self._read_record(
                        now_pos, 'S', convert_type=_CalmetData, jumpsize=UVW_jumpsize)
                    tmpV, now_pos = self._read_record(
                        now_pos, 'S', convert_type=_CalmetData, jumpsize=UVW_jumpsize)
                    if tmpU is not None:
                        self.U[t, k] = tmpU[5]
                    if tmpV is not None:
                        self.V[t, k] = tmpV[5]
                    if self.LCALGRD:
                        tmpW, now_pos = self._read_record(
                            now_pos, 'S', convert_type=_CalmetData, jumpsize=UVW_jumpsize)
                        if tmpW is not None:
                            self.W[t, k] = tmpW[5]
                if tmpU is not None:
                    NDATHRB, IBSEC, NDATHRE, IESEC = list(tmpU)[1:5]
                    self.BEGTIME[t] = datetime.strptime(
                        str(NDATHRB), "%Y%j%H") + timedelta(seconds=int(IBSEC))
                    self.ENDTIME[t] = datetime.strptime(
                        str(NDATHRE), "%Y%j%H") + timedelta(seconds=int(IESEC))
    #                print self.BEGTIME[t], self.ENDTIME[t], now_pos
                # # Temperature
                if self.LCALGRD and self.IRTYPE == 1:
                    for k in range(self.NZ):
                        tmpZTEMP, now_pos = self._read_record(
                            now_pos, 'S', convert_type=_CalmetData, jumpsize=UVW_jumpsize)
                        if tmpZTEMP is not None:
                            self.ZTEMP[t, k] = tmpZTEMP[5]
                # # 2D Met Fields
                if self.IRTYPE == 1:
                    for labelname, varname in MET2D_LIST:
                        dt = _CalmetDataI if varname.startswith(
                            'I') else _CalmetData
                        tmp, now_pos = self._read_record(
                            now_pos, 'S', convert_type=dt)
    #                    self.__dict__['CLAB%s' % labelname] = tmp[0]
                        self.__dict__[varname][t] = tmp[5]
            # # Useless Labels
    #        if tmpU is not None:
    #            self.CLABU = tmpU[0]
    #        if tmpV is not None:
    #            self.CLABV = tmpV[0]
    #        if self.LCALGRD:
    #            if tmpW is not None:
    #                self.CLABW = tmpW[0]
    #        if self.LCALGRD and self.IRTYPE == 1:
    #            if tmpZTEMP is not None:
    #                self.CLABT = tmpZTEMP[0]
            except CalmetStopIteration:
                sys.stderr.write(
                    "Warning: Calmet output file %s may be incomplete!\n" % self.fname)
                break

    def _read_record(self, pos, dtype, shape=1, convert_type=None, add_to_dataset=False, jumpsize=0):
        try:
            recsize = np.memmap(self._f, dtype='i%d' %
                                self._recheadlen, mode='c', offset=pos, shape=(1,))[0]
        except ValueError:
            raise CalmetStopIteration

        if recsize == 0:
            return None, pos + jumpsize + self._recheadlen * 2
        if convert_type is None:
            if dtype == 'S':
                dtype = 'S%d' % recsize
            data = np.memmap(self._f, dtype=dtype, mode='c',
                             offset=pos + self._recheadlen, shape=shape)
            if shape == 1:
                data = data[0]
        else:
            c = 0
            new_cv_type = []
            for cp in convert_type:
                name = cp[0]
                tp = cp[1]
                if name == 'IDUM':
                    name = 'IDUM%d' % c
                    c += 1
                if '%' in tp:
                    tp = tp % self.__dict__
                new_cv_type.append((name, tp))
            self._f.seek(pos + self._recheadlen)
            data = np.fromfile(self._f, dtype=new_cv_type, count=1)[0]
            if add_to_dataset is True:
                for i, cp in enumerate(new_cv_type):
                    if not cp[0].startswith('IDUM') and not cp[0].startswith('CLAB'):
                        self.__dict__[cp[0]] = data[i]
#        print pos + recsize + self._recheadlen * 2
        return data, pos + recsize + self._recheadlen * 2
