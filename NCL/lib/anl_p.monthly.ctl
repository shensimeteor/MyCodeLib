dset ^anl_p.%y4%m2.gr
undef 9.999E+20
title anl_p monthly average
options template big_endian
ydef  145  linear -90.0 1.25
xdef  288  linear   0.0 1.25
tdef   403  linear  01jan1979  1mo
zdef 23  levels
1000 925 850 700 600 500 400 300 250 200 150 100 70 50 30 20 10 7 5 3 2 1 0.4
vars 13 
HGTprs   23 99 ** Geopotential height            [gpm]
TMPprs   23 99 ** Air temperature                  [K]
SPFHprs  12 99 ** Specific humidity            [kg/kg]
DEPRprs   8 99 ** Dew point depression             [K]
UGRDprs  23 99 ** Zonal wind                     [m/s]
VGRDprs  23 99 ** Meridional wind                [m/s]
CWATprs  12 99 ** Cloud water content          [kg/kg]
PRMSLmsl  0 99 ** Pressure reduced to MSL         [Pa]
TMPsfc    0 99 ** Surface 2m air temperature       [K]
SPFHsfc   0 99 ** Surface 2m specific humidity [kg/kg]
DEPRsfc   0 99 ** Surface 2m dew point depression  [K]
UGRDsfc   0 99 ** Surface 10m zonal wind         [m/s]
VGRDsfc   0 99 ** Surface 10m meridional wind    [m/s]
ENDVARS
