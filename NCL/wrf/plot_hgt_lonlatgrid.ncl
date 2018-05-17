load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "initial_mpres_d01.ncl"
load "/glade/u/home/sishen/MyLib/NCL/lib/ss_ncl.ncl"
begin

    f=addfile("wrfout_d01_hgt.nc", "r")
    var=wrf_user_getvar(f, "HGT", 0)
    lon2d=wrf_user_getvar(f, "XLONG", 0)
    lat2d=wrf_user_getvar(f, "XLAT", 0)
    var@lon2d=lon2d
    var@lat2d=lat2d

    wks=gsn_open_wks("png", "wrf_var_lonlat")
    gsn_define_colormap(wks, "BlAqGrYeOrReVi200")

    res=initial_mpres()
    res@cnFillOn = True
    res@cnLinesOn = False
    res@gsnSpreadColors= True
    res@gsnAddCyclic = False
    res@mpMinLonF = lower_limit(min(lon2d), 5) 
    res@mpMaxLonF = upper_limit(max(lon2d), 5)
    res@mpMinLatF = lower_limit(min(lat2d), 5)
    res@mpMaxLatF = upper_limit(max(lat2d), 5)
    plot=gsn_csm_contour_map(wks, var, res)

end
