load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "initial_mpres_d01.ncl"

begin

    f=addfile("wrfout_d01_hgt.nc", "r")
    var=wrf_user_getvar(f, "HGT", 0)

    wks=gsn_open_wks("png", "wrf_var_native")
    gsn_define_colormap(wks, "BlAqGrYeOrReVi200")

    res=True
    res@cnFillOn = True
    res@cnLinesOn = False
    res@gsnSpreadColors= True

    plot=wrf_contour(f, wks, var, res)

    mres=initial_mpres()
    plotx=wrf_map_overlays(f, wks, (/plot/), True, mres)
    
end
