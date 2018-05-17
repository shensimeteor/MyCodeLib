; --------------------------------------------------------------------------
; Description:
;
;   plot the map & draw markers (lon/lat) given by list_points.txt
;
; History:
;
;   2014-08-16   [shensi]: First creation.
; --------------------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

    data=asciiread("list_points.txt", -1, "float")
    npt=dimsizes(data)/2
    lonlat=onedtond(data, (/npt,2/))
    lon=lonlat(:,0)
    lat=lonlat(:,1)
    wks=gsn_open_wks("pdf","world_map_ce_markers")
    res=True
    res@gsnDraw=False
    res@gsnFrame=False
    res@mpCenterLonF=180
    plot=gsn_csm_map_ce(wks,res)
    
    mpres=True
    mpres@gsMarkerIndex=4  ;;;
    mpres@gsMarkerSizeF = 16.0 ;;;
    plot_x=gsn_add_polymarker(wks,plot,lon,lat,mpres)

    draw(plot)
    frame(wks)
end
