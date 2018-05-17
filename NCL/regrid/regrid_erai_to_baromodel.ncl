load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

    erai_file="ERA_I_1.nc"
    level_select=500
    file_output="erai_input_500hpa.nc"

    orography_file="erai-orography.nc"
    f_oro=addfile(orography_file, "r")
    ghs_short=f_oro->z
    ghs=short2flt(ghs_short(0,::-1,:))
    f_init=addfile(erai_file, "r")
    gd=short2flt(f_init->z(:,{level_select},::-1,:))
    u=short2flt(f_init->u(:,{level_select},::-1,:))
    v=short2flt(f_init->v(:,{level_select},::-1,:))

    printVarSummary(ghs&latitude)

    nlat=121
    nlon=240
    lat_low=latGlobeF(nlat, "lat", "latitude", "degrees_north")
    lon_low=lonGlobeF(nlon, "lon", "longitude", "degrees_east")
    
    ghs_low=area_conserve_remap_Wrap(ghs&longitude, ghs&latitude, ghs, lon_low, lat_low, False)
    
;    ghs_low=smth9(ghs_low, 0.5, 0.25, True)
    ghs_low=ghs_low*0.0
    gd_low=area_conserve_remap_Wrap(gd&longitude, gd&latitude, gd, lon_low, lat_low, False)
    gd_low(0,:,:)=gd_low(0,:,:)-ghs_low
    u_low=area_conserve_remap_Wrap(u&longitude, u&latitude, u, lon_low, lat_low, False)
    v_low=area_conserve_remap_Wrap(v&longitude, v&latitude, v, lon_low, lat_low, False)


    system("/bin/rm -f "+file_output) 
    ncdf=addfile(file_output,"c")
    filedimdef(ncdf, "time", -1, True)
    ncdf->u=u_low
    ncdf->v=v_low
    ncdf->gd=gd_low
    ncdf->ghs=ghs_low
end
