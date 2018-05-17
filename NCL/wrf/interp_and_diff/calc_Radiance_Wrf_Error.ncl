load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "read_obs_radiance_txt.ncl"

begin

 if (.not. isvar("file_in")) then
     print(" ")
     print(" You did not define the input file --> EXIT")
     exit
 end if

 if(.not. isvar("obs_txt_file")) then
     print(" ")
     print(" You did not define the obs_txt_file  --> EXIT")
     exit
 end if

 f=addfile(file_in+".nc", "r")
 var=f->SWDOWN

 obs_rad=read_obs_radiance_txt(obs_txt_file, 6)
 opt=True
 opt@returnInt = True
 pos=wrf_user_ll_to_ij(f, obs_rad@lons, obs_rad@lats, opt)
 idx = pos -1
 wrf_value = var(idx(0,:), idx(1,:))
 diff_value = wrf_value - obs_rad

 ;write out
 npt=dimsizes(obs_rad)
 title="stnid, lons, lats, obs_rad, wrf_rad, error_rad"
 lines=new((/npt+1/), "string")
 lines(0)=title
 do i=0, npt-1
     lines(i+1) = obs_rad@stnids(i) + sprintf(",%8.3f", obs_rad@lons(i)) + sprintf(",%8.3f", obs_rad@lats(i)) + \
                sprintf(",%8.2", obs_rad(i))+sprintf(",%8.2", wrf_value(i)) + sprintf(",%8.2f", diff_value(i))
 end do
 asciiwrite("output.txt", lines)
end
