load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"

begin

    f=addfile("test_hgt_plot/wrfout_d01_hgt.nc", "r")
    listxy1d=asciiread("list_xy.txt", -1, "float")
    nxy=dimsizes(listxy1d)/2
    listxy=onedtond(listxy1d, (/nxy,2/))
    listllx=wrf_user_ij_to_ll(f, listxy(:,0), listxy(:,1), False)
    print(dimsizes(listllx))
    listll=new((/nxy,2/), "float")
    listll(:,0)= listllx(0,:)
    listll(:,1)= listllx(1,:)

    opt=True
    opt@fout="output_list_ll.txt"
    write_matrix(listll, "2f8.3", opt)


end
