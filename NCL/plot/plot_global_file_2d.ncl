load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

; --------------------------------------------------------------------------
; Description:
;
;   plot 2d-vars of file (global; nc) in a pdf file
;   parameter: file, var_list (string list: "u v h oro"), tidx, outfile
;
; History:
;
;   2014-06-16   [shensi]: First creation.
; --------------------------------------------------------------------------

begin
    filepath="/disk1/home/shensi/barotropic-model/prepare_init/erai_input.nc"
    vars=(/"u","v","gd","ghs"/)
    tidx=0
    outfile="2dvar_t0.pdf"

    f=addfile(filepath,"r")
    nvar=dimsizes(vars)

    wks=gsn_open_wks("pdf", outfile)
    gsn_define_colormap(wks,"BlueDarkRed18")
    res=True
    res@gsnSpreadColors = True
    res@cnFillOn=True
    res@cnLinesOn=False

    do i=0,nvar-1
        dim_sizes=dimsizes(f->$vars(i)$)
        ndim=dimsizes(dim_sizes)
        if(ndim .eq. 3) then
            ;;time variant
            field=f->$vars(i)$(tidx,:,:)
            title=vars(i)+"@t="+tidx
        else
            ;;time invariant
            field=f->$vars(i)$
            title=vars(i)
        end if
        res@tiMainString = title 
        plot=gsn_csm_contour_map_ce(wks, field, res)
        delete(dim_sizes)
    end do


end
