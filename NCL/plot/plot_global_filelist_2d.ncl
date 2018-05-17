load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

; --------------------------------------------------------------------------
; Description:
;
;   specify vars, plot a series of file (series of tidx)
;   parameter : filepath(for 1 file) .or. file_list(for multi-files)
;                vars, tidx_bgn, tidx_end, outfile_prefix 
;   output : one output file with each variable as "outfile_prefix""var".pdf
;   NOTICE : vars can not be time-invariant
;
; History:
;
;   2014-06-16   [shensi]: First creation.
; --------------------------------------------------------------------------

begin

;    filepath="/disk1/home/shensi/barotropic-model/prepare_init/erai_input.nc"
    file_list="list_files.txt"
    filepath = systemfunc("cat "+file_list)
    vars=(/"u","v","gd"/)
    tidx_bgn=0 ;;
    tidx_end=-1 ;; -1 means the end tidx of each file
    outfile_prefix="fig_"
    
    nfile=dimsizes(filepath)
    nvar=dimsizes(vars)
    do ivar=0, nvar-1
        var=vars(ivar)
        wks=gsn_open_wks("pdf", outfile_prefix+var)
        gsn_define_colormap(wks,"BlueDarkRed18")
        res=True
        res@gsnSpreadColors = True
        res@cnFillOn=True
        res@cnLinesOn=False
        do i=0, nfile-1
            f=addfile(filepath(i),"r")
            file_title=systemfunc("basename "+filepath(i))
            ;;time variant
            print((/file_title/))
            dim_shape=dimsizes(f->$vars(ivar)$)
            print((/dim_shape/))
            time_len=dim_shape(0)
            if(tidx_end .lt. 0) then
                tidx_end=time_len-1
            end if
            if(tidx_end .ge. time_len) then
                print((/"WARNING: tidx_end is larger than time_len of file "+ filepath(i)/))
                tidx_end=time_len-1
            end if
            do tidx=tidx_bgn, tidx_end
                field=f->$vars(ivar)$(tidx,:,:)
                title=file_title+" @t="+tidx
                res@tiMainString = title 
                plot=gsn_csm_contour_map_ce(wks, field, res)
            end do
            delete(dim_shape)
        end do
        delete(wks)
        delete(res)
    end do
end
