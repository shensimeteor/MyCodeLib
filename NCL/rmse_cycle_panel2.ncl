load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

function tcorcoef(x:numeric, y:numeric)
local xycov, xstdv, ystdv, xycor
begin
    xycov=sum((x-avg(x))*(y-avg(y)))
    xstdv=sqrt(sum((x-avg(x))^2))
    ystdv=sqrt(sum((y-avg(y))^2))
    xycor=xycov/(xstdv*ystdv)
    return(xycor)
end



begin
mx=40
my=40
nt=120
title_wks="paper_rmse_ctrlxb_paper_panel"
EXPT_DIR=(/"obs4_noB/","obs03_noB/"/)
path_xt=(/"series_true.dat","series_true.dat"/)
path_xa_drp=EXPT_DIR+"xas_drp4dvar.dat"
path_xa_tr4dvar=EXPT_DIR+"xas_tr4dvar.dat"
path_xa_idrp=EXPT_DIR+"xas_drp4dvnc.dat"
path_xa_idrp_regenPy=EXPT_DIR+"xas_drp4dvnc_RI.dat"
path_xb=EXPT_DIR+"xs_bckg.dat"
path_obs=new(nt,string)

wks=gsn_open_wks("eps",title_wks)
gsn_define_colormap(wks,"default")
coord=fspan(0,nt/4,nt)
res=True
;res@wkColorModel="cmyk"
res@xyLineColors        = (/1,4,2,7,3/)  
; tr4dvar rmse: black,1 ; 
; drp rmse: blue, 4
; idrp rmse, red, 2; 
; idrp_regenPy (update ensemble Py, not update Px), pink, 7;
; xb rmse, green ,3;


res@xyLineThicknessF = 4.0
res@trYMinF= -1
res@trYMaxF= 7
res@tiXAxisFont=2
res@tiYAxisFont=2
res@tiXAxisFontThicknessF=3
res@tiYAxisFontThicknessF=3
res@tiXAxisString="Time(days)"
res@tiYAxisString="RMSE"
res@gsnDraw=False
res@gsnFrame=False
res@vpHeightF=0.4
res@vpWidthF=0.8
res@xyMonoDashPattern=True
;res@trYMaxF= 12
res@tmYLLabelFontHeightF=0.02
res@tiXAxisFontHeightF=0.025
res@tiYAxisFontHeightF=0.025
res2=True
res2@xyMarkLineModes = "Markers"
res2@xyMarkers = 3
res2@xyMarkerColor = 19
res2@xyMarkerThicknessF = 2
res2@gsnDraw=False
res2@gsnFrame=False
res3=True
res3@xyMarkLineModes = "Markers"
res3@xyMarkers = 3
res3@xyMarkerColor = 7
res3@gsnDraw=False
res3@gsnFrame=False
normdiff=new(nt,float)
;i=1
rmse_drp=new(nt,float)
rmse_tr4dvar=new(nt,float)
rmse_idrp=new(nt,float)
rmse_idrp_regenPy=new(nt,float)
;rmse_obs=new(nt,float)
rmse_xb=new(nt,float)
plot=new(2,graphic)
do iexpt=0,1
  do i=1,nt
    xt=fbindirread(path_xt(iexpt),i-1,(/mx/),"float")
    xa_drp=fbindirread(path_xa_drp(iexpt),i-1,(/mx/),"float")
    xa_tr4var=fbindirread(path_xa_tr4dvar(iexpt),i-1,(/mx/),"float")
    xa_idrp=fbindirread(path_xa_idrp(iexpt),i-1,(/mx/),"float")
    xa_idrp_regenPy=fbindirread(path_xa_idrp_regenPy(iexpt),i-1,(/mx/),"float")
    rmse_drp(i-1)=sqrt(sum((xa_drp-xt)^2)/mx)
    rmse_tr4dvar(i-1)=sqrt(sum((xa_tr4var-xt)^2)/mx)
    rmse_idrp(i-1)=sqrt(sum((xa_idrp-xt)^2)/mx)
    rmse_idrp_regenPy(i-1)=sqrt(sum((xa_idrp_regenPy-xt)^2)/mx)


    xb=fbindirread(path_xb(iexpt),i-1,(/mx/),"float")
    rmse_xb(i-1)=sqrt(sum((xb-xt)^2)/mx)
  end do
  plot(iexpt)=gsn_xy(wks,coord,(/rmse_tr4dvar,rmse_drp,rmse_idrp,rmse_idrp_regenPy,rmse_xb/),res)
end do
res_panel=True
res_panel@gsnPanelFigureStrings = (/"A","B"/)
res_panel@amJust   = "TopLeft"
res_panel@gsnPanelFigureStringsPerimOn=True
res_panel@gsnPanelFigureStringsFontHeightF=0.03
gsn_panel(wks, plot, (/2,1/), res_panel)
;;print("print RMSEs------")
;;print("4dvar "+avg(rmse_tr4dvar))
;;print("DRP "+avg(rmse_drp))
;;print("IDRP_NoIntegrate "+avg(rmse_idrp))
;;print("IDRP_ReIntegrate "+avg(rmse_idrp_regenPy))
;;print("print cor.coef. vs XB")
;;print("4dvar "+tcorcoef(rmse_tr4dvar, rmse_xb))
;;print("DRP "+tcorcoef(rmse_drp, rmse_xb))
;;print("IDRP_NoIntegrate "+tcorcoef(rmse_idrp, rmse_xb))
;;print("IDRP_ReIntegrate "+tcorcoef(rmse_idrp_regenPy, rmse_xb))
;;print("IDRP_NI-IDRP_RI "+tcorcoef(rmse_idrp - rmse_idrp_regenPy, rmse_xb)) 
;;
;;to_plot_scatter_vsxbrmse=True
;;if(to_plot_scatter_vsxbrmse) then
;;    res_scat=True
;;    res_scat@xyMarkLineMode    = "Markers"
;;    res_scat@xyMarkers=(/3,4,7/)
;;    res_scat@xyMarkerColors = (/4,2,7/)
;;    res_scat@tiMainString="Scatter of (RMSE_xb, RMSE_schemes)"
;;    res_scat@tiXAxisString="RMSE_xb"
;;    res_scat@tiYAxisString="RMSE_schemes"
;;    plot  = gsn_csm_xy (wks, rmse_xb, (/rmse_drp, rmse_idrp, rmse_idrp_regenPy/), res_scat)
;;end if
;;;;average of RMSE when RMSE_xb <= xb_critical
;;xb_critical=4
;;ind_satisfy=ind(rmse_xb .le. xb_critical)
;;print("print RMSEs------")
;;print("DRP "+avg(rmse_drp(ind_satisfy)))
;;print("IDRP_NoIntegrate "+avg(rmse_idrp(ind_satisfy)))
;;print("IDRP_ReIntegrate "+avg(rmse_idrp_regenPy(ind_satisfy)))




;plot=gsn_xy(wks,coord,(/rmse_tr4dvar,rmse_drp,rmse_idrp/),res)



;do i=1,nt
;    path_obs(i-1)="Obs/fullobs"+sprinti("%0.3i",i-1)
;;    path_obs_ana(i-1)="Obs_ana/ya_l_"+sprinti("%0.1i",i)+".dat"
;    xt=fbindirread(path_xt,i-1,(/mx/),"float")
;    xb=fbindirread(path_xb,i-1,(/mx/),"float")
;    xa=fbindirread(path_xa,i-1,(/mx/),"float")
;    yo_pos=fbindirread(path_obs(i-1),0,(/my/),"integer")
;    yo_val=fbindirread(path_obs(i-1),1,(/my/),"float")
;;    ya_pos=fbindirread(path_obs_ana(i-1),0,(/my/),"integer")
;;    ya_val=fbindirread(path_obs_ana(i-1),1,(/my/),"float")
;    yo_pos@_FillValue=-1
;    yo_val@_FillValue=-9999.9
;;    ya_pos@_FillValue=-1
;;    ya_val@_FillValue=-9999.9
;    normdiff(i-1)=sqrt(sum((xa-xt)^2)/mx)
;    plot1 = gsn_xy(wks,coord,(/xt,xb,xa/),res)
;    plot2 = gsn_xy(wks,yo_pos,yo_val,res2)
; ;   plot3 = gsn_xy(wks,ya_pos,ya_val,res3)
;    overlay(plot1,plot2)
;   ; overlay(plot1,plot3)
;    draw(plot1)
;    frame(wks)
;end do
;
;print(normdiff)

end
