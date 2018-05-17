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

nx=40  ;;
nline=2  ;;
title_wks="LGE_SPREAD"  ;;
xcoord=fspan(0, 9.75, 40) ;;

filename="LGE_spread.csv" ;;
data=asciiread(filename, (/nx, nline/), "float") 

wks=gsn_open_wks("eps",title_wks)
gsn_define_colormap(wks,"default")
res=True
res@xyLineColors        = (/4,2/)
;; 2: read
;; 3: green
;; 4: blue
res@xyLineThicknessF = 4.0
;res@trYMinF= -1  
;res@trYMaxF= 7 
;res@tiXAxisFont=2
;res@tiYAxisFont=2
;res@tiXAxisFontThicknessF=3
;res@tiYAxisFontThicknessF=3
res@tiXAxisString="Time(days)" ;; 
res@tiYAxisString="Spread (10^4 m2/s)" ;;
res@gsnDraw=False
res@gsnFrame=False
;res@vpHeightF=0.4
;res@vpWidthF=0.8
res@xyMonoDashPattern=True
res@xyYStyle="Log"
res@tmYLMode="Explicit"
res@tmYLValues=(/1, 2, 5, 10, 20, 50, 100, 200/)
res@tmYLLabels=(/1, 2, 5, 10, 20, 50, 100, 200/)
res@trYMinF= 5
;res@tmYLLabelFontHeightF=0.02
;res@tiXAxisFontHeightF=0.025
;res@tiYAxisFontHeightF=0.025

plot=gsn_xy(wks, xcoord, transpose(data), res)
draw(plot)
frame(wks)

end
