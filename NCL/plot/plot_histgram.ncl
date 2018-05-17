load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin
   
   filename="histgram_value.txt"
   nbar=50

   wks=gsn_open_wks("pdf","histgram")

   data_1d=asciiread(filename, -1, "float")
   
   res=True
   res@gsnHistogramSelectNiceIntervals = True
   res@gsnHistogramComputePercentages  = True
   res@tmXBLabelStride  = 2
   res@tmXBLabelAngleF                 = 315.
   res@gsnHistogramNumberOfBins        = nbar
   plot=gsn_histogram(wks,data_1d,res)

end
