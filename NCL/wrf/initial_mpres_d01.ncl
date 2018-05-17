undef("initial_mpres")
function initial_mpres()
local mpres
begin
    mpres=True
    mpres@mpPerimOn = True
    mpres@mpDataBaseVersion = "MediumRes"
    mpres@mpOutlineOn           = True         ; Turn on map outlines
    mpres@mpOutlineSpecifiers = (/"China:states"/)
    mpres@mpDataSetName = "Earth..4"
    mpres@mpGeophysicalLineColor = "black"     ; color of continental outlines
    mpres@mpFillOn = False
    mpres@mpAreaMaskingOn            = True
    mpres@mpMaskAreaSpecifiers       = (/"China"/)
    mpres@mpNationalLineColor     = "black"
    mpres@mpUSStateLineColor     = "black"
    mpres@mpGeophysicalLineColor      = "Black"
    mpres@mpNationalLineColor         = "Black"
    mpres@mpUSStateLineColor          = "Black"
    mpres@mpGridLineColor             = "Black"
    mpres@mpLimbLineColor             = "Black"
    mpres@mpPerimLineColor            = "Black"
    mpres@mpGeophysicalLineThicknessF = 1.5
    mpres@mpNationalLineThicknessF    = 1.5
    mpres@mpUSStateLineThicknessF     = 1.5
    mpres@mpGridAndLimbOn = True
    mpres@mpGridLineThicknessF        = 1.5
    mpres@mpLimbLineThicknessF        = 1.5
    return(mpres)
end
