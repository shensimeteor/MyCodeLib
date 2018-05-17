undef("ss_readerai_2d")  ;; tidx/t_idx, begin with 0, same to the below
function ss_readerai_2d(f:file,varname:string,t_idx:integer) ;;{{{
local var,var_short
begin
    var_short=f->$varname$(t_idx,:,:)
    var=tofloat(var_short) * tofloat(var_short@scale_factor) + tofloat(var_short@add_offset)
    return(var)
end  ;;}}}

undef("ss_readerai_3d")
function ss_readerai_3d(f:file,varname:string,t_idx:integer) ;;{{{
local var,var_short
begin
    var_short=f->$varname$(t_idx,:,:,:)
    var=tofloat(var_short) * tofloat(var_short@scale_factor) + tofloat(var_short@add_offset)
    return(var)
end  ;;}}}

;;depressed, because can use the more general interface: ss_read_ctl/ss_read_grads (but this function can NOT be deleted!)
undef("ss_readgrapespost_prepare") ;;can NOT be deleted!!
function ss_readgrapespost_prepare(ctlname:string)  ;;{{{
local nvar, nlevel, levels, varname, varlevel, exe, var_begin_records, i, count
begin
    exe="$SS_NCL_LIB/resolve_gradsctl"
    nvar=systemfunc(exe+" -nvar "+ctlname)
    nlevel=systemfunc(exe+" -nlevel "+ctlname)
    levels=systemfunc(exe+" -level "+ctlname)
    varname=systemfunc(exe+" -varname "+ctlname)
    varlevel=systemfunc(exe+" -varlevel "+ctlname)
    ctlinfo=True
    ctlinfo@nvar=tointeger(nvar)
    ctlinfo@nlevel=tointeger(nlevel)
    ctlinfo@levels=tofloat(levels)
    ctlinfo@varname=varname
    ctlinfo@varlevel=tointeger(varlevel)
    var_begin_records=new((/ctlinfo@nvar/),"float")
    count=0
    do i=0,ctlinfo@nvar-1
        var_begin_records(i)=count
        count=count+ctlinfo@varlevel(i)
    end do
    ctlinfo@var_begin_records=tointeger(var_begin_records)
    ctlinfo@records_per_t=sum(ctlinfo@varlevel)
    return(ctlinfo)
end  ;;}}}
undef("ss_readgrapespost_read")
function ss_readgrapespost_read(fname:string, ctlinfo:logical, varname:string, t_idx:integer) ;;{{{
local varidx, begin_record, i, var
begin
    varidx=ind(str_squeeze(varname) .eq. str_squeeze(ctlinfo@varname))
   ; if(varname .eq. ctlinfo@varname(1)) then
  ;  if(" t" .eq. "t") then
  ;      print(12)
  ;  end if
    begin_record=ctlinfo@var_begin_records(varidx) + ctlinfo@records_per_t * t_idx
    setfileoption("bin","ReadByteOrder","BigEndian")
    if(ctlinfo@varlevel(varidx) .eq. 1) then
        var=fbinrecread(fname, begin_record, (/180,360/), "float")
    end if
    if(ctlinfo@varlevel(varidx) .gt. 1) then
        var=new((/ctlinfo@varlevel(varidx), 180, 360/), "float")
        do i=1, ctlinfo@nlevel
            var(i-1,:,:)=fbinrecread(fname, begin_record+i-1, (/180,360/), "float")
        end do
    end if
    return(var)
end  ;;}}}
undef("ss_readjra25gr_prepare")
function ss_readjra25gr_prepare(ctlname:string) ;;{{{    
begin
    ctlinfo=ss_readgrapespost_prepare(ctlname)
    return(ctlinfo)
end  ;;}}}
undef("ss_readjra25gr_read")
function ss_readjra25gr_read(fname:string, ctlinfo:logical, varname:string, t_idx:integer) ;;{{{    
local varidx, begin_record, i, var
begin
    varidx=ind(str_squeeze(varname) .eq. str_squeeze(ctlinfo@varname))
    begin_record=ctlinfo@var_begin_records(varidx) + ctlinfo@records_per_t * t_idx
    setfileoption("bin","ReadByteOrder","BigEndian")
    if(ctlinfo@varlevel(varidx) .eq. 1) then
        var=fbinrecread(fname, begin_record, (/145,288/), "float")
    end if
    if(ctlinfo@varlevel(varidx) .gt. 1) then
        var=new((/ctlinfo@varlevel(varidx), 145, 288/), "float")
        do i=1, ctlinfo@varlevel(varidx)
            var(i-1,:,:)=fbindirread(fname, begin_record+i-1, (/145,288/), "float")
        end do
    end if
    return(var)
end  ;;}}}
;;depressed over

undef("ss_read_ctl") 
;; if write directly (rec=), is_seq=False; if sequantially, is_seq=True: both unit is a 2D xy slice
function ss_read_ctl(ctlname:string, nx: integer, ny: integer, use_big_endian:logical, is_seq: logical) ;;{{{
begin
    ctlinfo=ss_readgrapespost_prepare(ctlname)
    ctlinfo@nx=nx
    ctlinfo@ny=ny
    ctlinfo@use_big_endian=use_big_endian
    ctlinfo@is_seq=is_seq
    return(ctlinfo)
end ;;}}}

undef("ss_read_grads")
function ss_read_grads(fname:string, ctlinfo:logical, varname:string, t_idx:integer) ;;{{{
local varidx, begin_record, i, var
begin
    varidx=ind(str_squeeze(varname) .eq. str_squeeze(ctlinfo@varname))
    begin_record=ctlinfo@var_begin_records(varidx) + ctlinfo@records_per_t * t_idx
    if(ctlinfo@use_big_endian) then
        setfileoption("bin","ReadByteOrder","BigEndian")
    else
        setfileoption("bin","ReadByteOrder","LittleEndian")
    end if
    if(ctlinfo@varlevel(varidx) .eq. 1) then
        if(ctlinfo@is_seq) then
            var=fbinrecread(fname, begin_record, (/ctlinfo@ny, ctlinfo@nx/), "float")
        else
            var=fbindirread(fname, begin_record, (/ctlinfo@ny, ctlinfo@nx/), "float")
        end if
    end if
    if(ctlinfo@varlevel(varidx) .gt. 1) then
        var=new((/ctlinfo@varlevel(varidx), ctlinfo@ny, ctlinfo@nx/), "float")
        do i=1, ctlinfo@varlevel(varidx)
            if(ctlinfo@is_seq) then
                var(i-1,:,:)=fbinrecread(fname, begin_record+i-1, (/ctlinfo@ny, ctlinfo@nx/), "float")
            else
                var(i-1,:,:)=fbindirread(fname, begin_record+i-1, (/ctlinfo@ny, ctlinfo@nx/), "float")
            end if
        end do
    end if
    return(var)
end ;;}}}


undef("ss_copy_VarMeta") 
;; copy var_from's att/dim&coord/coord's att to var_to. Dims of var_to can be a subset of these of var_from
procedure ss_copy_VarMeta(var_from, var_to) ;;{{{
local atts, i, dims, dimlen, dims_to, idx
begin
    ;;att
    atts=getvaratts(var_from)
    if( .not. ismissing(atts) ) then
        do i=0, dimsizes(atts)-1
            var_to@$atts(i)$=var_from@$atts(i)$
        end do
    end if
    ;;dim & dim's att
    dims=getvardims(var_from)
    dimlen=new((/dimsizes(dims)/),"integer")
    do i=0, dimsizes(dims)-1
        dimlen(i)=dimsizes(var_from&$dims(i)$)
    end do
    dims_to=dimsizes(var_to)
    do i=0, dimsizes(dims_to)-1
       idx=ind(dims_to(i) .eq. dimlen)
       if( ismissing(idx) ) then
           print((/"ERROR: var_to has no such dim that corresponding with any dims of var_from"/))
           exit
       end if
       if( dimsizes(idx) .gt. 1) then
           print((/"ERROR: var_to has more than 1 dims that has the corresponding size with var_from"/))
           exit
       end if
       var_to!i=dims(idx)
       var_to&$dims(idx)$=var_from&$dims(idx)$
    end do
end  ;;}}}

;; (res, data, n, interval): set Atts for res to define the cnLevels explicitly that satisfies symmetric around 0
undef("setatts_colorbar_explicit") ;; zero-centered {{{
procedure setatts_colorbar_explicit(res: logical, data: numeric, n: integer, interval: numeric)
begin
   if(all(ismissing(data)))
       return
   end if
   cmin=min(data)
   cmax=max(data)
   absmax=max((/cmax, abs(cmin)/))
   if(interval .lt. 0) then
       temp=absmax/interval
       uplim=ceil(temp)*interval
   else
       uplim=absmax
   end if
   print((/cmin,cmax,absmax,uplim/))
   res@cnLevelSelectionMode="ExplicitLevels"
   res@cnLevels=fspan(-uplim, uplim, n)
end  ;;}}}




;; if not end with .nc, then add .nc
undef("nc_addpostfix")
function nc_addpostfix(filename: string)
local afilename, len, postfix, newfilename
begin

    afilename=stringtochar(filename)
    len=strlen(filename)
    postfix=chartostring(afilename(len-2:len))
    if(postfix .eq. ".nc")
        newfilename=filename
    else
        newfilename=filename+".nc"
    end if
    return(newfilename)
end

;; read csvfile, return col_out (matrix, col_ids starts with 0): float, skip nline_head
undef("read_csvfile")
function read_csvfile(filename: string, col_ids: integer, nline_head: integer)
local ncol, nrow, nrow_out, col_out, i, out
begin
    ncol=dimsizes(col_ids)
    nrow=tointeger(systemfunc("cat "+filename+" | wc -l "))
    nrow_out=nrow-nline_head
    col_out=new((/nrow_out, ncol/),"float")
    do i=0,ncol-1
        out=systemfunc("cat "+filename + " | cut -d , -f "+(col_ids(i)+1))
        col_out(:,i)=tofloat(out)
    end do
    return(col_out)
end    

;; get lower_limit under some unit
undef("lower_limit")
function lower_limit(value, unit)
local lowerlim, x
begin
    x=floor(value/unit)
    lowerlim=unit*x
    return(lowerlim)
end

undef("upper_limit")
function upper_limit(value, unit)
local upperlim, x
begin
    x=ceil(value/unit)
    upperlim=unit*x
    return(upperlim)
end

undef("basename")
function basename(fileordir)
local base
begin
    base=systemfunc("basename "+fileordir)
    return(base)
end
