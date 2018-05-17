#ifndef INC_COMMON
#define INC_COMMON
#ifdef DEBUG
#define RP_ERROR(m) call print_messagex("error",trim(m),__FILE__,__LINE__,trim(PROCEDURE_NAME))
#define RP_PAUSE(m) call print_messagex("pause",trim(m),__FILE__,__LINE__,trim(PROCEDURE_NAME))
#define RP_WARNING(m) call print_messagex("warning",trim(m),__FILE__,__LINE__,trim(PROCEDURE_NAME))
#define RP_LOG(m) call print_messagex("log",trim(m),__FILE__,__LINE__,trim(PROCEDURE_NAME))
#define NC_CHECK(m) if(m/=NF90_NOERR) then; call print_messagex("error", trim(NF90_STRERROR(m)), __FILE__,__LINE__,trim(PROCEDURE_NAME)); end if
#define VSL_CHECK(m) if( .not. any(m == (/VSL_ERROR_OK, VSL_STATUS_OK/))) then; call print_message("error", trim(vsl_strerror(m)), __FILE__, __LINE__, trim(PROCEDURE_NAME)); end if
#define LPK_CHECK(m) if(m/=0) then; call print_messagex("error","LAPACK95 meets error, output info="//trim(adjustl(tostring(ival=m))), __FILE__,__LINE__, trim(PROCEDURE_NAME)); endif

#define TRACE1(m1) print*,m1
#define TRACE2(m1,m2) print*,m1,m2
#define TRACE3(m1,m2,m3) print*,m1,m2,m3
#define TRACE4(m1,m2,m3,m4) print*,m1,m2,m3,m4
#define TRACE5(m1,m2,m3,m4,m5) print*,m1,m2,m3,m4,m5
#define TRACE6(m1,m2,m3,m4,m5,m6) print*,m1,m2,m3,m4,m5,m6

#define DEALLOC(x) if(allocated(x)) then; deallocate(x) ; end if

#else
#define RP_ERROR(m) call print_message("error",trim(m),__FILE__,__LINE__,trim(PROCEDURE_NAME))
#define RP_PAUSE(m) call print_message("pause",trim(m),__FILE__,__LINE__,trim(PROCEDURE_NAME))
#define RP_WARNING(m) call print_message("warning",trim(m),__FILE__,__LINE__,trim(PROCEDURE_NAME))
#define RP_LOG(m) call print_message("log",trim(m),__FILE__,__LINE__,trim(PROCEDURE_NAME))
#define NC_CHECK(m) if( m/= NF90_NOERR) then; call print_message("error", trim(NF90_STRERROR(m)), __FILE__,__LINE__,trim(PROCEDURE_NAME)); end if
#define VSL_CHECK(m) if( .not. any(m == (/VSL_ERROR_OK, VSL_STATUS_OK/))) then; call print_message("error", trim(vsl_strerror(m)), __FILE__, __LINE__, trim(PROCEDURE_NAME)); end if
#define LPK_CHECK(m) if(m/=0) then; call print_message("error","LAPACK95 meets error, output info="//trim(adjustl(tostring(ival=m))), __FILE__,__LINE__, trim(PROCEDURE_NAME)); endif

#define TRACE1(m1) print*,m1
#define TRACE2(m1,m2) print*,m1,m2
#define TRACE3(m1,m2,m3) print*,m1,m2,m3
#define TRACE4(m1,m2,m3,m4) print*,m1,m2,m3,m4
#define TRACE5(m1,m2,m3,m4,m5) print*,m1,m2,m3,m4,m5
#define TRACE6(m1,m2,m3,m4,m5,m6) print*,m1,m2,m3,m4,m5,m6

#define TRACE(m) print*, #m, " ", m
#define TRACEX(m) print*, trim(PROCEDURE_NAME)," ", #m, " ",m
#define ASSUREX(m) if(.not.(m)) then; call print_message("error",#m//" NOT satisfy",__FILE__,__LINE__,trim(PROCEDURE_NAME)); end if

#define DEALLOC(x) if(allocated(x)) then; deallocate(x) ; end if

#endif //DEBUG


!!tbgn, tlast should are be common-variables (wherever TIME_* is involked, tbgn/tlast must be accessible)
#ifdef TIME
#define TIME_START(tbgn) call cpu_time(tbgn)
#define TIME_CLICK(t, tbgn, tlast) if(tlast<0) tlast=tbgn; call cpu_time(t); write(*,'("----Elapsed-time: total=",f10.4,"sec; event=",f10.4,"sec")')  t-tbgn, t-tlast; tlast=t
#else
#define TIME_START(tbgn) continue
#define TIME_CLICK(t, tbgn, tlast) continue
#endif  //TIME
 

#endif
