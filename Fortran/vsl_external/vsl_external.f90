! --------------------------------------------------------------------------
! Description:
!
!   an external module for mkl.vsl
!
! History:
!
!   2014-06-28:
!
!       [shensi]: First creation.
! --------------------------------------------------------------------------
include "mkl_vsl.f90"
module vsl_external
use mkl_vsl_type
use mkl_vsl
implicit none


    ! variables

contains

    ! methods
    function vsl_strerror(errorcode)  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="vsl_strerror"
        integer :: errorcode
        character(len=70) :: vsl_strerror
        select case (errorcode)
        case ( VSL_ERROR_CPU_NOT_SUPPORTED )
            vsl_strerror = "mkl_vsl error: VSL_ERROR_CPU_NOT_SUPPORTED" 
        case ( VSL_ERROR_FEATURE_NOT_IMPLEMENTED )
            vsl_strerror = "mkl_vsl error: VSL_ERROR_FEATURE_NOT_IMPLEMENTED" 
        case ( VSL_ERROR_UNKNOWN )
            vsl_strerror = "mkl_vsl error: VSL_ERROR_UNKNOWN" 
        case ( VSL_ERROR_BADARGS )
            vsl_strerror = "mkl_vsl error: VSL_ERROR_BADARGS" 
        case ( VSL_ERROR_MEM_FAILURE )
            vsl_strerror = "mkl_vsl error: VSL_ERROR_MEM_FAILURE" 
        case ( VSL_ERROR_NULL_PTR )
            vsl_strerror = "mkl_vsl error: VSL_ERROR_NULL_PTR" 
        case ( VSL_RNG_ERROR_INVALID_BRNG_INDEX )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_INVALID_BRNG_INDEX" 
        case ( VSL_RNG_ERROR_LEAPFROG_UNSUPPORTED )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_LEAPFROG_UNSUPPORTED" 
        case ( VSL_RNG_ERROR_SKIPAHEAD_UNSUPPORTED )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_SKIPAHEAD_UNSUPPORTED" 
        case ( VSL_RNG_ERROR_BRNGS_INCOMPATIBLE )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_BRNGS_INCOMPATIBLE" 
        case ( VSL_RNG_ERROR_BAD_STREAM )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_BAD_STREAM" 
        case ( VSL_RNG_ERROR_BRNG_TABLE_FULL )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_BRNG_TABLE_FULL" 
        case ( VSL_RNG_ERROR_BAD_STREAM_STATE_SIZE )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_BAD_STREAM_STATE_SIZE" 
        case ( VSL_RNG_ERROR_BAD_WORD_SIZE )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_BAD_WORD_SIZE" 
        case ( VSL_RNG_ERROR_BAD_NSEEDS )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_BAD_NSEEDS" 
        case ( VSL_RNG_ERROR_BAD_NBITS )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_BAD_NBITS" 
        case ( VSL_RNG_ERROR_BAD_UPDATE )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_BAD_UPDATE" 
        case ( VSL_RNG_ERROR_NO_NUMBERS )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_NO_NUMBERS" 
        case ( VSL_RNG_ERROR_INVALID_ABSTRACT_STREAM )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_INVALID_ABSTRACT_STREAM" 
        case ( VSL_RNG_ERROR_NONDETERM_NOT_SUPPORTED )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_NONDETERM_NOT_SUPPORTED" 
        case ( VSL_RNG_ERROR_NONDETERM_NRETRIES_EXCEEDED )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_NONDETERM_NRETRIES_EXCEEDED" 
        case ( VSL_RNG_ERROR_FILE_CLOSE )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_FILE_CLOSE" 
        case ( VSL_RNG_ERROR_FILE_OPEN )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_FILE_OPEN" 
        case ( VSL_RNG_ERROR_FILE_WRITE )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_FILE_WRITE" 
        case ( VSL_RNG_ERROR_FILE_READ )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_FILE_READ" 
        case ( VSL_RNG_ERROR_BAD_FILE_FORMAT )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_BAD_FILE_FORMAT" 
        case ( VSL_RNG_ERROR_UNSUPPORTED_FILE_VER )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_UNSUPPORTED_FILE_VER" 
        case ( VSL_RNG_ERROR_BAD_MEM_FORMAT )
            vsl_strerror = "mkl_vsl error: VSL_RNG_ERROR_BAD_MEM_FORMAT" 
        case ( VSL_SS_ERROR_ALLOCATION_FAILURE )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_ALLOCATION_FAILURE" 
        case ( VSL_SS_ERROR_BAD_DIMEN )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_DIMEN" 
        case ( VSL_SS_ERROR_BAD_OBSERV_N )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_OBSERV_N" 
        case ( VSL_SS_ERROR_STORAGE_NOT_SUPPORTED )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_STORAGE_NOT_SUPPORTED" 
        case ( VSL_SS_ERROR_BAD_INDC_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_INDC_ADDR" 
        case ( VSL_SS_ERROR_BAD_WEIGHTS )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_WEIGHTS" 
        case ( VSL_SS_ERROR_BAD_MEAN_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MEAN_ADDR" 
        case ( VSL_SS_ERROR_BAD_2R_MOM_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_2R_MOM_ADDR" 
        case ( VSL_SS_ERROR_BAD_3R_MOM_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_3R_MOM_ADDR" 
        case ( VSL_SS_ERROR_BAD_4R_MOM_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_4R_MOM_ADDR" 
        case ( VSL_SS_ERROR_BAD_2C_MOM_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_2C_MOM_ADDR" 
        case ( VSL_SS_ERROR_BAD_3C_MOM_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_3C_MOM_ADDR" 
        case ( VSL_SS_ERROR_BAD_4C_MOM_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_4C_MOM_ADDR" 
        case ( VSL_SS_ERROR_BAD_KURTOSIS_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_KURTOSIS_ADDR" 
        case ( VSL_SS_ERROR_BAD_SKEWNESS_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_SKEWNESS_ADDR" 
        case ( VSL_SS_ERROR_BAD_MIN_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MIN_ADDR" 
        case ( VSL_SS_ERROR_BAD_MAX_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MAX_ADDR" 
        case ( VSL_SS_ERROR_BAD_VARIATION_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_VARIATION_ADDR" 
        case ( VSL_SS_ERROR_BAD_COV_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_COV_ADDR" 
        case ( VSL_SS_ERROR_BAD_COR_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_COR_ADDR" 
        case ( VSL_SS_ERROR_BAD_QUANT_ORDER_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_QUANT_ORDER_ADDR" 
        case ( VSL_SS_ERROR_BAD_QUANT_ORDER )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_QUANT_ORDER" 
        case ( VSL_SS_ERROR_BAD_QUANT_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_QUANT_ADDR" 
        case ( VSL_SS_ERROR_BAD_ORDER_STATS_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_ORDER_STATS_ADDR" 
        case ( VSL_SS_ERROR_MOMORDER_NOT_SUPPORTED )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_MOMORDER_NOT_SUPPORTED" 
        case ( VSL_SS_NOT_FULL_RANK_MATRIX )
            vsl_strerror = "mkl_vsl error: VSL_SS_NOT_FULL_RANK_MATRIX" 
        case ( VSL_SS_ERROR_ALL_OBSERVS_OUTLIERS )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_ALL_OBSERVS_OUTLIERS" 
        case ( VSL_SS_ERROR_BAD_ROBUST_COV_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_ROBUST_COV_ADDR" 
        case ( VSL_SS_ERROR_BAD_ROBUST_MEAN_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_ROBUST_MEAN_ADDR" 
        case ( VSL_SS_ERROR_METHOD_NOT_SUPPORTED )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_METHOD_NOT_SUPPORTED" 
        case ( VSL_SS_ERROR_NULL_TASK_DESCRIPTOR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_NULL_TASK_DESCRIPTOR" 
        case ( VSL_SS_ERROR_BAD_OBSERV_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_OBSERV_ADDR" 
        case ( VSL_SS_ERROR_SINGULAR_COV )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_SINGULAR_COV" 
        case ( VSL_SS_ERROR_BAD_POOLED_COV_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_POOLED_COV_ADDR" 
        case ( VSL_SS_ERROR_BAD_POOLED_MEAN_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_POOLED_MEAN_ADDR" 
        case ( VSL_SS_ERROR_BAD_GROUP_COV_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_GROUP_COV_ADDR" 
        case ( VSL_SS_ERROR_BAD_GROUP_MEAN_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_GROUP_MEAN_ADDR" 
        case ( VSL_SS_ERROR_BAD_GROUP_INDC_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_GROUP_INDC_ADDR" 
        case ( VSL_SS_ERROR_BAD_GROUP_INDC )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_GROUP_INDC" 
        case ( VSL_SS_ERROR_BAD_OUTLIERS_PARAMS_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_OUTLIERS_PARAMS_ADDR" 
        case ( VSL_SS_ERROR_BAD_OUTLIERS_PARAMS_N_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_OUTLIERS_PARAMS_N_ADDR" 
        case ( VSL_SS_ERROR_BAD_OUTLIERS_WEIGHTS_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_OUTLIERS_WEIGHTS_ADDR" 
        case ( VSL_SS_ERROR_BAD_ROBUST_COV_PARAMS_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_ROBUST_COV_PARAMS_ADDR" 
        case ( VSL_SS_ERROR_BAD_ROBUST_COV_PARAMS_N_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_ROBUST_COV_PARAMS_N_ADDR" 
        case ( VSL_SS_ERROR_BAD_STORAGE_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_STORAGE_ADDR" 
        case ( VSL_SS_ERROR_BAD_PARTIAL_COV_IDX_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_PARTIAL_COV_IDX_ADDR" 
        case ( VSL_SS_ERROR_BAD_PARTIAL_COV_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_PARTIAL_COV_ADDR" 
        case ( VSL_SS_ERROR_BAD_PARTIAL_COR_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_PARTIAL_COR_ADDR" 
        case ( VSL_SS_ERROR_BAD_MI_PARAMS_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_PARAMS_ADDR" 
        case ( VSL_SS_ERROR_BAD_MI_PARAMS_N_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_PARAMS_N_ADDR" 
        case ( VSL_SS_ERROR_BAD_MI_BAD_PARAMS_N )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_BAD_PARAMS_N" 
        case ( VSL_SS_ERROR_BAD_MI_PARAMS )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_PARAMS" 
        case ( VSL_SS_ERROR_BAD_MI_INIT_ESTIMATES_N_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_INIT_ESTIMATES_N_ADDR" 
        case ( VSL_SS_ERROR_BAD_MI_INIT_ESTIMATES_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_INIT_ESTIMATES_ADDR" 
        case ( VSL_SS_ERROR_BAD_MI_SIMUL_VALS_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_SIMUL_VALS_ADDR" 
        case ( VSL_SS_ERROR_BAD_MI_SIMUL_VALS_N_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_SIMUL_VALS_N_ADDR" 
        case ( VSL_SS_ERROR_BAD_MI_ESTIMATES_N_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_ESTIMATES_N_ADDR" 
        case ( VSL_SS_ERROR_BAD_MI_ESTIMATES_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_ESTIMATES_ADDR" 
        case ( VSL_SS_ERROR_BAD_MI_SIMUL_VALS_N )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_SIMUL_VALS_N" 
        case ( VSL_SS_ERROR_BAD_MI_OUTPUT_PARAMS )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_OUTPUT_PARAMS" 
        case ( VSL_SS_ERROR_BAD_MI_PRIOR_N_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_PRIOR_N_ADDR" 
        case ( VSL_SS_ERROR_BAD_MI_PRIOR_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_MI_PRIOR_ADDR" 
        case ( VSL_SS_SEMIDEFINITE_COR )
            vsl_strerror = "mkl_vsl error: VSL_SS_SEMIDEFINITE_COR" 
        case ( VSL_SS_ERROR_BAD_PARAMTR_COR_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_PARAMTR_COR_ADDR" 
        case ( VSL_SS_ERROR_BAD_COR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_COR" 
        case ( VSL_SS_ERROR_BAD_STREAM_QUANT_PARAMS_N_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_STREAM_QUANT_PARAMS_N_ADDR" 
        case ( VSL_SS_ERROR_BAD_STREAM_QUANT_PARAMS_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_STREAM_QUANT_PARAMS_ADDR" 
        case ( VSL_SS_ERROR_BAD_STREAM_QUANT_PARAMS_N )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_STREAM_QUANT_PARAMS_N" 
        case ( VSL_SS_ERROR_BAD_STREAM_QUANT_PARAMS )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_STREAM_QUANT_PARAMS" 
        case ( VSL_SS_ERROR_BAD_STREAM_QUANT_ORDER_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_STREAM_QUANT_ORDER_ADDR" 
        case ( VSL_SS_ERROR_BAD_STREAM_QUANT_ORDER )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_STREAM_QUANT_ORDER" 
        case ( VSL_SS_ERROR_BAD_STREAM_QUANT_ADDR )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_STREAM_QUANT_ADDR" 
        case ( VSL_SS_ERROR_BAD_PARTIAL_COV_IDX )
            vsl_strerror = "mkl_vsl error: VSL_SS_ERROR_BAD_PARTIAL_COV_IDX" 
        end select
    end function vsl_strerror  !!}}}

    function getseed_byclock()  !!{{{
    implicit none
        character(len=*),parameter :: PROCEDURE_NAME="getseed_byclock"
        integer :: cnt_rate, cnt_max, getseed_byclock
        call system_clock(count=getseed_byclock)
    end function getseed_byclock  !!}}}

end module vsl_external

