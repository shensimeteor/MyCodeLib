	  '  j   k820309              13.0        T                                                                                                           
       utility.F90 UTILITY                                                                                                    6$        @                                                          #LOWERCASE%CHAR    #LOWERCASE%ICHAR    #LOWERCASE%LEN    #STRING    H r      5 O p                                                                    CHAR                                                 ICHAR                                                 LEN            @                                                   1 $        @                                                          #BIGGERCASE%CHAR    #BIGGERCASE%ICHAR 	   #BIGGERCASE%LEN 
   #STRING    H r 
     5 O p                                                                    CHAR                                            	     ICHAR                                            
     LEN            @                                                   1 $         @                                                          #TOSTRING%ADJUSTL    #TOSTRING%TRIM    #TOSTRING%PRESENT    #TOSTRING%COUNT    #IVAL    #FVAL    #SVAL    #DVAL    #FMAT                                                            ADJUSTL                                                 TRIM                                                 PRESENT                                                 COUNT            @                                                      @                                    	                  @                                   	                  @                                   
                  @                                                   1 #         @                                                     #PRINT_MESSAGE%ADJUSTL    #PRINT_MESSAGE%TRIM    #PRINT_MESSAGE%PRESENT    #MSGTYPE    #MSGSTRING    #MSGFILE    #MSGLINE    #MSGPROC                                                    ADJUSTL                                                 TRIM                                                 PRESENT            @                                                   1            @                                                   1            @                                                   1            @                                                     @                                                   1 #         @                                                      #STR_SPLIT%SCAN     #STR_SPLIT%ALLOCATED !   #STR_SPLIT%LEN_TRIM "   #STRING #   #CH $   #NSUB %   #SUBS &                                                    SCAN                                            !     ALLOCATED                                            "     LEN_TRIM            @                              #                     1            @                              $                                      D                                 %            ,        D @                              &                                   &                                           1 #         @                                   '                   #ALLOCREAD_LINE%ALLOCATED (   #ALLOCREAD_LINE%TRIM )   #FILENAME *   #NLINE +   #LINES ,                                              (     ALLOCATED                                            )     TRIM            @                              *                     1           D                                 +            ,        D @                              ,                                   &                                           1 #         @                                   -                   #WRITE_FLOAT_MATRIX2%SIZE .   #WRITE_FLOAT_MATRIX2%TRIM /   #WRITE_FLOAT_MATRIX2%PRESENT 0   #A 1   #FUNIT 2   #FNAME 3   #ELEMENT_FMAT 4                                              .     SIZE                                            /     TRIM                                            0     PRESENT           @                               1                   	               &                   &                                                      @                               2                       @                              3                     1            @                              4                     1 %         @                                5                           #BEGIN_SEARCH_UNIT 6                                              6            #         @                                   7                   #WRITE_INTEGER_MATRIX2%SIZE 8   #WRITE_INTEGER_MATRIX2%TRIM 9   #WRITE_INTEGER_MATRIX2%PRESENT :   #A ;   #FUNIT <   #FNAME =   #ELEMENT_FMAT >                                              8     SIZE                                            9     TRIM                                            :     PRESENT           
 @                               ;                                 &                   &                                                      @                               <                       @                              =                     1            @                              >                     1 $         @                                ?     ,                    #KICKOUT_FILEPATH%INDEX @   #KICKOUT_FILEPATH%LEN_TRIM A   #FILEPATH B                                                      @     INDEX                                            A     LEN_TRIM            @                              B                     1 $         @                                C     ,                    #REPLACE_FILESUFFIX%INDEX D   #REPLACE_FILESUFFIX%TRIM E   #FILENAME F   #NEW G                                                      D     INDEX                                            E     TRIM            @                              F                     1            @                              G                     1 %         @                                 H                    	       #LON1 I   #LON2 J                                              I     	                                                  J     	       %         @                                 K                   	       #GET_AVERAGE%SUM L   #GET_AVERAGE%SIZE M   #X N                                              L     SUM                                            M     SIZE           @                               N                   	               &                                           %         @                                 O                   	       #GET_COVA_ANOMALY%SUM P   #GET_COVA_ANOMALY%SIZE Q   #GET_COVA_ANOMALY%PRESENT R   #X1 S   #X2 T   #OP_ISBIAS U                                              P     SUM                                            Q     SIZE                                            R     PRESENT           @                               S                   	               &                                                     @                               T                   	               &                                                      @                               U            %         @                                 V                   	       #GET_COVA_ORIGIN%SUM W   #GET_COVA_ORIGIN%SIZE X   #GET_COVA_ORIGIN%PRESENT Y   #X1 Z   #X2 [   #OP_ISBIAS \                                              W     SUM                                            X     SIZE                                            Y     PRESENT           @                               Z                   	 	              &                                                     @                               [                   	 
              &                                                      @                               \            %         @                                 ]                   	       #GET_CORR%SQRT ^   #GET_CORR%SUM _   #GET_CORR%SIZE `   #X1 a   #X2 b                                              ^     SQRT                                            _     SUM                                            `     SIZE           @                               a                   	               &                                                     @                               b                   	               &                                           #         @                                   c                    #START d   #FINISH e   #NPTS f   #ARRAY g                                              d     	                                                  e     	                                                  f                     D                                 g                    	     p          5 � p        r f       5 � p        r f                     $         @                                h     
                      #T_PRE i                                                                             i     	          �         fn#fn $   �   q       UTILITY_UNIT_SCREEN    -  �       LOWERCASE    �  =      LOWERCASE%CHAR     6  >      LOWERCASE%ICHAR    t  <      LOWERCASE%LEN !   �  L   a   LOWERCASE%STRING    �  �       BIGGERCASE     �  =      BIGGERCASE%CHAR !     >      BIGGERCASE%ICHAR    F  <      BIGGERCASE%LEN "   �  L   a   BIGGERCASE%STRING    �  �       TOSTRING !   �  @      TOSTRING%ADJUSTL    �  =      TOSTRING%TRIM !   (  @      TOSTRING%PRESENT    h  >      TOSTRING%COUNT    �  @   a   TOSTRING%IVAL    �  @   a   TOSTRING%FVAL    &  @   a   TOSTRING%SVAL    f  @   a   TOSTRING%DVAL    �  L   a   TOSTRING%FMAT    �  �       PRINT_MESSAGE &   �  @      PRINT_MESSAGE%ADJUSTL #   	  =      PRINT_MESSAGE%TRIM &   H	  @      PRINT_MESSAGE%PRESENT &   �	  L   a   PRINT_MESSAGE%MSGTYPE (   �	  L   a   PRINT_MESSAGE%MSGSTRING &    
  L   a   PRINT_MESSAGE%MSGFILE &   l
  @   a   PRINT_MESSAGE%MSGLINE &   �
  L   a   PRINT_MESSAGE%MSGPROC    �
  �       STR_SPLIT    �  =      STR_SPLIT%SCAN $   �  B      STR_SPLIT%ALLOCATED #   ,  A      STR_SPLIT%LEN_TRIM !   m  L   a   STR_SPLIT%STRING    �  P   a   STR_SPLIT%CH    	  @   a   STR_SPLIT%NSUB    I  �   a   STR_SPLIT%SUBS    �  �       ALLOCREAD_LINE )   |  B      ALLOCREAD_LINE%ALLOCATED $   �  =      ALLOCREAD_LINE%TRIM (   �  L   a   ALLOCREAD_LINE%FILENAME %   G  @   a   ALLOCREAD_LINE%NLINE %   �  �   a   ALLOCREAD_LINE%LINES $     �       WRITE_FLOAT_MATRIX2 )   �  =      WRITE_FLOAT_MATRIX2%SIZE )   (  =      WRITE_FLOAT_MATRIX2%TRIM ,   e  @      WRITE_FLOAT_MATRIX2%PRESENT &   �  �   a   WRITE_FLOAT_MATRIX2%A *   I  @   a   WRITE_FLOAT_MATRIX2%FUNIT *   �  L   a   WRITE_FLOAT_MATRIX2%FNAME 1   �  L   a   WRITE_FLOAT_MATRIX2%ELEMENT_FMAT    !  g       GET_FUNIT ,   �  @   a   GET_FUNIT%BEGIN_SEARCH_UNIT &   �  �       WRITE_INTEGER_MATRIX2 +   �  =      WRITE_INTEGER_MATRIX2%SIZE +   �  =      WRITE_INTEGER_MATRIX2%TRIM .     @      WRITE_INTEGER_MATRIX2%PRESENT (   \  �   a   WRITE_INTEGER_MATRIX2%A ,      @   a   WRITE_INTEGER_MATRIX2%FUNIT ,   @  L   a   WRITE_INTEGER_MATRIX2%FNAME 3   �  L   a   WRITE_INTEGER_MATRIX2%ELEMENT_FMAT !   �  �       KICKOUT_FILEPATH '   y  >      KICKOUT_FILEPATH%INDEX *   �  A      KICKOUT_FILEPATH%LEN_TRIM *   �  L   a   KICKOUT_FILEPATH%FILEPATH #   D  �       REPLACE_FILESUFFIX )   �  >      REPLACE_FILESUFFIX%INDEX (   ,  =      REPLACE_FILESUFFIX%TRIM ,   i  L   a   REPLACE_FILESUFFIX%FILENAME '   �  L   a   REPLACE_FILESUFFIX%NEW "     d       LON_NEIGHBOR_DIFF '   e  @   a   LON_NEIGHBOR_DIFF%LON1 '   �  @   a   LON_NEIGHBOR_DIFF%LON2    �  �       GET_AVERAGE     g  <      GET_AVERAGE%SUM !   �  =      GET_AVERAGE%SIZE    �  �   a   GET_AVERAGE%X !   l  �       GET_COVA_ANOMALY %   .  <      GET_COVA_ANOMALY%SUM &   j  =      GET_COVA_ANOMALY%SIZE )   �  @      GET_COVA_ANOMALY%PRESENT $   �  �   a   GET_COVA_ANOMALY%X1 $   s  �   a   GET_COVA_ANOMALY%X2 +   �  @   a   GET_COVA_ANOMALY%OP_ISBIAS     ?  �       GET_COVA_ORIGIN $   �  <      GET_COVA_ORIGIN%SUM %   :   =      GET_COVA_ORIGIN%SIZE (   w   @      GET_COVA_ORIGIN%PRESENT #   �   �   a   GET_COVA_ORIGIN%X1 #   C!  �   a   GET_COVA_ORIGIN%X2 *   �!  @   a   GET_COVA_ORIGIN%OP_ISBIAS    "  �       GET_CORR    �"  =      GET_CORR%SQRT    �"  <      GET_CORR%SUM     #  =      GET_CORR%SIZE    ]#  �   a   GET_CORR%X1    �#  �   a   GET_CORR%X2    u$  t       FSPAN    �$  @   a   FSPAN%START    )%  @   a   FSPAN%FINISH    i%  @   a   FSPAN%NPTS    �%  �   a   FSPAN%ARRAY    ]&  z       STR_TIME    �&  @   a   STR_TIME%T_PRE 