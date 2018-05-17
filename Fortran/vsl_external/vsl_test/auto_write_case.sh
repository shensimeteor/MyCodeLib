#!/bin/bash

test -e auto_case.f90 && /bin/rm -rf auto_case.f90
for code in $(cat errorcodes.txt); do
    echo "case ( $code )"
    echo "    vsl_strerror = \"mkl_vsl error: $code\" "
done
