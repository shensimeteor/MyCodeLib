#!/bin/bash
FC=ifort
FPP=fpp

$FC -c ../utility.F90

$FPP -I../../ ArgsManager.F90 > ArgsManager.f90
$FC -c ArgsManager.f90

$FC -c test_filepath.F90

$FC *.o -o test_filepath.exe
