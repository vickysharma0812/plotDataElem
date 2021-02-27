program main
use stringifor
use plotDataElem_Class
implicit none

type( plotDataElem_ ) :: obj
type( string ), allocatable :: varName( : )
type( string ) :: fileName( 3 )

fileName = [string( "../" ), string( "Plot_Data_Elem"), string( "" )]
allocate( varName( 3 ) )
varName( 1 ) = String( "P" )
varName( 2 ) = String( "T" )
varName( 3 ) = String( "S_hyd" )

call initiate( obj = obj, &
  & fileName = fileName, &
  & varName = varName, &
  & dim = 1 )

call display( obj = obj, msg = "" )

end program main

! gfortran -o test_PlotDataElem_Initiate test_PlotDataElem_Initiate.f90 -I$include/extpkgs $lib/extpkgs/*.o *.o
