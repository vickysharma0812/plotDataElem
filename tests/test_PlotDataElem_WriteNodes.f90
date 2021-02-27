program main
  use stringifor
  use plotDataElem_Class
  use tableDelaunay
  implicit none
  
  type( plotDataElem_ ) :: obj
  type( string ), allocatable :: varName( : )
  type( string ) :: fileName( 3 )
  integer, allocatable :: elements( :, : )
  
  fileName = [string( "../" ), string( "Plot_Data_Elem"), string( "" )]
  allocate( varName( 3 ) )
  varName( 1 ) = String( "P" )
  varName( 2 ) = String( "T" )
  varName( 3 ) = String( "S_hyd" )
  
  call initiate( obj = obj, &
    & fileName = fileName, &
    & varName = varName, &
    & dim = 1, tElements = 10 )
  
  call WriteNodes( obj = obj, ZoneID = 1, path = "./" )
  call WriteElements( 'Zone_1', elements )
end program main