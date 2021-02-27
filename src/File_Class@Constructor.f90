SUBMODULE( File_Class ) Constructor
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE init_file
  Obj % Path = String( Path )
  Obj % FileName = String( FileName )
  Obj % Extension = String( Extension )
  Obj % Status = String( Status )
  Obj % Action = String( Action )

  IF( PRESENT( Access ) ) THEN
    Obj % Access = String(Access)
  ELSE
    Obj % Access = String( " " )
  END IF
  CALL OpenFile( Obj )
  Obj % WriteNo = 0
END PROCEDURE init_file

!------------------------------------------------------------------------------
!                                                                       File
!------------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  Ans % isOpen = .FALSE.
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                                 OpenFile
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file
  ! Define internal variable
  TYPE( String ) :: FileName
  INTEGER( I4B ) :: tsize, uni

  FileName = FileName % Join ( [Obj % Path, Obj % FileName, Obj % Extension] )

  INQUIRE( file = FileName % Raw,  number = uni )

  ! if uni = -1 then filename is not opened, else it is already opened
  IF( uni .EQ. -1 ) THEN
    tsize = LEN_TRIM( Obj % Access )
    IF( tsize .EQ. 0 ) THEN
      OPEN( &
        & NewUnit = Obj % UnitNo,  &
        &	FILE = TRIM( FileName % Raw ), &
        &	STATUS = TRIM( Obj % STATUS % Raw ), &
        &	ACTION = TRIM( Obj % ACTION % Raw ), &
        &	IOSTAT = Obj % IOSTAT  )
    ELSE
      OPEN( &
        & NewUnit = Obj % UnitNo,  &
        &	FILE = TRIM( FileName % Raw ), &
        &	STATUS = TRIM( Obj % STATUS % Raw ), &
        &	ACTION = TRIM( Obj % ACTION % Raw ), &
        & ACCESS = TRIM( Obj % Access % Raw ), &
        &	IOSTAT = Obj % IOSTAT  )
    END IF
    IF( Obj % IOSTAT .NE. 0 ) THEN
      Obj % isOpen = .FALSE.
      WRITE( *, "(A)") "FATAL ERROR:: File could not be opened"
      WRITE( *, * ) "IOSTAT = ", obj%IOSTAT
      WRITE( *, "(A)" ) "PROGRAM STOPPED"
      STOP
    ELSE
      Obj % isOpen = .TRUE.
    END IF
  ELSE
    Obj % isOpen = .TRUE.
    Obj % UnitNo = uni
  END IF

END PROCEDURE open_file

!----------------------------------------------------------------------------
!                                                           OpenFileToWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_write_a
  CALL Initiate( Obj, Path, FileName, Extension, "REPLACE", "WRITE" )
END PROCEDURE open_file_write_a

!----------------------------------------------------------------------------
!                                                            OpenFileToWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_write_b
  CALL Initiate( Obj, PFE( 1 ) % Raw, PFE( 2 ) % Raw, PFE( 3 ) % Raw , &
    & "REPLACE", "WRITE" )
END PROCEDURE open_file_write_b

!----------------------------------------------------------------------------
!                                                            OpenFileToWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_write_c
  CALL Initiate( Obj, Path % Raw, FileName % Raw, Extension % Raw, &
  & "REPLACE", "WRITE" )
END PROCEDURE open_file_write_c

!----------------------------------------------------------------------------
!                                                           OpenFileToRead
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Read_a
  CALL Initiate( Obj, Path, FileName, Extension, "OLD", "READ" )
END PROCEDURE open_file_Read_a

!----------------------------------------------------------------------------
!                                                            OpenFileToRead
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Read_b
  CALL Initiate( Obj, PFE( 1 ) % Raw, PFE( 2 ) % Raw, PFE( 3 ) % Raw , &
    & "OLD", "READ" )
END PROCEDURE open_file_Read_b

!----------------------------------------------------------------------------
!                                                            OpenFileToRead
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Read_c
  CALL Initiate( Obj, Path % Raw, FileName % Raw, Extension % Raw, &
  & "OLD", "READ" )
END PROCEDURE open_file_Read_c

!----------------------------------------------------------------------------
!                                                           OpenFileToAppend
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Append_a
  CALL Initiate( Obj, Path, FileName, Extension, "REPLACE", "APPEND" )
END PROCEDURE open_file_Append_a

!----------------------------------------------------------------------------
!                                                            OpenFileToAppend
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Append_b
  CALL Initiate( Obj, PFE( 1 ) % Raw, PFE( 2 ) % Raw, PFE( 3 ) % Raw , &
    & "REPLACE", "APPEND" )
END PROCEDURE open_file_Append_b

!----------------------------------------------------------------------------
!                                                            OpenFileToAppend
!----------------------------------------------------------------------------

MODULE PROCEDURE open_file_Append_c
  CALL Initiate( Obj, Path % Raw, FileName % Raw, Extension % Raw, &
  & "REPLACE", "APPEND" )
END PROCEDURE open_file_Append_c

!----------------------------------------------------------------------------
!                                                                 CloseFile
!----------------------------------------------------------------------------

MODULE PROCEDURE close_file
  IF( Obj % isOpen ) THEN
    CLOSE( Obj % Unitno )
    Obj % isOpen = .FALSE.
  END IF
END PROCEDURE close_file

!----------------------------------------------------------------------------
!                                                                 ReopenFile
!----------------------------------------------------------------------------

MODULE PROCEDURE reopen_file
  CALL CloseFile( Obj )
  CALL OpenFile( Obj )
END PROCEDURE reopen_file

END SUBMODULE Constructor




