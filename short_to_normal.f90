!***********************************************************************
!                                                                      *
      PROGRAM short_to_normal
!-----------------------------------------------
!                                                                      *
!   Program for changing the format for the expasion from the new      *
!   foramt to the old format                                           *
!                                                                      *
!***********************************************************************

!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      integer, parameter :: fil_1 = 1 
      integer, parameter :: fil_2 = 2
      CHARACTER :: FILNAM*256 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------

      CHARACTER(LEN=256) :: pl1,pl2,pl3,cl1,cl2,cl3
      CHARACTER(LEN=256) :: line1,line2,line3,line4,line5,record
      INTEGER            :: IOS,ncsf(100),nblock,i
      CHARACTER(LEN=5)   :: Jblock(100)
       
!-----------------------------------------------


       
      print *, "Input file: list with shortened CSFs"
      read(*,'(a)') FILNAM
      
      open(unit=fil_1, file=FILNAM, status='old', position='asis') 
      open(unit=fil_2, file='rcsf.out', status='unknown', position='asis') 
      
      ncsf = 0 
     
      read(fil_1,'(A)')line1
      read(fil_1,'(A)')line2
      read(fil_1,'(A)')line3
      read(fil_1,'(A)')line4
      read(fil_1,'(A)')line5

      write(fil_2,'(A)')trim(line1)
      write(fil_2,'(A)')trim(line2)
      write(fil_2,'(A)')trim(line3)
      write(fil_2,'(A)')trim(line4)
      write(fil_2,'(A)')trim(line5)

      read(fil_1,'(A)')cl1
      read(fil_1,'(A)')cl2
      read(fil_1,'(A)')cl3

      pl1=cl1
      pl2=cl2
      pl3=cl3

      write(fil_2,'(A)')trim(cl1)
      write(fil_2,'(A)')trim(cl2)
      write(fil_2,'(A)')trim(cl3)

      do
         read (fil_1, '(A)', IOSTAT=IOS) record
         if (IOS == 0) then
	    if(record(1:2).ne.' *')then
	       backspace(fil_1)
               read (fil_1, '(A)', IOSTAT=IOS) cl1
               if(cl1(len_trim(cl1):len_trim(cl1)).eq.'+'.or.&
                  cl1(len_trim(cl1):len_trim(cl1)).eq.'-')then
                    pl3=cl3
                    write(fil_2,'(A)')trim(pl1)
		    write(fil_2,'(A)')trim(pl2)
		    write(fil_2,'(A)')trim(cl1)
               else
	            read (fil_1, '(A)', IOSTAT=IOS) cl2
                    if(cl2(len_trim(cl2):len_trim(cl2)).eq.'+'.or.&
                    cl2(len_trim(cl2):len_trim(cl2)).eq.'-')then
                        pl2=cl1
                        pl3=cl2
                        write(fil_2,'(A)')trim(pl1)
		        write(fil_2,'(A)')trim(cl1)
		        write(fil_2,'(A)')trim(cl2)
		    else
                        read (fil_1, '(A)', IOSTAT=IOS) cl3
                        if(cl3(len_trim(cl3):len_trim(cl3)).eq.'+'.or.&
                        cl3(len_trim(cl3):len_trim(cl3)).eq.'-')then
                            pl1=cl1
                            pl2=cl2
                            pl3=cl3
                            write(fil_2,'(A)')trim(cl1)
		            write(fil_2,'(A)')trim(cl2)
		            write(fil_2,'(A)')trim(cl3)
		        endif    
		    endif    
               endif
            else 
               write(fil_2,'(A)')trim(record)
            endif
         else
            exit
         endif
      enddo

end 
