       program test_read_amsua

!############################################################
	 integer dn(15,30),hour
!	 character filename*70
	 character filename*42
	 real wn(15),rad(15,30),bt(15,30),lightspeed
	 real*4 second
	 data c1,c2/1.1910439E-5,1.4387686/!	unit:       mw/(m^2 str cm^-4)      cm k
	 data wn/23.8,31.4,50.3,52.8,53.596,54.40,54.94,55.50,57.30,57.30,57.30,57.30,57.30,57.30,89.00/  ! GHz
!		AMSU-A1: 23.8 and 31.4, AMSU-A2: 50.3-89
	 data lightspeed/2.997924E+10/  ! cm/s

       include "SATE_ATOVS1B_AMA1B_no_C.H"
!#############################################################
!	filename='nss.amax.nm.d08086.s1203.e1356.b2990405.wi.l6627083.1b'
!	filename='nss.amax.nm.d08336.s0141.e0327.b3345557.wi.16180153.1b'  ! ftp
	filename='NSS.AMAX.NN.D14227.S0116.E0256.B4758788.GC'
!	filename='NSS.AMAX.NN.D09001.S0000.E0155.B1863638.WI'  ! ncdc ftp push, 1st, have header
!   filename=''    ! download from class

	  
	  open(101,file=filename,form='unformatted',access='direct',recl=2560,status='old')
     
!	open(101,file=filename,form='binary',recl=1,convert='big_endian')
     
         read(101,rec=1)ama_hd
	   print*,filename
       print*,'scanline=',ama_h_callocsclin
	   print*,'ama_h_startdatatime=',ama_h_startdatatime
!		print out header file infomation here, "ama_h_..."


!      open(102,file='time_'//filename//'.txt')
!	ama_scnlin,hour,minute,second   
   
!	open(103,file='latlon_rad_'//filename//'.txt')
!	ama_scnlin,lat(30 points)
!	ama_scnlin,lon(30 points)

!      open(104,file='rad_'//filename//'.txt')
!	scanline,m(channles),(rad(m,n),n=1,30), 30 points

       open(105,file='all_chs_rad_'//filename//'.txt')
!       scanline,point,lat,lon,rad_ch1_AMSU-A2,rad_ch2_AMSU-A2,rad_ch1-AMSU-A1,
!	rad_ch2-AMSU-A1,rad_ch3-AMSU-A1,rad_ch4-AMSU-A1,...rad_ch13-AMSU-A1

!      open(106,file='Tbb_'//filename//'.txt')
!	scanline,m(channels),(Tbb(m,n),n=1,30), 30 points  
  
!       open(107,file='all_chs_Tbb_'//filename//'.txt')
!       scanline,point,lat,lon,Tbb_ch1_AMSU-A2,Tbb_ch2_AMSU-A2,Tbb_ch1-AMSU-A1,
!	Tbb_ch2-AMSU-A1,Tbb_ch3-AMSU-A1,Tbb_ch4-AMSU-A1,...Tbb_ch13-AMSU-A1

       nrec=1
       
       do i=1,ama_h_callocsclin
       
          nrec=nrec+1     
          read(101,rec=nrec)ama_dt          ! integer*2 ama_dt(1280)
     
		hour=int(ama_scnlintime/1000/60/60)
		minute=(ama_scnlintime-hour*60*60*1000)/1000/60
		second=(ama_scnlintime-hour*60*60*1000-minute*60*1000)/1000.
		
!	write(102,'(i4,2x,i2,2x,i2,2x,f6.3)')ama_scnlin,hour,minute,second

!	write(103,'(i4,30(f8.2,2x))')ama_scnlin,(ama_pos(2*j-1)/10000.,j=1,30)
!	write(103,'(i4,30(f8.2,2x))')ama_scnlin,(ama_pos(2*j)/10000.,j=1,30)	

!			do j=1,30
!			write(102,'(i4,2x,i2,2x,i2,2x,f5.2,2x,f8.2,2x,f8.2)')
!     $		ama_scnlin,hour,minute,second,
!     $		ama_pos(2*j-1)/10000.,ama_pos(2*j)/10000.
!			enddo

		if(i.eq.1)then
		print*,'*',ama_pmcal
		print*,'*******'
!		print*,ama2_earth_data
		endif

        do n=1,30
	                  
			if(ama1_id.ne.0)then
				dn(3:15,n)=ama1_earth_data(5:17,n)
			elseif(ama2_id.ne.0)then
				dn(1:2,n)=ama2_earth_data(3:4,n)
!			if(i.eq.1.and.n.eq.1)print*,dn(1:2,n)
			endif
	
          do m=1,15       ! AMSU-A2(ch1,ch2),AMSU-A1(ch1-ch13)

		rad(m,n)=ama_pmcal((m-1)*3+1)*1.E-19*dn(m,n)**2+ama_pmcal((m-1)*3+2)*1.E-13*dn(m,n)+ama_pmcal((m-1)*3+3)*1.E-9
!	           radiance unit: mw/(m^2 str cm^-1)

		if(rad(m,n).le.0.)rad(m,n)=-9999.99
	
	    enddo
      
	  enddo  !do n

           do m=1,15        ! AMSU-A2(ch1,ch2),AMSU-A1(ch1-ch13)
!	      write(104,*)i,m,(rad(m,n),n=1,30)
!		  scanline,m(channles),(rad(m,n),n=1,30), 30 points	      
              do n=1,30
                  if(rad(m,n).eq.-9999.99)then
					bt(m,n)=0
                  else
	    wavelengthum=wn(m)*1E9/lightspeed
		bt(m,n)=c2*wavelengthum/log(1+c1*wavelengthum**3/rad(m,n))
		if(i.eq.1.and.m.eq.3.and.n.eq.1)print*,ama_pos(2*n-1)/10000.,ama_pos(2*n)/10000.,bt(m,1)
                  endif     		 
	if(i.eq.1.and.n.eq.1)print*,m,dn(m,n),rad(m,n),bt(m,n)
              enddo
!          write(105,'(i3,1x,i2,1x,30(f12.6,1x))')i,m,(bt(m,n),n=1,30)           
!           write(106,*)i,m,(bt(m,n),n=1,30)
!		 scanline,m(channels),(Tbb(m,n),n=1,30), 30 points           
           enddo

		do n=1,30
	    write(105,*)i,n,ama_pos(2*n-1)/10000.,ama_pos(2*n)/10000.,(rad(m,n),m=1,15)   
!       scanline,point,lat,lon,rad_ch1_AMSU-A2,rad_ch2_AMSU-A2,rad_ch1-AMSU-A1,
!	rad_ch2-AMSU-A1,rad_ch3-AMSU-A1,rad_ch4-AMSU-A1,...rad_ch13-AMSU-A1

!		write(107,*)i,n,ama_pos(2*n-1)/10000.,ama_pos(2*n)/10000.,(bt(m,n),m=1,15)    
!      format: scanline,point,lat,lon,Tbb_ch1_AMSU-A2,Tbb_ch2_AMSU-A2,Tbb_ch1-AMSU-A1,
!	 Tbb_ch2-AMSU-A1,Tbb_ch3-AMSU-A1,Tbb_ch4-AMSU-A1,...Tbb_ch13-AMSU-A1
		enddo
        
        enddo  !do i
  
         close(102)
         close(103)
         close(104)
         close(105)
	   close(106)
         
         end program

