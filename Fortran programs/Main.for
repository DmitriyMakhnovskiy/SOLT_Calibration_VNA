
	 USE MSIMSLMD

	 INTEGER scenario,flag 

	 WRITE(*,*)'****************< Version 1 >*****************'
	 WRITE(*,*)'*                                            *'
	 WRITE(*,*)'*  SOLT.exe console application corrects     *'
	 WRITE(*,*)'*  the measured S-parameters using           *'
	 WRITE(*,*)'*  the SOLT calibration (coaxial or on PCB). *'
	 WRITE(*,*)'*  See the report for full details.          *'
	 WRITE(*,*)'*                                            *'
	 WRITE(*,*)'*      Sensing Materials Technology Ltd      *'
	 WRITE(*,*)'*                                            *'
	 WRITE(*,*)'*                Plymouth, UK                *'
	 WRITE(*,*)'*                                            *'
	 WRITE(*,*)'*************< 18th June 2018 >***************'
	 WRITE(*,*)''
	 PAUSE

	 flag=0
	 DO WHILE(flag.EQ.0)

	 WRITE(*,*)'****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'Choose your scenario (1, 2, 3, 4) for the SOL(T)'
	 WRITE(*,*)'calibration and the corresponding measurements. '
	 WRITE(*,*)''
	 WRITE(*,*)'1: 1-port SOL calibration and measurements'
	 WRITE(*,*)''
	 WRITE(*,*)'2: 1-port ideal calibration and measurements on PCB'
	 WRITE(*,*)''
	 WRITE(*,*)'3: 2-port SOLT calibration and measurements'
	 WRITE(*,*)''
	 WRITE(*,*)'4: 2-port ideal calibration and measurements on PCB'
	 WRITE(*,*)''
	 WRITE(*,*)'Enter 1, 2, 3, or 4.'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 READ(*,*) scenario

	 SELECT CASE(scenario)

	 CASE(1)

	     WRITE(*,*)'*************************************************'
		 WRITE(*,*)''
	     WRITE(*,*)'Please check that the frequency range'
	     WRITE(*,*)'of the calibration SOLT files includes'
	     WRITE(*,*)'the frequency range of the measured MSOLT files.'
	     WRITE(*,*)''
	     WRITE(*,*)'Otherwise, the program will be terminated!'
	     WRITE(*,*)''
	     WRITE(*,*)'*************************************************'
	     WRITE(*,*)''
	     PAUSE

	     flag=1
		 CALL Scenario1
	 
	 CASE(2)
	 
	     flag=1
		 CALL Scenario2
	 
	 CASE(3)

	     WRITE(*,*)'*************************************************'
		 WRITE(*,*)''
	     WRITE(*,*)'Please check that the frequency range'
	     WRITE(*,*)'of the calibration SOLT files includes'
	     WRITE(*,*)'the frequency range of the measured MSOLT files.'
	     WRITE(*,*)''
	     WRITE(*,*)'Otherwise, the program will be terminated!'
	     WRITE(*,*)''
	     WRITE(*,*)'*************************************************'
	     WRITE(*,*)''
	     PAUSE
	 
	     flag=1
		 CALL Scenario3
	 
	 CASE(4)

	     flag=1
		 CALL Scenario4
	 
	 CASE DEFAULT
	 
	 WRITE (*, *)'Command not recognized. Please try again.'
	 flag=0
	 PAUSE
      
	 END SELECT
	   
	 END DO

       STOP

	 END
