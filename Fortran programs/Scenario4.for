	
	 SUBROUTINE Scenario4

	 INTEGER i,NMSOLT,NM,flag
	 CHARACTER *15 Name

	 DOUBLE PRECISION f(1)
	 DOUBLE PRECISION Re(1),Im(1)
	 
	 DOUBLE PRECISION, ALLOCATABLE :: freqMSOLT(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11MS(:),ImS11MS(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11MO(:),ImS11MO(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11ML(:),ImS11ML(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22MS(:),ImS22MS(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22MO(:),ImS22MO(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22ML(:),ImS22ML(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11MT(:),ImS11MT(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS21MT(:),ImS21MT(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS12MT(:),ImS12MT(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22MT(:),ImS22MT(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS11M(:),ImS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS21M(:),ImS21M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS12M(:),ImS12M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReS22M(:),ImS22M(:)
 	 DOUBLE PRECISION, ALLOCATABLE :: ForwardH(:),ReverseH(:)
 	 DOUBLE PRECISION, ALLOCATABLE :: ForwardReS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ForwardImS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseReS11M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseImS11M(:)
 	 DOUBLE PRECISION, ALLOCATABLE :: ForwardReS21M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ForwardImS21M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseReS21M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseImS21M(:)
 	 DOUBLE PRECISION, ALLOCATABLE :: ForwardReS12M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ForwardImS12M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseReS12M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseImS12M(:)
 	 DOUBLE PRECISION, ALLOCATABLE :: ForwardReS22M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ForwardImS22M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseReS22M(:)
	 DOUBLE PRECISION, ALLOCATABLE :: ReverseImS22M(:)
	 DOUBLE COMPLEX unit,cunit
  	 DOUBLE COMPLEX SM(4),SA(4),Z
	 DOUBLE COMPLEX	ForwardSM(4),ForwardSA(4)
	 DOUBLE COMPLEX	ReverseSM(4),ReverseSA(4)
	 DOUBLE COMPLEX	ForwardZ,ReverseZ
	 DOUBLE COMPLEX SMSOLT(10),SOLT(10),ESOLT(10)
	 PARAMETER(unit=(1.0,0.0),cunit=(0.0,1.0),zero=(0.0,0.0))

	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'The frequency in the files must be in Hz.'
	 WRITE(*,*)''
	 WRITE(*,*)'Please check!'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE
	 
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'Please check that you have copied the following'
	 WRITE(*,*)'CSV files into the calculation folder (14 in total):'
	 WRITE(*,*)''
	 WRITE(*,*)'S11MS.csv'
	 WRITE(*,*)'S11MO.csv'
	 WRITE(*,*)'S11ML.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S22MS.csv'
	 WRITE(*,*)'S22MO.csv'
	 WRITE(*,*)'S22ML.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S11MT.csv'
	 WRITE(*,*)'S21MT.csv'
	 WRITE(*,*)'S12MT.csv'
	 WRITE(*,*)'S22MT.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'S11M.csv'
	 WRITE(*,*)'S21M.csv'
	 WRITE(*,*)'S12M.csv'
	 WRITE(*,*)'S22M.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'All MSOLT-files must have the same frequency points.'
	 WRITE(*,*)''
	 WRITE(*,*)'When measuring the dispersion, S**M.csv files '
	 WRITE(*,*)'must have the same frequency points as MSOLT-files.'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE

	 Name='S11MS.csv'
       Call NLine(Name,NMSOLT)

	 Name='S11M.csv'
       Call NLine(Name,NM)

	 ALLOCATE(freqMSOLT(NMSOLT))

	 ALLOCATE(ReS11MS(NMSOLT),ImS11MS(NMSOLT))
	 ALLOCATE(ReS11MO(NMSOLT),ImS11MO(NMSOLT))
	 ALLOCATE(ReS11ML(NMSOLT),ImS11ML(NMSOLT))

	 ALLOCATE(ReS22MS(NMSOLT),ImS22MS(NMSOLT))
	 ALLOCATE(ReS22MO(NMSOLT),ImS22MO(NMSOLT))
	 ALLOCATE(ReS22ML(NMSOLT),ImS22ML(NMSOLT))

	 ALLOCATE(ReS11MT(NMSOLT),ImS11MT(NMSOLT))
	 ALLOCATE(ReS21MT(NMSOLT),ImS21MT(NMSOLT))
	 ALLOCATE(ReS12MT(NMSOLT),ImS12MT(NMSOLT))
	 ALLOCATE(ReS22MT(NMSOLT),ImS22MT(NMSOLT))
	
	 OPEN(10,FILE='S11MS.csv')
	 OPEN(20,FILE='S11MO.csv')
	 OPEN(30,FILE='S11ML.csv')
       DO i=1,NMSOLT
	    READ(10,*) freqMSOLT(i),ReS11MS(i),ImS11MS(i)
	    READ(20,*) freqMSOLT(i),ReS11MO(i),ImS11MO(i)
	    READ(30,*) freqMSOLT(i),ReS11ML(i),ImS11ML(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)

	 OPEN(10,FILE='S22MS.csv')
	 OPEN(20,FILE='S22MO.csv')
	 OPEN(30,FILE='S22ML.csv')
       DO i=1,NMSOLT
	    READ(10,*) freqMSOLT(i),ReS22MS(i),ImS22MS(i)
	    READ(20,*) freqMSOLT(i),ReS22MO(i),ImS22MO(i)
	    READ(30,*) freqMSOLT(i),ReS22ML(i),ImS22ML(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)

	 OPEN(10,FILE='S11MT.csv')
	 OPEN(20,FILE='S21MT.csv')
	 OPEN(30,FILE='S12MT.csv')
	 OPEN(40,FILE='S22MT.csv')
       DO i=1,NMSOLT
	    READ(10,*) freqMSOLT(i),ReS11MT(i),ImS11MT(i)
	    READ(20,*) freqMSOLT(i),ReS21MT(i),ImS21MT(i)
	    READ(30,*) freqMSOLT(i),ReS12MT(i),ImS12MT(i)
	    READ(40,*) freqMSOLT(i),ReS22MT(i),ImS22MT(i)
       END DO
       CLOSE(10)
       CLOSE(20)
       CLOSE(30)
	 CLOSE(40)
       
	 flag=0
	 DO WHILE(flag.EQ.0)
	 WRITE(*,*)''
	 WRITE(*,*)'***********************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'What did you measure:'
	 WRITE(*,*)''
	 WRITE(*,*)'dispersion of S-parameters? - Enter "d"'
	 WRITE(*,*)''
	 WRITE(*,*)'or'
	 WRITE(*,*)''
	 WRITE(*,*)'field dependence of S-parameters? - Enter "f"'
	 WRITE(*,*)''
	 WRITE(*,*)'***********************************************'
	 WRITE(*,*)''
	 READ(*,*) Name

       SELECT CASE (Name)
       
	 CASE ('d','D')

	 ALLOCATE(ReS11M(NMSOLT),ImS11M(NMSOLT))
	 ALLOCATE(ReS21M(NMSOLT),ImS21M(NMSOLT))
	 ALLOCATE(ReS12M(NMSOLT),ImS12M(NMSOLT))
	 ALLOCATE(ReS22M(NMSOLT),ImS22M(NMSOLT))
	 OPEN(10,FILE='S11M.csv')
	 OPEN(20,FILE='S21M.csv')
	 OPEN(30,FILE='S12M.csv')
	 OPEN(40,FILE='S22M.csv')
       DO i=1,NMSOLT
	    READ(10,*) freqMSOLT(i),ReS11M(i),ImS11M(i)
	    READ(20,*) freqMSOLT(i),ReS21M(i),ImS21M(i)
	    READ(30,*) freqMSOLT(i),ReS12M(i),ImS12M(i)
	    READ(40,*) freqMSOLT(i),ReS22M(i),ImS22M(i)
       END DO
	 CLOSE(10)
	 CLOSE(20)
	 CLOSE(30)
	 CLOSE(40)
 
 	 OPEN(10,FILE='S11corrected.csv')
 	 OPEN(20,FILE='S22corrected.csv')
 	 OPEN(30,FILE='S12corrected.csv')
 	 OPEN(40,FILE='S21corrected.csv')
	 OPEN(50,FILE='Zcorrected.csv')
	 
		SOLT(1)=-unit
	    SOLT(2)=unit
	    SOLT(3)=zero
	    SOLT(4)=-unit
	    SOLT(5)=unit
	    SOLT(6)=zero
	    SOLT(7)=zero
	    SOLT(8)=unit
	    SOLT(9)=unit
	    SOLT(10)=zero

	 DO i=1,NMSOLT

		SMSOLT(1)=ReS11MS(i)*unit+ImS11MS(i)*cunit
	    SMSOLT(2)=ReS11MO(i)*unit+ImS11MO(i)*cunit
	    SMSOLT(3)=ReS11ML(i)*unit+ImS11ML(i)*cunit
	    SMSOLT(4)=ReS22MS(i)*unit+ImS22MS(i)*cunit
	    SMSOLT(5)=ReS22MO(i)*unit+ImS22MO(i)*cunit
	    SMSOLT(6)=ReS22ML(i)*unit+ImS22ML(i)*cunit
	    SMSOLT(7)=ReS11MT(i)*unit+ImS11MT(i)*cunit
	    SMSOLT(8)=ReS21MT(i)*unit+ImS21MT(i)*cunit
	    SMSOLT(9)=ReS12MT(i)*unit+ImS12MT(i)*cunit
	    SMSOLT(10)=ReS22MT(i)*unit+ImS22MT(i)*cunit

	    SM(1)=ReS11M(i)*unit+ImS11M(i)*cunit
	    SM(2)=ReS21M(i)*unit+ImS21M(i)*cunit
	    SM(3)=ReS12M(i)*unit+ImS12M(i)*cunit
	    SM(4)=ReS22M(i)*unit+ImS22M(i)*cunit

	    CALL SOLTErrors(SMSOLT,SOLT,ESOLT)
	    CALL SOLTCorrection(ESOLT,SM,SA)

	    Z=100.0*(unit-SA(4))/SA(4)

	    WRITE(10,100) freqMSOLT(i),REAL(SA(1)),IMAG(SA(1))
100		FORMAT(F20.8,',',F20.8,',',F20.8)

	    WRITE(20,200) freqMSOLT(i),REAL(SA(2)),IMAG(SA(2))
200		FORMAT(F20.8,',',F20.8,',',F20.8)

	    WRITE(30,300) freqMSOLT(i),REAL(SA(3)),IMAG(SA(3))
300		FORMAT(F20.8,',',F20.8,',',F20.8)

	    WRITE(40,400) freqMSOLT(i),REAL(SA(4)),IMAG(SA(4))
400		FORMAT(F20.8,',',F20.8,',',F20.8)

	    WRITE(50,500) freqMSOLT(i),REAL(Z),IMAG(Z),ABS(Z)
500		FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8)

	 END DO

	 CLOSE(10)
	 CLOSE(20)
	 CLOSE(30)
	 CLOSE(40)
	 CLOSE(50)

	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'Output files:'
	 WRITE(*,*)''
	 WRITE(*,*)'S11corrected.csv'
	 WRITE(*,*)'S21corrected.csv'
	 WRITE(*,*)'S12corrected.csv'
	 WRITE(*,*)'S22corrected.csv'
	 WRITE(*,*)'Zcorrected.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'The impedance was calculated from S21-parameter.'
	 WRITE(*,*)''
	 WRITE(*,*)'Columns of the output S-parameter and Z files:'
	 WRITE(*,*)''
	 WRITE(*,*)'f(Hz), Re[S], Im[S]'
	 WRITE(*,*)''
	 WRITE(*,*)'f(Hz), Re[Z], Im[Z], |Z)|'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE
	 
	 DEALLOCATE(ReS11M,ImS11M)
	 DEALLOCATE(ReS21M,ImS21M)
	 DEALLOCATE(ReS12M,ImS12M)
	 DEALLOCATE(ReS22M,ImS22M)
	 DEALLOCATE(freqMSOLT)
	 DEALLOCATE(ReS11MS,ImS11MS)
	 DEALLOCATE(ReS11MO,ImS11MO)
	 DEALLOCATE(ReS11ML,ImS11ML)
	 DEALLOCATE(ReS11MT,ImS11MT)
	 DEALLOCATE(ReS21MT,ImS21MT)
	 DEALLOCATE(ReS12MT,ImS12MT)
	 DEALLOCATE(ReS22MT,ImS22MT)
	 
	 flag=1         
       
       CASE ('f','F')

       WRITE(*,*)''
	 WRITE(*,*)'Enter the frequency (Hz) at which'
	 WRITE(*,*)'the field dependencies of S-parameters were measured.'
	 READ(*,*)f(1)

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS11MS,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS11MS,1,f,Im)
	 DEALLOCATE(ReS11MS,ImS11MS)
	 ALLOCATE(ReS11MS(1),ImS11MS(1))
	 ReS11MS=Re
	 ImS11MS=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS11MO,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS11MO,1,f,Im)
	 DEALLOCATE(ReS11MO,ImS11MO)
	 ALLOCATE(ReS11MO(1),ImS11MO(1))
	 ReS11MO=Re
	 ImS11MO=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS11ML,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS11ML,1,f,Im)
	 DEALLOCATE(ReS11ML,ImS11ML)
	 ALLOCATE(ReS11ML(1),ImS11ML(1))
	 ReS11ML=Re
	 ImS11ML=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS22MS,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS22MS,1,f,Im)
	 DEALLOCATE(ReS22MS,ImS22MS)
	 ALLOCATE(ReS22MS(1),ImS22MS(1))
	 ReS22MS=Re
	 ImS22MS=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS22MO,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS22MO,1,f,Im)
	 DEALLOCATE(ReS22MO,ImS22MO)
	 ALLOCATE(ReS22MO(1),ImS22MO(1))
	 ReS22MO=Re
	 ImS22MO=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS22ML,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS22ML,1,f,Im)
	 DEALLOCATE(ReS22ML,ImS22ML)
	 ALLOCATE(ReS22ML(1),ImS22ML(1))
	 ReS22ML=Re
	 ImS22ML=Im
	 
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS11MT,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS11MT,1,f,Im)
	 DEALLOCATE(ReS11MT,ImS11MT)
	 ALLOCATE(ReS11MT(1),ImS11MT(1))
	 ReS11MT=Re
	 ImS11MT=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS21MT,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS21MT,1,f,Im)
	 DEALLOCATE(ReS21MT,ImS21MT)
	 ALLOCATE(ReS21MT(1),ImS21MT(1))
	 ReS21MT=Re
	 ImS21MT=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS12MT,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS12MT,1,f,Im)
	 DEALLOCATE(ReS12MT,ImS12MT)
	 ALLOCATE(ReS12MT(1),ImS12MT(1))
	 ReS12MT=Re
	 ImS12MT=Im

	 CALL DCSIEZ(NMSOLT,freqMSOLT,ReS22MT,1,f,Re)
	 CALL DCSIEZ(NMSOLT,freqMSOLT,ImS22MT,1,f,Im)
	 DEALLOCATE(ReS22MT,ImS22MT)
	 ALLOCATE(ReS22MT(1),ImS22MT(1))
	 ReS22MT=Re
	 ImS22MT=Im

 	 ALLOCATE(ForwardH(NM),ReverseH(NM))
 	 ALLOCATE(ForwardReS11M(NM),ForwardImS11M(NM))
	 ALLOCATE(ReverseReS11M(NM),ReverseImS11M(NM))
 	 ALLOCATE(ForwardReS21M(NM),ForwardImS21M(NM))
	 ALLOCATE(ReverseReS21M(NM),ReverseImS21M(NM))
 	 ALLOCATE(ForwardReS12M(NM),ForwardImS12M(NM))
	 ALLOCATE(ReverseReS12M(NM),ReverseImS12M(NM))
 	 ALLOCATE(ForwardReS22M(NM),ForwardImS22M(NM))
	 ALLOCATE(ReverseReS22M(NM),ReverseImS22M(NM))

 	 OPEN(10,FILE='S11M.csv')
 	 OPEN(20,FILE='S21M.csv')
 	 OPEN(30,FILE='S12M.csv')
 	 OPEN(40,FILE='S22M.csv')
       DO i=1,NM
	    READ(10,*) ForwardH(i),ForwardReS11M(i),ForwardImS11M(i)
     *		      ,ReverseH(i),ReverseReS11M(i),ReverseImS11M(i) 
	    READ(20,*) ForwardH(i),ForwardReS21M(i),ForwardImS21M(i)
     *		      ,ReverseH(i),ReverseReS21M(i),ReverseImS21M(i) 
	    READ(30,*) ForwardH(i),ForwardReS12M(i),ForwardImS12M(i)
     *		      ,ReverseH(i),ReverseReS12M(i),ReverseImS12M(i) 
	    READ(40,*) ForwardH(i),ForwardReS22M(i),ForwardImS22M(i)
     *		      ,ReverseH(i),ReverseReS22M(i),ReverseImS22M(i) 
       END DO
	 CLOSE(10)
	 CLOSE(20)
	 CLOSE(30)
	 CLOSE(40)
 
	 OPEN(60,FILE='S11corrected.csv')
 	 OPEN(70,FILE='S22corrected.csv')
 	 OPEN(80,FILE='S12corrected.csv')
 	 OPEN(90,FILE='S21corrected.csv')
	 OPEN(100,FILE='Zcorrected.csv')

	 SMSOLT(1)=ReS11MS(1)*unit+ImS11MS(1)*cunit
	 SMSOLT(2)=ReS11MO(1)*unit+ImS11MO(1)*cunit
	 SMSOLT(3)=ReS11ML(1)*unit+ImS11ML(1)*cunit
	 SMSOLT(4)=ReS22MS(1)*unit+ImS22MS(1)*cunit
	 SMSOLT(5)=ReS22MO(1)*unit+ImS22MO(1)*cunit
	 SMSOLT(6)=ReS22ML(1)*unit+ImS22ML(1)*cunit
	 SMSOLT(7)=ReS11MT(1)*unit+ImS11MT(1)*cunit
	 SMSOLT(8)=ReS21MT(1)*unit+ImS21MT(1)*cunit
	 SMSOLT(9)=ReS12MT(1)*unit+ImS12MT(1)*cunit
	 SMSOLT(10)=ReS22MT(1)*unit+ImS22MT(1)*cunit

	 SOLT(1)=-unit
	 SOLT(2)=unit
	 SOLT(3)=zero
	 SOLT(4)=-unit
	 SOLT(5)=unit
	 SOLT(6)=zero
	 SOLT(7)=zero
	 SOLT(8)=unit
	 SOLT(9)=unit
	 SOLT(10)=zero

	 CALL SOLTErrors(SMSOLT,SOLT,ESOLT)

	 DO i=1,NM

	    ForwardSM(1)=ForwardReS11M(i)*unit+ForwardImS11M(i)*cunit
	    ForwardSM(2)=ForwardReS21M(i)*unit+ForwardImS21M(i)*cunit
	    ForwardSM(3)=ForwardReS12M(i)*unit+ForwardImS12M(i)*cunit
	    ForwardSM(4)=ForwardReS22M(i)*unit+ForwardImS22M(i)*cunit

	    ReverseSM(1)=ReverseReS11M(i)*unit+ReverseImS11M(i)*cunit
	    ReverseSM(2)=ReverseReS21M(i)*unit+ReverseImS21M(i)*cunit
	    ReverseSM(3)=ReverseReS12M(i)*unit+ReverseImS12M(i)*cunit
	    ReverseSM(4)=ReverseReS22M(i)*unit+ReverseImS22M(i)*cunit

		CALL SOLTCorrection(ESOLT,ForwardSM,ForwardSA)
		CALL SOLTCorrection(ESOLT,ReverseSM,ReverseSA)

	    ForwardZ=100.0*(unit-ForwardSA(4))/ForwardSA(4)
	    ReverseZ=100.0*(unit-ReverseSA(4))/ReverseSA(4)

	   WRITE(60,600) ForwardH(i),REAL(ForwardSA(1)),IMAG(ForwardSA(1))
     *		        ,ReverseH(i),REAL(ReverseSA(1)),IMAG(ReverseSA(1))
600	   FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *                ,',',F20.8)

	   WRITE(70,700) ForwardH(i),REAL(ForwardSA(2)),IMAG(ForwardSA(2))
     *		        ,ReverseH(i),REAL(ReverseSA(2)),IMAG(ReverseSA(2))
700	   FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *                ,',',F20.8)

	   WRITE(80,800) ForwardH(i),REAL(ForwardSA(3)),IMAG(ForwardSA(3))
     *		        ,ReverseH(i),REAL(ReverseSA(3)),IMAG(ReverseSA(3))
800	   FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *                ,',',F20.8)

	   WRITE(90,900) ForwardH(i),REAL(ForwardSA(4)),IMAG(ForwardSA(4))
     *		        ,ReverseH(i),REAL(ReverseSA(4)),IMAG(ReverseSA(4))
900	   FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *                ,',',F20.8)

	    WRITE(100,1000) ForwardH(i),REAL(ForwardZ),IMAG(ForwardZ)
     *		          ,ABS(ForwardZ),ReverseH(i),REAL(ReverseZ)
     *                  ,IMAG(ReverseZ),ABS(ReverseZ)
1000		FORMAT(F20.8,',',F20.8,',',F20.8,',',F20.8,',',F20.8
     *           ,',',F20.8,',',F20.8,',',F20.8)

	 END DO

	 CLOSE(60)
	 CLOSE(70)
	 CLOSE(80)
	 CLOSE(90)
	 CLOSE(100)

	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 WRITE(*,*)'Output files:'
	 WRITE(*,*)''
	 WRITE(*,*)'S11corrected.csv'
	 WRITE(*,*)'S21corrected.csv'
	 WRITE(*,*)'S12corrected.csv'
	 WRITE(*,*)'S22corrected.csv'
	 WRITE(*,*)'Zcorrected.csv'
	 WRITE(*,*)''
	 WRITE(*,*)'The impedance was calculated from S21-parameter.'
	 WRITE(*,*)''
	 WRITE(*,*)'Columns of the output S-parameter and Z files:'
	 WRITE(*,*)''
	 WRITE(*,*)'H(-->), Re[S], Im[S], H(<--), Re[S], Im[S]'
	 WRITE(*,*)''
	 WRITE(*,*)'H(-->), Re[Z], Im[Z], |Z|, H(<--), Re[Z], Im[Z], |Z|'
	 WRITE(*,*)''
	 WRITE(*,*)'where'
	 WRITE(*,*)''
	 WRITE(*,*)'H(-->) is the forward magnetic field (arb. units)'
	 WRITE(*,*)'H(<--) is the reverse magnetic field (arb. units)'
	 WRITE(*,*)''
	 WRITE(*,*)'*****************************************************'
	 WRITE(*,*)''
	 PAUSE
 
	 DEALLOCATE(freqMSOLT)
	 DEALLOCATE(ReS11MS,ImS11MS)
	 DEALLOCATE(ReS11MO,ImS11MO)
	 DEALLOCATE(ReS11ML,ImS11ML)
	 DEALLOCATE(ReS22MS,ImS22MS)
	 DEALLOCATE(ReS22MO,ImS22MO)
	 DEALLOCATE(ReS22ML,ImS22ML)
 	 DEALLOCATE(ReS11MT,ImS11MT)
	 DEALLOCATE(ReS21MT,ImS21MT)
	 DEALLOCATE(ReS12MT,ImS12MT)
	 DEALLOCATE(ReS22MT,ImS22MT)
 	 DEALLOCATE(ForwardH,ReverseH)
 	 DEALLOCATE(ForwardReS11M,ForwardImS11M)
	 DEALLOCATE(ReverseReS11M,ReverseImS11M)
 	 DEALLOCATE(ForwardReS21M,ForwardImS21M)
	 DEALLOCATE(ReverseReS21M,ReverseImS21M)
 	 DEALLOCATE(ForwardReS12M,ForwardImS12M)
	 DEALLOCATE(ReverseReS12M,ReverseImS12M)
 	 DEALLOCATE(ForwardReS22M,ForwardImS22M)
	 DEALLOCATE(ReverseReS22M,ReverseImS22M)

	 flag=1
	 
       CASE DEFAULT
       
	 WRITE (*, *)'Command not recognized. Please try again.'
	 flag=0
	 PAUSE
      
	 END SELECT
	 
	 END DO

	 RETURN

	 END
