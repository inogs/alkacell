	program chekdata_pHrange
	
c created by CGP 
c read output file of alka_cellaparta 
c check for pH data outside range 3 and 3.5
c write a new input file in the right format

	IMPLICIT NONE
		INTEGER MaxPts
	PARAMETER (MaxPts = 100)
	INTEGER J,JJ,I
	DOUBLE PRECISION S, PT, SiT, T, M0, C, DAcid, W0
	DOUBLE PRECISION phMIN, phMAX
	DOUBLE PRECISION V(MaxPts), E(MaxPts),Vnew(MaxPts), Enew(MaxPts)
	DOUBLE PRECISION PH(MaxPts), H(MaxPts), PHnew(MaxPts), Hnew(MaxPts)
	character*50, Sstr, PTstr,SITstr, Tstr,W0str, Cstr, DACIDstr
c read output file

	OPEN (40, FILE = 'C:\AlkaOpenCell\src\output')

	READ (40, *)
	READ (40, *)
	READ (40, *) ! Header
	READ (40, '(1X, ''S = '',F7.3)') S
	write(*,*) Sstr
	READ (40, '(1X, ''PT = '',F6.2)') PT
	READ (40, '(1X, ''SIT = '',F6.2)') SIT
	READ (40, *)
	READ (40, '(1X, ''T = '',F6.2)') T
	READ (40, '(1X, ''M0 = '',F7.3)') M0
	READ (40, '(1X, ''C = '',F7.5)') C
	READ (40, '(1X, ''DACID = '', F7.5, '' g/cm3'' /)') DACID
	READ (40,*)
	READ (40,*)
	READ (40,*)
	READ (40,*) ! line of E0
	READ (40,*) ! line of AT
	READ (40,*) ! line of s
	READ (40,*) ! line of V/cm3 ...
	READ (40,*)
	
	DO 200 I = 1, MaxPts
	READ (40,*, END=300) V(I), E(I), PH(I), H(I)
200   CONTINUE
300   CONTINUE

	CLOSE (40)
	JJ=0
	phMIN = 3.00
      phMAX = 3.50

      DO 201 J= 1, I
	    IF (PH(J).ge.phMIN.and.PH(J).le.phMAX) then
	       JJ = JJ+1
	       Vnew(JJ) =  V(J)
	       Enew(JJ) =  E(J)
	       PHnew(JJ)= PH(J)
	       Hnew(JJ) =  H(J)
          ENDIF
201   CONTINUE


      W0= M0
c write new input file


	Sstr    ='     salinity of sample'
	PTstr   ='     micromol/kg total phosphate'
	SITstr  ='     micromol/kg total silicate'
	Tstr    ='     oC sample temp when titrated'
	W0str   ='     g  weight of sample titrated'
	Cstr    ='     mol/kg conc of acid titrant'
	DACIDstr='     g/cm3 density acid titrant'
	OPEN (41, FILE = 'C:\AlkaOpenCell\src\input_new')
      
	WRITE (41, '(''SAMPLE CO2 TITRATION DATA checked for pH'')') 
	WRITE (41,*)
	WRITE (41,'(''S =     '', F7.3, A)') S, Sstr
	WRITE (41,'(''PT =    '', F6.2, A)') PT, PTstr
	WRITE (41,'(''SiT =   '', F6.2, A)') SIT, SITstr
	WRITE (41,*)
	WRITE (41,'(''T =     '', F6.2, A)') T, Tstr
	WRITE (41,'(''W0 =    '', F7.3, A)') W0, W0str
	WRITE (41,'(''C =     '', F7.5, A)') C, Cstr
	WRITE (41,'(''DACID=  '', F7.5, A)') DACID, DACIDstr
	WRITE (41,*)
	WRITE (41,'(''V/cm3 E/V'')')
	WRITE (41,'(''------ --------'')')
	

	DO 202 I = 1, JJ
	 WRITE (41, '(F5.3,1X,F8.6)') Vnew(I), Enew(I)
202   CONTINUE
	CLOSE (41)

	END



