from rocketcea.cea_obj import CEA_Obj

Pc = 40.0
eps = 5.0
ispObj = CEA_Obj( oxName='GOX', fuelName='GH2')

cea_file = open("online_gox_gh2.out", "r")
lineL = cea_file.readlines()
cea_file.close()
print( len(lineL) )

i = 0
while i < len(lineL):

    line = lineL[i].strip()
    if line.startswith('O/F='):
        print(line)
        MR = float( line.split()[1] )
        i = i + 16
        print( lineL[i].strip() )
        cpL = ispObj.get_HeatCapacities( Pc=Pc, MR=MR, eps=eps )
        s = '   '.join( ['%6.4f'%cp for cp in cpL] )
        print( 'RocketCEA MR=%4g'%MR, s )

        cp_ceaL = [float(cp) for cp in lineL[i].strip().split()[-3:]  ]
        s = '   '.join( [' good '  if abs(cpL[i]-cp_ceaL[i])<0.0001 else ' FAIL ' for i in range(3)] )
        print( '                ', s )

    i = i + 1