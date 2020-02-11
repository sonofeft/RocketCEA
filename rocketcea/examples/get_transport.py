
from rocketcea.cea_obj import CEA_Obj

ispObj = CEA_Obj( oxName='LOX', fuelName='LH2')

print("""
----------- Printing Standard Output ---------
s = ispObj.get_full_cea_output( Pc=1000.0, MR=6.0, eps=40.0, frozen=0, frozenAtThroat=1,
                                short_output=1, show_transport=1)

 TRANSPORT PROPERTIES (GASES ONLY)
   CONDUCTIVITY IN UNITS OF MILLICALORIES/(CM)(K)(SEC)

 VISC,MILLIPOISE   1.0588   1.0153  0.43328

  WITH EQUILIBRIUM REACTIONS

 Cp, CAL/(G)(K)    2.0951   1.9171   0.7308
 CONDUCTIVITY      4.1444   3.6272   0.5411
 PRANDTL NUMBER    0.5352   0.5366   0.7123

  WITH FROZEN REACTIONS

 Cp, CAL/(G)(K)    0.9029   0.8950   0.7302
 CONDUCTIVITY      1.3519   1.2760   0.5398
 PRANDTL NUMBER    0.7071   0.7121   0.7135
 ====================================================""")

cpEch, visEch, conEch, prEch = ispObj.get_Chamber_Transport(Pc=1000.0, MR=6.0, eps=40.0)
cpFch, visFch, conFch, prFch = ispObj.get_Chamber_Transport(Pc=1000.0, MR=6.0, eps=40.0, frozen=1)

cpEth, visEth, conEth, prEth = ispObj.get_Throat_Transport(Pc=1000.0, MR=6.0, eps=40.0)
cpFth, visFth, conFth, prFth = ispObj.get_Throat_Transport(Pc=1000.0, MR=6.0, eps=40.0, frozen=1)

cpEex, visEex, conEex, prEex = ispObj.get_Exit_Transport(Pc=1000.0, MR=6.0, eps=40.0)
cpFex, visFex, conFex, prFex = ispObj.get_Exit_Transport(Pc=1000.0, MR=6.0, eps=40.0, frozen=1)

sOut =  """           Calling Transport Functions.
VISC,MILLIPOISE   %6.4f   %6.4f  %6.4f
 VISC,MILLIPOISE   %6.4f   %6.4f  %6.4f
 
  WITH EQUILIBRIUM REACTIONS

 Cp, CAL/(G)(K)    %6.4f   %6.4f   %6.4f
 CONDUCTIVITY      %6.4f   %6.4f   %6.4f
 PRANDTL NUMBER    %6.4f   %6.4f   %6.4f

  WITH FROZEN REACTIONS

 Cp, CAL/(G)(K)    %6.4f   %6.4f   %6.4f
 CONDUCTIVITY      %6.4f   %6.4f   %6.4f
 PRANDTL NUMBER    %6.4f   %6.4f   %6.4f
"""%( visEch, visEth, visEex, visFch, visFth, visFex,
      cpEch, cpEth, cpEex, conEch, conEth, conEex, prEch, prEth, prEex,
      cpFch, cpFth, cpFex, conFch, conFth, conFex, prFch, prFth, prFex)
print( sOut )
print()
print( 'ispObj.get_HeatCapacities( Pc=1000.0, MR=6.0, eps=40.0, frozen=0)' )
print( ispObj.get_HeatCapacities( Pc=1000.0, MR=6.0, eps=40.0, frozen=0) )

print( 'ispObj.get_HeatCapacities( Pc=1000.0, MR=6.0, eps=40.0, frozen=1)' )
print( ispObj.get_HeatCapacities( Pc=1000.0, MR=6.0, eps=40.0, frozen=1) )
