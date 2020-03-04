from rocketcea.cea_obj import CEA_Obj

Pc=1000.0
MR=6.0
Pamb=14.7

ispObj = CEA_Obj( oxName='LOX', fuelName='LH2')

# get nozzle area ratio that has Pexit equal to Pamb
eps_pamb = ispObj.get_eps_at_PcOvPe(Pc=Pc, MR=MR, PcOvPe=Pc/Pamb)

# run under-expanded, Pexit equals Pambient, over-expanded and separated flow
for eps in [4., eps_pamb, 12., 40.]:
    CFcea, CFamb, mode = ispObj.get_PambCf( Pamb=Pamb, Pc=Pc, MR=MR, eps=eps)
    print('eps=%7g, CFcea=%7g, CFamb=%7g, mode=%s'%(eps, CFcea, CFamb, mode) )
