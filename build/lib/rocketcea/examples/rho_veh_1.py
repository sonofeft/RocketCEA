
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ReferenceStage

#rp = RhoIspPlot(bipropL=[('LOX','LH2'),('N2O4','MMH')], nsteps_sg=119, nsteps_isp=119)
rp = RhoIspPlot()


stg_obj = ReferenceStage( WtPayload=10000.0 )

rp.add_rho_isp_contours(label_frac_pos=0.2)

rp.add_stage_param_contours( stg_obj, set_param='DeltaV', param_value=5000.0,
                             plot_param_valD={'GLOW':[20000, 19000, 18000, 17000, 16000], 
                                              'MassFrac':[.65,.7,.75,.8,.85],
                                              'CubicFt':[75,100,200,300,400]},
                             label_frac_posD={'GLOW':0.9, 'CubicFt':.4},
                             plot_paramL=['GLOW','CubicFt','MassFrac'], num_ticks=6)
    
rp.savefig('rho_veh_1.png', dpi=120)
rp.show()

