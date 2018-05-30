
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ReferenceStage

rp = RhoIspPlot(nsteps_sg=100, nsteps_isp=100)


stg_obj = ReferenceStage( WtPayload=10000.0, volCuInRef=180*1728, WtInertRef=1121.0,
                          Name='Delta Upper stg')

rp.add_rho_isp_contours(label_frac_pos=0.2)

rp.add_stage_param_contours( stg_obj, set_param='DeltaV', param_value=8000,
                             label_frac_posD={'GLOW':0.45, 'CubicFt':.4, 'MassFrac':.1},
                             plot_param_valD={'GLOW':[ 20000, 22000, 24000, 26000], 
                                              'MassFrac':[0.88, .9, .92, .94, .95, .96],
                                              'CubicFt':[100, 200, 300, 400]},
                             plot_paramL=['CubicFt','GLOW','MassFrac'], num_ticks=10)

png_name = __file__[:-2] + 'png'
rp.savefig(png_name, dpi=120)
rp.show()

