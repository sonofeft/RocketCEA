
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ReferenceStage

rp = RhoIspPlot(nsteps_sg=100, nsteps_isp=100)


stg_obj = ReferenceStage( WtPayload=10000.0, volCuInRef=180*1728, WtInertRef=1121.0,
                          Name='Delta Upper Stg')

rp.add_rho_isp_contours(label_frac_pos=0.2)

rp.add_stage_param_contours( stg_obj, set_param='VolPropellant', param_value=180*1728,
                             label_frac_posD={'GLOW':0.45, 'DeltaV':.4, 'MassFrac':.1},
                             plot_param_valD={'GLOW':[ 14000, 16000, 18000, 20000, 22000, 24000, 26000, 280000], 
                                              'MassFrac':[0.86, 0.88, .9, .92],
                                              'DeltaV':[4000, 5000, 6000, 7000, 8000, 9000]},
                             plot_paramL=['DeltaV','GLOW','MassFrac'], num_ticks=10)

png_name = __file__[:-2] + 'png'
rp.savefig(png_name, dpi=120)
rp.show()

