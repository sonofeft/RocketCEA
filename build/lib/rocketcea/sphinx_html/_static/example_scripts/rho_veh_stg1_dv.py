
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ReferenceStage

rp = RhoIspPlot(bipropL=[('LOX','LH2'), ('LOX','RP1')], Pc=1000., eps=16)

stg_obj = ReferenceStage( WtPayload=1600000.0, volCuInRef=75500.0*1728, WtInertRef=300000.0,
                          Name='Saturn V 1st stg')

rp.add_rho_isp_contours(label_frac_pos=0.2)

rp.add_stage_param_contours( stg_obj, set_param='DeltaV', param_value=12000,
                             label_frac_posD={'GLOW':0.9, 'CubicFt':.4, 'MassFrac':.8},
                             plot_param_valD={'GLOW':[6e6, 5.5e6, 5e6, 4.5e6],
                                              'CubicFt':[60000, 70000, 90000, 110000, 13000]},
                             plot_paramL=['GLOW','CubicFt'], num_ticks=6)

png_name = __file__[:-2] + 'png'
rp.savefig(png_name, dpi=120)
rp.show()

