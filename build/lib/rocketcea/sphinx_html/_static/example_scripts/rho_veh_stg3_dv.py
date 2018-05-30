
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ReferenceStage

rp = RhoIspPlot(bipropL=[('LOX','LH2'), ('LOX','RP1')], Pc=735., eps=27.5)

stg_obj = ReferenceStage( WtPayload=260000.0, volCuInRef=11180*1728, WtInertRef=29300.0,
                          Name='Saturn V 3rd stg')

rp.add_rho_isp_contours(label_frac_pos=0.2)

rp.add_stage_param_contours( stg_obj, set_param='DeltaV', param_value=8000,
                             label_frac_posD={'GLOW':0.5, 'CubicFt':.4, 'MassFrac':.1},
                             plot_param_valD={'GLOW':[ 475000, 500000, 525000, 550000, 575000, 600000], 
                                              'MassFrac':[0.88, .9, .92, .94, .95, .96],
                                              'CubicFt':[2000, 4000, 6000, 8000, 10000, 12000]},
                             plot_paramL=['CubicFt','GLOW'], num_ticks=10)

png_name = __file__[:-2] + 'png'
rp.savefig(png_name, dpi=120)
rp.show()

