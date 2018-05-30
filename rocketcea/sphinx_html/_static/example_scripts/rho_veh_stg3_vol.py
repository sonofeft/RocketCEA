
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ReferenceStage

rp = RhoIspPlot(bipropL=[('LOX','LH2'), ('LOX','RP1')], Pc=735., eps=27.5)

stg_obj = ReferenceStage( WtPayload=260000.0, volCuInRef=11180*1728, WtInertRef=29300.0,
                          Name='Saturn V 3rd stg')

rp.add_rho_isp_contours(label_frac_pos=0.2)

rp.add_stage_param_contours( stg_obj, set_param='VolPropellant', param_value=11180*1728,
                             label_frac_posD={'GLOW':0.2, 'CubicFt':.4, 'DeltaV':.5},
                             plot_param_valD={'DeltaV':[5000,10000,15000,20000], 
                                              'MassFrac':[0.86,0.88,0.9,0.92,0.94,0.95, .96, .97],
                                              'GLOW':[500000, 600000, 800000, 1000000, 1200000]    },
                             plot_paramL=['DeltaV','GLOW'], num_ticks=6)

png_name = __file__[:-2] + 'png'
rp.savefig(png_name, dpi=120)
rp.show()

