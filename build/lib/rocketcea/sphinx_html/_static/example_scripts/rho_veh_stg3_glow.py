
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ReferenceStage

rp = RhoIspPlot(bipropL=[('LOX','LH2'), ('LOX','RP1'), ('LOX','CH4')], Pc=735., eps=27.5)

stg_obj = ReferenceStage( WtPayload=260000.0, volCuInRef=11180*1728, WtInertRef=29300.0,
                          Name='Saturn V 3rd stg')

rp.add_rho_isp_contours(label_frac_pos=0.2)

rp.add_stage_param_contours( stg_obj, set_param='GLOW', param_value=524300.0,
                             label_frac_posD={ 'CubicFt':.4, 'DeltaV':.5},
                             plot_param_valD={ 'CubicFt':[4000, 6000, 8000, 10000, 12000],
                                               'DeltaV' :[7000, 7500, 8000, 8500] },
                             plot_paramL=['DeltaV','CubicFt'], num_ticks=6)

png_name = __file__[:-2] + 'png'
rp.savefig(png_name, dpi=120)
rp.show()

