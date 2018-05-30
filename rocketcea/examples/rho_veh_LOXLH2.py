
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ReferenceStage

rp = RhoIspPlot(bipropL=[('LOX','LH2')], nsteps_sg=119, nsteps_isp=119, 
               legend_loc='upper right', eps=84, Pc=475,
               show_mr_on_plot=True, show_frozen_isp_on_plot=True)


stg_obj = ReferenceStage( WtPayload=20000.0, volCuInRef=2185*1728, WtInertRef=5428.0,
                          Name='Centaur Upper Stg' )

#rp.add_rho_isp_contours(label_frac_pos=0.2)

rp.add_stage_param_contours( stg_obj, set_param='VolPropellant', param_value=2185*1728,
                             label_frac_posD={'GLOW':0.4, 'DeltaV':.4},
                             plot_param_valD={'DeltaV':[10000, 11000, 12000, 13000, 14000, 15000, 16000, 17000]},
                             plot_paramL=['GLOW','DeltaV'], num_ticks=6)
    

png_name = __file__[:-2] + 'png'
rp.savefig(png_name, dpi=120)
rp.show()


