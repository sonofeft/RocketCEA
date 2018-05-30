
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ConstMassFracStage

rp = RhoIspPlot(bipropL=[('LOX','LH2'),('N2O4','MMH')], nsteps_sg=90, nsteps_isp=90)

stg_obj = ConstMassFracStage( mass_frac=0.8, WtPayload=1000.0 )

rp.add_rho_isp_contours()

rp.add_stage_param_contours( stg_obj, set_param='DeltaV', param_value=7200.0,
                             plot_paramL=['GLOW'], num_ticks=16)
    
rp.savefig('const_mass_frac1.png', dpi=120)
rp.show()

