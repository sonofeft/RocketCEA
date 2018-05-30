
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ConstMassFracStage

rp = RhoIspPlot(bipropL=[('LOX','LH2'),('N2O4','MMH')], nsteps_sg=90, nsteps_isp=90)

stg_obj = ConstMassFracStage( mass_frac=0.8, WtPayload=1000.0 )

rp.add_rho_isp_contours()

rp.add_stage_param_contours( stg_obj, set_param='GLOW', param_value=2000.0,
                             plot_paramL=['DeltaV'],
                             plot_param_valD={'DeltaV':[4500., 5000., 5500., 6000., 6500., 7000.]})
    
rp.savefig('const_mass_frac2.png', dpi=120)
rp.show()

