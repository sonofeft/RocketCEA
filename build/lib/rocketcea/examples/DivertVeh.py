
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot
from rocketcea.biprop_utils.veh_stage_obj import ReferenceStage


stg_obj = ReferenceStage(volCuInRef=441.0 , WtInertRef=20.0, WtPayload=30.0,
        Name='N2H4 DACS')
        
rp = RhoIspPlot(bipropL=[('N2O4','MMH'), ('CLF5','N2H4')], 
                monopropL=['HYD40', 'HAN315', 'HAN269'])


rp.add_rho_isp_contours(label_frac_pos=0.2)

rp.add_stage_param_contours( stg_obj, set_param='GLOW', param_value=64.0,
                             label_frac_posD={'GLOW':0.9, 'VolPropellant':.4},
                             plot_param_valD={'DeltaV':[1900, 2100, 2300, 2500, 2700, 2900, 3100, 3300], 
                                              'MassFrac':[.44, .46, .48, .5, .52],
                                              'VolPropellant':[350, 375, 400, 425, 450]},
                             plot_paramL=['DeltaV','VolPropellant'], num_ticks=6)
    
rp.savefig('divert_veh.png', dpi=120)
rp.show()

