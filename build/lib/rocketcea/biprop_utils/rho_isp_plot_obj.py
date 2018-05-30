import pylab as plt
import numpy as np
from rocketcea.biprop_utils.InterpProp_scipy import InterpProp
from math import exp, log

from rocketcea.biprop_utils.density_at_mr import bulkDensity, add_prop_sg, got_sg, get_sg
from rocketcea.cea_obj import CEA_Obj

from rocketcea.biprop_utils.mr_t_limits import MR_Temperature_Limits
from rocketcea.biprop_utils.mr_peak_at_eps_pc import MR_Peak_At_EpsPc
from rocketcea.biprop_utils.run_cea_w_error_corr import  run_cea_odf

from rocketcea.biprop_utils.calc_plot_range import calcMinMaxRange
from rocketcea.biprop_utils.contour_supt import getLocalAngle, label_frac_ipos
from rocketcea.biprop_utils.back_terp import find_first_terp

colorL = ['r','g','b','darkcyan','deepskyblue','darkorange','brown','deeppink',
    'maroon','crimson','seagreen','fuchsia','darkviolet' ]

def add_frozen_isp_to_mr_obj( mr_obj,  Pc=500., eps=20. ):

    oxName = mr_obj.mrLimitsObj.oxName
    fuelName = mr_obj.mrLimitsObj.fuelName

    mr_obj.isp_frozenL = [] # build an Isp frozen list
    mr_obj.mr_frozenL = [] # build an Isp frozen list
    mr_obj.sg_frozenL = [] # build an Isp frozen list
    
    for imr,mr in enumerate( mr_obj.mrL ):
        isp_frozen = run_cea_odf(mr_obj.mrLimitsObj.ispODEObj, Pc=Pc, MR=mr, eps=eps )
        if isp_frozen > mr_obj.isp_min:
            mr_obj.isp_frozenL.append( isp_frozen )
            mr_obj.mr_frozenL.append( mr )
            
            sg = bulkDensity(oxName, fuelName, mr)
            mr_obj.sg_frozenL.append( sg )

def add_sg_isp_to_mr_obj( mr_obj ):
    
    oxName = mr_obj.mrLimitsObj.oxName
    fuelName = mr_obj.mrLimitsObj.fuelName
    
    mr_obj.sgL = [] # build a bulkDensity list
    mr_obj.sg_ispL = [] # build a rho * Isp list
    
    for imr,mr in enumerate( mr_obj.mrL ):
        sg = bulkDensity(oxName, fuelName, mr)
        mr_obj.sgL.append( sg )
        
        mr_obj.sg_ispL.append( sg * mr_obj.isL[imr] )
        
    mr_obj.sg_min = min( mr_obj.sgL )
    mr_obj.sg_max = max( mr_obj.sgL )

def get_label_coordinates( mr_obj, sg_offset=0., is_offset=0.):
    sg = (mr_obj.sg_min + mr_obj.sg_max)/2.0 + sg_offset
    #isp = (mr_obj.isp_min + mr_obj.isp_max)/2.0 + is_offset
    isp = mr_obj.isp_max
    
    oxName = mr_obj.mrLimitsObj.oxName
    fuelName = mr_obj.mrLimitsObj.fuelName
    desc = oxName + '/' + fuelName
    
    return sg, isp, desc

def min_max( M ): # return the min and max in matrix
    lo = np.min(M)
    hi = np.max(M)
    return lo, hi

class RhoIspPlot( object ):

    def __init__(self, figsize=(10,8), dpi=70, Pc=500., eps=20.,
                 nsteps_sg=100, nsteps_isp=100, legend_loc='best', show_mr_on_plot=False,
                 show_frozen_isp_on_plot=False,
                 bipropL=None,
                 monopropL=None,
                 spec_gravD=None):
        
        if bipropL is None:
            bipropL = [('LOX','LH2'), ('N2O4','MMH'), ('LOX','RP1'), ('FLOX80','CH4'),('IRFNA','MHF3'),
                       ('LOX','CH4'), ('F2','H2'), ('F2','N2H4'), ('CLF5','N2H4'),('N2F4','N2H4')]
                     
        self.figsize = figsize
        self.dpi = dpi
        self.Pc = Pc
        self.eps = eps
        
        self.headline = 'Pc=%g psia, Area Ratio=%g:1'%(self.Pc, self.eps)
        
        self.nsteps_sg = nsteps_sg  # controls grid on plot for interplating stage properties
        self.nsteps_isp = nsteps_isp
        
        # 'best', 'upper right', 'right', 'center', 'lower center', 'center right', etc.
        self.legend_loc = legend_loc 
        self.show_mr_on_plot = show_mr_on_plot
        self.show_frozen_isp_on_plot = show_frozen_isp_on_plot
        
        self.bipropL = bipropL
        
        if monopropL is None:
            monopropL = []
        self.monopropL = monopropL
        self.monoprop_sgIspD = {} #index:name, value:(sg, Isp)
        
        self.fig = plt.figure(figsize=figsize, dpi=dpi) # figsize=(NxM) inches
        self.added_props_to_plot = False

        if spec_gravD is not None:
            for key, val in spec_gravD.items():
                add_prop_sg(name, val)

        self.mr_objL = []
        
        self.isp_min = 9999.0 # initialize extents of plot
        self.isp_max = 0.0
        self.sg_min = 9999.0
        self.sg_max = 0.0
        
        for (oxName, fuelName) in bipropL:
            
            if not got_sg(oxName):
                raise Exception('%s Specific Gravity Missing...\n'%oxName +\
                                '   add to spec_gravD["%s"]'%oxName)
            if not got_sg(fuelName):
                raise Exception('%s Specific Gravity Missing...\n'%fuelName +\
                                '   add to spec_gravD["%s"]'%fuelName)
            
            tobj = MR_Temperature_Limits(oxName=oxName, fuelName=fuelName,
            TC_LIMIT=1500.0, PcNominal=Pc, epsNominal=eps,
            MR_MIN=0.0, MR_MAX=200.0)
            
            
            self.mr_objL.append( MR_Peak_At_EpsPc(tobj, pc=Pc, eps=eps) )
            add_sg_isp_to_mr_obj( self.mr_objL[-1] )
            
            if show_frozen_isp_on_plot:
                add_frozen_isp_to_mr_obj( self.mr_objL[-1],  Pc=Pc, eps=eps )
            
            self.isp_min = min(self.isp_min, self.mr_objL[-1].isp_min)
            self.isp_max = max(self.isp_max, self.mr_objL[-1].isp_max)
            self.sg_min = min(self.sg_min, self.mr_objL[-1].sg_min)
            self.sg_max = max(self.sg_max, self.mr_objL[-1].sg_max)

        for propName in self.monopropL:
            sg = get_sg( propName )
            if sg is not None:
                self.sg_min = min(self.sg_min, sg)
                self.sg_max = max(self.sg_max, sg)
                
                ispObj = CEA_Obj( propName=propName)
                Isp = ispObj.get_Isp(Pc=Pc, eps=eps)

                self.isp_min = min(self.isp_min, Isp)
                self.isp_max = max(self.isp_max, Isp)
                
                self.monoprop_sgIspD[propName] = (sg, Isp)
                
            

        self.rhobRange = self.sg_max - self.sg_min
        self.ispRange = self.isp_max - self.isp_min

    def add_stage_param_contours(self, stg_obj, set_param='DeltaV', param_value=2000.0,
                                 plot_paramL=None, # can be any of ['DeltaV', 'GLOW', 'VolPropellant', 'WtPropellant', 'WtInert']
                                 plot_param_valD=None, # e.g. {'GLOW':[100, 200, 300]}
                                 label_frac_posD=None, # e.g. {'GLOW':0.1}
                                 num_ticks=10): # used in "pretty" ranges
        
        if plot_param_valD is None:
            plot_param_valD = {}
            
        if label_frac_posD is None:
            label_frac_posD = {}
        
        if plot_paramL is None:
            if set_param != 'GLOW':
                plot_paramL = ['GLOW']
            else:
                plot_paramL = ['DeltaV']
        
        line2 = 'Pc=%g psia, Area Ratio=%g:1'%( self.Pc, self.eps)
        callD = {set_param:param_value} # e.g. {'DeltaV':2100.0}
        
        if set_param == 'DeltaV':
            self.headline = stg_obj.stage_desc +  '\nDeltaV=%g ft/sec, %s'%(param_value, line2)
            call_func = stg_obj.setDeltaV
        elif set_param == 'VolPropellant':
            s = '%g'%param_value
            if 'e' in s:
                s = '%g'%(param_value/1728.0,)
                self.headline = stg_obj.stage_desc + '\nVolPropellant=%s ft^3, %s'%(s, line2)
            else:
                self.headline = stg_obj.stage_desc + '\nVolPropellant=%g in^3, %s'%(param_value, line2)
                
            call_func = stg_obj.setVolPropellant
        elif set_param == 'WtPropellant':
            self.headline = stg_obj.stage_desc + '\nWtPropellant=%g lbm, %s'%(param_value, line2)
            call_func = stg_obj.setWtPropellant
        elif set_param == 'WtInert':
            self.headline = stg_obj.stage_desc + '\nWtInert=%g lbm, %s'%(param_value, line2)
            call_func = stg_obj.setWtInert
        elif set_param == 'GLOW':
            self.headline = stg_obj.stage_desc + '\nGLOW=%g lbm, %s'%(param_value, line2)
            call_func = stg_obj.setGLOW
        else:
            print('ERROR... did not recognize set_param="%s" in add_stage_param_contours'%set_param)
            sys.exit()
            
        # build a grid of values.
        #  set one of DeltaV, VolPropellant, WtPropellant, WtInert, calculate the rest.
        
        sgArr = np.linspace(self.sg_min, self.sg_max, self.nsteps_sg)
        isArr = np.linspace(self.isp_min, self.isp_max, self.nsteps_isp)
        #print 'sgArr =',sgArr
        #print 'isArr =',isArr
        #print '-'*55
        
        # create empty matrices to receive state data
        dvMatrix = np.empty(shape=(self.nsteps_sg, self.nsteps_isp), dtype=np.float64)
        volMatrix = np.empty(shape=(self.nsteps_sg, self.nsteps_isp), dtype=np.float64)
        wpMatrix = np.empty(shape=(self.nsteps_sg, self.nsteps_isp), dtype=np.float64)
        wiMatrix = np.empty(shape=(self.nsteps_sg, self.nsteps_isp), dtype=np.float64)
        glowMatrix = np.empty(shape=(self.nsteps_sg, self.nsteps_isp), dtype=np.float64)
        muMatrix = np.empty(shape=(self.nsteps_sg, self.nsteps_isp), dtype=np.float64)
        
        bad_ijL = [] # keep a list of i,j positions that fail
        
        for i, sg in enumerate(sgArr):
            for j, isp in enumerate(isArr):
                callD['sg'] = sg
                callD['Isp'] = isp
                call_func( **callD )
                
                if stg_obj.DeltaV is None:
                    bad_ijL.append( (i,j) )
                    dvMatrix[i][j] = 0
                    volMatrix[i][j] = 0
                    wpMatrix[i][j] = 0
                    wiMatrix[i][j] = 0
                    glowMatrix[i][j] = 0
                    muMatrix[i][j] = 0
                else:
                    dvMatrix[i][j] = stg_obj.DeltaV
                    volMatrix[i][j] = stg_obj.VolPropellant
                    wpMatrix[i][j] = stg_obj.WtPropellant
                    wiMatrix[i][j] = stg_obj.WtInert
                    glowMatrix[i][j] = stg_obj.GLOW
                    muMatrix[i][j] = stg_obj.MassFrac
                    
        #print 'bad_ijL =',bad_ijL
        #print 'glowMatrix'
        #for i in range(len(sgArr)):
        #    print glowMatrix[i]
        #print 'glow min, max=',min_max( glowMatrix )
        #print 'MassFrac min, max=',min_max( muMatrix )
        # ==================================            
        
        def build_a_contour_curve( target_val, M ):
            
            xyPlotL = [] # list of (x,y) pairs to be plotted i.e. (sg, Isp)
    
            for i, sg in enumerate(sgArr):
                is_ans = find_first_terp( target_val, M[i], isArr )
                #print 'is_ans =',is_ans
                if is_ans is not None:
                    xyPlotL.append( (sgArr[i], is_ans) )
    
            for j, isp in enumerate(isArr):
                sg_ans = find_first_terp( target_val, M[:,j], sgArr )
                #print 'sg_ans =',sg_ans
                if sg_ans is not None:
                    xyPlotL.append( (sg_ans, isArr[j]) )
            
            if xyPlotL:
                xyPlotL.sort()
            return xyPlotL
        # ==================================            
        
        def build_contour_curves( valueL, M):
            
            xyL = [] # list of tuples (val, xyPlotL)
            for val in valueL:
                #print 'making xyPlotL for val =',val
                xyPlotL = build_a_contour_curve( val, M )
                if xyPlotL:
                    xyL.append( (val, xyPlotL) )
                
            return xyL
        # ==================================            
        
        def add_curves_to_figure( xyL, color=(.5,0,0), alpha=0.5, 
                                  label='', units='lbm', 
                                  line_style='--', label_frac_pos=0.3):
            
            for (val, xyPlotL) in xyL:
                
                xL = [v[0] for v in xyPlotL]
                yL = [v[1] for v in xyPlotL]
            
                s = '%g %s'%(val, units)
                bbox = {'fc':(1,1,1), 'pad':0, 'facecolor':'w', 'edgecolor':'w', 'alpha':alpha} 
                propsD = {'ha':'center', 'va':'center', 'bbox':bbox}
                
                if label:
                    plt.plot(xL, yL, line_style, color=color, label=label)
                    label = ''
                else:
                    plt.plot(xL, yL, line_style, color=color)
                
                
                #ipos = min(len(xL)-1,  max(0, int( label_frac_pos * len(xL) )))
                ipos = label_frac_ipos( xL, yL, self.rhobRange, self.ispRange,  label_frac=label_frac_pos )
                
                rotationAng = getLocalAngle( xL[ipos], xL, yL, self.rhobRange, self.ispRange, self.figsize  )
                
                plt.text(xL[ipos], yL[ipos], s, fontdict=propsD, rotation=rotationAng, color=color)
        # ==================================            
        
        
        def build_valueL_xyL( pname, M ):
            if pname in plot_param_valD:
                valueL = plot_param_valD[pname]
            else:
                lo, hi = min_max( M )
                lo, hi, step = calcMinMaxRange(lo, hi, num_ticks=num_ticks)
                #print 'lo, hi, step =',lo, hi, step
                valueL = list( np.arange(lo, hi, step) )
                
            xyL = build_contour_curves( valueL, M)
            return xyL
        
        # plot each of the parameters
        if 'DeltaV' in plot_paramL:
            xyL = build_valueL_xyL( 'DeltaV', dvMatrix )
            add_curves_to_figure( xyL, color=(0,0.5,0.5), alpha=0.5,
                                  label=r'$\Delta V$' , units='ft/s', 
                                  line_style=':', label_frac_pos=label_frac_posD.get('DeltaV',0.3) )
            
        if 'GLOW' in plot_paramL:
            xyL = build_valueL_xyL( 'GLOW', glowMatrix )
            add_curves_to_figure( xyL, color=(.5,0,0), alpha=0.5, 
                                  label='GLOW',  units='lbm', 
                                  line_style=':', label_frac_pos=label_frac_posD.get('GLOW',0.3) )
            
        if 'VolPropellant' in plot_paramL:
            xyL = build_valueL_xyL( 'VolPropellant', volMatrix )
            add_curves_to_figure( xyL, color=(.5,0,.5), alpha=0.5, 
                                  label=r'$V_{prop}$',  units=r'$in^3$', 
                                  line_style='--', label_frac_pos=label_frac_posD.get('VolPropellant',0.3) )
            
        if 'CubicFt' in plot_paramL:
            xyL = build_valueL_xyL( 'CubicFt', volMatrix/1728.0 )
            add_curves_to_figure( xyL, color=(.5,0,.5), alpha=0.5, 
                                  label=r'$V_{prop}$',  units=r'$ft^3$', 
                                  line_style='--', label_frac_pos=label_frac_posD.get('CubicFt',0.3) )
            
        if 'WtPropellant' in plot_paramL:
            xyL = build_valueL_xyL( 'WtPropellant', wpMatrix )
            add_curves_to_figure( xyL, color=(0,.5,0), alpha=0.5, 
                                  label=r'$W_{prop}$',  units='lbm', 
                                  line_style='--', label_frac_pos=label_frac_posD.get('WtPropellant',0.3) )
                                  
        if 'WtInert' in plot_paramL:
            xyL = build_valueL_xyL( 'WtInert', wiMatrix )
            add_curves_to_figure( xyL, color=(0,0,.5), alpha=0.5, 
                                  label='Winert',  units='lbm', 
                                  line_style='--', label_frac_pos=label_frac_posD.get('WtInert',0.3) )

            
        if 'MassFrac' in plot_paramL:
            xyL = build_valueL_xyL( 'MassFrac', muMatrix )
            add_curves_to_figure( xyL, color=(.5,.5,0), alpha=0.5, 
                                  label=r'$\mu_{stg}$',  units=r'$\mu$', 
                                  line_style=':', label_frac_pos=label_frac_posD.get('MassFrac',0.3) )

        plt.legend( loc=self.legend_loc )
        
    
    def add_rho_isp_contours(self, label_frac_pos=0.3, num_ticks=10):
        
        sgArr = np.linspace(self.sg_min, self.sg_max, self.nsteps_sg)
        isArr = np.linspace(self.isp_min, self.isp_max, self.nsteps_isp)
        
        dsg = self.rhobRange / self.nsteps_sg
        disp = self.ispRange / self.nsteps_isp
        
        sgIslo, sgIshi, sgIsstep = calcMinMaxRange(self.sg_min*self.isp_min, self.sg_max*self.isp_max, num_ticks=num_ticks)
        label = r'$\rho$ Isp'

        val = sgIslo + sgIsstep
        while val < sgIshi:
            xyPlotL = []
            for sg in sgArr:
                isp_test = val / sg
                if isp_test>self.isp_min and isp_test<self.isp_max:
                    xyPlotL.append( (sg, isp_test) )
            for isp in isArr:
                sg_test = val / isp
                if sg_test>self.sg_min and sg_test<self.sg_max:
                    xyPlotL.append( (sg_test, isp) )
            xyPlotL.sort()
            
            c = (.2,.2,.2,.2)
            s = '%g %s'%(val, r'$\rho$ Isp')
            bbox = {'fc':(1,1,1,0.8), 'pad':0, 'facecolor':'w', 'edgecolor':'w'} 
            propsD = {'ha':'center', 'va':'center', 'bbox':bbox}

            if xyPlotL:
                xL = [xy[0] for xy in xyPlotL]
                yL = [xy[1] for xy in xyPlotL]
                plt.plot(xL, yL, '--', color=c)

                
                if label:
                    plt.plot(xL, yL, '--', color=c, label=label)
                    label = ''
                else:
                    plt.plot(xL, yL, '--', color=c)

                
                #ipos = min(len(xL)-1,  max(0, int( label_frac_pos * len(xL) )))
                ipos = label_frac_ipos( xL, yL, self.rhobRange, self.ispRange, label_frac=label_frac_pos )

                rotationAng = getLocalAngle( xL[ipos], xL, yL, self.rhobRange, self.ispRange, self.figsize  )
                
                plt.text(xL[ipos], yL[ipos], s, fontdict=propsD, rotation=rotationAng, color='k')
                    
            val += sgIsstep


    def add_propellants(self):
        if self.added_props_to_plot:
            return

        # properties for MR labels
        bbox = {'fc':(1,1,1,0.8), 'pad':4, 'facecolor':'w', 'edgecolor':'w'} 
        mr_propsD = {'ha':'center', 'va':'bottom', 'bbox':bbox, 'fontsize':14}
        mr_frozen_propsD = {'ha':'center', 'va':'top', 'bbox':bbox, 'fontsize':14}

        self.added_props_to_plot = True
        
        for i, obj in enumerate( self.mr_objL ):
            c = colorL[i % len(colorL)]
            plt.plot(obj.sgL, obj.isL, '-', color=c, linewidth=3)
            
            if self.show_frozen_isp_on_plot:
                plt.plot(obj.sg_frozenL, obj.isp_frozenL, '-', color=c, linewidth=3)
            
            sg, isp, desc = get_label_coordinates( obj, sg_offset=0., is_offset=0.)
            propsD = {'ha':'center', 'va':'bottom', 'color':c, 'weight':'bold', 'fontsize':12}
            plt.text(sg, isp, desc, fontdict=propsD)
            
            if self.show_mr_on_plot:
                # add MR labels to curve
                ileft = int(len(obj.mrL)/10)
                iright = len(obj.mrL) - int(ileft/2)
                mrlo, mrhi, mrstep = calcMinMaxRange(obj.mrL[0], obj.mrL[-1], num_ticks=10)
                mr_isp_terp = InterpProp(obj.mrL, obj.isL)
                mr_sg_terp = InterpProp(obj.mrL, obj.sgL)
                
                mr = mrlo
                #print 'showing MR for',desc,'mrlo, mrhi, mrstep=',mrlo, mrhi, mrstep
                s = 'MReq='
                while mr < mrhi+mrstep/2.:
                    if mr>=obj.mrL[ileft] and mr<=obj.mrL[iright]:
                        sg = mr_sg_terp( mr )
                        Isp = mr_isp_terp( mr )
                        
                        rotationAng = getLocalAngle( sg, obj.sgL, obj.isL, self.rhobRange, self.ispRange, self.figsize  )
                        plt.text( sg, Isp, s+'%g'%mr, fontdict=mr_propsD, rotation=rotationAng, color=c)
                        s = ''
                    
                    mr += mrstep
            
                if self.show_frozen_isp_on_plot:
                    ileft = int(len(obj.mr_frozenL)/10)
                    iright = len(obj.mr_frozenL) - int(ileft/2)
                    mrlo, mrhi, mrstep = calcMinMaxRange(obj.mr_frozenL[0], obj.mr_frozenL[-1], num_ticks=6)
                    mr_isp_terp = InterpProp(obj.mr_frozenL, obj.isp_frozenL)
                    mr_sg_terp = InterpProp(obj.mr_frozenL, obj.sg_frozenL)
                    
                    mr = mrlo
                    #print 'showing MR for',desc,'mrlo, mrhi, mrstep=',mrlo, mrhi, mrstep
                    s = 'MRfroz='
                    while mr < mrhi+mrstep/2.:
                        if mr>=obj.mr_frozenL[ileft] and mr<=obj.mr_frozenL[iright]:
                            sg = mr_sg_terp( mr )
                            Isp = mr_isp_terp( mr )
                            
                            rotationAng = getLocalAngle( sg, obj.sg_frozenL, obj.isp_frozenL, self.rhobRange, self.ispRange, self.figsize  )
                            plt.text( sg, Isp, s+'%g'%mr, fontdict=mr_frozen_propsD, rotation=rotationAng, color=c)
                            s = ''
                        
                        mr += mrstep
                    
            
            
        for propName, (sg, Isp) in self.monoprop_sgIspD.items():
            
            plt.plot([sg], [Isp], 'o', color='b', markersize=10)
            propsD = {'ha':'center', 'va':'bottom', 'color':'b', 'weight':'bold'}
            plt.text(sg, Isp+2, propName, fontdict=propsD)
            
        
        plt.xlabel('Specific Gravity (g/ml)')
        plt.ylabel('CEA IspVac (sec)')
        plt.title(self.headline)
        
    def show(self):
        self.add_propellants()
        plt.show()

    def savefig(self, fname, dpi=120):
        self.add_propellants()
        plt.savefig(fname, dpi=dpi)

if __name__=="__main__":
    from veh_stage_obj import ConstMassFracStage
    
    #rp = RhoIspPlot()
    rp = RhoIspPlot(bipropL=[('LOX','LH2'),('N2O4','MMH')], nsteps_sg=90, nsteps_isp=90)
    
    stg_obj = ConstMassFracStage( mass_frac=0.8, WtPayload=1000.0 )
    
    rp.add_rho_isp_contours()
    #rp.add_stage_param_contours( stg_obj, set_param='GLOW', param_value=2000.0,
    #                             plot_paramL=['DeltaV'])
    rp.add_stage_param_contours( stg_obj, set_param='DeltaV', param_value=7200.0,
                                 plot_paramL=['GLOW'], num_ticks=16)
    
    rp.show()