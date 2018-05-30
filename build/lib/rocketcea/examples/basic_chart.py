
from rocketcea.biprop_utils.rho_isp_plot_obj import RhoIspPlot

rp = RhoIspPlot()

rp.add_rho_isp_contours(label_frac_pos=0.4)

png_name = __file__[:-2] + 'png'
rp.savefig(png_name, dpi=120)
rp.show()

