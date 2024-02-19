python_sources = [
    '__init__.py',
    '_version.py',
    'blends.py',
    'cea_obj_w_units.py',
    'cea_obj.py',
    'clean_init.py',
    'excel_const.py',
    'find_mingw_lib.py',
    'Goal.py',
    'input_cards.py',
    'Isp.py',
    'separated_Cf.py',
    'short_win_path.py',
    'show_html_help.py',
    'units.py',
    'xlChart.py',
    'xlChFormula.py',
]

py.install_sources(
  python_sources,
  pure: false,
  subdir: 'rocketcea'
)

subdir('biprop_utils')
subdir('tests')
subdir('examples')
