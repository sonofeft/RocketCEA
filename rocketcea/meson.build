pycea_source = custom_target('py_ceamodule.c',
  input: ['py_cea.f'],
  output: ['py_ceamodule.c', 'py_cea-f2pywrappers.f'],
  command : [py, '-m', 'numpy.f2py', '@INPUT@', '--build-dir', '@OUTDIR@', '-m', 'py_cea', '--lower']
)

py.extension_module(
    'py_cea',
    ['py_cea.f', pycea_source],
    incdir_f2py / 'fortranobject.c',
    include_directories: inc_np,
    dependencies: py_dep,
    install: true,
    subdir: 'rocketcea',
)

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

other_sources = [
  'f.inp', 
  'f.out', 
  'thermo.inp',
  'thermo.lib',
  'thermo.out', 
  'temp.dat',
  'trans.inp', 
  'trans.lib',
  'trans.out', 
  'py_cea.f', 
  'py_cea.inc',
]

py.install_sources(
  [python_sources, other_sources],
  pure: false,
  subdir: 'rocketcea'
)

subdir('biprop_utils')
subdir('tests')
subdir('examples')
