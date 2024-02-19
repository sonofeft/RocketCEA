project('rocketcea', 'c',
    meson_version: '>=0.64.0',
)

add_languages('fortran')

py_mod = import('python')
py = import('python').find_installation(pure: false)
py_dep = py.dependency()

incdir_numpy = run_command(py,
  ['-c', 'import os; os.chdir(".."); import numpy; print(numpy.get_include())'],
  check : true
).stdout().strip()

incdir_f2py = run_command(py,
    ['-c', 'import os; os.chdir(".."); import numpy.f2py; print(numpy.f2py.get_include())'],
    check : true
).stdout().strip()

inc_np = include_directories(incdir_numpy, incdir_f2py)

pycea_source = custom_target('py_ceamodule.c',
  input: ['rocketcea/py_cea.f'],
  output: ['py_ceamodule.c', 'py_cea-f2pywrappers.f'],
  command : [py, '-m', 'numpy.f2py', '@INPUT@', '-m', 'py_cea', '--lower']
)

py.extension_module(
    'py_cea',
    ['rocketcea/py_cea.f', pycea_source],
    incdir_f2py / 'fortranobject.c',
    include_directories: inc_np,
    dependencies: py_dep,
    install: true,
    subdir: 'rocketcea',
)
