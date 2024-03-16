project('rocketcea', 'c',
    meson_version: '>=0.64.0',
)

add_languages('fortran')

py_mod = import('python')
py = import('python').find_installation(pure: false)
py_dep = py.dependency()


# MinGW-W64
#add_project_link_arguments( '-lquadmath', '-static', language: 'c')
if host_machine.system() == 'windows' and meson.get_compiler('c').get_id() == 'gcc'
  add_project_link_arguments( '-lquadmath', '-static', language: 'c')
#  add_project_arguments('-mlong-double-64', language: 'c')
#  add_project_arguments('-DMS_WIN64', language: 'c')
endif


incdir_numpy = run_command(py,
  ['-c', 'import os; os.chdir(".."); import numpy; print(numpy.get_include())'],
  check : true
).stdout().strip()

incdir_f2py = run_command(py,
    ['-c', 'import os; os.chdir(".."); import numpy.f2py; print(numpy.f2py.get_include())'],
    check : true
).stdout().strip()

inc_np = include_directories(incdir_numpy, incdir_f2py)

subdir('rocketcea')


