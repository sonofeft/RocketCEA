
cd .\docs
sphinx-build -b html -d _build/doctrees  . _build/html
cd ..

xcopy /S .\docs\_build\html .\rocketcea\sphinx_html /y

python setup.py sdist

rem only upload source to PyPI
twine upload --repository testpypi --skip-existing dist/*.gz
