# Building the documentation

## Pre-requisites

```bash
brew install sphinx-doc

Ubuntu 20.04
apt-get install python3-pip python3-dev python3-setuptools apache2 -y
pip3 install --upgrade pip
```

Ensure any post install steps are executed.

```bash
sphinx-build -b html . builddir
pip3 install sphinx-rtd-theme
pip3 install recommonmark
pip3 install sphinx_markdown_tables --user
pip3 install sphinxemoji --user
```

## Building documentation

```bash
sphinx-build doc html
```

Open the documentation at `html/index.html`
