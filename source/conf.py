# At top on conf.py (with other import statements)
import recommonmark
from recommonmark.transform import AutoStructify

# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'TextBook'
copyright = '2024, Alfred Xiang'
author = 'Alfred Xiang'
release = 'v0.1'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = ['recommonmark','sphinx_markdown_tables','sphinx_rtd_dark_mode']

templates_path = ['_templates']
exclude_patterns = []

language = 'zh_CN'

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']


# At the bottom of conf.py
def setup(app):
        app.add_config_value('recommonmark_config', {
            'enable_math':True,
            'enable_inline_math':True,
            'auto_toc_tree_section': '目录',
            'enable_auto_toc_tree': True},True)
        app.add_transform(AutoStructify)

