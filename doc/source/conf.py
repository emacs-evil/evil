# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
#

import os, sys
sys.path.append(os.path.abspath("./_ext"))


# -- Project information -----------------------------------------------------

project = 'Evil'
copyright = '2011-2019, Eivind Fonn, Frank Fischer, Vegard Øye'
author = 'Eivind Fonn, Frank Fischer, Vegard Øye'

# The full version, including alpha/beta/rc tags
release = '1.15.0'

master_doc = 'index'


latex_elements = {
    'fontpkg': r'\usepackage{palatino} \usepackage{inconsolata}',
    'maketitle': r"""
    \newcommand\sphinxbackoftitlepage{{
      Copyright {copyright}.

      \begin{{quote}}
      Permission is granted to copy, distribute and/or modify this
      document under the terms of the GNU Free Documentation License,
      Version 1.3 or any later version published by the Free Software
      Foundation; with no Invariant Sections, no Front-Cover Texts,
      and no Back-Cover Texts.  A copy of the license is included in
      the section entitled ``GNU Free Documentation License''.
      \end{{quote}}

      The Evil team thanks everyone at gmane.emacs.vim-emulation for
      their feedback and contributions.
    }}
    \sphinxmaketitle
    """.format(copyright=copyright),
}


texinfo_documents = [
    (master_doc, 'evil', 'Evil documentation', author, 'evil',
     'Extensible vi layer for Emacs', 'Emacs'),
]


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'elisp',
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']
