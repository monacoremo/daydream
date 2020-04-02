project = 'Daydream'
label = project + ' Documentation'
copyright = '2020, Remo'
author = 'Remo'

# The short X.Y version
version = '0.1'
# The full version, including alpha/beta/rc tags
release = '0.1 alpha'


# -- General configuration ---------------------------------------------------

extensions = []
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'index'
language = None
exclude_patterns = ['_build']
pygments_style = None


# -- Options for HTML output -------------------------------------------------

html_theme = 'alabaster'
html_static_path = ['_static']


# -- Options for LaTeX output ------------------------------------------------

latex_elements = {
    'papersize': 'a4paper',
    'pointsize': '10pt',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    (master_doc, project + '.tex', label, author, 'manual'),
]


# -- Options for manual page output ------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, project.lower(), label, [author], 1)
]


# -- Options for Epub output -------------------------------------------------

epub_title = project
epub_exclude_files = ['search.html']
