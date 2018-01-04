c = get_config()

c.TerminalInteractiveShell.editor = "emacsclient -ta ''"

c.InteractiveShellApp.exec_lines = [
    'import pint\n',
    'ur = pint.UnitRegistry()\n',
    'Q_ = ur.Quantity\n',

    'import itertools, sys, os\n',
]
