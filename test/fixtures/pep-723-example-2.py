"""
docstring for module with Windows file encoding
"""
# /// script
# dependencies = [
#   "humanize",
#   "tzdata",
# ]
# ///

from datetime import datetime
from zoneinfo import ZoneInfo

import humanize

# other code does not matter; I'm trying to get the LSP server to
# resolve the import. If it does, then it has used the correct venv.
#

print (humanize.naturaltime(datetime.now(ZoneInfo("UTC"))))
